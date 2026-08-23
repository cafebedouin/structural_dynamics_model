% ============================================================================
% CONSTRAINT STORY: dignified_death__autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
 *   constraint_id: dignified_death__autonomy_primary
 *   human_readable: Autonomy-Primary Dignified Death: Medically Gated Final Authority
 *   domain: bioethics/medical_law/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the autonomy_primary reading of the
 *   dignified_death kernel as a single epsilon-invariant constraint. The
 *   standing arrangement under contest — the story's epsilon referent — is
 *   the medico-legal gatekeeping apparatus through which autonomy-framed
 *   aid-in-dying regimes actually operate: legislature-defined eligibility
 *   criteria (terminal-prognosis windows, capacity requirements, waiting
 *   periods), assessing and administering physicians whose willingness is a
 *   practical precondition, and state review and reporting machinery.
 *   Assessed by this reading's own lights — dignity resides in
 *   self-determination, so the suffering individual holds final authority
 *   over timing and method of death — the arrangement delivers a genuine good
 *   to one class (eligible patients receive a scheduled, protected, medically
 *   supported death) while conditioning the promised 'final' authority on
 *   gatekeeper approval: eligibility criteria deny exit to chronically and
 *   psychiatrically suffering patients whose agony fails the boundary,
 *   clinician discretion can veto formally qualifying applicants, and the
 *   procedural apparatus taxes dying applicants with delay, repetition, and
 *   scrutiny. Prohibition jurisdictions, where no access exists at all, are a
 *   DIFFERENT constraint with a different epsilon (see the
 *   prohibition_decomposition omega and network links); this story does not
 *   average across them. Claim and metrics are authored independently:
 *   claimed_type records the structural judgment (an autonomy norm entangled
 *   with medical gatekeeping — coordination and asymmetric extraction in one
 *   structure); the metrics record the arrangement's observed operation. KEY
 *   AGENTS (by structural relationship): - state_health_authorities:
 *   Agenda-setting regulator ([institutional]/[arbitrage]) — writes the
 *   eligibility criteria, collects fiscal and liability relief -
 *   medical_profession_gatekeepers: Administering gatekeeper
 *   ([institutional]/[arbitrage]) — holds the practical veto; collects
 *   expanded professional jurisdiction over dying -
 *   eligible_suffering_patients: Coordinated beneficiary
 *   ([powerless]/[trapped]) — receives the authorized exit the arrangement
 *   exists to provide - ineligible_suffering_patients: Primary target
 *   ([powerless]/[trapped]) — bears prolonged suffering against will behind
 *   the eligibility boundary - patient_family_members: Burden-bearing
 *   bystander ([moderate]/[constrained]) — absorbs care labor and grief;
 *   decision authority routes around them - disability_rights_advocates:
 *   Excluded objector ([organized]/[constrained]) — reads the criteria as a
 *   valuation of disabled life; no seat in criteria design -
 *   resource_poor_rural_patients: Formally eligible, practically excluded
 *   ([powerless]/[trapped]) — nominal access without reachable providers -
 *   bioethics_analysts: Analytical observer ([analytical]/[analytical]) —
 *   compares criteria drift and rhetoric-outcome gaps across jurisdictions
 *
 * KEY AGENTS:
 *   - state_health_authorities: agenda-setting regulator, institutional power, arbitrage exit — defines eligibility, collects fiscal and liability relief
 *   - medical_profession_gatekeepers: administering gatekeeper, institutional power, arbitrage exit — practical veto-holder, collects professional jurisdiction over dying
 *   - eligible_suffering_patients: coordinated beneficiary, powerless, trapped — receives the authorized exit
 *   - ineligible_suffering_patients: primary target, powerless, trapped — bears prolonged suffering against will
 *   - patient_family_members: burden-bearing bystander, moderate, constrained — absorbs care labor and grief
 *   - disability_rights_advocates: excluded objector, organized, constrained — no seat in criteria design
 *   - resource_poor_rural_patients: formally eligible, practically excluded, powerless, trapped
 *   - bioethics_analysts: analytical observer, analytical power, analytical exit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dignified_death__autonomy_primary, 0.52).
domain_priors:suppression_score(dignified_death__autonomy_primary, 0.62).
domain_priors:theater_ratio(dignified_death__autonomy_primary, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, extractiveness, 0.52).
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignified_death__autonomy_primary, tangled_rope).
narrative_ontology:human_readable(dignified_death__autonomy_primary, "Autonomy-Primary Dignified Death: Medically Gated Final Authority").
narrative_ontology:topic_domain(dignified_death__autonomy_primary, "bioethics/medical_law/political_philosophy").

domain_priors:requires_active_enforcement(dignified_death__autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignified_death__autonomy_primary, '19a59399-ac67-4a04-8120-124e673472f7').
narrative_ontology:cs_kernel_codification('19a59399-ac67-4a04-8120-124e673472f7', distributed).
narrative_ontology:cs_authority_grounding('19a59399-ac67-4a04-8120-124e673472f7', lineage).
narrative_ontology:cs_interpretation_layer_present('19a59399-ac67-4a04-8120-124e673472f7').
narrative_ontology:cs_reading_relation('19a59399-ac67-4a04-8120-124e673472f7', dignified_death__sanctity_primary, forecloses).
narrative_ontology:cs_reading_relation('19a59399-ac67-4a04-8120-124e673472f7', dignified_death__relational_autonomy, influences).
narrative_ontology:cs_axiom('19a59399-ac67-4a04-8120-124e673472f7', foundational, self_determination_constitutes_dying_dignity).
narrative_ontology:cs_axiom_status(self_determination_constitutes_dying_dignity, holdable).
narrative_ontology:cs_axiom_grounding('19a59399-ac67-4a04-8120-124e673472f7', self_determination_constitutes_dying_dignity, deontological).
narrative_ontology:cs_axiom('19a59399-ac67-4a04-8120-124e673472f7', foundational, sufferer_holds_final_authority_over_death_timing_and_method).
narrative_ontology:cs_axiom_status(sufferer_holds_final_authority_over_death_timing_and_method, holdable).
narrative_ontology:cs_axiom_grounding('19a59399-ac67-4a04-8120-124e673472f7', sufferer_holds_final_authority_over_death_timing_and_method, deontological).
narrative_ontology:cs_reference_frame('19a59399-ac67-4a04-8120-124e673472f7', individual_self_sovereignty_over_dying).
narrative_ontology:cs_drift_state('19a59399-ac67-4a04-8120-124e673472f7', contemporary_gated_enactment_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('19a59399-ac67-4a04-8120-124e673472f7', '').
narrative_ontology:cs_kernel_id(dignified_death__autonomy_primary, dignified_death).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignified_death__autonomy_primary, eligible_suffering_patients).
narrative_ontology:constraint_beneficiary(dignified_death__autonomy_primary, medical_profession_gatekeepers).
narrative_ontology:constraint_victim(dignified_death__autonomy_primary, ineligible_suffering_patients).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dignified_death__autonomy_primary, patient_family_members).
narrative_ontology:constraint_victim(dignified_death__autonomy_primary, patient_family_members).
narrative_ontology:constraint_vindicates(dignified_death__autonomy_primary, regulated_access_safeguard_doctrine).
narrative_ontology:constraint_vindicates(dignified_death__autonomy_primary, terminal_prognosis_eligibility_boundary).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafts and revises the statute defining who may request medical assistance in dying and under what conditions: eligibility criteria (terminal-prognosis windows, capacity requirements, waiting periods), provider licensing, and the review and reporting apparatus. Collects fiscal relief as authorized deaths shorten high-cost end-of-life care, and contains malpractice and criminal liability by routing every death through auditable state-designed procedure. Nothing binds it to the current boundary; it can redraw the criteria at will.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, state_health_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Assesses requests, certifies capacity and eligibility, prescribes or administers. No death proceeds without a willing physician, so the profession holds the practical veto regardless of statutory text. Conscientious objectors may decline entirely and keep practicing; participants bear moral distress, documentation load, and occasional legal exposure. The arrangement concentrates jurisdiction over dying inside medicine — a territorial expansion of the profession that precedes and outlasts any individual case.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, medical_profession_gatekeepers, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(dignified_death__autonomy_primary, medical_profession_gatekeepers, beneficiary).

% Faces a terminal prognosis inside the eligibility window, retains decisional capacity, and secures a willing assessor. For them the arrangement converts a forbidden act into a scheduled, supported, legally protected death on a date they help choose. Their access depends entirely on staying inside the criteria and finding a provider; step outside the window or lose capacity, and the door closes.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, eligible_suffering_patients, beneficiary,
    powerless, immediate, trapped, national).

% Suffers grievously — chronic non-terminal illness, psychiatric torment, advanced disability, or declining capacity — but fails an eligibility criterion, so the arrangement offers nothing except the continuation being fled. Remaining options are degraded: refusing food and fluids over weeks, traveling abroad at great cost if mobile and resourced, or clandestine acts exposing helpers to prosecution. The norm proclaiming final authority over death is precisely what defines them as unqualified to exercise it.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, ineligible_suffering_patients, payer,
    powerless, immediate, trapped, national).

% Absorbs the caregiving labor, witnesses the suffering, and grieves on both sides of the decision — some lobby for access, others plead against it, and the statute routes decision authority around them either way. They gain relief when suffering ends sooner and bear loss, suspicion of pressuring the patient, and, where they assist outside the law, criminal exposure.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, patient_family_members, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(dignified_death__autonomy_primary, patient_family_members, beneficiary).

% Organized opposition outside the design room: reads the eligibility criteria as a public statement that certain disabled lives are not worth continuing, and warns that expansion follows fiscal pressure. Testifies, litigates, and campaigns, but holds no seat on the review bodies that write the criteria their constituents live under.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, disability_rights_advocates, excluded,
    organized, biographical, constrained, national).

% Formally inside the eligibility class but practically outside the service: no willing provider within reach, no travel budget, thin palliative infrastructure. For them access exists on paper only, and the arrangement's benefits concentrate where providers cluster. Would object that a right they cannot reach is a statement about whose suffering counts.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, resource_poor_rural_patients, excluded,
    powerless, immediate, trapped, national).

% Studies the arrangement across jurisdictions: compares criteria drift, request demographics, oversight outcomes, and the gap between autonomy rhetoric and gated delivery. Publishes, advises commissions, and holds no stake in any particular death.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, bioethics_analysts, observer,
    analytical, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dignified_death__autonomy_primary, state_health_authorities).
narrative_ontology:fixing_cost_class(dignified_death__autonomy_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Channels voluntary-death requests through one regulated medical pathway: eligibility screening, capacity verification, waiting periods, provider administration, and state review replace clandestine suicide, ad hoc physician discretion, and criminalized assistance with a single auditable procedure a pluralistic society can inspect.
% TRANSFER_FUNCTION: Moves decision authority over death from the suffering individual to a gatekeeping chain (legislature-set criteria, assessing physicians, review committees); moves the timing of death from the individual's chosen moment to the earliest moment the apparatus permits; moves legal risk from patients and families to licensed institutions; and, through prognosis-linked eligibility, moves high-cost end-of-life care off institutional budgets earlier than natural dying would.
% ABSENT_VOICES: Disability-rights advocates, who read the eligibility criteria as a public valuation of disabled life and hold no seat on the bodies that write the criteria; ineligible chronic and psychiatric sufferers, whose testimony is structurally unheard because the criteria define their suffering as non-qualifying; poor and rural patients, for whom formal eligibility is practically void; and, in prohibition jurisdictions, the entire class of denied patients, who have no seat in any enactment conversation at all.
% DISAPPEARANCE_RATIONALE: If the gated arrangement vanished overnight, eligible patients would return to clandestine exits, refused sustenance, or unrelieved dying; physicians would lose the regulated channel and resume case-by-case discretion under criminal exposure; the state would lose its oversight data and its liability container; families would absorb both the suffering and the legal risk the arrangement currently intermediates. The end-of-life landscape reorganizes around whatever replaces it — which is why every party fights over the criteria rather than ignoring them.
% FOUNDING_PROBLEM: Clandestine, unregulated dying: terminally ill people ending their lives alone, violently, or at unaccountable physician discretion, helpers exposed to prosecution, and no social mechanism to distinguish requested release from abandonment or abuse. Early enactments (Oregon 1997, the Netherlands 2002) traded blanket prohibition for controlled access to solve it.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: pre-legislation coronial and forensic records documenting violent suicide methods among the terminally ill (Oregon pre-1997, Netherlands pre-2002 court cases), palliative-medicine literature documenting refractory suffering at life's end, and testimony of families who lost members to solitary violent exits. Disability-rights organizations — opponents of the arrangement — corroborate that clandestine dying was real while disputing that gated access answers it; theirs is the strongest available attestation because they collect nothing from either answer.
narrative_ontology:disappearance_verdict(dignified_death__autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(dignified_death__autonomy_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dignified_death__autonomy_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dignified_death__autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(dignified_death__autonomy_primary, 0.52, 'stealth/ox-alpha', 'none', direct).

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
 *   Epsilon 0.52 sits mid-band for this reading: the referent arrangement extracts substantially (denied exit for whole classes of sufferers, conditional rather than final authority, procedural taxation of the dying) while delivering a real coordination good (safe, regulated, legally protected access for eligible patients), so it is neither negligible nor confiscatory. Suppression 0.62 is authored as a raw structural property — unscaled by power or scope — reflecting that persistence depends on active machinery: statutory boundaries, backstop penalties for assistance outside the criteria, and provider scarcity functioning as passive enforcement; alternatives (voluntary stopping of eating and drinking, foreign travel, palliative sedation) survive but degraded, which is why suppression sits below confiscatory levels. Theater_ratio 0.38: serial review committees rarely overturn determinations, waiting periods are routinely waived or shortened, and documentation rituals largely legitimate decisions already made — while capacity assessment retains genuine protective function, so the ratio is elevated but not dominant. Accessibility_collapse 0.48: alternatives collapse almost completely for the immobile, impecunious, and cognitively declining, and only partially for everyone else. Resistance 0.55 is bidirectional — expansionist litigation against the criteria and restrictionist campaigns against expansion meet the arrangement from both sides, plus clinician refusals to participate. The three measurement series share one time grid (points 0, 5, 10, 15, 20, 25, 29) per the alignment rule; trajectories are monotonic rather than cyclical — no intermittent-reinforcement cycle appears in the record. Rising base_extractiveness models extraction accumulation as criteria ratchet outward while gatekeeping consolidates; the rising suppression_requirement series tracks genuine enforcement-capacity growth (reporting mandates, referral rules, provider-pool tightening), which is why it is authored as a series rather than left to the scalar. Receipt surface: gains demonstrably accrue to the state seat (fiscal relief, liability containment), and the cost class of fixing — reopening the eligibility settlement against organized opposition on both flanks — is prohibitive for the officeholders who could fix it.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setting seats and the payer seats should compute different types from identical metrics. From the state and profession seats the arrangement is prudent public medicine they designed: screened, safe, accountable, the criteria as considered policy. From the ineligible-patient seat the same structure is the prolongation of suffering administered under the banner of the very norm that promises final authority — the reading proclaiming self-determination is the one defining them as unqualified for it. Same-level divergence among the powerless: eligible and ineligible patients hold identical power atoms and identical trapped exit, yet sit on opposite sides of a criterion boundary neither chose; among the formally eligible, urban resourced patients and rural poor patients diverge completely on effective access despite identical legal status. The engine computes these divergences from the structural data; the authored claim adjudicates nothing.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the low end: eligible_suffering_patients receive the arrangement's core good (d near the beneficiary pole), and medical_profession_gatekeepers — listed as beneficiaries because the arrangement concentrates professional jurisdiction over dying inside medicine — derive low d from that declaration. The victim declaration drives the high end: ineligible_suffering_patients bear the arrangement's costs with trapped exit, placing them near the full-target pole. Two overrides correct derivations the structural data alone would misread. Institutional atom pinned to 0.25: both institutional seats are agenda-setters, and a derivation reading enforcement position alone would push them target-ward, but both are capture-side collectors — the state accrues fiscal relief and liability containment, the profession accrues jurisdiction — tempered by real borne costs (administrative load, moral distress), so d sits well short of the beneficiary pole. Moderate atom pinned to 0.55: family members appear in no beneficiary or victim array and would otherwise take a canonical fallback; their actual position is near-symmetric and slightly cost-weighted (care labor, grief, overridden preferences, against relief from shortened witnessing). Spatial scope is national across the operative seats, so scope amplification of extractiveness is modest and uniform.
 *
 * MANDATROPHY ANALYSIS:
 *   Classification discipline cuts both ways here. Reading the arrangement as pure coordination (its official self-description: compassionate, safeguarded access) erases the denied-exit victim set — the people the eligibility boundary defines as unqualified for the dignity the norm proclaims. Reading it as pure extraction (the sanctity opposition's characterization) erases the genuine good delivered to eligible patients and the real protective work capacity screening does. The tangled_rope claim holds both halves in one structure: the same gate that delivers an authorized death to one class delivers prolonged suffering to another, and active enforcement is what holds the boundary. On mandatrophy: the founding problem (clandestine, unregulated, persecuted dying) is contested rather than dead — proponents cite continuing unmet demand; opponents contend the arrangement manufactures its own necessity. The mandate has nonetheless drifted from catastrophe-prevention toward allocation-management of death itself, which is why the eligibility-ratchet omega carries the forward-looking risk: if criteria expand under fiscal pressure faster than suffering-class testimony enters the design room, extraction accumulation compounds and the coordination half thins.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of the dignified_death kernel (autonomy_primary). What would change structurally under the sibling readings, and where exactly is the disagreement located?',
    'Author the sibling stories (dignified_death__sanctity_primary, dignified_death__relational_autonomy) and compare victim sets, beneficiary sets, and epsilon at matched referents; the disagreement localizes to the locus-of-death-authority premise.',
    'Under sanctity_primary the victim set inverts (the choosing patient becomes the transgressor); under relational_autonomy the victim set expands to pressured vulnerable persons and authority redistributes to the triad — per-seat classifications computed from this story''s data do not transfer to siblings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one reading of the dignified_death kernel; siblings re-specify the victim set by relocating decision authority.').

omega_variable(
    eligibility_criteria_ratchet,
    'Do eligibility criteria ratchet monotonically outward (terminal-only toward chronic, psychiatric, and minority-age inclusion, on the Benelux trajectory), converting a narrow catastrophe-prevention mandate into general death-on-request?',
    'Longitudinal comparison of eligibility statutes and review-body rulings across jurisdictions and decades; track the proportion of assisted deaths following eligibility-expansion amendments.',
    'Continued ratchet supports the extraction-accumulation reading of the rising base_extractiveness series and thins the coordination half of the structure; a stable boundary would support the safeguard reading and lower epsilon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eligibility_criteria_ratchet, empirical, 'Whether the eligibility boundary expands monotonically under fiscal and advocacy pressure.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression structural (statutes, penalties, provider scarcity) or internalized (patients self-censor requests as burdensome; clinicians internalize an ethos that a death request signals failed care)?',
    'Compare request rates and request-withholding interviews across jurisdictions before and after liberalization; if withheld requests persist where legal barriers drop, the internalized component is substantial.',
    'If internalized, effective suppression exceeds the structural measure and persists after reform — the arrangement''s coercive force travels inside patients and clinicians rather than residing in the statute.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized components of the arrangement''s suppressive force.').

omega_variable(
    consent_authenticity_under_dependency,
    'For patients whose exit runs entirely through the gatekeeping apparatus and who depend on the caregivers and institutions the arrangement regulates, is the request for death authentic self-determination or adaptation to felt burden?',
    'Not resolvable by outcome data alone; requires structured qualitative study of decision contexts, triangulated across jurisdictions with differing safeguard designs. This is the exact fault line between this reading and relational_autonomy.',
    'Widespread burden-adaptation shrinks the genuine coordination good, raises epsilon, and shifts weight toward the relational reading''s procedural safeguards; robust authenticity confirms this reading''s premise.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consent_authenticity_under_dependency, conceptual, 'Authenticity of autonomous death requests under dependency — the autonomy/relational fault line.').

omega_variable(
    prohibition_decomposition,
    'Does the epsilon authored here (the gated enactment arrangement) also describe prohibition jurisdictions, or is prohibition a structurally distinct constraint?',
    'Decompose: author dignified_death__prohibition_regime as a separate story (denied exit with no compensating access, criminalized assistance, no oversight good) and link it in the network; compare epsilon at the shared reading.',
    'From this reading''s seat, prohibition computes as materially more extractive with no coordination half; merging the two arrangements into one story would contaminate epsilon and blur the victim set. Kept separate, each carries one stable epsilon.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(prohibition_decomposition, conceptual, 'Epsilon invariance: gated enactment and prohibition are two constraints, not one constraint measured two ways.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignified_death__autonomy_primary, 0, 29).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dign_tr_t0, dignified_death__autonomy_primary, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(dign_tr_t0, observed).
narrative_ontology:measurement(dign_tr_t5, dignified_death__autonomy_primary, theater_ratio, 5, 0.26).
narrative_ontology:measurement_basis(dign_tr_t5, observed).
narrative_ontology:measurement(dign_tr_t10, dignified_death__autonomy_primary, theater_ratio, 10, 0.3).
narrative_ontology:measurement_basis(dign_tr_t10, observed).
narrative_ontology:measurement(dign_tr_t15, dignified_death__autonomy_primary, theater_ratio, 15, 0.33).
narrative_ontology:measurement_basis(dign_tr_t15, observed).
narrative_ontology:measurement(dign_tr_t20, dignified_death__autonomy_primary, theater_ratio, 20, 0.36).
narrative_ontology:measurement_basis(dign_tr_t20, observed).
narrative_ontology:measurement(dign_tr_t25, dignified_death__autonomy_primary, theater_ratio, 25, 0.37).
narrative_ontology:measurement_basis(dign_tr_t25, observed).
narrative_ontology:measurement(dign_tr_t29, dignified_death__autonomy_primary, theater_ratio, 29, 0.38).
narrative_ontology:measurement_basis(dign_tr_t29, observed).

% Extraction over time
narrative_ontology:measurement(dign_be_t0, dignified_death__autonomy_primary, base_extractiveness, 0, 0.4).
narrative_ontology:measurement_basis(dign_be_t0, observed).
narrative_ontology:measurement(dign_be_t5, dignified_death__autonomy_primary, base_extractiveness, 5, 0.43).
narrative_ontology:measurement_basis(dign_be_t5, observed).
narrative_ontology:measurement(dign_be_t10, dignified_death__autonomy_primary, base_extractiveness, 10, 0.46).
narrative_ontology:measurement_basis(dign_be_t10, observed).
narrative_ontology:measurement(dign_be_t15, dignified_death__autonomy_primary, base_extractiveness, 15, 0.48).
narrative_ontology:measurement_basis(dign_be_t15, observed).
narrative_ontology:measurement(dign_be_t20, dignified_death__autonomy_primary, base_extractiveness, 20, 0.5).
narrative_ontology:measurement_basis(dign_be_t20, observed).
narrative_ontology:measurement(dign_be_t25, dignified_death__autonomy_primary, base_extractiveness, 25, 0.51).
narrative_ontology:measurement_basis(dign_be_t25, observed).
narrative_ontology:measurement(dign_be_t29, dignified_death__autonomy_primary, base_extractiveness, 29, 0.52).
narrative_ontology:measurement_basis(dign_be_t29, observed).

% Suppression requirement over time
narrative_ontology:measurement(dign_su_t0, dignified_death__autonomy_primary, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(dign_su_t0, observed).
narrative_ontology:measurement(dign_su_t5, dignified_death__autonomy_primary, suppression_requirement, 5, 0.53).
narrative_ontology:measurement_basis(dign_su_t5, observed).
narrative_ontology:measurement(dign_su_t10, dignified_death__autonomy_primary, suppression_requirement, 10, 0.56).
narrative_ontology:measurement_basis(dign_su_t10, observed).
narrative_ontology:measurement(dign_su_t15, dignified_death__autonomy_primary, suppression_requirement, 15, 0.58).
narrative_ontology:measurement_basis(dign_su_t15, observed).
narrative_ontology:measurement(dign_su_t20, dignified_death__autonomy_primary, suppression_requirement, 20, 0.6).
narrative_ontology:measurement_basis(dign_su_t20, observed).
narrative_ontology:measurement(dign_su_t25, dignified_death__autonomy_primary, suppression_requirement, 25, 0.61).
narrative_ontology:measurement_basis(dign_su_t25, observed).
narrative_ontology:measurement(dign_su_t29, dignified_death__autonomy_primary, suppression_requirement, 29, 0.62).
narrative_ontology:measurement_basis(dign_su_t29, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dignified_death__autonomy_primary, resource_allocation).
narrative_ontology:affects_constraint(dignified_death__autonomy_primary, dignified_death__sanctity_primary).
narrative_ontology:affects_constraint(dignified_death__autonomy_primary, dignified_death__relational_autonomy).
narrative_ontology:affects_constraint(dignified_death__autonomy_primary, dignified_death__prohibition_regime).

% DUAL FORMULATION NOTE:
% The colloquial label 'dignity in dying' decomposes into at least four structurally distinct constraints: this autonomy_primary reading (the gated enactment arrangement, epsilon 0.52, tangled profile); sanctity_primary (whose endorsed arrangement is prohibition — from its seat the violation falls on the terminating act itself); relational_autonomy (triad-distributed authority under procedural safeguards); and the prohibition regime as a standalone arrangement (highest epsilon from the autonomy seat — denied exit with no compensating access). Upstream/downstream: sanctity_primary is historically upstream — its doctrinal prohibitions built the legal baseline this reading contests; autonomy_primary influences relational_autonomy — the individualist reading's documented failure modes (felt-burden pressure, isolation of dependent patients) generate exactly the safeguards the relational reading formalizes. Every family member links the others via network.affects_constraints; epsilon is authored per reading and never averaged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dignified_death__autonomy_primary, institutional, 0.25).
constraint_indexing:directionality_override(dignified_death__autonomy_primary, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
