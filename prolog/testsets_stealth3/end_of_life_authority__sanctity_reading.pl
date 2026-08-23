% ============================================================================
% CONSTRAINT STORY: end_of_life_authority__sanctity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_authority__sanctity_reading, []).

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
 *   constraint_id: end_of_life_authority__sanctity_reading
 *   human_readable: Sanctity-of-Life Categorical Prohibition on Intentional Life-Ending
 *   domain: medical_ethics/end_of_life_policy
 *
 * SUMMARY:
 *   This file instantiates ONE reading of the end_of_life_authority kernel:
 *   the sanctity_reading, under which human life has intrinsic value and
 *   intentional life-ending is prohibited regardless of individual
 *   preference. The arrangement described is the standing categorical
 *   prohibition: criminal bans on assisted suicide and euthanasia, active
 *   enforcement machinery, and a physician role confined to life
 *   preservation. Per the epsilon-referent rule for kernel readings, epsilon
 *   is authored for THAT standing arrangement as THIS reading assesses it:
 *   the reading regards compelling continued life as the discharge of an
 *   absolute duty rather than a taking, so its own-lights epsilon sits low;
 *   the residual reflects the suffering the tradition itself acknowledges and
 *   the enforcement burden placed on conscientious actors. The sibling
 *   readings (autonomy_reading, slippery_slope_mechanism) are separate
 *   constraints in separate files with their own epsilon values and victim
 *   sets; nothing about them is averaged into this one. The claim/metric
 *   split is deliberate: the type is stated from structural analysis (a real
 *   trust-and-protection coordination function, identifiable payers, active
 *   enforcement, concentrated receipt of vindicated authority), while the
 *   metrics describe the arrangement as this reading honestly measures its
 *   operation.
 *
 * KEY AGENTS:
 *   - - legislature_criminal_authorities: Agenda setter (institutional/mobile) — writes and maintains the prohibition statutes; could amend or repeal
 *   - - religious_sanctity_institutions: Doctrinal beneficiary and historical agenda setter (organized/identity_locked) — collect vindicated authority; drive retention campaigns
 *   - - competent_suffering_patients: Primary target (powerless/trapped) — refused assistance; bear extended suffering
 *   - - pressure_vulnerable_groups: Dual-positioned seat (organized/constrained) — shielded beneficiaries where pressure is real, payers where preference is authentic; indistinguishable case-by-case
 *   - - palliative_care_establishment: Secondary beneficiary (organized/constrained) — receives mandated demand and funding argument strength
 *   - - licensed_physicians: Front-line administering enforcers (organized/identity_locked) — refuse requests, bear conscience and liability costs, defend the frame
 *   - - incompetent_stage_patients: Excluded voice (powerless/trapped) — governed prospectively, unable to participate
 *   - - bioethics_commissions: Analytical observer (institutional/analytical) — evidence reviews, no decision power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_authority__sanctity_reading, 0.2).
domain_priors:suppression_score(end_of_life_authority__sanctity_reading, 0.72).
domain_priors:theater_ratio(end_of_life_authority__sanctity_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_authority__sanctity_reading, tangled_rope).
narrative_ontology:human_readable(end_of_life_authority__sanctity_reading, "Sanctity-of-Life Categorical Prohibition on Intentional Life-Ending").
narrative_ontology:topic_domain(end_of_life_authority__sanctity_reading, "medical_ethics/end_of_life_policy").

domain_priors:requires_active_enforcement(end_of_life_authority__sanctity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_authority__sanctity_reading, '034d5c36-8e27-4288-bc31-e7698f2d5650').
narrative_ontology:cs_kernel_codification('034d5c36-8e27-4288-bc31-e7698f2d5650', formalized).
narrative_ontology:cs_authority_grounding('034d5c36-8e27-4288-bc31-e7698f2d5650', lineage).
narrative_ontology:cs_interpretation_layer_present('034d5c36-8e27-4288-bc31-e7698f2d5650').
narrative_ontology:cs_reading_relation('034d5c36-8e27-4288-bc31-e7698f2d5650', end_of_life_authority__autonomy_reading, forecloses).
narrative_ontology:cs_reading_relation('034d5c36-8e27-4288-bc31-e7698f2d5650', end_of_life_authority__slippery_slope_mechanism, influences).
narrative_ontology:cs_axiom('034d5c36-8e27-4288-bc31-e7698f2d5650', foundational, intentional_life_ending_categorical_impermissible).
narrative_ontology:cs_axiom_status(intentional_life_ending_categorical_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('034d5c36-8e27-4288-bc31-e7698f2d5650', intentional_life_ending_categorical_impermissible, deontological).
narrative_ontology:cs_axiom('034d5c36-8e27-4288-bc31-e7698f2d5650', secondary, physician_role_limited_to_life_preservation).
narrative_ontology:cs_axiom_status(physician_role_limited_to_life_preservation, holdable).
narrative_ontology:cs_axiom_grounding('034d5c36-8e27-4288-bc31-e7698f2d5650', physician_role_limited_to_life_preservation, conventional).
narrative_ontology:cs_reference_frame('034d5c36-8e27-4288-bc31-e7698f2d5650', categorical_life_preservation_duty).
narrative_ontology:cs_drift_state('034d5c36-8e27-4288-bc31-e7698f2d5650', contemporary_assisted_dying_expansion_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('034d5c36-8e27-4288-bc31-e7698f2d5650', '').
narrative_ontology:cs_kernel_id(end_of_life_authority__sanctity_reading, end_of_life_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, pressure_vulnerable_groups).
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, religious_sanctity_institutions).
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, palliative_care_establishment).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, competent_suffering_patients).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, pressure_vulnerable_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, licensed_physicians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts and maintains the criminal statutes prohibiting assisted suicide and euthanasia, and periodically debates amendments that usually fail. Holds the pen that could narrow or repeal the prohibition, and answers to lobbying from religious bodies, medical associations, disability organizations, and patient-advocacy coalitions.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, legislature_criminal_authorities, agenda_setter,
    institutional, generational, mobile, national).

% Teach that human life carries intrinsic value no individual may forfeit by choice, and campaign persistently to keep the prohibition on the statute books. Whenever the ban survives a challenge they collect doctrinal vindication and continued standing over bioethics questions. Abandoning the teaching is unthinkable for these institutions; their identity is constituted by it.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, religious_sanctity_institutions, beneficiary,
    organized, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(end_of_life_authority__sanctity_reading, religious_sanctity_institutions, agenda_setter).

% Competent adults with irreversible illness and suffering they experience as unbearable, who ask for help to die and are refused by law. No domestic legal route exists; a minority with money and cross-border mobility can reach permissive jurisdictions abroad, the rest cannot. They bear the added duration and intensity of suffering that the refusal extends.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, competent_suffering_patients, payer,
    powerless, immediate, trapped, national).

% Elderly, disabled, and economically disadvantaged people whom the ban shields from overt or subtle pressure, real or anticipated, to accept death so as not to burden others. Many in these groups defend the ban for exactly this reason, through organized advocacy. Members whose wish to die is authentic rather than coerced bear the same refusal as anyone else, and no third party can reliably tell the two cases apart.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, pressure_vulnerable_groups, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_authority__sanctity_reading, pressure_vulnerable_groups, payer).

% Hospice and palliative services presented, and funded, as the humane alternative that makes assisted dying unnecessary. Every legislative debate that ends with the ban retained strengthens their claim on budgets and referral flows. Their professional charter binds them to the healing-only frame.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, palliative_care_establishment, beneficiary,
    organized, biographical, constrained, national).

% Administer the ban at the bedside: they must refuse requests, manage symptoms within double-effect limits, and face prosecution or license loss if they assist a death. Professional identity is fused with the healing-only self-definition; physicians who privately favor reform typically defend the frame publicly, because breaking it threatens the profession's self-conception and liability position.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, licensed_physicians, agenda_setter,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_authority__sanctity_reading, licensed_physicians, beneficiary).

% People with advanced dementia or profound brain impairment whom the prohibition governs prospectively: the rule shapes what may later be done for or to them. They cannot speak in any hearing, and their advance wishes carry little or no legal weight for hastened death anywhere.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, incompetent_stage_patients, excluded,
    powerless, immediate, trapped, national).

% Government-appointed panels and scholarly bodies that take testimony from all seats, commission evidence reviews on safeguards and abuse in permissive jurisdictions, and publish recommendations. They decide nothing themselves but shape what legislatures come to treat as settled.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, bioethics_commissions, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(end_of_life_authority__sanctity_reading, religious_sanctity_institutions).
narrative_ontology:fixing_cost_class(end_of_life_authority__sanctity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a shared boundary norm that medicine heals and the community does not authorize intentional killing: patients can place unconditional trust in physicians, vulnerable members can trust that no institutional pathway presses them toward death, and a common standard holds that no life is waivable.
% TRANSFER_FUNCTION: Moves decision-authority over death-timing from individuals and their physicians to the collective norm; moves the burden of extended terminal suffering onto patients and their families; preserves doctrinal authority for religious bodies and professional-identity authority for medicine, and channels terminal-care demand into palliative and hospice provision.
% ABSENT_VOICES: Incompetent-stage patients (advanced dementia, profound brain impairment) whom the rule governs prospectively are absent from every hearing by incapacity, and advance directives rarely reach hastened death in any jurisdiction. Future generations who inherit either the ban or its repeal are likewise unrepresented.
% DISAPPEARANCE_RATIONALE: Overnight repeal would open assisted-dying frameworks within years, following the peer-jurisdiction pattern; the physician role would be redefined, the palliative sector would lose its mandated-alternative status and funding argument, religious authority over bioethics would contract, and the vulnerable-population protection architecture would need to be rebuilt as case-by-case safeguards.
% FOUNDING_PROBLEM: Prevent killing dressed as mercy: protect the old, disabled, poor, and otherwise disposable from being ended for others' convenience or under pressure, and preserve medicine's identity as a healing profession that does not kill, a line codified forcefully after the wartime atrocities.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting set: disability-rights coalitions that oppose expansion cite documented coercion and burden-pressure cases in permissive jurisdictions; statutory review and oversight bodies in legalized jurisdictions publish compliance findings that keep the protection concern alive. Religious institutions attest the problem is fully live; autonomy advocates attest the blanket instrument is obsolete; the two attestations disagree, which is why the status is contested rather than dead.
narrative_ontology:disappearance_verdict(end_of_life_authority__sanctity_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_authority__sanctity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_authority__sanctity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(end_of_life_authority__sanctity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_authority__sanctity_reading, 0.2, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_authority__sanctity_reading_tests).
:- end_tests(end_of_life_authority__sanctity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon 0.2 is the reading's honest own-lights measure of the standing prohibition: discharging an absolute protective duty is not a taking, but the tradition itself registers the tragedy of extended suffering and the conscience-burden on clinicians, so the value is nonzero. Suppression 0.72 is a raw structural property, deliberately NOT scaled: criminal exposure, professional discipline, and the absence of any domestic legal route close off assistance, while only extractiveness gets directionality and scope scaling downstream. Theater 0.25: the deterrent and trust-maintaining function is real, but a growing share of activity is ceremonial reaffirmation and repeated defensive legislative ritual. Accessibility_collapse 0.55: alternatives only partly collapse, because treatment refusal, palliative sedation, and letting-die remain lawful everywhere, and cross-border arbitrage persists for the mobile — the ban targets intentional life-ending by others, not all death-acceleration. Resistance 0.6: organized advocacy coalitions, test-case litigation, and recurring bills; notably, nominally powerless patients exercise coalition power through allied organizations, which is why resistance is far above zero despite individually powerless payers. The temporal series share one grid ({0,10,20,30,40,50}). Base_extractiveness rises gently as a growing minority of competent sufferers visibly contests what earlier generations accepted quietly. The suppression_requirement series FALLS deliberately: it tracks enforcement-capacity change, modeling the guard-sleeping dynamic in which compassionate-case prosecutions become rare and charging guidelines soften while the structural wall (statute, professional discipline) stands — hence a declining active-force trajectory alongside a stable-to-rising structural suppression scalar. Physician identity-lock: the profession has institutionally fused with its healing-only function; the seat bears real costs yet defends the frame, and would recompute toward payer only if a legitimate assisted-dying specialty broke the fusion.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from identical structural data. From the agenda-setter seat the prohibition is protective coordination it maintains; from the competent-sufferer seat the same structure is forced continuation of unbearable suffering. The pressure-vulnerable seat splits on the authenticity question (see coercion_authenticity_ambiguity omega): protection where coercion is real, cost where preference is authentic, with no third party able to sort cases. The physician seat is identity-locked: even while bearing conscience and liability costs it defends the arrangement, because exit would dissolve professional self-conception — organizational identity has become the function. Receipt is not benefit: religious institutions hold a beneficiary role (doctrinal vindication) and are also the seat where the norm's vindicated authority demonstrably concentrates; palliative and medical seats receive secondary flows.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (pressure_vulnerable_groups, religious_sanctity_institutions, palliative_care_establishment) derive low d — the prohibition subsidizes them. Declared victims (competent_suffering_patients; pressure_vulnerable_groups where preference is authentic) derive high d — they bear the transfer. Licensed_physicians straddle: administering enforcers who also bear the constraint's costs, placing them mid-range rather than at either pole. Bioethics commissions take the analytical seat. No directionality overrides are used: the beneficiary/victim declarations plus exit atoms produce the correct spread, and the vulnerable seat's dual positioning is carried by its secondary_role plus the authenticity omega rather than by a blunt power-atom override that would distort every powerless agent.
 *
 * MANDATROPHY ANALYSIS:
 *   Without the structural layer, the reading's rhetoric presents as timeless moral law and invites false-summit absorption: declaring beneficiaries, victims, and active enforcement surfaces a maintained construction with identifiable holders and a capturable receipt, which blocks the mountain reading. Conversely, the genuine trust-and-protection coordination function blocks the opposite mislabel: the ban is not pure extraction wearing a moral costume, because it solves a real collective problem (murder dressed as mercy; coercive pressure on the dependent), so its costs cannot be read as pure cover. The founding mandate is contested rather than dead — the abuse problem persists wherever assistance legalizes — so no mandatrophy-resolved declaration is made, and the R5 mismatch consumer finds no dead-status-plus-world_rearranges flag here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coercion_authenticity_ambiguity,
    'For a request to die from a member of a pressure-vulnerable group, can any third-party process distinguish authentic settled preference from preference shaped by burden-guilt, ableism, or economic dependence?',
    'Longitudinal qualitative follow-up of requestors in permissive jurisdictions, comparing stated reasons against independent indicators of dependency and external pressure, with matched control cohorts.',
    'If most such requests are authentic, the prohibition''s cost falls disproportionately on the very groups it names as protected and the dual-positioned seat resolves toward payer; if most are coerced, the protection function dominates and the low own-lights extraction is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_authenticity_ambiguity, conceptual, 'Authentic-versus-coerced request ambiguity for the protected populations.').

omega_variable(
    reading_commitment_structure,
    'This story is the sanctity_reading of the end_of_life_authority kernel; what changes structurally if a sibling reading is instantiated instead?',
    'Generate the sibling files (autonomy_reading, slippery_slope_mechanism) and diff victim sets, enforcement profiles, and epsilon; the disagreement localizes to whether individual preference overrides the life-preservation duty.',
    'Under the autonomy reading, competent suffering patients leave the victim set and the coercion-risk population shifts from shielded to exposed; under the slope reading, the governed population widens toward incompetent and non-terminal cases. This file''s classification covers only the categorical-prohibition arrangement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_commitment_structure, conceptual, 'Committer routing: this constraint is one reading of the end-of-life-authority kernel, with named siblings.').

omega_variable(
    sanctity_naturalness_vs_construction,
    'Is the intrinsic-value norm a deep cross-cultural moral structure approaching natural-law status, or a maintained construction serving identifiable institutional holders?',
    'Comparative and historical analysis of end-of-life norms across traditions that otherwise share moral frameworks, plus traced flows of enforcement financing, litigation funding, and retention campaigning.',
    'A deep-structure finding pushes the arrangement toward rope-like coordination with negligible capture; a construction-with-holders finding confirms the receipt concentration in the doctrinal seat and sharpens the hybrid classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sanctity_naturalness_vs_construction, conceptual, 'Whether the sanctity principle is discovered moral structure or maintained institutional construction.').

omega_variable(
    internalized_preservation_pressure,
    'Does the prohibition''s expressive teaching raise internalized pressure on vulnerable people to endure silently and treat death-wishes as forbidden talk — suppression carried in cognition rather than imposed by statute?',
    'Cross-jurisdiction surveys of vulnerable cohorts on felt permission to discuss death-wishes and request-help willingness, comparing ban jurisdictions with permissive ones.',
    'If internalized pressure is substantial, the effective lived suppression exceeds the structural measure and the own-lights epsilon understates experienced cost; the omega bounds that gap rather than resolving it away.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internalized_preservation_pressure, empirical, 'Internalized component of suppression among the protected populations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_authority__sanctity_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t0, end_of_life_authority__sanctity_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(end__tr_t10, end_of_life_authority__sanctity_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(end__tr_t20, end_of_life_authority__sanctity_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(end__tr_t30, end_of_life_authority__sanctity_reading, theater_ratio, 30, 0.2).
narrative_ontology:measurement(end__tr_t40, end_of_life_authority__sanctity_reading, theater_ratio, 40, 0.23).
narrative_ontology:measurement(end__tr_t50, end_of_life_authority__sanctity_reading, theater_ratio, 50, 0.25).

% Extraction over time
narrative_ontology:measurement(end__be_t0, end_of_life_authority__sanctity_reading, base_extractiveness, 0, 0.13).
narrative_ontology:measurement(end__be_t10, end_of_life_authority__sanctity_reading, base_extractiveness, 10, 0.15).
narrative_ontology:measurement(end__be_t20, end_of_life_authority__sanctity_reading, base_extractiveness, 20, 0.16).
narrative_ontology:measurement(end__be_t30, end_of_life_authority__sanctity_reading, base_extractiveness, 30, 0.18).
narrative_ontology:measurement(end__be_t40, end_of_life_authority__sanctity_reading, base_extractiveness, 40, 0.19).
narrative_ontology:measurement(end__be_t50, end_of_life_authority__sanctity_reading, base_extractiveness, 50, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t0, end_of_life_authority__sanctity_reading, suppression_requirement, 0, 0.78).
narrative_ontology:measurement(end__su_t10, end_of_life_authority__sanctity_reading, suppression_requirement, 10, 0.74).
narrative_ontology:measurement(end__su_t20, end_of_life_authority__sanctity_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(end__su_t30, end_of_life_authority__sanctity_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(end__su_t40, end_of_life_authority__sanctity_reading, suppression_requirement, 40, 0.65).
narrative_ontology:measurement(end__su_t50, end_of_life_authority__sanctity_reading, suppression_requirement, 50, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_authority__sanctity_reading, identity_coordination).
narrative_ontology:affects_constraint(end_of_life_authority__sanctity_reading, end_of_life_authority__autonomy_reading).
narrative_ontology:affects_constraint(end_of_life_authority__sanctity_reading, end_of_life_authority__slippery_slope_mechanism).

% DUAL FORMULATION NOTE:
% The colloquial label 'end-of-life ethics' covers three structurally distinct constraints sharing one kernel, decomposed per the epsilon-invariance principle: the sanctity categorical prohibition (this file, low own-lights extraction over the standing ban, victims include competent sufferers and authentically-prefering vulnerable members), the autonomy permission-right (different epsilon, different victim set), and the slippery-slope expansion monitor (an empirical claim, downstream of both). Family edges run sanctity -> autonomy (foreclosure within any single framework) and sanctity -> slope (the protective rationale the slope reading quantifies). No single file averages across readings; each carries one stable epsilon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
