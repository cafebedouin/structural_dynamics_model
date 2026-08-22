% ============================================================================
% CONSTRAINT STORY: end_of_life_authority__sanctity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    domain_priors:emerges_naturally/1,
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
 *   human_readable: Sanctity-of-Life Categorical Prohibition on Intentional Life-Ending (Sanctity Reading)
 *   domain: medical_ethics/bioethics/end_of_life_policy
 *
 * SUMMARY:
 *   This story instantiates the sanctity_reading of the end_of_life_authority
 *   kernel: the claim that human life carries intrinsic value, so intentional
 *   life-ending is prohibited regardless of individual preference, and the
 *   physician's role is confined to preserving life. The standing arrangement
 *   under contest — the categorical prohibition regime as it operates in
 *   prohibitive jurisdictions — is the extractiveness referent, assessed by
 *   this reading's own lights. The reading's own structural accounting places
 *   the coercion-vulnerable (elderly, disabled, economically disadvantaged)
 *   in the victim set alongside competent suffering patients: the same
 *   statute that shields them from pressure strips their own authority and
 *   extends suffering they cannot otherwise escape. Sibling readings
 *   (autonomy_reading, slippery_slope_mechanism) are separate constraint
 *   files and are neither described inside this constraint nor averaged into
 *   its values. Authoring note: the reading claims mountain status — the
 *   prohibition presented as a moral law that would hold without enforcement
 *   — while the structural data names beneficiaries, victims, and active
 *   enforcement; the false-summit test is intentional, and the claim and the
 *   metrics are authored independently of each other and of any predicted
 *   engine output.
 *
 * KEY AGENTS:
 *   - legislative_prohibition_authorities: agenda-setter (institutional/mobile) — maintains the criminal ban; could repeal it; collects retained boundary authority, not payment
 *   - medical_councils: agenda-setter and beneficiary (institutional/constrained) — enforces the physician role definition from which its own authority derives
 *   - religious_authorities: beneficiary (institutional/identity_locked) — doctrinal source of the reading; collects reaffirmed moral authority with each defense of the ban
 *   - palliative_care_sector: beneficiary (organized/mobile) — receives the extended patient-years and service demand the ban produces
 *   - disability_protection_organizations: beneficiary (organized/mobile) — collect the legal form of the protection their constituency seeks
 *   - competent_suffering_patients: payer (powerless/trapped) — bear refusal of settled requests regardless of competence or preference
 *   - coercion_vulnerable_elderly: payer and beneficiary (powerless/trapped) — protected from pressure and denied self-authority by the same statute
 *   - disabled_individuals: payer and beneficiary (powerless/trapped) — same dual position; presumed incapable of authentic preference
 *   - economically_disadvantaged_patients: payer (powerless/trapped) — bear the ban's costs with the fewest priced alternatives available
 *   - physicians: payer and beneficiary (organized/constrained) — bear role constraint and prosecution exposure; receive role-trust protection
 *   - families_of_the_dying: payer (moderate/trapped) — carry caregiving labor and criminal exposure for assistance
 *   - bioethics_community: observer (analytical/analytical) — sees the full structure; collects nothing and bears nothing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_authority__sanctity_reading, 0.22).
domain_priors:suppression_score(end_of_life_authority__sanctity_reading, 0.7).
domain_priors:theater_ratio(end_of_life_authority__sanctity_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_authority__sanctity_reading, mountain).
narrative_ontology:human_readable(end_of_life_authority__sanctity_reading, "Sanctity-of-Life Categorical Prohibition on Intentional Life-Ending (Sanctity Reading)").
narrative_ontology:topic_domain(end_of_life_authority__sanctity_reading, "medical_ethics/bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(end_of_life_authority__sanctity_reading).
domain_priors:emerges_naturally(end_of_life_authority__sanctity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_authority__sanctity_reading, '378b2a44-7bd9-47dd-be20-780e8a504122').
narrative_ontology:cs_kernel_codification('378b2a44-7bd9-47dd-be20-780e8a504122', fixed_text).
narrative_ontology:cs_authority_grounding('378b2a44-7bd9-47dd-be20-780e8a504122', lineage).
narrative_ontology:cs_interpretation_layer_present('378b2a44-7bd9-47dd-be20-780e8a504122').
narrative_ontology:cs_reading_relation('378b2a44-7bd9-47dd-be20-780e8a504122', end_of_life_authority__autonomy_reading, forecloses).
narrative_ontology:cs_reading_relation('378b2a44-7bd9-47dd-be20-780e8a504122', end_of_life_authority__slippery_slope_mechanism, influences).
narrative_ontology:cs_axiom('378b2a44-7bd9-47dd-be20-780e8a504122', foundational, intentional_life_ending_intrinsically_impermissible).
narrative_ontology:cs_axiom_status(intentional_life_ending_intrinsically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('378b2a44-7bd9-47dd-be20-780e8a504122', intentional_life_ending_intrinsically_impermissible, deontological).
narrative_ontology:cs_axiom('378b2a44-7bd9-47dd-be20-780e8a504122', secondary, physician_role_limited_to_life_preservation).
narrative_ontology:cs_axiom_status(physician_role_limited_to_life_preservation, holdable).
narrative_ontology:cs_axiom_grounding('378b2a44-7bd9-47dd-be20-780e8a504122', physician_role_limited_to_life_preservation, conventional).
narrative_ontology:cs_reference_frame('378b2a44-7bd9-47dd-be20-780e8a504122', hippocratic_sanctity_synthesis).
narrative_ontology:cs_drift_state('378b2a44-7bd9-47dd-be20-780e8a504122', contemporary_permissive_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('378b2a44-7bd9-47dd-be20-780e8a504122', '').
narrative_ontology:cs_kernel_id(end_of_life_authority__sanctity_reading, end_of_life_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, religious_authorities).
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, palliative_care_sector).
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, disability_protection_organizations).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, competent_suffering_patients).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, coercion_vulnerable_elderly).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, disabled_individuals).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, economically_disadvantaged_patients).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, medical_councils).
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, coercion_vulnerable_elderly).
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, disabled_individuals).
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, physicians).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, physicians).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, families_of_the_dying).
narrative_ontology:constraint_vindicates(end_of_life_authority__sanctity_reading, sanctity_of_life_doctrine).
narrative_ontology:constraint_vindicates(end_of_life_authority__sanctity_reading, hippocratic_non_killing_norm).
narrative_ontology:constraint_vindicates(end_of_life_authority__sanctity_reading, deontological_prohibition_on_intentional_killing).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enact and maintain the criminal statutes that ban intentional life-ending and defend them in constitutional litigation. They could repeal the ban by ordinary legislation at any time, but repeal carries concentrated opposition from religious bodies, disability organizations, and medical councils against a dispersed constituency of dying patients. They collect no direct payment from the ban's operation; what accrues to them is the retained authority to define the lawful boundary of medical practice.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, legislative_prohibition_authorities, agenda_setter,
    institutional, generational, mobile, national).

% License physicians and discipline those who assist a death, publish ethics guidance that defines the physician's role as preserving life, and strike off or prosecute practitioners who cross the line. The role definition sustains the profession's public-trust position, from which the councils' own authority derives; their room to redefine the role is bounded by statute and by the doctrinal bodies they share the field with.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, medical_councils, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_authority__sanctity_reading, medical_councils, beneficiary).

% Teach that human life carries intrinsic value that no individual may trade away, and their teaching supplies the doctrinal core of the categorical ban. Each legislative defense of the ban reaffirms their standing as moral authorities in public bioethics. They cannot abandon the teaching without dissolving the identity that constitutes them; leaving would be self-annihilation rather than a cost they could weigh.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, religious_authorities, beneficiary,
    institutional, civilizational, identity_locked, global).

% Operate hospices and palliative services for people at the end of life. The ban keeps every dying person inside their service population for the full remaining length of their illness, and funding arguments in legalization debates have repeatedly tied palliative expansion to maintaining the prohibition. Their services remain necessary under any regime, so they could adapt to legalization, but under the standing ban their patient-years and revenue are at their maximum.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, palliative_care_sector, beneficiary,
    organized, generational, mobile, national).

% Organize against assisted-dying legalization on behalf of members who fear being steered toward death by cost pressure, caregiver fatigue, or social devaluation of disabled life. The ban is the legal form of the protection they seek, and their advocacy is a load-bearing part of the coalition that sustains it. Their members' own individual preferences about their deaths are, like everyone else's, overruled by the same ban.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, disability_protection_organizations, beneficiary,
    organized, generational, mobile, national).

% Adults with terminal or incurable illness who are mentally competent, endure suffering they judge unbearable, and request help to die. The ban refuses the request regardless of how well-established their competence or how settled their preference. Their legal routes end at the jurisdiction's border: travel to a permissive country costs money and physical capacity many lack, and clandestine or violent methods expose them to failed attempts and their families to prosecution.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, competent_suffering_patients, payer,
    powerless, immediate, trapped, national).

% Older people whose situations — dependence on caregivers, inheritances waiting on their deaths, mounting care costs — make them targets of pressure toward death, and who are also individuals with their own preferences about how their lives end. The ban shields them from being pushed, and it also strips them of the authority to choose for themselves, extending any suffering they cannot otherwise escape. Protection and denial arrive through the same statute.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, coercion_vulnerable_elderly, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_authority__sanctity_reading, coercion_vulnerable_elderly, beneficiary).

% People living with disability who face the same dual position: the ban protects them from a culture that treats their lives as costly burdens, and the ban also overrules their own competent choices and presumes their preferences are not really their own. They can exit neither the protection nor the denial; both are fixed features of their legal position.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, disabled_individuals, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_authority__sanctity_reading, disabled_individuals, beneficiary).

% Poor and working-class people with severe illness. Every alternative the ban leaves open is priced: travel to a permissive jurisdiction, top-tier palliative care, private legal advice. What remains for them is the public system's standard of relief and the ban's refusal. Cost-saving rhetoric makes them the group most often discussed as candidates for pressured death and the group least able to buy any exit the ban permits.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, economically_disadvantaged_patients, payer,
    powerless, immediate, trapped, national).

% Practice under a role definition that forbids them to assist a death however insistent the request, exposes them to prosecution and license loss if they do, and leaves them to manage the moral weight of refused requests at the bedside. The same role definition protects them from being turned into instruments of others' decisions and underwrites the trust their patients place in them. Changing the role requires the councils and legislatures, not them.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, physicians, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_authority__sanctity_reading, physicians, beneficiary).

% Carry the caregiving labor, the watching of prolonged dying, and — for those who would help a loved one die — the criminal exposure the ban attaches to assistance. They cannot leave the situation without abandoning the person, and the law gives them no lawful way to shorten the dying they must witness.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, families_of_the_dying, payer,
    moderate, biographical, trapped, national).

% Analyzes the permissibility regimes across jurisdictions, publishes the comparative evidence on safeguards, coercion incidents, and expansion patterns, and supplies the conceptual vocabulary in which all the contesting parties argue. It collects nothing from the ban's operation and bears none of its costs.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, bioethics_community, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(end_of_life_authority__sanctity_reading, palliative_care_sector).
narrative_ontology:fixing_cost_class(end_of_life_authority__sanctity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves, once and centrally, three coordination problems that would otherwise be re-litigated at every deathbed: how to keep the physician's role unambiguous enough that patients can trust it; how to protect people exposed to pressure toward death without case-by-case adjudication of every family's motives; and how to hold a single social boundary against intentional killing rather than negotiating it per case.
% TRANSFER_FUNCTION: Moves the authority over the timing and manner of death from dying individuals and their physicians to the legislative and professional apparatus; converts the final span of a terminal illness from a patient-directed ending into a care-managed process, directing the care labor (much of it unpaid and familial) and the palliative service demand of that extended span toward the care sector; and delivers reaffirmed moral authority to the doctrinal institutions whose teaching the prohibition embodies.
% ABSENT_VOICES: The competent dying have the weakest formal seat: too ill to litigate in their own names, present in legislative hearings as individual anecdotes rather than as a constituency, and represented by proxies on both sides who claim to speak for them. The not-yet-vulnerable — future elderly and disabled people — are invoked by advocacy organizations but do not speak for themselves. Poor patients are least present of all: the debate's forums (courts, ethics commissions, legislative committees) are priced and professionalized venues they rarely reach.
% DISAPPEARANCE_RATIONALE: If the categorical prohibition vanished overnight, end-of-life practice would reorganize around per-case autonomy safeguards as it has in permissive jurisdictions — eligibility criteria, waiting periods, second-opinion requirements replacing the blanket ban — the palliative sector's patient population and funding arguments would restructure, religious institutions would lose a major bioethical anchor and their public authority would contract, and the coercion-protection function would have to be rebuilt through eligibility design rather than prohibition. The coercion-vulnerable would gain self-authority while losing the blanket shield; the competent suffering would gain a legal route they currently lack entirely.
% FOUNDING_PROBLEM: The arrangement was built to solve three problems at once: to protect people whose circumstances — age, disability, poverty, dependence — expose them to pressure toward death from parties who stand to gain from it; to preserve the trust-boundary of medicine by keeping the healing role and the killing role separate; and to hold a categorical moral boundary that intentional life-ending is not an option a society may make available, whatever safeguards accompany it. The categorical form was hardened in the twentieth century, particularly in the aftermath of the state euthanasia programs of the Nazi era.
% FOUNDING_PROBLEM_CORROBORATION: Health-services researchers publishing on the permissive jurisdictions (Oregon, the Netherlands, Belgium, Canada) — outside every benefiting party — attest that coercion pressure on elderly, disabled, and economically precarious people near death is real and documented, corroborating the founding problem's liveness. Cross-spectrum bioethical literature concedes the vulnerability problem while contesting whether the categorical remedy is necessary. No source outside the beneficiary coalition attests that the categorical form specifically remains required; the corroboration covers the problem, not the standing remedy.
narrative_ontology:disappearance_verdict(end_of_life_authority__sanctity_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_authority__sanctity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_authority__sanctity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(end_of_life_authority__sanctity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_authority__sanctity_reading, 0.22, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_authority__sanctity_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, ExtMetricName, E),
    domain_priors:suppression_score(end_of_life_authority__sanctity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(end_of_life_authority__sanctity_reading),
    narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(end_of_life_authority__sanctity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.22 — reading-indexed, assessed by this reading's own lights over the standing prohibition regime: the arrangement withholds a permission rather than transferring goods to a seat, and the reading's own accounting names cost-bearers (the competent suffering, the coercion-vulnerable) without counting their costs as anyone's gain. The series rises from 0.10 to 0.22 across the interval as the acknowledged cost ledger grows: hospice-era alternatives make refusal costlier to justify, and accumulated high-profile refusals enlarge the named victim set. Suppression (0.70) is authored as the raw structural property it is — unscaled by power or scope; only extractiveness is scaled, by directionality and spatial scope, in the engine's computation. The suppression series is U-shaped rather than monotonic: severe criminal enforcement early, mid-interval discretionary tolerance as the hospice movement normalized aggressive symptom control, then renewed hardening as enforcement substituted vectors (licensing discipline, information control, facilitator prosecution) in response to growing autonomy pressure — enforcement-vector substitution, not oscillation. Theater (0.44, ending just under the Goodhart line) tracks the shift from uniformly enforced criminal prohibition to selectively enforced declaratory norm: covert assistance widely tolerated, prosecutions occasional and symbolic, ethics-code and legislative reaffirmations an increasing share of the arrangement's visible activity. Accessibility collapse (0.60) reflects partial closure: in-jurisdiction legal routes are fully shut, but priced exits — travel to permissive jurisdictions, top-tier palliative care — remain to those with money and mobility, which is precisely why the burden concentrates on the economically disadvantaged. Resistance (0.65) is the sustained autonomy movement: litigation, legislative campaigns, and durable majority-poll shifts inside several prohibitive jurisdictions; the payer seats are individually powerless, and the observed coalition vector (families, sympathetic physicians, disability dissenters, advocacy organizations) is how their resistance aggregates — the coalition check for a multi-victim arrangement with powerless seats. All three series share one time grid (t = 0,10,20,30,40,50,60) so every metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the agenda-setter seats the arrangement is a boundary held in trust: legislatures and medical councils experience maintenance rather than gain and would describe the constraint as the price of a trustworthy profession. From the beneficiary seats it is vindication: the palliative sector receives the extended patient-years, the religious authorities receive reaffirmed authority, the disability organizations receive the legal form of the protection they sought. From the payer seats the same structure is categorical denial: competent sufferers experience refusal of settled requests; the coercion-vulnerable experience protection and denudation arriving through one statute — shielded from being pushed, stripped of choosing. The dual-positioned seats (payer with secondary beneficiary) are this reading's structural signature: the engine should compute their directionality as net-target but not full-target, and the divergence between their computed type and the agenda-setter's computed type is the perspectival measurement this story exists to take. The authored mountain claim adjudicates none of this — it is the reading's self-presentation, left standing for the engine to test.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations: religious_authorities (identity_locked exit — the teaching constitutes them, so they sit deep at the beneficiary end), palliative_care_sector (mobile — they could adapt to any regime, which damps their directionality toward pure beneficiary), disability_protection_organizations (organized, mobile). Victim declarations: competent_suffering_patients and economically_disadvantaged_patients (pure payers, trapped — near the full-target end), and the dual-positioned coercion_vulnerable_elderly and disabled_individuals (payer with secondary beneficiary — trapped, but their protection benefit damps directionality below the pure payers'). Physicians carry payer with secondary beneficiary: role constraint and prosecution exposure against role-trust protection. Agenda-setters (legislatures, medical councils) declare no beneficiary or victim position; the engine derives their directionality from power-atom fallbacks, which is honest — they collect retained authority, not payment. No directionality overrides are used: the beneficiary/victim declarations plus exit options already produce the intended per-seat spread, and the dual-positioned seats are exactly where the derivation's damping should operate.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — coercion protection, the healing/killing trust boundary, the categorical moral line — is live: permissive jurisdictions document real pressure cases, and no participant disputes that vulnerability at the deathbed exists. founding_problem_status = live with disappearance_verdict = world_rearranges produces no zombie mismatch for the mismatch consumer. The mandatrophy risk in this reading is subtler: the theater series (0.10 rising to 0.44) shows the arrangement's visible activity shifting from enforcement to reaffirmation while covert practice normalizes — the categorical form may be drifting toward declaratory maintenance of a boundary that professional discretion already manages differently, which is the Goodhart-drift signal to watch rather than a resolved mandate death. The structural data prevents the two mislabels this debate habitually trades in: reading the arrangement as pure extraction would erase the genuine protection the coercion-vulnerable receive through the same statute; reading it as pure coordination would erase the competent sufferers whose settled requests are refused and the priced-exit structure that dumps costs on the poor. The claim/metric divergence — a mountain claim over data with beneficiaries, victims, active enforcement, and rising theater — is the false-summit test this story is built to run.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intrinsic_value_natural_law_ambiguity,
    'Is the categorical prohibition grounded in an irreducible moral fact about life''s intrinsic value (the reading''s mountain claim, asserted as holding regardless of enforcement), or is it a constructed, actively enforced arrangement whose persistence tracks identifiable beneficiaries and enforcement capacity?',
    'Cross-jurisdictional and cross-temporal comparison: a moral law would hold its structure constant while enforcement varies; a constructed arrangement''s operation should co-vary with religious institutional strength, palliative-sector economics, and disability-organization advocacy. Natural experiments: jurisdictions that repealed the ban without collapsing into coerced death, and enforcement intensity tracking beneficiary pressure rather than moral consensus.',
    'If constructed, the mountain claim fails and the arrangement classifies from its structural data (beneficiaries, victims, active enforcement) per seat; if genuinely deontological, the beneficiary data is incidental byproduct rather than capture and the low reading-indexed extractiveness stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intrinsic_value_natural_law_ambiguity, conceptual, 'Natural moral law versus constructed enforced arrangement — the false-summit question this story''s mountain claim plus declared beneficiaries is built to test.').

omega_variable(
    sibling_reading_epsilon_divergence,
    'This story is the sanctity_reading instantiation of the end_of_life_authority kernel; the sibling autonomy_reading authors extractiveness for the same standing prohibition regime from the opposite lights. Which reading''s accounting does the kernel-level classification track, and where exactly does the disagreement bind — the moral authority of individual preference, or the empirical coercion risk?',
    'Engine-side per-seat computation over the shared stakeholder surface, plus direct comparison of the sibling stories'' authored extractiveness values over the identical referent; the divergence itself is the recorded datum, not an error to be reconciled.',
    'If per-seat effective extraction for the payer seats computes high despite this reading''s low base value, the sanctity self-account is falsified at those seats and the kernel''s operative classification follows the autonomy reading''s accounting.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_epsilon_divergence, conceptual, 'Reading-indexed extractiveness divergence over a shared referent — committer structure routed to omega per kernel-reading rules.').

omega_variable(
    vulnerable_net_position,
    'Are the coercion-vulnerable (elderly, disabled, economically disadvantaged) net-protected or net-harmed by the categorical form, relative to a safeguarded-autonomy alternative that would both guard against pressure and honor competent settled preference?',
    'Comparative outcomes across permissive and prohibitive jurisdictions: coercion incident rates, elderly and disabled suicide rates, palliative access equity, and documented pressure cases, controlling for reporting and ascertainment effects.',
    'If net-harmed, the payer position dominates their seat, the coordination function thins toward cover for the benefiting seats, and their dual positioning resolves to full target; if net-protected, the coordination leg strengthens and their dual position resolves toward beneficiary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vulnerable_net_position, empirical, 'Net protection versus net harm for the pressured-vulnerable cohort under the categorical form.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the suppression bearing on dying patients structural (criminal statutes, closed legal routes, licensing threats, information control) or internalized (duty-to-endure norms that persist as self-restraint and family expectation after any legal change)?',
    'Post-legalization demand trajectories: if requests rise sharply beyond what access alone predicts, a prior internalized component is revealed; qualitative work on patient and family self-restraint norms tests the mechanism directly.',
    'An internalized component means the constraint''s effective suppression exceeds the structural measure and would partially survive repeal; a purely structural component means repeal dissolves it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized suppression mechanism in end-of-life self-restraint.').

omega_variable(
    slippery_slope_empirical_dependency,
    'Does the sanctity reading''s practical enforcement coalition depend on the slippery-slope sibling''s empirical claim that autonomy-based regimes expand beyond competent terminal cases, and what happens to the coalition if that claim is falsified?',
    'Longitudinal eligibility data from permissive jurisdictions (terminal to chronic to psychiatric to incompetent to minor expansions), assessed for trend versus threshold effects and for safeguard-driven versus advocacy-driven expansion.',
    'If expansion is real, the reading''s consequentialist reinforcement holds and its coalition includes empirical-prudence seats; if falsified, the reading stands on the deontological axiom alone and its coalition narrows to identity-grounded beneficiary seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(slippery_slope_empirical_dependency, empirical, 'Dependency of the sanctity coalition on the sibling expansion claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_authority__sanctity_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eola_sanctity_tr_t0, end_of_life_authority__sanctity_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(eola_sanctity_tr_t10, end_of_life_authority__sanctity_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement(eola_sanctity_tr_t20, end_of_life_authority__sanctity_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement(eola_sanctity_tr_t30, end_of_life_authority__sanctity_reading, theater_ratio, 30, 0.25).
narrative_ontology:measurement(eola_sanctity_tr_t40, end_of_life_authority__sanctity_reading, theater_ratio, 40, 0.31).
narrative_ontology:measurement(eola_sanctity_tr_t50, end_of_life_authority__sanctity_reading, theater_ratio, 50, 0.38).
narrative_ontology:measurement(eola_sanctity_tr_t60, end_of_life_authority__sanctity_reading, theater_ratio, 60, 0.44).

% Extraction over time
narrative_ontology:measurement(eola_sanctity_be_t0, end_of_life_authority__sanctity_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(eola_sanctity_be_t10, end_of_life_authority__sanctity_reading, base_extractiveness, 10, 0.12).
narrative_ontology:measurement(eola_sanctity_be_t20, end_of_life_authority__sanctity_reading, base_extractiveness, 20, 0.15).
narrative_ontology:measurement(eola_sanctity_be_t30, end_of_life_authority__sanctity_reading, base_extractiveness, 30, 0.17).
narrative_ontology:measurement(eola_sanctity_be_t40, end_of_life_authority__sanctity_reading, base_extractiveness, 40, 0.19).
narrative_ontology:measurement(eola_sanctity_be_t50, end_of_life_authority__sanctity_reading, base_extractiveness, 50, 0.21).
narrative_ontology:measurement(eola_sanctity_be_t60, end_of_life_authority__sanctity_reading, base_extractiveness, 60, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(eola_sanctity_su_t0, end_of_life_authority__sanctity_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(eola_sanctity_su_t10, end_of_life_authority__sanctity_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement(eola_sanctity_su_t20, end_of_life_authority__sanctity_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(eola_sanctity_su_t30, end_of_life_authority__sanctity_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement(eola_sanctity_su_t40, end_of_life_authority__sanctity_reading, suppression_requirement, 40, 0.65).
narrative_ontology:measurement(eola_sanctity_su_t50, end_of_life_authority__sanctity_reading, suppression_requirement, 50, 0.68).
narrative_ontology:measurement(eola_sanctity_su_t60, end_of_life_authority__sanctity_reading, suppression_requirement, 60, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_authority__sanctity_reading, identity_coordination).
narrative_ontology:affects_constraint(end_of_life_authority__sanctity_reading, end_of_life_authority__autonomy_reading).
narrative_ontology:affects_constraint(end_of_life_authority__sanctity_reading, end_of_life_authority__slippery_slope_mechanism).

% DUAL FORMULATION NOTE:
% The colloquial label 'the euthanasia debate' conflates three structurally distinct claims that this kernel decomposes into three constraint files. This story (sanctity_reading) instantiates the categorical prohibition as the reading presents it: a deontological boundary with intrinsic grounding — mountain-claimed, low reading-indexed extractiveness over the standing prohibition regime. The sibling end_of_life_authority__autonomy_reading assesses the SAME standing arrangement from the opposite lights (autonomy forgone is the harm; high extractiveness over the identical referent) — the extractiveness divergence over a shared referent is the designed OQ-26 datum, not a defect to reconcile. The sibling end_of_life_authority__slippery_slope_mechanism concerns a different arrangement entirely (permissive regimes and their expansion trajectory). The sanctity reading forecloses the autonomy reading's core premise within any single normative framework and structurally influences (amplifies) the slippery-slope claim without depending on it logically.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
