% ============================================================================
% CONSTRAINT STORY: end_of_life_authority__autonomy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   human_readable: Autonomy-Grounded Medical Aid in Dying Regime (Eligibility-Gated Access Framework)
 *   domain: bioethics/end-of-life policy
 *
 * SUMMARY:
 *   Wherever the autonomy reading of end-of-life authority has been enacted —
 *   Oregon 1997, the Netherlands and Belgium 2002, Colombia 1997/2015, Canada
 *   2016 and 2021, and the constitutional-court openings in Germany, Austria,
 *   and Italy around 2019-2020 — the reading has taken institutional form as
 *   an eligibility-gated access regime: statutory criteria define who may
 *   request aid in dying, waiting periods and dual assessments verify the
 *   request, review commissions audit every case, and enforcement runs in
 *   both directions (against physicians who exceed criteria and against
 *   anyone assisting outside the channels). This story models that standing
 *   arrangement as the autonomy reading itself assesses it: the core promise
 *   is delivered to those who qualify, while the gates extract real
 *   suffering-time from those who do not, and the machinery's procedural
 *   share grows with maturity. The colloquial label 'right to die' is
 *   deliberately decomposed: this file is the autonomy reading only, with its
 *   own epsilon, victim set, and trajectory; the sanctity reading
 *   (categorical prohibition) and the slippery-slope mechanism (empirical
 *   expansion claim) are separate constraints linked through the network
 *   block. KEY AGENTS (by structural relationship): -
 *   eligible_requesting_patients: intended beneficiary (powerless/trapped) —
 *   receives the delivered service - ineligible_suffering_patients: primary
 *   target (powerless/trapped) — bears prolonged suffering behind the
 *   eligibility line - participating_physicians: dual-positioned instrument
 *   (powerful/constrained) — collects safe harbor, bears compliance and moral
 *   load - oversight_review_commissions: administrator
 *   (institutional/constrained) — sets working precedent, grows with caseload
 *   - legislative_bodies: principal agenda-setter (institutional/mobile) —
 *   draws and redraws the eligibility line - palliative_care_establishment:
 *   absorbed former opponent (organized/identity_locked) -
 *   sanctity_aligned_religious_institutions and
 *   disability_rights_organizations: organized resisters bearing diffuse
 *   institutional costs - cross_border_medical_travelers: arbitrageurs of the
 *   patchwork (moderate/arbitrage) - underground_assistance_networks:
 *   suppressed extra-channel actors (organized/constrained) -
 *   international_human_rights_bodies: analytical observer
 *
 * KEY AGENTS:
 *   - eligible_requesting_patients — intended beneficiary (powerless/trapped)
 *   - ineligible_suffering_patients — primary target (powerless/trapped)
 *   - participating_physicians — dual-positioned clinician seat (powerful/constrained)
 *   - oversight_review_commissions — administrator (institutional/constrained)
 *   - legislative_bodies — principal agenda-setter (institutional/mobile)
 *   - palliative_care_establishment — absorbed former opponent (organized/identity_locked)
 *   - sanctity_aligned_religious_institutions — doctrinal resister (organized/identity_locked)
 *   - disability_rights_organizations — resister with sidelined-design complaint (organized/constrained)
 *   - cross_border_medical_travelers — patchwork arbitrageur (moderate/arbitrage)
 *   - underground_assistance_networks — suppressed extra-channel actor (organized/constrained)
 *   - international_human_rights_bodies — analytical observer (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_authority__autonomy_reading, 0.49).
domain_priors:suppression_score(end_of_life_authority__autonomy_reading, 0.55).
domain_priors:theater_ratio(end_of_life_authority__autonomy_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, extractiveness, 0.49).
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_authority__autonomy_reading, tangled_rope).
narrative_ontology:human_readable(end_of_life_authority__autonomy_reading, "Autonomy-Grounded Medical Aid in Dying Regime (Eligibility-Gated Access Framework)").
narrative_ontology:topic_domain(end_of_life_authority__autonomy_reading, "bioethics/end-of-life policy").

domain_priors:requires_active_enforcement(end_of_life_authority__autonomy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_authority__autonomy_reading, '87373fa8-98ed-4673-a760-deed5fdcd6fe').
narrative_ontology:cs_kernel_codification('87373fa8-98ed-4673-a760-deed5fdcd6fe', distributed).
narrative_ontology:cs_authority_grounding('87373fa8-98ed-4673-a760-deed5fdcd6fe', distributed).
narrative_ontology:cs_reading_relation('87373fa8-98ed-4673-a760-deed5fdcd6fe', end_of_life_authority__sanctity_reading, forecloses).
narrative_ontology:cs_reading_relation('87373fa8-98ed-4673-a760-deed5fdcd6fe', end_of_life_authority__slippery_slope_mechanism, influences).
narrative_ontology:cs_axiom('87373fa8-98ed-4673-a760-deed5fdcd6fe', foundational, competent_autonomous_request_grounds_assistance_right).
narrative_ontology:cs_axiom_status(competent_autonomous_request_grounds_assistance_right, holdable).
narrative_ontology:cs_axiom_grounding('87373fa8-98ed-4673-a760-deed5fdcd6fe', competent_autonomous_request_grounds_assistance_right, deontological).
narrative_ontology:cs_axiom('87373fa8-98ed-4673-a760-deed5fdcd6fe', secondary, procedural_safeguards_facilitate_not_substantively_veto).
narrative_ontology:cs_axiom_status(procedural_safeguards_facilitate_not_substantively_veto, holdable).
narrative_ontology:cs_axiom_grounding('87373fa8-98ed-4673-a760-deed5fdcd6fe', procedural_safeguards_facilitate_not_substantively_veto, instrumental).
narrative_ontology:cs_reference_frame('87373fa8-98ed-4673-a760-deed5fdcd6fe', individual_self_authority_over_death_timing).
narrative_ontology:cs_drift_state('87373fa8-98ed-4673-a760-deed5fdcd6fe', contemporary_post_carter_post_bgh_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('87373fa8-98ed-4673-a760-deed5fdcd6fe', '').
narrative_ontology:cs_kernel_id(end_of_life_authority__autonomy_reading, end_of_life_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_authority__autonomy_reading, eligible_requesting_patients).
narrative_ontology:constraint_beneficiary(end_of_life_authority__autonomy_reading, participating_physicians).
narrative_ontology:constraint_beneficiary(end_of_life_authority__autonomy_reading, palliative_care_establishment).
narrative_ontology:constraint_beneficiary(end_of_life_authority__autonomy_reading, oversight_review_commissions).
narrative_ontology:constraint_victim(end_of_life_authority__autonomy_reading, ineligible_suffering_patients).
narrative_ontology:constraint_victim(end_of_life_authority__autonomy_reading, cross_border_medical_travelers).
narrative_ontology:constraint_victim(end_of_life_authority__autonomy_reading, underground_assistance_networks).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(end_of_life_authority__autonomy_reading, legislative_bodies).
narrative_ontology:constraint_victim(end_of_life_authority__autonomy_reading, participating_physicians).
narrative_ontology:constraint_victim(end_of_life_authority__autonomy_reading, palliative_care_establishment).
narrative_ontology:constraint_victim(end_of_life_authority__autonomy_reading, sanctity_aligned_religious_institutions).
narrative_ontology:constraint_victim(end_of_life_authority__autonomy_reading, disability_rights_organizations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adults judged competent and meeting their jurisdiction's criteria who formally request medical aid in dying. They receive physician assessment, prescribed medication or administration, and legal protection for everyone involved. Their other paths — continued treatment, palliative sedation, stopping food and fluids, travel abroad — stay open but are slower or harder on their families, and most proceed within months of a first formal request.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, eligible_requesting_patients, beneficiary,
    powerless, immediate, trapped, national).

% People with severe psychiatric illness, advanced dementia, or non-terminal degenerative disease who want help to die but sit on the wrong side of the eligibility line. They keep living in conditions they call intolerable; some spend savings traveling to permissive countries, others stop eating and drinking, and some die of their underlying disease during repeated assessments or waiting periods that they came close to satisfying.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, ineligible_suffering_patients, payer,
    powerless, immediate, trapped, national).

% Physicians who assess requests, prescribe or administer, and file the reports their statute requires. Participation converts what was once felony exposure into protected practice and sometimes billable work, alongside documentation duties, review-board scrutiny, and the weight of deciding who qualifies. Conscientious objectors transfer the caseload to colleagues, and in thinly served regions a handful of willing physicians carry an entire region.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, participating_physicians, beneficiary,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_authority__autonomy_reading, participating_physicians, payer).

% Statutory bodies that collect every case report, check compliance, publish annual data, and refer suspected violations to prosecutors. Their rulings become working precedent that shapes which requests succeed; their staff and budgets grow with caseload. They answer to the legislature that chartered them and cannot change eligibility criteria on their own.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, oversight_review_commissions, agenda_setter,
    institutional, generational, constrained, national).

% Parliaments and state legislatures that wrote the eligibility lines, waiting periods, and reporting duties, and that amend them when public pressure, court rulings, or commission findings demand it. Each amendment redraws who may ask and who may not. Holding a regulated middle position lets members defend the arrangement against both outright prohibition and unregulated practice.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, legislative_bodies, agenda_setter,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_authority__autonomy_reading, legislative_bodies, beneficiary).

% Hospice and palliative medicine as a field. It fought legalization for decades, then absorbed it — receiving mandated referrals and funding-parity concessions in exchange for neutrality. Practitioners split between collaborating with aid-in-dying programs and refusing on principle, and the field's public stance has softened as referral volumes grew. Its professional self-conception — neither hastening nor abandoning death — makes wholesale embrace or rejection equally unavailable.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, palliative_care_establishment, beneficiary,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_authority__autonomy_reading, palliative_care_establishment, payer).

% Churches and right-to-life organizations that hold intentional life-ending impermissible regardless of consent. They litigate, campaign, and counsel against the practice, and they carry a diffuse institutional cost as legal normalization erodes their standing in public bioethics. Their doctrine permits no compromising participation and no official neutrality.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, sanctity_aligned_religious_institutions, payer,
    organized, civilizational, identity_locked, global).

% Advocacy groups arguing that offering death where social support falls short devalues disabled lives and exposes poor or dependent people to subtle pressure toward choosing it. They testify at every expansion hearing and lose most of them; several report being consulted after positions were effectively set rather than before.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, disability_rights_organizations, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(end_of_life_authority__autonomy_reading, disability_rights_organizations, excluded).

% Patients living where assistance is banned or closed to their diagnosis who pay foreign clinics — most famously in Switzerland — several thousand dollars to accompany their deaths. They travel while gravely ill, often without family legally able to attend, and their bodies return home as repatriation cases.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, cross_border_medical_travelers, payer,
    moderate, immediate, arbitrage, global).

% Right-to-die societies and lay helpers who advise or accompany suicides where no lawful channel exists, accepting prosecution risk; several founders have been tried and convicted, and their manuals circulate regardless. Where courts later widened the legal space — Germany's 2020 ruling, Austria's 2020 decision — parts of this network reorganized into licensed associations almost overnight.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, underground_assistance_networks, payer,
    organized, biographical, constrained, national).

% UN treaty monitors, the European Court of Human Rights, and comparable bodies that hear petitions from both directions — applicants demanding a protected right to die, and disability monitors condemning expansion as a danger. Their findings reshape national debates; they administer nothing.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(end_of_life_authority__autonomy_reading, diffuse).
narrative_ontology:fixing_cost_class(end_of_life_authority__autonomy_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converts clandestine, error-prone self-killing into medically supervised, reported, and reviewed deaths: capacity assessment, voluntariness verification, uniform documentation, cause-of-death classification, and legal certainty for clinicians are solved once, centrally, instead of leaving every death to improvised private arrangement.
% TRANSFER_FUNCTION: Moves decisional authority over the timing of death from a state-and-profession monopoly to qualifying individuals; moves procedural labor (assessment, paperwork, review) onto physicians and commissions; and moves suffering-time and proof-burden onto requesters, who must establish eligibility before anything moves.
% ABSENT_VOICES: Ineligible sufferers — psychiatric patients, the non-terminal in strict regimes, people with dementia past the capacity threshold — were not in the room when eligibility lines were drawn and remain outside them; minors are excluded everywhere with essentially no consultation; patients who died while waiting or appealing are permanently absent; disability advocates report structured lateness to the drafting table.
% DISAPPEARANCE_RATIONALE: If the regime vanished overnight, several thousand deaths per year would reroute through travel clinics, stopped eating and drinking, and underground assistance; physicians would lose safe harbor and withdraw; prosecutions of helpers would resume in former permissive zones; review commissions and their datasets would dissolve; and prohibition jurisdictions bordering permissive ones would face renewed inbound pressure.
% FOUNDING_PROBLEM: Unbearable, irremediable suffering at life's end combined with legal exposure for anyone who helps relieve it by hastening death — how to honor requests for release without opening abuse of people made vulnerable by illness, age, or dependency.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the Lancet Commission on Global Access to Palliative Care and WHO reports document the untreated-suffering gap; Oregon's annual division-authored reports record inadequately controlled symptoms among requesters; the Supreme Court of Canada in Carter v. Canada accepted live evidence that the existing prohibition caused disproportionate harm; palliative medicine journals independently document demand the specialty cannot absorb.
narrative_ontology:disappearance_verdict(end_of_life_authority__autonomy_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_authority__autonomy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_authority__autonomy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(end_of_life_authority__autonomy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_authority__autonomy_reading, 0.49, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_authority__autonomy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(end_of_life_authority__autonomy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(end_of_life_authority__autonomy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.49: the regime delivers its core good to qualifiers while the gates extract suffering-time from the ineligible, delay-cost from near-qualifiers, travel-and-exclusion costs from the patchwork's losers, and compliance burden from physicians — substantial but nowhere near snare-level, because the primary intended population is genuinely served. Suppression 0.55: raw structural coercion — prosecution of extra-channel assistance, sanctioning of physicians who deviate, and closure of lawful alternatives outside the gate — moderated by two decades of decriminalization drift at the edges. Theater_ratio 0.30 and rising slowly: retrospective review commissions that concur with the original assessment in the overwhelming majority of cases (Belgium's federal commission is the clearest case) increasingly certify rather than scrutinize, a classic Goodhart drift from review quality to case throughput. Accessibility_collapse 0.45: once the gated route is understood, alternatives (Swiss travel, VSED, palliative sedation, underground help) remain partially reachable, so alternatives do not fully collapse. Resistance 0.62: the arrangement fights a two-front war — sanctity-aligned and disability-advocacy opposition to its existence and expansion, and patient-advocacy pressure against its restrictiveness — which is why it meets continuous, organized, bidirectional resistance. All three tracked series share one eight-point grid (t=0..28) so no metric row borrows another's endpoints; the trajectories are monotone drift, not oscillation, so no cycle-lengthening was needed. Suppression_requirement is tracked temporally because the story specifically traces enforcement-capacity migration: vigorous prosecution of extra-systemic helpers in the early era (convictions of prominent assisters, licensing threats) relaxing after the German and Austrian constitutional openings, while intra-regime compliance enforcement held constant — the scalar 0.55 matches the end-state. Suppression is authored as a raw structural property and is deliberately left unscaled; only extractiveness is scaled by directionality and scope downstream.
 *
 * PERSPECTIVAL GAP:
 *   Per-seat divergence is the point of this story. From the eligible patient's seat the arrangement computes as near-pure enablement — a hard-won liberation with modest procedural friction. From the ineligible patient's seat the same gates compute as denial-with-paperwork: the right the reading proclaims stops exactly at their diagnosis, and their trapped exit position amplifies whatever extraction reaches them. The physician seat straddles: safe harbor and professional meaning on one side, gatekeeping duty and moral residue on the other — which is why a directionality override lifts physicians off the pure-beneficiary floor. The sanctity-aligned seat rejects the entire apparatus as illegitimate regardless of how gently it operates, but that assessment belongs to the sibling reading's own story file, not to this one's metrics. Identity-lock dynamics bind two seats: the palliative establishment fuses professional identity with the ethos of neither hastening nor abandoning death, so it can neither join nor defeat the regime and instead bargains for referral flows; religious institutions fuse doctrinal identity with categorical impermissibility, making exit from opposition unthinkable without dissolving the institution. If the palliative identity frame broke, the field would likely convert from negotiated neutrality to full-service integration, lowering friction for requesters; if the doctrinal frame broke, the strongest organized resistance would demobilize and expansion would accelerate.
 *
 * DIRECTIONALITY LOGIC:
 *   Eligible requesting patients sit nearest the beneficiary end (d near 0.0): subsidized access, trapped exit irrelevant to their favorable position. Ineligible suffering patients sit near the full-target end (d near 1.0): they fund the regime's respectability with their prolonged suffering while receiving nothing from it. Cross-border travelers and underground networks are targets with partial mitigation — travelers buy their way out through arbitrage, networks accept risk as mission cost. Participating physicians are overridden to d=0.32: the automatic derivation from their beneficiary listing would place them near 0.1, but their real position mixes genuine gain (protected practice, fees in some systems) with compliance burden, review exposure, and the gatekeeping role that makes them the regime's hands — a mid-low value reflecting net benefit with real extraction borne. Oversight commissions and legislatures derive as low-d administrators and beneficiaries of legitimacy; commissions' gains are bounded and byproduct-shaped (staff and budget follow caseload; they did not design the gates and do not campaign to enlarge them), and no named seat captures the extraction as designed rent — the gain_flow 'diffuse' assertion is affirmative: each seat was checked, physicians' gains are offset by duties, commissions' accretion is consequential rather than purposive, and legitimacy disperses across the political system. Receipt is nonetheless real: the suffering-time transferred from the ineligible purchases the public trust that keeps the whole arrangement lawful, and that trust lands on no single chair.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem remains live — aging populations, cancer prevalence, documented untreated-symptom burdens, and steady request volumes corroborated by sources outside the benefiting parties — so this is not a mandate that outlived its function and no mandatrophy resolution is declared. The classification guards against mislabeling in both directions: calling the regime pure coordination (a rope) would erase the measurable suffering-time the gates extract from the ineligible; calling it pure extraction (a snare) would erase the fact that the primary population is genuinely served and would hand the sanctity reading's conclusion to this file by fiat. Tangled rope holds both truths: real coordination function, real asymmetric extraction, active enforcement required to sustain both. The forward-looking risk is Goodhart drift in the review layer: if theater_ratio continues climbing past roughly 0.5 while the founding problem stays live, the review apparatus would be certifying rather than safeguarding, and the extraction side of the ledger would grow without any corresponding coordination gain. The R5 mismatch consumer finds status=live paired with verdict=world_rearranges — no zombie flag — and the rising theater series is flagged for monitoring rather than reclassification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment_split,
    'Is death-timing authority alienable by the competent person (this autonomy_reading), inalienably held by God, nature, or community (sanctity_reading), or empirically self-eroding wherever delegated (slippery_slope_mechanism)? This file is one reading only — its epsilon and victim set are indexical to that reading and must not be averaged across siblings.',
    'Not resolvable by data at the kernel level; resolution occurs per-jurisdiction through constitutional adjudication and legislation, which simply selects a reading locally. Cross-jurisdiction comparison of victim sets documents the split without closing it.',
    'If the sanctity reading prevails in a jurisdiction, the victim set inverts (lives intentionally ended become the harmed class) and this file''s classification does not transfer; if the slippery-slope mechanism dominates the discourse, this reading''s legitimacy erodes regardless of its per-case performance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment_split, conceptual, 'Indexical split of the end_of_life_authority kernel across three sibling readings.').

omega_variable(
    eligibility_expansion_asymptote,
    'Does the observed eligibility expansion (Belgium to minors 2014, Canada dropping reasonably-foreseeable-death in 2021 and moving toward mental-illness and mature-minor tracks, Dutch psychiatric practice growth) approach an asymptote at competent-adult terminal cases-plus-margin, or continue outward?',
    'Longitudinal statutory-amendment tracking and commission caseload composition data over the next decade, particularly the Canadian Track-2/mental-illness sequence.',
    'Continued expansion lowers gate-extraction for newly eligible classes while extending the regime''s footprint and feeding the slippery-slope sibling''s evidentiary base; an asymptote would stabilize this story''s metrics near current values and starve the sibling mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eligibility_expansion_asymptote, empirical, 'Trajectory and limit of eligibility-criteria expansion.').

omega_variable(
    capacity_screening_reliability,
    'How reliably do capacity assessments and depression screens separate autonomous requests from requests driven by treatable despair, given that only a small fraction of requesters receive formal psychiatric evaluation in most regimes?',
    'Outcome studies comparing requesters referred for psychiatric consultation against those waved through, plus longitudinal regret/error audits where death certificates and commission files allow linkage.',
    'If screening systematically misses coerced or despair-driven requests, part of what this story measures as honored autonomy is concealed extraction, raising effective extractiveness and widening the victim set to nominally eligible patients.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_screening_reliability, empirical, 'Validity of the capacity and voluntariness screen at the regime''s front door.').

omega_variable(
    economic_pressure_conversion,
    'Does material deprivation convert nominally voluntary requests into economically coerced ones — patients choosing death because care, housing, or disability support is unaffordable or unavailable (the central Canadian Track-2 criticism)?',
    'Socioeconomic profiling of requester cohorts against matched non-requester cohorts with equivalent prognoses, and case-series analysis of requests citing care-access failure.',
    'If conversion is real, the victim set widens beyond the ineligible to include some nominally eligible requesters, effective extractiveness rises materially, and the disability-advocacy seat''s objections move from commentary-grade to correction-grade evidence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(economic_pressure_conversion, empirical, 'Whether poverty converts choice into coercion inside the eligibility gate.').

omega_variable(
    suppression_patchwork_representation,
    'Does the story-level suppression scalar represent the whole patchwork, given that enforcement intensity fell sharply in permissive-and-liberalizing jurisdictions while remaining near-full criminal enforcement in prohibition enclaves covering most of the world''s population?',
    'Comparative docket analysis of assistance-related prosecutions and licensing actions across permissive, transitional, and prohibition jurisdictions over the interval.',
    'If prohibition-enclave enforcement dominates the weighted picture, story-level suppression is understated; if permissive-zone relaxation dominates, overstated — either skew shifts the computed type at seats located in the corresponding zone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_patchwork_representation, empirical, 'Whether one suppression scalar can represent a juridically fragmented enforcement landscape.').

omega_variable(
    review_theater_drift,
    'Do retrospective review commissions exert substantive control over practice quality, or have they drifted into rubber-stamp certification (near-total concurrence rates, minimal referral-to-prosecutor volume)?',
    'Commission dissent and referral rates over time, sampled audit of reviewed cases against independent clinical standards, and comparison of pre-review versus post-review violation detection.',
    'If certification has replaced scrutiny, theater_ratio is understated and rising faster than modeled, strengthening the Goodhart-drift reading and pulling late-interval classification toward degraded maintenance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(review_theater_drift, empirical, 'Functional versus ceremonial status of the oversight review layer.').

omega_variable(
    framing_institution_vs_legitimacy_claim,
    'Should the commitment system be framed as the statutory apparatus itself (formalized rules administered by commissions and legislatures) or as the legitimacy claim layered above it (the autonomy doctrine that makes the apparatus defensible at all)? The two framings yield different kernel_codification and authority_grounding profiles.',
    'Test which framing reproduces the observed dispute structure: if contests always resolve through statute amendment, the institutional frame fits; if contests resolve through doctrine revision (court rulings redefining the autonomy premise), the legitimacy-claim frame fits.',
    'Under the legitimacy-claim framing, authority_grounding shifts from distributed toward expertise-and-lineage hybrids anchored in human-rights jurisprudence, changing the CS pattern classification and the drift vector''s anchor point.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_institution_vs_legitimacy_claim, conceptual, 'Framing under-determination in the commitment-system classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_authority__autonomy_reading, 0, 28).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eola_autonomy_tr_t0, end_of_life_authority__autonomy_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(eola_autonomy_tr_t4, end_of_life_authority__autonomy_reading, theater_ratio, 4, 0.21).
narrative_ontology:measurement(eola_autonomy_tr_t8, end_of_life_authority__autonomy_reading, theater_ratio, 8, 0.24).
narrative_ontology:measurement(eola_autonomy_tr_t12, end_of_life_authority__autonomy_reading, theater_ratio, 12, 0.26).
narrative_ontology:measurement(eola_autonomy_tr_t16, end_of_life_authority__autonomy_reading, theater_ratio, 16, 0.27).
narrative_ontology:measurement(eola_autonomy_tr_t20, end_of_life_authority__autonomy_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(eola_autonomy_tr_t24, end_of_life_authority__autonomy_reading, theater_ratio, 24, 0.29).
narrative_ontology:measurement(eola_autonomy_tr_t28, end_of_life_authority__autonomy_reading, theater_ratio, 28, 0.3).

% Extraction over time
narrative_ontology:measurement(eola_autonomy_be_t0, end_of_life_authority__autonomy_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(eola_autonomy_be_t4, end_of_life_authority__autonomy_reading, base_extractiveness, 4, 0.54).
narrative_ontology:measurement(eola_autonomy_be_t8, end_of_life_authority__autonomy_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(eola_autonomy_be_t12, end_of_life_authority__autonomy_reading, base_extractiveness, 12, 0.47).
narrative_ontology:measurement(eola_autonomy_be_t16, end_of_life_authority__autonomy_reading, base_extractiveness, 16, 0.46).
narrative_ontology:measurement(eola_autonomy_be_t20, end_of_life_authority__autonomy_reading, base_extractiveness, 20, 0.47).
narrative_ontology:measurement(eola_autonomy_be_t24, end_of_life_authority__autonomy_reading, base_extractiveness, 24, 0.48).
narrative_ontology:measurement(eola_autonomy_be_t28, end_of_life_authority__autonomy_reading, base_extractiveness, 28, 0.49).

% Suppression requirement over time
narrative_ontology:measurement(eola_autonomy_su_t0, end_of_life_authority__autonomy_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(eola_autonomy_su_t4, end_of_life_authority__autonomy_reading, suppression_requirement, 4, 0.66).
narrative_ontology:measurement(eola_autonomy_su_t8, end_of_life_authority__autonomy_reading, suppression_requirement, 8, 0.63).
narrative_ontology:measurement(eola_autonomy_su_t12, end_of_life_authority__autonomy_reading, suppression_requirement, 12, 0.6).
narrative_ontology:measurement(eola_autonomy_su_t16, end_of_life_authority__autonomy_reading, suppression_requirement, 16, 0.58).
narrative_ontology:measurement(eola_autonomy_su_t20, end_of_life_authority__autonomy_reading, suppression_requirement, 20, 0.56).
narrative_ontology:measurement(eola_autonomy_su_t24, end_of_life_authority__autonomy_reading, suppression_requirement, 24, 0.55).
narrative_ontology:measurement(eola_autonomy_su_t28, end_of_life_authority__autonomy_reading, suppression_requirement, 28, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_authority__autonomy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(end_of_life_authority__autonomy_reading, end_of_life_authority__sanctity_reading).
narrative_ontology:affects_constraint(end_of_life_authority__autonomy_reading, end_of_life_authority__slippery_slope_mechanism).
narrative_ontology:affects_constraint(end_of_life_authority__autonomy_reading, informed_consent_medical_authority).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'right to die' under kernel end_of_life_authority. The label covers three structurally distinct claims with different epsilon referents and different victim sets: this autonomy_reading file (standing arrangement = eligibility-gated access regime; victims = the ineligible, the patchwork's losers, suppressed extra-channel actors), the sanctity_reading (standing arrangement = categorical prohibition; its victim and beneficiary sets invert this file's), and the slippery_slope_mechanism (an empirical claim whose evidentiary base IS this file's temporal record — hence the influences edge from this reading to it). Upstream, informed_consent_medical_authority feeds this reading: the autonomy premise is the informed-consent doctrine extended to the timing of death itself. All family members link through network.affects_constraints; no single story averages across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(end_of_life_authority__autonomy_reading, powerful, 0.32).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
