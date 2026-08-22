% ============================================================================
% CONSTRAINT STORY: legitimate_health_intervention__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_health_intervention__bodily_autonomy_primary, []).

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
 *   constraint_id: legitimate_health_intervention__bodily_autonomy_primary
 *   human_readable: State-Coerced Medical Intervention Regime (Bodily Autonomy Primary Reading)
 *   domain: public_health/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   The standing arrangement under contest is the state-coerced medical
 *   intervention regime: immunization and treatment requirements enforced
 *   through school entry, employment conditions, institutional residence, and
 *   exclusion from public facilities, with legitimacy claimed from
 *   population-level health benefit. The bodily_autonomy_primary reading
 *   assesses that arrangement by its own lights: consent is the legitimacy
 *   condition for medical intervention, so requirements enforced by
 *   livelihood and access leverage take bodily decision-rights without
 *   consent. The claim/metric gap is deliberate: the constraint is CLAIMED as
 *   tangled_rope (a real epidemic-control coordination function with
 *   asymmetric extraction through the same structure) while the authored
 *   metrics describe substantially extractive, actively enforced operation
 *   with crisis-driven ratcheting — the engine measures the divergence; do
 *   not reconcile the claim to the metrics. This story is one member of a
 *   three-reading constraint family (with public_health_primary and
 *   proportionality_reading); the colloquial label 'legitimate health
 *   intervention' decomposes because the readings assign different victim
 *   sets, different epsilon, and different authority structures to the same
 *   arrangement.
 *
 * KEY AGENTS:
 *   - state_public_health_authorities: agenda setter (institutional/constrained, national) — administers the requirement machinery, collects compliance and expanded emergency authority
 *   - mandate_coerced_individuals: primary target (powerless/constrained) — undergo required interventions under threat of employment, schooling, or access loss
 *   - livelihood_penalized_refusers: primary target (powerless/trapped) — refuse and bear realized termination and exclusion costs
 *   - adverse_event_bearers_without_recourse: primary target (powerless/trapped) — bear intervention injuries through narrow compensation channels
 *   - high_risk_medically_vulnerable_individuals: beneficiary (powerless/constrained) — depend on population uptake they cannot self-supply
 *   - employers_and_access_institutions: secondary beneficiary and local enforcer (powerful/mobile) — administer requirements at the point of entry, drop them when costs rise
 *   - medical_licensing_bodies: enforcement node (institutional/constrained) — discipline exemption issuance, expanding professional authority with each requirement regime
 *   - institutionalized_persons: excluded seat (powerless/trapped) — face the strongest requirement forms with the least voice (prisons, military, long-term care)
 *   - constitutional_courts: analytical observer (institutional/analytical) — adjudicate the legitimacy contest between bodily-integrity claims and police-power doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_health_intervention__bodily_autonomy_primary, 0.65).
domain_priors:suppression_score(legitimate_health_intervention__bodily_autonomy_primary, 0.68).
domain_priors:theater_ratio(legitimate_health_intervention__bodily_autonomy_primary, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, extractiveness, 0.65).
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_health_intervention__bodily_autonomy_primary, tangled_rope).
narrative_ontology:human_readable(legitimate_health_intervention__bodily_autonomy_primary, "State-Coerced Medical Intervention Regime (Bodily Autonomy Primary Reading)").
narrative_ontology:topic_domain(legitimate_health_intervention__bodily_autonomy_primary, "public_health/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(legitimate_health_intervention__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_health_intervention__bodily_autonomy_primary, '4351a804-da21-4bc8-b101-a5f0e142c319').
narrative_ontology:cs_kernel_codification('4351a804-da21-4bc8-b101-a5f0e142c319', formalized).
narrative_ontology:cs_authority_grounding('4351a804-da21-4bc8-b101-a5f0e142c319', lineage).
narrative_ontology:cs_interpretation_layer_present('4351a804-da21-4bc8-b101-a5f0e142c319').
narrative_ontology:cs_reading_relation('4351a804-da21-4bc8-b101-a5f0e142c319', legitimate_health_intervention__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('4351a804-da21-4bc8-b101-a5f0e142c319', legitimate_health_intervention__proportionality_reading, forecloses).
narrative_ontology:cs_axiom('4351a804-da21-4bc8-b101-a5f0e142c319', foundational, bodily_integrity_requires_affirmative_consent).
narrative_ontology:cs_axiom_status(bodily_integrity_requires_affirmative_consent, holdable).
narrative_ontology:cs_axiom_grounding('4351a804-da21-4bc8-b101-a5f0e142c319', bodily_integrity_requires_affirmative_consent, deontological).
narrative_ontology:cs_axiom('4351a804-da21-4bc8-b101-a5f0e142c319', foundational, public_benefit_never_legitimates_nonconsensual_intervention).
narrative_ontology:cs_axiom_status(public_benefit_never_legitimates_nonconsensual_intervention, holdable).
narrative_ontology:cs_axiom_grounding('4351a804-da21-4bc8-b101-a5f0e142c319', public_benefit_never_legitimates_nonconsensual_intervention, deontological).
narrative_ontology:cs_axiom('4351a804-da21-4bc8-b101-a5f0e142c319', secondary, emergency_conditions_do_not_suspends_consent_rights).
narrative_ontology:cs_axiom_status(emergency_conditions_do_not_suspends_consent_rights, holdable).
narrative_ontology:cs_axiom_grounding('4351a804-da21-4bc8-b101-a5f0e142c319', emergency_conditions_do_not_suspends_consent_rights, deontological).
narrative_ontology:cs_reference_frame('4351a804-da21-4bc8-b101-a5f0e142c319', informed_consent_baseline).
narrative_ontology:cs_drift_state('4351a804-da21-4bc8-b101-a5f0e142c319', contemporary_post_pandemic_mandate_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4351a804-da21-4bc8-b101-a5f0e142c319', '').
narrative_ontology:cs_kernel_id(legitimate_health_intervention__bodily_autonomy_primary, legitimate_health_intervention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__bodily_autonomy_primary, state_public_health_authorities).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__bodily_autonomy_primary, high_risk_medically_vulnerable_individuals).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__bodily_autonomy_primary, employers_and_access_institutions).
narrative_ontology:constraint_victim(legitimate_health_intervention__bodily_autonomy_primary, mandate_coerced_individuals).
narrative_ontology:constraint_victim(legitimate_health_intervention__bodily_autonomy_primary, livelihood_penalized_refusers).
narrative_ontology:constraint_victim(legitimate_health_intervention__bodily_autonomy_primary, adverse_event_bearers_without_recourse).
narrative_ontology:constraint_vindicates(legitimate_health_intervention__bodily_autonomy_primary, police_powers_doctrine).
narrative_ontology:constraint_vindicates(legitimate_health_intervention__bodily_autonomy_primary, population_immunity_externality_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set intervention requirements through statute and emergency order and enforce them through school entry rules, employment conditions, and exclusion from public facilities. Collect compliance, surveillance data, and expanded emergency authority when requirements are upheld. Their alternatives are bounded by legislation, judicial review, and funding conditions; relinquishing the requirement power carries political cost they rarely absorb.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, state_public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Undergo required interventions because refusal would cost employment, schooling, or access to services. Their decision space contains refusal-with-penalty, exemption applications of uncertain availability, and jurisdiction change; each alternative carries costs they bear personally while the intervention's benefits diffuse across the population.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, mandate_coerced_individuals, payer,
    powerless, biographical, constrained, national).

% Declined required interventions and lost jobs, educational enrollment, military standing, or access as a result. Re-entering covered institutions requires accepting the intervention; their remaining alternatives are informal-economy work, relocation, or enduring the penalty. They carry the enforcement's realized cost.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, livelihood_penalized_refusers, payer,
    powerless, biographical, trapped, national).

% Suffered injuries following required interventions and find the compensation channels narrow: injury tables, filing deadlines, and damage caps exclude many claims. Their harm is done and cannot be exited; the adequacy of recourse determines whether the risk they bore was assumed or imposed.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, adverse_event_bearers_without_recourse, payer,
    powerless, biographical, trapped, national).

% Rely on high population uptake of immunization for protection their own bodies cannot provide (contraindicated or less effective for them). They gain from the participation the requirements produce and have no individual exit from exposure to circulating disease.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, high_risk_medically_vulnerable_individuals, beneficiary,
    powerless, biographical, constrained, national).

% Administer the requirements at the point of employment, enrollment, or entry, gaining a standardized risk-management instrument and liability cover. They can drop requirements when political or labor-market costs rise — many did after the pandemic surge — which makes their enforcement conditional rather than committed.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, employers_and_access_institutions, beneficiary,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(legitimate_health_intervention__bodily_autonomy_primary, employers_and_access_institutions, agenda_setter).

% Discipline physicians who issue exemptions outside accepted medical criteria, policing the exemption channel the requirements depend on for legitimacy. Their authority over professional practice expands with each requirement regime; their alternative is a narrower disciplinary mandate bounded by their own evidence standards.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, medical_licensing_bodies, agenda_setter,
    institutional, generational, constrained, national).

% Live inside institutions — prisons, military, long-term care — where intervention requirements attach with the least possibility of refusal and the least access to counsel or public voice. Their situation would draw the strongest objection from the consent tradition, and they are the least present in the conversation.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, institutionalized_persons, excluded,
    powerless, biographical, trapped, national).

% Adjudicate the legitimacy contest between bodily-integrity claims and police-power doctrines, applying precedents that balance individual liberty against public health authority. They collect no compliance and bear no intervention; their rulings reconfigure which enforcement forms survive.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimate_health_intervention__bodily_autonomy_primary, state_public_health_authorities).
narrative_ontology:fixing_cost_class(legitimate_health_intervention__bodily_autonomy_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Raises participation in protective health interventions above what voluntary choice supplies when benefits are diffuse and free-riding is available, and standardizes institutional risk policy across workplaces, schools, and care settings; epidemic control has historically required participation thresholds that individual consent decisions under-provide.
% TRANSFER_FUNCTION: Moves bodily decision-rights from individuals to state and institutional controllers; moves intervention compliance from refusers to public health authorities; under employment and access conditions, moves livelihood continuity from refusers to complying participants; moves disease risk away from the medically vulnerable and onto those who bear interventions or penalties.
% ABSENT_VOICES: Institutionalized persons (prisoners, service members, long-term-care residents) face the strongest forms of the requirement with the least voice; children are subjected through school-entry rules without consent capacity of their own; future persons inherit emergency-power precedents they had no part in. Refusers enter the record mainly through litigation after penalties have landed.
% DISAPPEARANCE_RATIONALE: Participation would fall toward voluntary levels and outbreak risk would concentrate among the unprotected; employers and schools would renegotiate risk policies individually; the medically vulnerable would lose protection they cannot self-supply; and the state's emergency health powers would contract toward their pre-requirement scope. Every named seat is positioned around this arrangement — its removal reorganizes each of those positions.
% FOUNDING_PROBLEM: Acute epidemic control: in smallpox-era and early twentieth-century outbreaks, voluntary participation could not reach the thresholds needed to interrupt transmission of high-mortality disease, and Jacobson v. Massachusetts (1905) established state authority to require vaccination as an exercise of police power.
% FOUNDING_PROBLEM_CORROBORATION: Epidemiological evidence from outside the beneficiary set corroborates that the founding problem is live for high-consequence pathogens (measles resurgence data, smallpox eradication history). Independent corroboration of the overreach reading comes from judicial decisions narrowing requirement authority, historical scholarship documenting the expansion of requirements from acute epidemic response into routine employment condition, and bioethics literature outside public health institutions. No party disputes that epidemic control was the founding problem; the parties dispute whether the standing arrangement still serves it.
narrative_ontology:disappearance_verdict(legitimate_health_intervention__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_health_intervention__bodily_autonomy_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_health_intervention__bodily_autonomy_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimate_health_intervention__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_health_intervention__bodily_autonomy_primary, 0.65, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_health_intervention__bodily_autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimate_health_intervention__bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimate_health_intervention__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.65 end-state because the requirement regime transfers bodily decision-rights, livelihood, and bodily risk under threat, while retaining a real epidemic-control function and exemption channels that keep it short of pure extraction. Suppression (0.68) is a raw structural property — it is NOT scaled by power or scope; the engine scales only extractiveness. The series shows a crisis ratchet rather than monotonic drift: baseline rose slowly across the first eighteen points as requirements expanded from school entry into employment and university conditions, spiked sharply at the pandemic peak (termination regimes, exclusion from public venues, federal contractor requirements), then receded only partially — the end-state trough (0.65) sits well above the pre-crisis baseline (0.56). The oscillation is itself partly an extraction mechanism: each emergency leaves behind enforcement infrastructure (digital pass systems, normalized employment conditions, expanded emergency authority) that does not fully demobilize, so successive crises start from a higher floor. Theater peaks during the crisis (crisis messaging, symbolic compliance performances) and recedes with enforcement. Accessibility collapse is moderate (0.50): refusal-with-penalty, exemptions, and jurisdiction change remain available but costly. Resistance is high (0.65): litigation, exemption movements, and state-level limits on enforcement produced the post-crisis relaxation — the powerless seats are not without coalition capacity, and their measured per-seat powerlessness understates their organized class action.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seats should compute differently. From the state and licensing seats the arrangement is a legitimate exercise of police power they administer; from the coerced, penalized, and harmed seats the same structure operates as a bodily-integrity violation enforced through livelihood leverage. The public_health_primary sibling reading would compute this arrangement near the coordination end (refusal as externality); this reading's seat computes it near the extraction end. Employers sit between: they enforce and benefit but can exit by dropping requirements. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   State authorities and licensing bodies sit near the beneficiary end: they collect compliance, authority, and expanded disciplinary reach. Employers benefit (risk instrument, liability cover) but bear administration costs and political exposure — near-beneficiary, slightly above the beneficiary pole. The medically vulnerable are genuine beneficiaries with no exit from the disease environment. Mandate-coerced individuals, penalized refusers, and adverse-event bearers sit near the full-target end: they transfer decision-rights, livelihood, or bodily risk under threat, with constrained or no exit. Institutionalized persons are targets whose exclusion from the conversation is part of the structure itself. Courts are analytical. The receipt surface names the state seat: compliance, expanded emergency authority, and precedent accrue there; employers collect secondary benefit, but the arrangement's gains land on the administering authority. Fixing cost is prohibitive for whoever could fix it: dismantling requires statutory repeal across jurisdictions, confronting institutional identity, and absorbing political blame for outbreaks.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — acute epidemic control of high-mortality disease — remains live for some pathogens and dead for others; the standing arrangement spans both. The tangled-rope classification prevents two mislabels: the public-health move that reads the whole arrangement as coordination (which would hide the employment-leverage coercion of refusers and the recourse gap for the harmed), and the pure-liberty move that reads it all as extraction (which would erase the genuine epidemic-control function protecting those who cannot self-protect). Mandatrophy is partial and contested: requirements aimed at eradication-scale threats have atrophied into routine employment conditions where the threat is endemic and low-mortality — that segment shows mandate-outlived-function dynamics — while the acute-threat segment retains live function. The mismatch between the founding problem's contested status and the world_rearranges verdict flags the arrangement for exactly this split evaluation rather than a single verdict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading (bodily_autonomy_primary) of the kernel legitimate_health_intervention — what would adopting a sibling reading change structurally, and where exactly is the disagreement located?',
    'Per-seat computation across the three sibling stories: compare victim sets, epsilon, and classification across the constraint family. The disagreement is located in whether population benefit can ever supply the legitimacy that individual consent supplies — this reading answers never, the siblings answer conditionally or straightforwardly.',
    'This reading holds ''regardless of public benefit'' as an absolutist threshold, so it structurally forecloses both siblings within a single framework; the siblings would remove the coerced from the victim set and drop epsilon toward the coordination-cost range.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: this story instantiates the bodily_autonomy_primary reading of the legitimate_health_intervention kernel.').

omega_variable(
    public_health_primary_structural_delta,
    'What would the public_health_primary sibling change if adopted as the governing reading of the same arrangement?',
    'Generate and classify the sibling story: mandate-coerced individuals leave the victim set (refusal recast as externality imposition), the state seat moves from extractor to coordinator, and epsilon drops toward the coordination-cost floor.',
    'The same arrangement classifies near the coordination end from that seat; the family comparison isolates the legitimacy premise as the entire classification delta.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(public_health_primary_structural_delta, conceptual, 'Sibling delta: public_health_primary recasts refusal as externality and the state as coordinator.').

omega_variable(
    proportionality_reading_structural_delta,
    'What would the proportionality_reading sibling change if adopted as the governing reading of the same arrangement?',
    'Generate and classify the sibling story: permissibility becomes a function of disease characteristics (severity, transmissibility, mortality), so the victim set shrinks to individuals subjected to disproportionate interventions and epsilon becomes threat-indexed rather than fixed.',
    'Classification becomes regime-dependent on epidemiological conditions; the absolutist consent threshold of this reading is replaced by a weighting function the engine cannot evaluate without disease-characteristic inputs.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proportionality_reading_structural_delta, conceptual, 'Sibling delta: proportionality_reading makes legitimacy threat-weighted rather than consent-absolute.').

omega_variable(
    consent_under_leverage_validity,
    'Is consent given under employment or access leverage genuine informed consent, or does the leverage itself void the consent — determining whether the victim set is only outright refusers or all leverage-compliers?',
    'Legal doctrine on economic coercion plus empirical study of compliance decision-making under threat (would the complier accept the intervention absent the penalty?); judicial treatment of consent under conditions of dependency.',
    'If leveraged consent counts, epsilon drops substantially and the victim set shrinks to refusers and the harmed; if it does not, the coerced-complier class dominates the victim set and epsilon rises toward the crisis-peak level as the standing condition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_under_leverage_validity, conceptual, 'Whether consent under livelihood leverage is consent for legitimacy purposes.').

omega_variable(
    enforcement_severity_regimes,
    'Does realized extraction track enforcement severity across jurisdictions and eras as the expected structural delta predicts?',
    'Cross-jurisdiction comparison of enforcement intensity (termination regimes, exclusion rules, fines) against realized refusal costs and uptake; natural experiments where courts barred enforcement.',
    'Low-enforcement jurisdictions may compute near the coordination end while termination regimes compute near the pure-extraction end; a single story-level epsilon would then misdescribe the family of local arrangements.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_severity_regimes, empirical, 'Epsilon variance across enforcement severity regimes.').

omega_variable(
    adverse_event_recourse_adequacy,
    'Are the compensation channels for mandated-intervention injuries adequate recourse, or does their narrowness (injury tables, filing deadlines, damage caps) make the required intervention a forced transfer of bodily risk?',
    'Actuarial and legal audit of compensation program coverage against observed injury claims: approval rates, payout adequacy relative to harm, exclusion categories.',
    'Adequate recourse would move the adverse-event seat from target toward symmetric risk-sharing; inadequate recourse confirms a forced risk transfer and raises epsilon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adverse_event_recourse_adequacy, empirical, 'Whether injury compensation adequacy determines the harmed seat''s structural position.').

omega_variable(
    crisis_ratchet_permanence,
    'Is the post-crisis relaxation a return toward baseline, or does each emergency permanently raise the enforcement floor (ratchet)?',
    'Track the post-2024 trajectory of enforcement infrastructure: whether digital pass systems, normalized employment conditions, and expanded emergency authority demobilize or persist into the next crisis.',
    'Full demobilization would date the ratchet reading as crisis noise; persistence would confirm the oscillation as an accumulation mechanism and project rising floors into future crises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crisis_ratchet_permanence, empirical, 'Whether the crisis cycle ratchets or mean-reverts.').

omega_variable(
    suppression_structural_vs_normative,
    'Is the measured suppression carried by formal enforcement machinery alone, or also by internalized civic-duty and social-pressure norms that would persist if formal enforcement were removed?',
    'Compare compliance and uptake in jurisdictions that removed formal enforcement against those that retained it; decompose compliance motivations into penalty-avoidance versus norm-internalization.',
    'If internalized norms carry a large share, removing enforcement would not collapse the arrangement and the suppression metric overstates the formal machinery''s necessity; the omega separates structural from internalized suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_normative, empirical, 'Structural versus internalized suppression mechanism in mandate compliance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_health_intervention__bodily_autonomy_primary, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bap_lhi_tr_t0, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(bap_lhi_tr_t0, observed).
narrative_ontology:measurement(bap_lhi_tr_t3, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 3, 0.19).
narrative_ontology:measurement_basis(bap_lhi_tr_t3, observed).
narrative_ontology:measurement(bap_lhi_tr_t6, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 6, 0.2).
narrative_ontology:measurement_basis(bap_lhi_tr_t6, observed).
narrative_ontology:measurement(bap_lhi_tr_t9, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 9, 0.22).
narrative_ontology:measurement_basis(bap_lhi_tr_t9, observed).
narrative_ontology:measurement(bap_lhi_tr_t12, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 12, 0.24).
narrative_ontology:measurement_basis(bap_lhi_tr_t12, observed).
narrative_ontology:measurement(bap_lhi_tr_t15, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 15, 0.26).
narrative_ontology:measurement_basis(bap_lhi_tr_t15, observed).
narrative_ontology:measurement(bap_lhi_tr_t18, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 18, 0.28).
narrative_ontology:measurement_basis(bap_lhi_tr_t18, observed).
narrative_ontology:measurement(bap_lhi_tr_t21, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 21, 0.4).
narrative_ontology:measurement_basis(bap_lhi_tr_t21, observed).
narrative_ontology:measurement(bap_lhi_tr_t24, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 24, 0.3).
narrative_ontology:measurement_basis(bap_lhi_tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(bap_lhi_be_t0, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(bap_lhi_be_t0, observed).
narrative_ontology:measurement(bap_lhi_be_t3, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 3, 0.46).
narrative_ontology:measurement_basis(bap_lhi_be_t3, observed).
narrative_ontology:measurement(bap_lhi_be_t6, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 6, 0.47).
narrative_ontology:measurement_basis(bap_lhi_be_t6, observed).
narrative_ontology:measurement(bap_lhi_be_t9, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 9, 0.49).
narrative_ontology:measurement_basis(bap_lhi_be_t9, observed).
narrative_ontology:measurement(bap_lhi_be_t12, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 12, 0.51).
narrative_ontology:measurement_basis(bap_lhi_be_t12, observed).
narrative_ontology:measurement(bap_lhi_be_t15, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 15, 0.53).
narrative_ontology:measurement_basis(bap_lhi_be_t15, observed).
narrative_ontology:measurement(bap_lhi_be_t18, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 18, 0.56).
narrative_ontology:measurement_basis(bap_lhi_be_t18, observed).
narrative_ontology:measurement(bap_lhi_be_t21, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 21, 0.75).
narrative_ontology:measurement_basis(bap_lhi_be_t21, observed).
narrative_ontology:measurement(bap_lhi_be_t24, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 24, 0.65).
narrative_ontology:measurement_basis(bap_lhi_be_t24, observed).

% Suppression requirement over time
narrative_ontology:measurement(bap_lhi_su_t0, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(bap_lhi_su_t0, observed).
narrative_ontology:measurement(bap_lhi_su_t3, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 3, 0.5).
narrative_ontology:measurement_basis(bap_lhi_su_t3, observed).
narrative_ontology:measurement(bap_lhi_su_t6, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 6, 0.52).
narrative_ontology:measurement_basis(bap_lhi_su_t6, observed).
narrative_ontology:measurement(bap_lhi_su_t9, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 9, 0.54).
narrative_ontology:measurement_basis(bap_lhi_su_t9, observed).
narrative_ontology:measurement(bap_lhi_su_t12, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 12, 0.56).
narrative_ontology:measurement_basis(bap_lhi_su_t12, observed).
narrative_ontology:measurement(bap_lhi_su_t15, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 15, 0.58).
narrative_ontology:measurement_basis(bap_lhi_su_t15, observed).
narrative_ontology:measurement(bap_lhi_su_t18, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 18, 0.6).
narrative_ontology:measurement_basis(bap_lhi_su_t18, observed).
narrative_ontology:measurement(bap_lhi_su_t21, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 21, 0.82).
narrative_ontology:measurement_basis(bap_lhi_su_t21, observed).
narrative_ontology:measurement(bap_lhi_su_t24, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 24, 0.68).
narrative_ontology:measurement_basis(bap_lhi_su_t24, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_health_intervention__bodily_autonomy_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(legitimate_health_intervention__bodily_autonomy_primary, legitimate_health_intervention__public_health_primary).
narrative_ontology:affects_constraint(legitimate_health_intervention__bodily_autonomy_primary, legitimate_health_intervention__proportionality_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'legitimate health intervention' decomposes into three structurally distinct readings of one kernel. This reading (bodily_autonomy_primary) authors epsilon 0.65 with mandate-coerced individuals, penalized refusers, and adverse-event bearers in the victim set and the state as extractor via employment/access leverage. The public_health_primary sibling removes the coerced from the victim set (refusal recast as externality imposition) and drops epsilon toward coordination-cost range. The proportionality_reading sibling makes epsilon a function of disease characteristics, confining victims to disproportionate interventions. Each family member carries its own epsilon, stakeholders, and classification; this file links to both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
