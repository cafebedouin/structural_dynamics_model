% ============================================================================
% CONSTRAINT STORY: refugee_convention_text__expansive_humanitarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_refugee_convention_text__expansive_humanitarian_reading, []).

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
 *   constraint_id: refugee_convention_text__expansive_humanitarian_reading
 *   human_readable: Refugee Convention Text Read as Unbendable Humanitarian Mandate (Expansive Reading)
 *   domain: international_law/migration_governance/human_rights
 *
 * SUMMARY:
 *   The standing arrangement under contest is the international refugee
 *   protection regime anchored in the 1951 Convention and 1967 Protocol text.
 *   This story instantiates ONE reading of that fixed text — the expansive
 *   humanitarian reading — under which the convention imposes an unbendable
 *   mandate: fear of persecution is well-founded even amid generalized
 *   violence and at the hands of non-state actors; the
 *   particular-social-group ground reaches gender, sexual orientation and
 *   gender identity, and clan affiliation; interception on the high seas and
 *   offshore transfer arrangements violate the non-return duty; and every
 *   arrival who raises a protection claim is owed a substantive assessment.
 *   The claim/metric gap is deliberate: the reading CLAIMS a
 *   coordination-bearing humanitarian floor while the authored metrics
 *   describe a structure that also imposes substantial, asymmetrically
 *   distributed, actively enforced costs on states — the engine measures that
 *   divergence; neither side was tuned to the other. Time points index years
 *   elapsed since the 1967 Protocol (t0 = 1967, t50 approximately the late
 *   2010s).
 *
 * KEY AGENTS:
 *   - asylum_seekers_fleeing_generalized_violence: Primary protected class (powerless/trapped) — gains admission and status only where the mandate's breadth applies
 *   - lgbtq_and_gender_persecution_claimants: Protected class whose claims exist only under the expansive social-group doctrine (powerless/trapped)
 *   - clan_minority_fleeing_members: Protected class relying on non-state persecution recognition (powerless/trapped)
 *   - receiving_states: Primary bearer of obligations (institutional/constrained) — funds and runs adjudication, resists through externalization designs
 *   - frontline_transit_states: Disproportionate cost bearers (organized/constrained) — host the largest displaced populations under the same duties
 *   - unhcr_supervisory_mission: Interpretive agenda-setter (institutional/analytical) — consolidates the reading through the Handbook and guidelines
 *   - national_and_regional_courts: Enforcement agenda-setter (institutional/analytical) — strike down interdiction and offshore-transfer schemes
 *   - human_rights_litigation_ngos: Enforcement drivers and mission beneficiaries (organized/mobile)
 *   - smuggling_networks: Incidental collectors on the lawful-access gap (organized/arbitrage)
 *   - sovereignist_political_movements: Excluded objectors (organized/constrained) — no seat in the doctrinal conversation
 *   - comparative_refugee_law_scholars: Analytical observers (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(refugee_convention_text__expansive_humanitarian_reading, 0.62).
domain_priors:suppression_score(refugee_convention_text__expansive_humanitarian_reading, 0.55).
domain_priors:theater_ratio(refugee_convention_text__expansive_humanitarian_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(refugee_convention_text__expansive_humanitarian_reading, tangled_rope).
narrative_ontology:human_readable(refugee_convention_text__expansive_humanitarian_reading, "Refugee Convention Text Read as Unbendable Humanitarian Mandate (Expansive Reading)").
narrative_ontology:topic_domain(refugee_convention_text__expansive_humanitarian_reading, "international_law/migration_governance/human_rights").

domain_priors:requires_active_enforcement(refugee_convention_text__expansive_humanitarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(refugee_convention_text__expansive_humanitarian_reading, '3321a30e-d2a4-45a9-a6fe-b2c3aa9d0281').
narrative_ontology:cs_kernel_codification('3321a30e-d2a4-45a9-a6fe-b2c3aa9d0281', fixed_text).
narrative_ontology:cs_authority_grounding('3321a30e-d2a4-45a9-a6fe-b2c3aa9d0281', lineage).
narrative_ontology:cs_interpretation_layer_present('3321a30e-d2a4-45a9-a6fe-b2c3aa9d0281').
narrative_ontology:cs_reading_relation('3321a30e-d2a4-45a9-a6fe-b2c3aa9d0281', refugee_convention_text__restrictive_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('3321a30e-d2a4-45a9-a6fe-b2c3aa9d0281', refugee_convention_text__procedural_integrity_reading, coexists_with).
narrative_ontology:cs_axiom('3321a30e-d2a4-45a9-a6fe-b2c3aa9d0281', foundational, non_refoulement_absolute_regardless_of_arrival_manner).
narrative_ontology:cs_axiom_status(non_refoulement_absolute_regardless_of_arrival_manner, holdable).
narrative_ontology:cs_axiom_grounding('3321a30e-d2a4-45a9-a6fe-b2c3aa9d0281', non_refoulement_absolute_regardless_of_arrival_manner, deontological).
narrative_ontology:cs_axiom('3321a30e-d2a4-45a9-a6fe-b2c3aa9d0281', foundational, persecution_risk_includes_non_state_and_generalized_harm).
narrative_ontology:cs_axiom_status(persecution_risk_includes_non_state_and_generalized_harm, holdable).
narrative_ontology:cs_axiom_grounding('3321a30e-d2a4-45a9-a6fe-b2c3aa9d0281', persecution_risk_includes_non_state_and_generalized_harm, deontological).
narrative_ontology:cs_reference_frame('3321a30e-d2a4-45a9-a6fe-b2c3aa9d0281', unbendable_humanitarian_mandate).
narrative_ontology:cs_drift_state('3321a30e-d2a4-45a9-a6fe-b2c3aa9d0281', contemporary_externalization_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3321a30e-d2a4-45a9-a6fe-b2c3aa9d0281', '').
narrative_ontology:cs_kernel_id(refugee_convention_text__expansive_humanitarian_reading, refugee_convention_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(refugee_convention_text__expansive_humanitarian_reading, asylum_seekers_fleeing_generalized_violence).
narrative_ontology:constraint_beneficiary(refugee_convention_text__expansive_humanitarian_reading, lgbtq_and_gender_persecution_claimants).
narrative_ontology:constraint_beneficiary(refugee_convention_text__expansive_humanitarian_reading, clan_minority_fleeing_members).
narrative_ontology:constraint_beneficiary(refugee_convention_text__expansive_humanitarian_reading, human_rights_litigation_ngos).
narrative_ontology:constraint_beneficiary(refugee_convention_text__expansive_humanitarian_reading, smuggling_networks).
narrative_ontology:constraint_victim(refugee_convention_text__expansive_humanitarian_reading, receiving_states).
narrative_ontology:constraint_victim(refugee_convention_text__expansive_humanitarian_reading, frontline_transit_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(refugee_convention_text__expansive_humanitarian_reading, national_and_regional_courts).
narrative_ontology:constraint_vindicates(refugee_convention_text__expansive_humanitarian_reading, non_refoulement_customary_norm).
narrative_ontology:constraint_vindicates(refugee_convention_text__expansive_humanitarian_reading, non_state_persecution_doctrine).
narrative_ontology:constraint_vindicates(refugee_convention_text__expansive_humanitarian_reading, expansive_particular_social_group_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Flee civil war, collapsed governance, or indiscriminate targeting. They cannot return without facing serious harm, and survival depends on reaching a state that accepts jurisdiction and assesses the claim on its merits. Their claim succeeds only where the reading counts generalized violence as grounding fear; under narrower readings most would be returned to the conditions they fled.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, asylum_seekers_fleeing_generalized_violence, beneficiary,
    powerless, immediate, trapped, global).

% Face violence from families, communities, or private actors because of identity or gender transgression, often with the origin state unwilling or unable to protect them. Recognition turns on whether membership in the group counts as a protected ground; the claim is frequently the only lawful path out of return to the persecuting environment.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, lgbtq_and_gender_persecution_claimants, beneficiary,
    powerless, biographical, trapped, global).

% Targeted by rival clans or clan-aligned militias in polities where clan affiliation determines physical security. Their protection depends on the reading treating clan membership as a social group and non-state actors as potential persecutors; without both extensions they have no qualifying claim.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, clan_minority_fleeing_members, beneficiary,
    powerless, biographical, trapped, regional).

% Party to the treaty and operator of asylum systems: reception, adjudication, housing, appeals. They shaped the original settlement and retain treaty-making power, but find exit costly given customary-norm arguments and reputational exposure, so resistance runs through externalization designs, safe-third-country designations, and procedural narrowing that courts then review against the mandate.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, receiving_states, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__expansive_humanitarian_reading, receiving_states, agenda_setter).

% Host the largest displaced populations, often for decades, with limited fiscal room. They bear the heaviest hosting load under the same obligations as wealthy distant states, convert the burden into aid negotiations, and cannot close borders without triggering refoulement findings and regional instability.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, frontline_transit_states, payer,
    organized, generational, constrained, regional).

% Supervises application of the treaty, issued the Handbook and the social-group guidelines that consolidated the expansive reading, intervenes in litigation, and monitors compliance. It depends on state funding while pressing states toward broader application of the duties.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, unhcr_supervisory_mission, agenda_setter,
    institutional, generational, analytical, global).

% Adjudicate refoulement, social-group, and extraterritorial-application claims; have struck down maritime interception and offshore-transfer schemes; and accrue docket volume and doctrinal authority as the mandate's breadth widens the flow of justiciable claims.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, national_and_regional_courts, agenda_setter,
    institutional, generational, analytical, continental).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__expansive_humanitarian_reading, national_and_regional_courts, beneficiary).

% Bring strategic cases, publish country-of-origin evidence, and shape doctrine through intervention. Their mission relevance and funding track the volume and salience of protection litigation, and they can redirect effort to adjacent fields if this one closes.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, human_rights_litigation_ngos, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__expansive_humanitarian_reading, human_rights_litigation_ngos, agenda_setter).

% Sell passage along the routes left open by the gap between protection on arrival and scarce lawful channels. Their rents rise when arrival-based protection is generous and legal pathways stay narrow, and they shift routes and methods quickly as enforcement patterns move.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, smuggling_networks, beneficiary,
    organized, immediate, arbitrage, global).

% Mobilize voters against expansive obligations and legislate caps, fast-track procedures, and externalization deals. They hold no seat in the doctrinal conversation where the reading is fixed; their instruments are elections and statutes, which courts then review against the mandate after enactment.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, sovereignist_political_movements, excluded,
    organized, generational, constrained, national).

% Trace the interpretive evolution from treaty text to Handbook to jurisprudence, document the divergence between the affirmed mandate and border practice, and supply the doctrinal genealogy on which courts and advocates draw.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, comparative_refugee_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(refugee_convention_text__expansive_humanitarian_reading, diffuse).
narrative_ontology:fixing_cost_class(refugee_convention_text__expansive_humanitarian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a common floor beneath interstate competition during displacement crises: by fixing who qualifies and forbidding return to harm, it removes the payoff each state would otherwise collect from pushing arrivals back, which unchecked would cascade into chain refoulement and regional destabilization. It also allocates the duty to assess claims through a shared definition rather than bilateral improvisation.
% TRANSFER_FUNCTION: Moves protection duties — reception, adjudication, housing, non-return — from arriving asylum seekers onto receiving and transit states; moves legal status and physical safety to recognized refugees; moves interpretive authority over the treaty text to courts and the supervisory mission; and, through the lawful-access gap, moves payment to smuggling networks.
% ABSENT_VOICES: Sovereignist constituencies hold no seat in the doctrinal conversation that fixes the reading — they speak through elections and statutes that courts review afterward. Frontline host states were marginal in the interpretive fora where the expansive reading consolidated, despite bearing the largest share of its costs. Rejected claimants rarely appear anywhere once their claim fails. Origin-country persecutors are definitionally absent.
% DISAPPEARANCE_RATIONALE: If the expansive mandate vanished overnight, maritime interception, offshore transfer, and summary return would resume at scale within months; the millions whose claims rest on generalized-violence, non-state, and social-group grounds would lose their qualifying pathway; the litigation, supervision, and advocacy economy built on the mandate would contract sharply; and bordering states would face renewed chain-refoulement cascades as each neighbor competed to push arrivals further back.
% FOUNDING_PROBLEM: Mid-twentieth-century mass displacement met closed borders: Jewish refugees were refused landing before the war, wartime returns delivered people directly to persecution, and postwar displacement had no standing rule for who must assess a protection claim. The convention was built so that flight from persecution would always find a hearing and never an automatic handover.
% FOUNDING_PROBLEM_CORROBORATION: Displacement totals in UNHCR statistical reporting, persecution findings in court records across jurisdictions, and ICRC and independent journalistic documentation of the conflicts driving flight all attest that the founding problem persists at greater scale than at founding; none of these sources sits inside the population the mandate protects.
narrative_ontology:disappearance_verdict(refugee_convention_text__expansive_humanitarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(refugee_convention_text__expansive_humanitarian_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(refugee_convention_text__expansive_humanitarian_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(refugee_convention_text__expansive_humanitarian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(refugee_convention_text__expansive_humanitarian_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(refugee_convention_text__expansive_humanitarian_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(refugee_convention_text__expansive_humanitarian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(refugee_convention_text__expansive_humanitarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.62 because the mandate imposes reception, adjudication, housing, and non-return duties on states at levels decoupled from their capacity or consent — deliberately so, since the reading's defining feature is unbendability — with the load concentrated on whoever sits at the frontier of arrival. Suppression is 0.55 as a RAW structural property (unscaled by power or scope; only extractiveness is scaled downstream): the arrangement is held up by courts voiding removals and transfers, supervisory monitoring, and the claimed customary closure of exit, though states retain real policy levers short of refoulement. Theater_ratio is 0.30: the core activity — individual status determination — is functionally real, but a growing compliance-performance layer (abbreviated offshore assessments, safe-third-country fictions, box-ticking interviews) rides on top of it, which the temporal series shows accumulating. Accessibility_collapse is 0.40 because alternatives — interdiction, offshore transfer, pushback — remain visibly and partially available wherever enforcement is weak, as documented practice shows. Resistance is 0.60: sustained, organized state resistance expressed through externalization schemes, procedural narrowing, and periodic open defiance of rulings. All three temporal series share one grid (t0..t50 at decade steps) so no metric row borrows another's endpoints. On the receipt surface: gains were checked seat by seat — protection value spreads across the three protected classes, doctrinal authority accrues to courts, mandate relevance to the supervisory mission, route rents to smuggling networks — and no single seat captures the majority, hence the affirmative 'diffuse'. Fixing (removing or bending the mandate) is prohibitive for any capable fixer: unilateral defection invites adverse rulings and reputational cost, and collective renegotiation faces holdout problems and humanitarian backlash exceeding the perceived fiscal relief.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently and the structural data is what drives that. From the receiving-state seat (institutional power, constrained exit, target-side position) the arrangement presents as imposed obligation with real but partial escape valves. From the three protected-class seats (powerless, trapped) the same structure presents as lifeline — the difference between a hearing and a handover. From the court and supervisory seats the structure presents as a legitimacy-bearing order they administer and expand. From the smuggling seat it presents as a rent surface. The engine derives these per-seat classifications from the declared positions; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   The three protected-class groups sit at the beneficiary end (d near 0): the mandate subsidizes them with safety and status at states' expense, and their trapped exit pins them to whatever protection the reading secures. Receiving states and frontline transit states sit at the target end (d near 1): they bear the transfer of duties, with constrained exit — customary-norm claims and reputational exposure close denunciation, and geography closes avoidance for frontline states more tightly than for distant ones. Courts and the supervisory mission are agenda-setters near the symmetric middle: they pay little and collect authority and mandate rather than money. Litigation NGOs benefit incidentally (mission relevance tracks caseload) with mobile exit. Smuggling networks are the sharpest incidental beneficiary: the mandate protects on arrival while leaving lawful channels narrow, and the resulting gap is priced by route sellers whose arbitrage-grade mobility puts them nearest the beneficiary pole. Because the extracted value disperses across protected classes, courts, the supervisory organ, and route rents rather than pooling in any one seat, gain_flow is authored as 'diffuse' after checking every named seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — mid-century mass displacement meeting closed borders and automatic handover — is live at larger scale, so this is not a mandate outliving its function and no mandatrophy resolution is declared. The classification discipline cuts both ways here. Against the snare mislabel: the coordination function is genuine — a common floor removing the payoff each state would otherwise collect from pushing arrivals back, which would cascade into chain refoulement — and the cost-bearing seats retain real policy space short of violation. Against the rope mislabel: the imposition is deliberately unbendable, asymmetrically distributed (frontline states pay far more than distant free-riders for the same floor), and requires continuous enforcement against resisting parties. Both halves are structural, which is what the tangled-rope claim asserts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the refugee_convention_text kernel: what structural changes would the restrictive_sovereignty_reading (minimum floor, individualized persecution proof, immutable-characteristic-only social groups) produce if it displaced this reading?',
    'Comparative doctrinal tracking of which reading governs in each major asylum jurisdiction, measured against treaty-body adoption rates and apex-court citation patterns.',
    'Under the restrictive reading the victim set collapses to individually targeted state persecution, interdiction and offshore processing become lawful, and effective extraction redistributes away from states toward rejected claimants.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Sibling-reading delta for the refugee convention kernel.').

omega_variable(
    customary_status_exit_question,
    'How robust is the customary-international-law status of non-refoulement — does exit remain effectively closed even for a state that denounces the treaty?',
    'Observe the post-denunciation behavior of any withdrawing or persistently non-applying state, and how international and national courts treat customary non-refoulement claims against non-parties.',
    'If the customary layer is thin, denunciation becomes a viable exit, the suppression measure drops sharply, and the arrangement drifts toward a voluntary commitment states can price and shed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_status_exit_question, empirical, 'Whether treaty exit is genuinely closed by the customary norm.').

omega_variable(
    frontline_free_rider_asymmetry,
    'Does the mandate concentrate hosting costs on frontline and transit states while distant states free-ride, and does that asymmetry erode the coordination function over time?',
    'Longitudinal burden-sharing data: hosting shares versus resettlement intakes and earmarked funding contributions by income group.',
    'A widening asymmetry would decay the coordination floor for the frontline seats specifically, pushing their computed classification toward pure imposition even if distant-state seats still register coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(frontline_free_rider_asymmetry, empirical, 'Burden asymmetry between frontline hosts and distant states.').

omega_variable(
    psg_boundlessness,
    'Is the expansive particular-social-group category bounded by any principled limit, or is it indefinitely extendable to new identity clusters as litigation generates them?',
    'Convergence analysis of appellate PSG criteria (protected-characteristic versus social-perception tests) across jurisdictions over successive decades.',
    'An unbounded category ratchets state-side costs with no stopping rule and steadily raises effective extraction; a principled boundary stabilizes the reading''s scope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(psg_boundlessness, conceptual, 'Boundary discipline of the particular-social-group doctrine.').

omega_variable(
    externalization_theater_ambiguity,
    'Do offshore-processing and interdiction schemes with abbreviated assessments evade the mandate or adapt it — is the measured theater_ratio capturing evasion or procedural innovation?',
    'Track court outcomes on specific schemes (extraterritorial non-refoulement rulings) and whether each redesigned scheme survives subsequent scrutiny or is struck down in turn.',
    'If schemes systematically evade, the theater series signals rising evasion and drift toward enforced extraction; if redesigned schemes survive as compliant variants, the theater reflects adaptive procedure rather than decay.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externalization_theater_ambiguity, empirical, 'Whether externalization practices are evasion or adaptation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(refugee_convention_text__expansive_humanitarian_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refugee_expansive_reading_tr_t0, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(refugee_expansive_reading_tr_t10, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 10, 0.13).
narrative_ontology:measurement(refugee_expansive_reading_tr_t20, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(refugee_expansive_reading_tr_t30, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement(refugee_expansive_reading_tr_t40, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 40, 0.27).
narrative_ontology:measurement(refugee_expansive_reading_tr_t50, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 50, 0.3).

% Extraction over time
narrative_ontology:measurement(refugee_expansive_reading_be_t0, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(refugee_expansive_reading_be_t10, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(refugee_expansive_reading_be_t20, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(refugee_expansive_reading_be_t30, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 30, 0.54).
narrative_ontology:measurement(refugee_expansive_reading_be_t40, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 40, 0.59).
narrative_ontology:measurement(refugee_expansive_reading_be_t50, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 50, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(refugee_expansive_reading_su_t0, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(refugee_expansive_reading_su_t10, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 10, 0.34).
narrative_ontology:measurement(refugee_expansive_reading_su_t20, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 20, 0.44).
narrative_ontology:measurement(refugee_expansive_reading_su_t30, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 30, 0.5).
narrative_ontology:measurement(refugee_expansive_reading_su_t40, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 40, 0.53).
narrative_ontology:measurement(refugee_expansive_reading_su_t50, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 50, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(refugee_convention_text__expansive_humanitarian_reading, resource_allocation).
narrative_ontology:affects_constraint(refugee_convention_text__expansive_humanitarian_reading, refugee_convention_text__restrictive_sovereignty_reading).
narrative_ontology:affects_constraint(refugee_convention_text__expansive_humanitarian_reading, refugee_convention_text__procedural_integrity_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the Refugee Convention' decomposes into three structurally distinct constraints, one per reading of the fixed text. The expansive reading carries the broadest victim set and the heaviest state-side cost imposition; the restrictive reading narrows victims and maximizes state discretion; the procedural reading holds the threshold flexible while hardening process. This file links to both siblings; influence between them runs through shared jurisprudence, treaty-body practice, and the interpretive authority of the supervisory mission.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
