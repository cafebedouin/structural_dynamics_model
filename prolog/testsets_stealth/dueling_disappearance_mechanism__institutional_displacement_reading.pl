% ============================================================================
% CONSTRAINT STORY: dueling_disappearance_mechanism__institutional_displacement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dueling_disappearance_mechanism__institutional_displacement_reading, []).

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
 *   constraint_id: dueling_disappearance_mechanism__institutional_displacement_reading
 *   human_readable: Gentleman-Class Duel Protocol as Peer Dispute-Resolution Coordination
 *   domain: historical sociology/legal history/cultural anthropology
 *
 * SUMMARY:
 *   Under this reading, the gentleman-class duel protocol was a working
 *   coordination mechanism for dispute resolution among armed social equals,
 *   and it fell to the fringe because competing institutions outperformed it
 *   at its own job. Professionalizing courts delivered enforceable judgments;
 *   bank credit and commercial reference systems made reputation financially
 *   legible without blood; libel actions gave insult a lawful remedy. As
 *   these substitutes matured across the interval (T measured in years from a
 *   1770 baseline; T=90 corresponds to 1860), gentlemen exited the protocol
 *   voluntarily, fastest where substitutes arrived earliest, slowest in the
 *   gaps the substitutes could not reach (frontier districts, the officer
 *   corps). The protocol persisted as an available-but-disfavored option,
 *   increasingly ritualized. KEY AGENTS (by structural relationship): -
 *   honor_community_gentlemen: Primary beneficiary and cost bearer
 *   (powerful/constrained) — collects feud-proof dispute settlement, bears
 *   the mortality tail - military_officer_corps: Beneficiary with the
 *   heaviest cost tail (organized/constrained) - political_officeholders:
 *   Beneficiary (powerful/constrained) — used the protocol for partisan
 *   quarrels - duel_seconds_and_arbiters: Protocol administrators
 *   (moderate/constrained) — negotiate, interpret, arrange - honor_refusers:
 *   Excluded voice (moderate/trapped) — prefer courts, priced out of refusal
 *   until substitutes legitimize it - clerical_and_press_reformers:
 *   Opposition observer seat (organized/mobile) - legal_historians:
 *   Analytical observer (analytical/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dueling_disappearance_mechanism__institutional_displacement_reading, 0.18).
domain_priors:suppression_score(dueling_disappearance_mechanism__institutional_displacement_reading, 0.15).
domain_priors:theater_ratio(dueling_disappearance_mechanism__institutional_displacement_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, accessibility_collapse, 0.22).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dueling_disappearance_mechanism__institutional_displacement_reading, rope).
narrative_ontology:human_readable(dueling_disappearance_mechanism__institutional_displacement_reading, "Gentleman-Class Duel Protocol as Peer Dispute-Resolution Coordination").
narrative_ontology:topic_domain(dueling_disappearance_mechanism__institutional_displacement_reading, "historical sociology/legal history/cultural anthropology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dueling_disappearance_mechanism__institutional_displacement_reading, '30d02a6e-1155-4025-ab54-098eb219794f').
narrative_ontology:cs_kernel_codification('30d02a6e-1155-4025-ab54-098eb219794f', formalized).
narrative_ontology:cs_authority_grounding('30d02a6e-1155-4025-ab54-098eb219794f', practice).
narrative_ontology:cs_interpretation_layer_present('30d02a6e-1155-4025-ab54-098eb219794f').
narrative_ontology:cs_reading_relation('30d02a6e-1155-4025-ab54-098eb219794f', dueling_disappearance_mechanism__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('30d02a6e-1155-4025-ab54-098eb219794f', dueling_disappearance_mechanism__overdetermined_composite_reading, coexists_with).
narrative_ontology:cs_axiom('30d02a6e-1155-4025-ab54-098eb219794f', foundational, institutional_substitution_sufficiency).
narrative_ontology:cs_axiom_status(institutional_substitution_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('30d02a6e-1155-4025-ab54-098eb219794f', institutional_substitution_sufficiency, empirically_contingent).
narrative_ontology:cs_axiom('30d02a6e-1155-4025-ab54-098eb219794f', secondary, voluntary_participation_no_coerced_victim_set).
narrative_ontology:cs_axiom_status(voluntary_participation_no_coerced_victim_set, holdable).
narrative_ontology:cs_axiom_grounding('30d02a6e-1155-4025-ab54-098eb219794f', voluntary_participation_no_coerced_victim_set, empirically_contingent).
narrative_ontology:cs_reference_frame('30d02a6e-1155-4025-ab54-098eb219794f', operative_honor_dispute_standard).
narrative_ontology:cs_drift_state('30d02a6e-1155-4025-ab54-098eb219794f', post_institutional_maturation, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('30d02a6e-1155-4025-ab54-098eb219794f', '').
narrative_ontology:cs_kernel_id(dueling_disappearance_mechanism__institutional_displacement_reading, dueling_disappearance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, honor_community_gentlemen).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, military_officer_corps).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, political_officeholders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__institutional_displacement_reading, honor_community_gentlemen).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__institutional_displacement_reading, military_officer_corps).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__institutional_displacement_reading, political_officeholders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Members of the propertied, educated class bound by the honor code. They receive feud-proof settlement of insults and quarrels and the standing that follows demonstrated courage; they bear the mortality risk, the expense of arms and seconds, and the social penalty for refusing a challenge. Before courts became a respectable alternative, declining meant exclusion from polite society; afterward, refusal could be presented as a preference for lawful remedy.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, honor_community_gentlemen, beneficiary,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(dueling_disappearance_mechanism__institutional_displacement_reading, honor_community_gentlemen, payer).

% Commissioned officers in services where the code governed peer standing and claims to courage. The protocol disciplined the officer body and adjudicated rivalries that mess culture could not absorb; it also killed officers at the highest rate of any participating group, and service discipline made refusal professionally ruinous. Exit was bounded by martial regulation and the intimacy of garrison life.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, military_officer_corps, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(dueling_disappearance_mechanism__institutional_displacement_reading, military_officer_corps, payer).

% Legislators, ministers, and party leaders who used the protocol to settle quarrels provoked by partisan newspapers. A duel answered a slander quickly and publicly where a libel action consumed years and satisfied no one; the price was the occasional death of indispensable men and criminal exposure where statutes existed.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, political_officeholders, beneficiary,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(dueling_disappearance_mechanism__institutional_displacement_reading, political_officeholders, payer).

% Experienced gentlemen who negotiated terms, interpreted the code, pressed for apology, and arranged grounds and weapons. They administered the protocol's procedures and absorbed disputes about its proper conduct; their standing rested on faithful service, and declining a request to act as second carried its own reputational cost.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, duel_seconds_and_arbiters, agenda_setter,
    moderate, biographical, constrained, regional).

% Men inside the honor class who preferred legal remedy or quiet endurance to combat but could not afford the sanction for saying so. Their preference was structurally unspeakable while refusal meant ruin; it surfaced in numbers only once courts and credit markets offered a face-saving alternative, and their mass exit is the substitution this story tracks.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, honor_refusers, excluded,
    moderate, biographical, trapped, national).

% Clergy, editors, and pamphleteers who documented casualties, condemned the practice from pulpit and column, and lobbied for statutes. They collected nothing from the protocol and paid nothing into it; their leverage grew as public opinion turned and as the substitutes gave wavering gentlemen somewhere else to stand.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, clerical_and_press_reformers, observer,
    organized, generational, mobile, national).

% Modern scholars reconstructing the protocol's operation and decline from case records, correspondence, and duel statistics across jurisdictions. They see the full structure, collect nothing from it, and bear none of its risks.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, legal_historians, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dueling_disappearance_mechanism__institutional_displacement_reading, diffuse).
narrative_ontology:fixing_cost_class(dueling_disappearance_mechanism__institutional_displacement_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives social equals a rule-governed, mutually understood procedure for resolving honor disputes: challenge, negotiation by seconds, agreed terms, single combat or apology. It replaces open-ended feud and private war with a bounded, scripted encounter whose outcome both parties and the community accept as closing the account.
% TRANSFER_FUNCTION: Moves mortal risk and compliance burden onto the duel principals (with legal exposure for seconds); moves restored standing and public vindication to the aggrieved party; moves deference toward demonstrated courage. Grievance accounts are settled at the price of risk borne by the participants themselves.
% ABSENT_VOICES: The men who wanted to refuse but could not afford to say so sit inside the honor community as a silenced bloc, represented here by honor_refusers; bereaved families of the killed had no seat in the protocol's deliberations; religious dissenters within the class objected continuously but were discounted as cowardice until legal alternatives made their position affordable.
% DISAPPEARANCE_RATIONALE: Through most of the interval the honor economy ran on the protocol: removing it overnight before the substitutes matured would have reopened feud dynamics among armed equals and forced improvised, less trusted arbitration. The historical record shows the actual rearrangement took decades and tracked the maturation of courts, credit instruments, and libel remedies; after substitution, only the residual fringe depended on it.
% FOUNDING_PROBLEM: Armed social equals needed a way to settle grievances among themselves where courts were distant, slow, partial, or considered beneath a gentleman's dignity, without sliding into destructive feuds that would consume the class.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary clerical and editorial opponents attested from outside the benefiting class that the founding problem was real while condemning the remedy; modern legal historians corroborate that thin court coverage and informal credit left a dispute-resolution vacuum the protocol filled, and that expanded court jurisdiction, commercial credit reporting, and libel actions dissolved it. No attestation from the protocol's own beneficiaries is relied upon.
narrative_ontology:disappearance_verdict(dueling_disappearance_mechanism__institutional_displacement_reading, world_rearranges).
narrative_ontology:founding_problem_status(dueling_disappearance_mechanism__institutional_displacement_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dueling_disappearance_mechanism__institutional_displacement_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dueling_disappearance_mechanism__institutional_displacement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dueling_disappearance_mechanism__institutional_displacement_reading, 0.18, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dueling_disappearance_mechanism__institutional_displacement_reading_tests).
:- end_tests(dueling_disappearance_mechanism__institutional_displacement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon's referent is the standing arrangement under contest — the honor-class duel protocol across 1770-1860 — assessed by this reading's own lights, never the endorsed institutional alternative. Extractiveness is low (0.12 rising to 0.18): participation delivered a real service (feud-proof settlement) at a price the participants accepted, and the gentle rise tracks functional atrophy — late-interval participants paid the full risk premium for a settlement the courts could now deliver more safely, widening the gap between cost borne and service received. Suppression is authored as a declining series (0.42 to 0.15) because this story specifically tracks enforcement-capacity decay: the honor community's sanction on refusers lost force as courts legitimized refusal, which is precisely the mechanism by which voluntary exit became possible; the end-state scalar 0.15 matches the series endpoint. Theater rises steeply (0.10 to 0.52) as deloping and pre-agreed misses spread — the deloping_function_ambiguity omega records whether this is exit technology or empty performance. Accessibility_collapse is low (0.22) because this reading's signature claim is that alternatives never collapsed: courts, banking, and libel law stayed fully accessible and were chosen. Resistance is moderate (0.45): clerical campaigns and statutes opposed the protocol for decades without killing it, consistent with a mechanism participants kept selecting on merit until something better arrived. Claimed type is rope on structural grounds — genuine coordination function, beneficiary class, no victim set, no dedicated coercive machinery — authored independently of the metric values; the engine computes per-seat classifications from the structural data.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the gentlemen's and officers' positions the protocol is a service they drew on: low effective extraction, genuine coordination. From the honor_refusers' position the same structure operated as coercion — trapped exit, sanction-priced silence — yielding materially higher effective extraction for that seat despite identical nominal class standing; what differentiates them is not power but exit options, which the derivation reads from the structural declarations. The seconds' administrative seat sits near symmetric: they ran the machinery and bore its liabilities. The reformer and historian seats observe without transacting. The engine computes this divergence from power, exit, and role data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   All three beneficiary groups are declared in base_properties.beneficiaries, driving their derived directionality toward the beneficiary end. Overrides are required for the two dual-positioned atoms because naive derivation from the beneficiary declaration alone would push the powerful and organized seats to near-full-beneficiary values, ignoring that these same seats bear the protocol's entire cost tail: honor_community_gentlemen and political_officeholders (powerful, d=0.35) and military_officer_corps (organized, d=0.40) were net beneficiaries who nonetheless supplied all of the mortality risk — the officer corps at the highest rate. The overrides place them net-subsidized but visibly short of pure beneficiary. The honor_refusers' position is carried by their excluded role and trapped exit in the stakeholder surface rather than by a directionality override, because the moderate power atom is shared with the willing administrators, whom an override would distort.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — settle grievances among armed equals where courts could not — is dead by interval end: the substitutes solved it, which is why founding_problem_status is dead while disappearance_verdict remains world_rearranges (the dependence was real for most of the interval and unwound slowly). mandatrophy_resolved is declared true accordingly. The classification work is preventing two symmetrical mislabels. Calling this a snare because men died mistakes the voluntary risk premium of a service participants kept buying for extraction backed by suppressed exits — the no-victim-set premise is load-bearing and is routed to its own omega. Calling the end-state a piton because theater exceeds half mistakes a mechanism that retains real function in genuine institutional gaps for one maintained by pure inertia; the fringe_gap_functionality omega makes that an empirical question rather than an assumption. The receipt surface records the honest end-state: gains diffuse across the honor class, no seat captures the compliance costs, and fixing (abandoning the protocol) was cheap once substitutes existed — the transient-neglect cell, not the captured or piton cells, which is what voluntary displacement looks like in the receipt grammar.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_institutional_displacement,
    'This constraint instantiates the institutional_displacement_reading of kernel dueling_disappearance_mechanism; would instantiating contraction_reading or overdetermined_composite_reading instead produce a structurally different constraint?',
    'Author the sibling readings as separate stories over the same referent and compare computed types and trajectories: contraction_reading attributes decline to dignity-culture displacement of honor-culture axioms (predicting alternatives rendered unthinkable rather than chosen, changing accessibility_collapse semantics); overdetermined_composite_reading distributes causation across legal prohibition, institutional modernization, cultural shift, and war trauma (predicting the same type with a faster, multi-front decline). The disagreement is located at causal sufficiency: whether institutional availability alone was sufficient to displace the protocol.',
    'Adopting the composite reading would make this story''s single-cause epsilon attribution overstate institutional sufficiency; adopting the contraction reading would relocate the mechanism from incentive structure to cultural axiom displacement, altering the interpretation of accessibility_collapse and the voluntariness of exit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_institutional_displacement, conceptual, 'Committer-frame routing: this story is one reading of the dueling-disappearance kernel; siblings are separate constraints, not parts of this one.').

omega_variable(
    participation_voluntariness,
    'Was duel participation genuinely voluntary coordination among equals, or sanction-coerced compliance that would constitute a victim set?',
    'Examine refusal rates and sanction severity across periods and jurisdictions, controlling for court access: if refusers faced systematic social and professional ruin independent of whether legal alternatives existed, the coercion was structural rather than incidental.',
    'A substantial coerced-participant set introduces victims, raises effective extraction for the powerless-within-the-class seats, and pushes classification away from pure coordination toward a hybrid with asymmetric extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(participation_voluntariness, empirical, 'Whether the no-victim-set premise of this reading survives scrutiny of refusal sanctions.').

omega_variable(
    cross_jurisdiction_sufficiency,
    'Does institutional substitute quality predict dueling decline across jurisdictions, as this reading''s mechanism requires, or did dueling persist alongside strong courts?',
    'Matched comparison of jurisdictions similar in honor-culture intensity but different in court reliability, credit infrastructure, and libel remedies (antebellum United States North versus South; Britain versus France, where functional courts coexisted with thriving dueling into the twentieth century).',
    'If strong courts systematically coexisted with persistent dueling, institutional substitution is insufficient on its own and the composite reading gains support; if court quality predicts decline, this reading''s signature prediction is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cross_jurisdiction_sufficiency, empirical, 'Jurisdictional test of whether substitute quality, not culture alone, drove displacement.').

omega_variable(
    fringe_gap_functionality,
    'Is residual late-interval dueling sustained by genuine institutional gaps the substitutes could not reach, or by inertial identity performance?',
    'Correlate residual duel incidence with local court access, credit-market penetration, and legal coverage of the participant population (frontier districts, officer corps exempt from civilian remedy); gap-correlated persistence supports continued functional coordination.',
    'Gap-correlated persistence confirms a coordination mechanism retaining real function at the margins; identity-driven persistence independent of gaps indicates the residue is performance, shifting interpretation toward degraded inertia.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fringe_gap_functionality, empirical, 'Tests the reading''s claim that surviving duels occupy real institutional gaps.').

omega_variable(
    deloping_function_ambiguity,
    'Is the rising ritualization of encounters (deloping, pre-agreed misses, bloodless satisfactions) a face-saving exit technology that eased voluntary substitution, or degenerate performative maintenance of a hollowed form?',
    'Sequence analysis: determine whether ritualized encounters progressively replaced lethal ones within the same communities (consistent with an exit ramp that lowered the cost of leaving the protocol) or coexisted with unchanged lethality elsewhere (consistent with empty performance).',
    'The exit-technology reading keeps rising theater benign and consistent with voluntary displacement; the performance reading recodes the theater trajectory as a symptom of functional atrophy maintained by inertia.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(deloping_function_ambiguity, conceptual, 'Ambiguity in what the rising theater ratio measures during the decline phase.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dueling_disappearance_mechanism__institutional_displacement_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inst_disp_duel_tr_t0, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(inst_disp_duel_tr_t15, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 15, 0.15).
narrative_ontology:measurement(inst_disp_duel_tr_t30, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement(inst_disp_duel_tr_t45, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 45, 0.3).
narrative_ontology:measurement(inst_disp_duel_tr_t60, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 60, 0.38).
narrative_ontology:measurement(inst_disp_duel_tr_t75, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 75, 0.45).
narrative_ontology:measurement(inst_disp_duel_tr_t90, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 90, 0.52).

% Extraction over time
narrative_ontology:measurement(inst_disp_duel_be_t0, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(inst_disp_duel_be_t15, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 15, 0.13).
narrative_ontology:measurement(inst_disp_duel_be_t30, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 30, 0.14).
narrative_ontology:measurement(inst_disp_duel_be_t45, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 45, 0.15).
narrative_ontology:measurement(inst_disp_duel_be_t60, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 60, 0.16).
narrative_ontology:measurement(inst_disp_duel_be_t75, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 75, 0.17).
narrative_ontology:measurement(inst_disp_duel_be_t90, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 90, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(inst_disp_duel_su_t0, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(inst_disp_duel_su_t15, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 15, 0.4).
narrative_ontology:measurement(inst_disp_duel_su_t30, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 30, 0.36).
narrative_ontology:measurement(inst_disp_duel_su_t45, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 45, 0.31).
narrative_ontology:measurement(inst_disp_duel_su_t60, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 60, 0.26).
narrative_ontology:measurement(inst_disp_duel_su_t75, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 75, 0.2).
narrative_ontology:measurement(inst_disp_duel_su_t90, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 90, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dueling_disappearance_mechanism__institutional_displacement_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__institutional_displacement_reading, contraction_reading).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__institutional_displacement_reading, overdetermined_composite_reading).

% DUAL FORMULATION NOTE:
% Family member of kernel dueling_disappearance_mechanism. The colloquial label 'why dueling disappeared' decomposes into structurally distinct claims per the epsilon-invariance principle: this story authors the institutional-substitution claim (incentive-side displacement by competing dispute-resolution mechanisms; low epsilon, no victim set, low accessibility_collapse because alternatives stayed open and won). Sibling stories author the cultural-axiom claim (contraction_reading) and the multi-cause claim (overdetermined_composite_reading) over the same referent, each with its own epsilon, metrics, and stakeholders. These edges are family links for contamination-propagation analysis, not causal assertions between mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dueling_disappearance_mechanism__institutional_displacement_reading, powerful, 0.35).
constraint_indexing:directionality_override(dueling_disappearance_mechanism__institutional_displacement_reading, organized, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
