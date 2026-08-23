% ============================================================================
% CONSTRAINT STORY: common_law_precedent_corpus__strict_stare_decisis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_law_precedent_corpus__strict_stare_decisis, []).

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
 *   constraint_id: common_law_precedent_corpus__strict_stare_decisis
 *   human_readable: Strict Stare Decisis: Backward-Binding Precedent with Extraordinary-Justification Departure Bar
 *   domain: legal/jurisprudential/constitutional
 *
 * SUMMARY:
 *   In the strict-adherence arrangement, the accumulated corpus of judicial
 *   holdings binds forward: a settled holding controls subsequent cases
 *   unless the deciding court articulates extraordinary justification for
 *   departure. The arrangement is presented by its administrators as
 *   constitutive of law itself, fidelity to decided law, while operating
 *   simultaneously as a genuine coordination mechanism (consistency,
 *   predictability, division of adjudicative labor) and as an asymmetric
 *   structure: those positioned inside the corpus's protections collect
 *   certainty rents, while those whose claims conflict with entrenched
 *   holdings face a narrow, costly, composition-sensitive departure channel.
 *   Vertical enforcement (apex reversal of lower-court deviation) and
 *   horizontal restraint (the justification bar at the apex) require
 *   continuous active maintenance. This story is one reading of the
 *   common_law_precedent_corpus kernel; see commentary.kernel_context and
 *   network.dual_formulation_note for the family decomposition. KEY AGENTS
 *   (by structural relationship): - supreme_court_judiciary: Agenda-setter
 *   and institutional beneficiary (institutional/identity_locked) —
 *   administers the binding rule, collects legitimacy and decision-cost
 *   savings - lower_court_judges: Payer (institutional/trapped) — bound
 *   vertically, discretion bounded by inherited holdings -
 *   repeat_player_litigants: Primary beneficiary (powerful/arbitrage) —
 *   shaped the corpus strategically, harvests its rigidity -
 *   incumbent_holding_beneficiaries: Beneficiary (powerful/arbitrage) —
 *   positions vindicated by holdings stay protected -
 *   norm_challenge_litigants: Payer (moderate/constrained) — bears the
 *   extraordinary-justification toll - discrete_and_insular_minorities: Payer
 *   (powerless/trapped) — interests fixed by holdings formed under earlier
 *   norms - future_litigant_generations: Payer (powerless/trapped) — inherit
 *   bindings never consented to - public_interest_advocates: Excluded
 *   (organized/constrained) — confined to the departure petition channel -
 *   constitutional_scholars: Analytical observer (analytical/analytical) —
 *   sees the full structure
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_law_precedent_corpus__strict_stare_decisis, 0.58).
domain_priors:suppression_score(common_law_precedent_corpus__strict_stare_decisis, 0.62).
domain_priors:theater_ratio(common_law_precedent_corpus__strict_stare_decisis, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, extractiveness, 0.58).
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_law_precedent_corpus__strict_stare_decisis, tangled_rope).
narrative_ontology:human_readable(common_law_precedent_corpus__strict_stare_decisis, "Strict Stare Decisis: Backward-Binding Precedent with Extraordinary-Justification Departure Bar").
narrative_ontology:topic_domain(common_law_precedent_corpus__strict_stare_decisis, "legal/jurisprudential/constitutional").

domain_priors:requires_active_enforcement(common_law_precedent_corpus__strict_stare_decisis).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_law_precedent_corpus__strict_stare_decisis, 'cd4a4ad9-493e-48ab-9b43-869fe04cacc6').
narrative_ontology:cs_kernel_codification('cd4a4ad9-493e-48ab-9b43-869fe04cacc6', formalized).
narrative_ontology:cs_authority_grounding('cd4a4ad9-493e-48ab-9b43-869fe04cacc6', lineage).
narrative_ontology:cs_interpretation_layer_present('cd4a4ad9-493e-48ab-9b43-869fe04cacc6').
narrative_ontology:cs_reading_relation('cd4a4ad9-493e-48ab-9b43-869fe04cacc6', common_law_precedent_corpus__evolutionary_framework, coexists_with).
narrative_ontology:cs_reading_relation('cd4a4ad9-493e-48ab-9b43-869fe04cacc6', common_law_precedent_corpus__pluralist_balancing, coexists_with).
narrative_ontology:cs_axiom('cd4a4ad9-493e-48ab-9b43-869fe04cacc6', foundational, holdings_bind_absent_extraordinary_justification).
narrative_ontology:cs_axiom_status(holdings_bind_absent_extraordinary_justification, holdable).
narrative_ontology:cs_axiom_grounding('cd4a4ad9-493e-48ab-9b43-869fe04cacc6', holdings_bind_absent_extraordinary_justification, deontological).
narrative_ontology:cs_axiom('cd4a4ad9-493e-48ab-9b43-869fe04cacc6', secondary, determinacy_value_exceeds_per_case_correction).
narrative_ontology:cs_axiom_status(determinacy_value_exceeds_per_case_correction, holdable).
narrative_ontology:cs_axiom_grounding('cd4a4ad9-493e-48ab-9b43-869fe04cacc6', determinacy_value_exceeds_per_case_correction, instrumental).
narrative_ontology:cs_reference_frame('cd4a4ad9-493e-48ab-9b43-869fe04cacc6', precedent_corpus_as_binding_authority).
narrative_ontology:cs_drift_state('cd4a4ad9-493e-48ab-9b43-869fe04cacc6', contemporary_departure_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cd4a4ad9-493e-48ab-9b43-869fe04cacc6', '').
narrative_ontology:cs_kernel_id(common_law_precedent_corpus__strict_stare_decisis, common_law_precedent_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__strict_stare_decisis, repeat_player_litigants).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__strict_stare_decisis, incumbent_holding_beneficiaries).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__strict_stare_decisis, supreme_court_judiciary).
narrative_ontology:constraint_victim(common_law_precedent_corpus__strict_stare_decisis, norm_challenge_litigants).
narrative_ontology:constraint_victim(common_law_precedent_corpus__strict_stare_decisis, discrete_and_insular_minorities).
narrative_ontology:constraint_victim(common_law_precedent_corpus__strict_stare_decisis, future_litigant_generations).
narrative_ontology:constraint_victim(common_law_precedent_corpus__strict_stare_decisis, lower_court_judges).
narrative_ontology:constraint_vindicates(common_law_precedent_corpus__strict_stare_decisis, rule_of_law_consistency_principle).
narrative_ontology:constraint_vindicates(common_law_precedent_corpus__strict_stare_decisis, judicial_neutrality_transmission_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The apex collegium writes and maintains the adherence rule: it screens which disputes ripen into holdings, polices lower-court conformity through reversal, and decides case-by-case whether a departure petition clears the extraordinary-justification bar. Its members serve long terms and inherit a corpus they did not create. The institution's claim to neutral transmission of decided law is fused with the adherence posture, so wholesale abandonment of the posture would undercut the legitimacy frame on which its authority rests. Individual members may dissent in writing, but the seat itself has no exit from the practice short of dissolving that frame.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, supreme_court_judiciary, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(common_law_precedent_corpus__strict_stare_decisis, supreme_court_judiciary, beneficiary).

% Apply settled holdings as given across thousands of routine cases. A departure attempt invites reversal, damage to reputation and advancement, and reassignment of the question upward; relief arrives only if the apex later revisits the holding. Their discretion is bounded by holdings formed before their tenure, often before their birth.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, lower_court_judges, payer,
    institutional, biographical, trapped, regional).

% Trade associations, large employers, insurers, and national firms that litigate continuously. They select which disputes reach the courts, settle or steer away the ones that would cut against them, invest in test cases that harden favorable holdings, and then operate for decades behind the resulting wall of settled law. They can shift forums, jurisdictions, and timing to protect their advantages, and they fund the defense of favored holdings when departure petitions appear.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, repeat_player_litigants, beneficiary,
    powerful, generational, arbitrage, national).

% Parties whose conduct, entitlements, or market positions rest on holdings already on the books. They pay little ongoing attention to the doctrine yet are shielded by the adherence default; their principal activity is opposing the rare petition that targets their position. Many acquired their position under earlier norms and hold it now on terms a fresh contest would not grant.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, incumbent_holding_beneficiaries, beneficiary,
    powerful, generational, arbitrage, national).

% Bring claims that conflict with entrenched holdings: new rights claims, changed-circumstances arguments, error-correction cases. To move at all they must assemble the extraordinary-justification case, demonstrating manifest error, untenable consequences, or deep changed conditions, at costs sized to a full appellate campaign, with success odds set by apex composition as much as by merit. Most can afford one campaign, not a sustained multi-generation effort.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, norm_challenge_litigants, payer,
    moderate, biographical, constrained, national).

% Groups whose legal status was fixed by holdings formed under earlier, less inclusive norms. They lack the resources for repeated departure campaigns, cannot wait out the corpus, and cannot exit the jurisdiction of the holdings that bind them. Their relief depends on apex personnel turnover opening a window that aligns with a petition they can still afford to press.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, discrete_and_insular_minorities, payer,
    powerless, generational, trapped, national).

% Inherit the accumulated bindings without having taken part in forming them. Each generation's normative commitments enter the system only through the same narrow departure channel their predecessors faced, and each inherits the compounded weight of every prior generation's unresolved errors.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, future_litigant_generations, payer,
    powerless, generational, trapped, national).

% Organizations that press for norm updates on behalf of the payer groups. Inside a strict-adherence court their arguments are admissible only recast as extraordinary-justification petitions; they hold no seat in the routine application of holdings, no vote on which disputes ripen, and no standing forum where the dosage question could be raised as policy rather than chased as exception.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, public_interest_advocates, excluded,
    organized, biographical, constrained, national).

% Study the corpus and the adherence doctrine across jurisdictions and eras: when departures occurred, what justifications were articulated, and how success tracked apex composition. They publish outside the operational loop and hold no stake in any particular holding's survival.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, constitutional_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(common_law_precedent_corpus__strict_stare_decisis, repeat_player_litigants).
narrative_ontology:fixing_cost_class(common_law_precedent_corpus__strict_stare_decisis, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates adjudication across time and across courts: like cases receive like treatment without each court re-deriving the answer; citizens and firms can plan around known rules; the apex decides a question once and thousands of lower applications reuse the result; re-litigation of settled questions is priced out.
% TRANSFER_FUNCTION: Moves decisional autonomy and norm-change opportunity from present challengers and future claimants to holders of positions already vindicated by the corpus; moves certainty-value disproportionately to repeat players able to price and purchase it; moves adjudicative labor downward, with the apex deciding once and lower courts applying many times.
% ABSENT_VOICES: Those frozen out by historical holdings were absent when the corpus formed and remain absent from the departure calculus except as petitioners bearing the full justification burden. Constituencies of the sibling readings, evolutionary and pluralist approaches to the same corpus, likewise have no seat inside a strict-adherence court; their position enters only as the exceptional case to be argued, never as the standing frame.
% DISAPPEARANCE_RATIONALE: Overnight removal would reopen every settled question at once: reliance investments priced against settled law would unwind, litigation volume would spike as previously foreclosed challenges filed, lower courts would lose the guidance that resolves most disputes without appeal, and the judiciary's claim to transmit decided law rather than remake it would collapse. The legal economy would reorganize around re-litigation and renegotiated certainty.
% FOUNDING_PROBLEM: Adjudication by multiple judges without a binding-past convention produced judge-specific outcomes, endless re-litigation of questions already decided, and exposure of consistent rulings to pressure applied to whichever judge stood in the way. The binding-precedent convention was built to make adjudication consistent, predictable, and cheap, and to place decided law beyond the reach of case-by-case political pressure.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: comparative-law scholarship records that every mature multi-judge system, including civil-law traditions with no formal stare decisis, converges on de facto consistency norms, indicating the underlying problem recurs wherever adjudication repeats; litigation-cost economics independently quantifies the value of predictability; and the strict reading's own critics attest the founding problem was real while disputing the present dosage. No attesting source is limited to the arrangement's beneficiaries.
narrative_ontology:disappearance_verdict(common_law_precedent_corpus__strict_stare_decisis, world_rearranges).
narrative_ontology:founding_problem_status(common_law_precedent_corpus__strict_stare_decisis, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_law_precedent_corpus__strict_stare_decisis, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(common_law_precedent_corpus__strict_stare_decisis, 'none', 1).
narrative_ontology:epsilon_provenance(common_law_precedent_corpus__strict_stare_decisis, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_law_precedent_corpus__strict_stare_decisis_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_law_precedent_corpus__strict_stare_decisis, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(common_law_precedent_corpus__strict_stare_decisis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.58: the departure toll is real and asymmetric, with challengers financing full appellate campaigns against odds set partly by apex composition, but bounded by the escape valve the doctrine itself concedes. Suppression 0.62: alternatives collapse to a single narrow channel, enforced vertically by reversal and horizontally by the justification bar; suppression is authored as a raw structural property and is not scaled by power or scope (only extractiveness is scaled downstream, by directionality and spatial scope). Theater 0.30: citation practice performs deference at growing length while the operative constraint concentrates in a small number of enforcement acts; the ratio is moderate because the binding force is genuinely operative, not vestigial. Accessibility_collapse 0.60: once the arrangement is understood, alternatives narrow to the petition channel but do not vanish, since occasional successful departures keep the channel visibly open. Resistance 0.55: constant petition pressure, sustained scholarly attack, and periodic successful overrulings. The three temporal series share one grid (t=0 to 60, step 10). Suppression_requirement oscillates rather than drifting monotonically: mid-century apex compositions opened the departure channel (trough 0.46 at t=30), later compositions restored adherence (0.62 at t=60). The cycle is driven by generational turnover in apex personnel interacting with external legitimacy pressure; the oscillation itself functions as intermittent reinforcement, since challengers rationally persist because turnover occasionally opens a window, which sustains petition pressure without permanently widening the channel. Base_properties reflect the end-state, adherence-hardened phase of the cycle. Identity-lock note: the judiciary's adherence is partly internalized professional identity, with fidelity to accumulated holdings constitutive of the judicial role self-concept, so incentive-side reform alone would not dissolve the constraint; if the fidelity frame broke publicly, enforcement would lose its spine and the arrangement would migrate toward the evolutionary sibling's structure.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat experiences the arrangement as its own discipline: the source of its neutrality claim, its workload economy, and its insulation from case-by-case political pressure. The trapped payer seats experience the identical structure as a closed door with a toll booth sized to their opponents' prior investment. Repeat players experience the rule as an asset they purchased; first-time challengers meet the same rule as a barrier. Coalition dynamics matter at the powerless end: when moderate-resource challengers combine with organized advocates, aggregated pressure has periodically aligned apex composition and opened windows, which is the historical mechanism behind the suppression trough. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (repeat players, incumbents, the apex institution) sit near the subsidized end: the arrangement protects acquired position and manufactures certainty that concentrated, patient actors can fully price. Declared victims (challengers, frozen-out minorities, future generations, lower courts) sit near the target end, with trapped exits amplifying their effective position: lower courts cannot deviate at all, minorities cannot outlast the corpus, future generations never entered the formation process. The apex judiciary is dual-positioned, administering the rule while collecting legitimacy and decision-cost savings yet also bearing the discipline's costs in constrained discretion and legitimacy exposure, placing it nearer the symmetric midpoint than its beneficiary listing alone implies. No directionality overrides are authored: the beneficiary/victim declarations plus exit differentiation already separate the seats, and the two institutional seats differ in role and exit rather than requiring a power-atom-level correction.
 *
 * MANDATROPHY ANALYSIS:
 *   Reading the arrangement as pure coordination, the orthodox self-presentation in which fidelity to decided law is simply what law is, would erase the identifiable victims and certify a stability the historical record contradicts, since holdings formed under discredited norms have persisted for generations under strict adherence. Reading it as pure extraction, the critical counter-presentation in which precedent is merely entrenchment, would erase the genuine coordination function that every mature adjudication system converges on and that the disappearance verdict confirms. The tangled-rope classification holds both halves: real coordination, real asymmetric extraction, active enforcement required to hold the asymmetry in place. The founding problem remains live, so no mandatrophy declaration is made: the arrangement has not outlived its function, though its dosage is precisely what the sibling readings contest.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint instantiates the strict_stare_decisis reading of the common_law_precedent_corpus kernel; which structural elements would flip under the evolutionary_framework or pluralist_balancing sibling readings?',
    'Adoption of a sibling reading by the apex court, observable as a declared shift in the departure standard (an official doctrine permitting normative-evolution reinterpretation, or domain-weighted balancing of stability against adaptation).',
    'Under evolutionary_framework the extraordinary-justification toll on norm_challenge_litigants collapses and the victim set thins toward low-extraction operation; under pluralist_balancing extraction becomes domain-indexed, heavy where holdings are dense and light where they are contested. Either adoption converts this constraint into a different constraint with a different epsilon.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Kernel-reading commitment: sibling readings instantiate different constraints with different epsilon values and victim structures.').

omega_variable(
    natural_vs_constructed_binding_force,
    'Is the binding force of past holdings a natural convergence property of any repeated adjudication practice, or a constructed doctrine intensified beyond what consistency-coordination alone requires?',
    'Comparative institutional analysis: systems lacking formal stare decisis still exhibit de facto consistency norms; measure the gap between consistency levels in non-doctrinal systems and the adherence levels the doctrine produces.',
    'If largely natural, the extractive component is the constructed intensification (the extraordinary-justification bar and its enforcement machinery), and reform targets the overlay; if wholly constructed, the entire arrangement is contestable policy and the beneficiary structure becomes decisive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_constructed_binding_force, empirical, 'Naturalness ambiguity of precedent-binding: converged practice versus enforced doctrine.').

omega_variable(
    escape_valve_genuineness,
    'Does the extraordinary-justification channel operate as a genuine safety valve that opens under sufficient merit, or as legitimation cover that admits departures mainly when apex personnel already favor them?',
    'Track cohorts of departure petitions across the interval: success rates conditional on petition quality versus conditional on apex-composition alignment; exploit natural experiments where similarly meritorious petitions faced different apex compositions.',
    'If the valve is composition-gated, effective suppression at challenger seats exceeds the authored scalar and the constraint trends toward pure extraction at those seats; if merit-responsive, the valve damps extraction as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(escape_valve_genuineness, empirical, 'Whether the departure channel is merit-responsive or composition-gated.').

omega_variable(
    judiciary_adherence_internalization,
    'Is the judiciary''s adherence to accumulated holdings carried by internalized professional identity (fidelity as constitutive of the judicial role) or by structural incentives (review hierarchy, advancement, confirmation politics)?',
    'Observe adherence behavior where structural incentives are absent: final-apex members with long terms and no further review still police their own departures heavily; compare with the stated reasoning of former members about their own past votes after leaving the seat.',
    'If identity-carried, incentive-side reforms will not loosen the constraint and the fused exit condition persists after any structural reform; if incentive-carried, restructuring the review hierarchy would materially widen the departure channel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judiciary_adherence_internalization, empirical, 'Internalized versus structural source of judicial adherence to the corpus.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_law_precedent_corpus__strict_stare_decisis, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 0, 0.16).
narrative_ontology:measurement_basis(comm_tr_t0, observed).
narrative_ontology:measurement(comm_tr_t10, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 10, 0.19).
narrative_ontology:measurement_basis(comm_tr_t10, observed).
narrative_ontology:measurement(comm_tr_t20, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 20, 0.22).
narrative_ontology:measurement_basis(comm_tr_t20, observed).
narrative_ontology:measurement(comm_tr_t30, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 30, 0.24).
narrative_ontology:measurement_basis(comm_tr_t30, observed).
narrative_ontology:measurement(comm_tr_t40, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 40, 0.26).
narrative_ontology:measurement_basis(comm_tr_t40, observed).
narrative_ontology:measurement(comm_tr_t50, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 50, 0.28).
narrative_ontology:measurement_basis(comm_tr_t50, observed).
narrative_ontology:measurement(comm_tr_t60, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 60, 0.3).
narrative_ontology:measurement_basis(comm_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 0, 0.4).
narrative_ontology:measurement_basis(comm_be_t0, observed).
narrative_ontology:measurement(comm_be_t10, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 10, 0.43).
narrative_ontology:measurement_basis(comm_be_t10, observed).
narrative_ontology:measurement(comm_be_t20, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 20, 0.45).
narrative_ontology:measurement_basis(comm_be_t20, observed).
narrative_ontology:measurement(comm_be_t30, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 30, 0.47).
narrative_ontology:measurement_basis(comm_be_t30, observed).
narrative_ontology:measurement(comm_be_t40, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 40, 0.51).
narrative_ontology:measurement_basis(comm_be_t40, observed).
narrative_ontology:measurement(comm_be_t50, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 50, 0.55).
narrative_ontology:measurement_basis(comm_be_t50, observed).
narrative_ontology:measurement(comm_be_t60, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 60, 0.58).
narrative_ontology:measurement_basis(comm_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(comm_su_t0, observed).
narrative_ontology:measurement(comm_su_t10, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 10, 0.53).
narrative_ontology:measurement_basis(comm_su_t10, observed).
narrative_ontology:measurement(comm_su_t20, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 20, 0.49).
narrative_ontology:measurement_basis(comm_su_t20, observed).
narrative_ontology:measurement(comm_su_t30, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 30, 0.46).
narrative_ontology:measurement_basis(comm_su_t30, observed).
narrative_ontology:measurement(comm_su_t40, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 40, 0.52).
narrative_ontology:measurement_basis(comm_su_t40, observed).
narrative_ontology:measurement(comm_su_t50, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 50, 0.58).
narrative_ontology:measurement_basis(comm_su_t50, observed).
narrative_ontology:measurement(comm_su_t60, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 60, 0.62).
narrative_ontology:measurement_basis(comm_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_law_precedent_corpus__strict_stare_decisis, enforcement_mechanism).
narrative_ontology:affects_constraint(common_law_precedent_corpus__strict_stare_decisis, common_law_precedent_corpus__evolutionary_framework).
narrative_ontology:affects_constraint(common_law_precedent_corpus__strict_stare_decisis, common_law_precedent_corpus__pluralist_balancing).

% DUAL FORMULATION NOTE:
% The colloquial label 'stare decisis' covers three structurally distinct arrangements of the common_law_precedent_corpus kernel, decomposed per the epsilon-invariance principle: strict_stare_decisis (this file, uniform backward binding with an extraordinary-justification departure bar and correspondingly high, uniform extraction on challengers), evolutionary_framework (a normative-evolution license lowers the departure toll and thins the victim set), and pluralist_balancing (domain-weighted binding makes extraction domain-indexed). Each reading carries its own epsilon, beneficiary/victim structure, and classification; the files link through network.affects_constraints. The relational gradient runs from this reading outward: the strict tradition supplies the default against which both siblings define themselves, so this story links to each sibling and neither sibling's epsilon is averaged into this one.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
