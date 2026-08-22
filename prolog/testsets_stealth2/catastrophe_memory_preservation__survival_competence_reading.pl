% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_preservation__survival_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_preservation__survival_competence_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: catastrophe_memory_preservation__survival_competence_reading
 *   human_readable: Catastrophe Memorial Rite as Intergenerational Threat-Recognition Drill (Survival-Competence Reading)
 *   domain: religious/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   A community that survived a defining catastrophe maintains an annual rite
 *   that fuses lamentation with rehearsed protective practice: mourning the
 *   dead in the same ceremony that drills the living in what the dead failed
 *   to see coming. This file authors ONE reading of the shared kernel — the
 *   survival-competence reading, under which the rite's operative function is
 *   the intergenerational transfer of threat-recognition capacity, and its
 *   costliness is the price of that transfer. The sibling readings
 *   (symbolic-continuity-only; once-functional-now-atrophied) are different
 *   constraints with different epsilon values, beneficiary structures, and
 *   classifications; they are separate files linked through
 *   network.affects_constraints, not positions folded into this one.
 *   Epsilon's referent here is the standing rite arrangement as actually
 *   practiced, assessed by this reading's own lights — never the reformed or
 *   purely mnemonic alternative this reading rejects. The claimed type
 *   (tangled_rope) and the authored metrics are independent facts: the claim
 *   states the structure I believe true (a genuine collective-action solution
 *   with asymmetric incidence), the metrics describe observed operation.
 *
 * KEY AGENTS:
 *   - ritual_officiants: agenda-setter (organized/identity_locked) — administers the rite, collects standing and support, teaches the transmission doctrine
 *   - present_generation_participants: primary payer (moderate/constrained) — bears the grief-drill cost now for deferred, contingent benefit
 *   - future_generation_descendants: deferred beneficiary (powerless/trapped) — receives or fails to receive the transmitted competence; cannot consent or exit
 *   - noncompliant_youth_members: sanctioned dissenters (powerless/constrained) — pay participation cost plus sanction while doubting the content
 *   - secular_emergency_planners: excluded institutional voice (institutional/mobile) — holds substitute drill infrastructure, barred from rite governance
 *   - emigrated_former_members: exercised-exit comparators (moderate/mobile) — demonstrate commemoration separable from drill
 *   - ritual_studies_observers: analytical seat (analytical/analytical) — could resolve the efficacy question; excluded from councils
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_preservation__survival_competence_reading, 0.63).
domain_priors:suppression_score(catastrophe_memory_preservation__survival_competence_reading, 0.55).
domain_priors:theater_ratio(catastrophe_memory_preservation__survival_competence_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, extractiveness, 0.63).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_preservation__survival_competence_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_preservation__survival_competence_reading, "Catastrophe Memorial Rite as Intergenerational Threat-Recognition Drill (Survival-Competence Reading)").
narrative_ontology:topic_domain(catastrophe_memory_preservation__survival_competence_reading, "religious/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_preservation__survival_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_preservation__survival_competence_reading, 'e99899c6-a235-443b-afed-527edbc96e3b').
narrative_ontology:cs_kernel_codification('e99899c6-a235-443b-afed-527edbc96e3b', distributed).
narrative_ontology:cs_authority_grounding('e99899c6-a235-443b-afed-527edbc96e3b', distributed).
narrative_ontology:cs_reading_relation('e99899c6-a235-443b-afed-527edbc96e3b', catastrophe_memory_preservation__mourning_practice_reading, forecloses).
narrative_ontology:cs_reading_relation('e99899c6-a235-443b-afed-527edbc96e3b', catastrophe_memory_preservation__hybrid_atrophy_reading, forecloses).
narrative_ontology:cs_axiom('e99899c6-a235-443b-afed-527edbc96e3b', foundational, ritual_participation_transfers_operational_competence).
narrative_ontology:cs_axiom_status(ritual_participation_transfers_operational_competence, holdable).
narrative_ontology:cs_axiom_grounding('e99899c6-a235-443b-afed-527edbc96e3b', ritual_participation_transfers_operational_competence, empirically_contingent).
narrative_ontology:cs_axiom('e99899c6-a235-443b-afed-527edbc96e3b', secondary, costly_affective_enactment_necessary_for_fidelity).
narrative_ontology:cs_axiom_status(costly_affective_enactment_necessary_for_fidelity, holdable).
narrative_ontology:cs_axiom_grounding('e99899c6-a235-443b-afed-527edbc96e3b', costly_affective_enactment_necessary_for_fidelity, instrumental).
narrative_ontology:cs_reference_frame('e99899c6-a235-443b-afed-527edbc96e3b', rite_as_living_transmission_regimen).
narrative_ontology:cs_drift_state('e99899c6-a235-443b-afed-527edbc96e3b', contemporary_secularized_modernity, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e99899c6-a235-443b-afed-527edbc96e3b', '2026-06-14T09:30:00Z').
narrative_ontology:cs_kernel_id(catastrophe_memory_preservation__survival_competence_reading, catastrophe_memory_preservation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__survival_competence_reading, future_generation_descendants).
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__survival_competence_reading, ritual_officiants).
narrative_ontology:constraint_victim(catastrophe_memory_preservation__survival_competence_reading, present_generation_participants).
narrative_ontology:constraint_victim(catastrophe_memory_preservation__survival_competence_reading, noncompliant_youth_members).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(catastrophe_memory_preservation__survival_competence_reading, emigrated_former_members).
narrative_ontology:constraint_vindicates(catastrophe_memory_preservation__survival_competence_reading, intergenerational_procedural_memory_hypothesis).
narrative_ontology:constraint_vindicates(catastrophe_memory_preservation__survival_competence_reading, embodied_encoding_doctrine).
narrative_ontology:constraint_vindicates(catastrophe_memory_preservation__survival_competence_reading, communal_preparedness_superiority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Conduct the annual rite cycle: set the calendar, lead the combined lamentation-and-rehearsal sequence, train successors, and enforce attendance norms through censure and ritual sanction. Their office, material support, and standing exist only insofar as the rite continues; abandoning the office would mean abandoning the role that organizes their life and their lineage of training. They teach, sincerely, that the rehearsals encode lifesaving recognition.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, ritual_officiants, agenda_setter,
    organized, generational, identity_locked, regional).

% Not yet present. They will inherit whatever recognition habits the rite manages to encode — evacuation judgment, stores discipline, warning-sign literacy — or will inherit nothing. They cannot consent to having tomorrow's preparedness purchased with today's compelled participation, and cannot exit an arrangement concluded before they existed. Whether they ever collect depends on a transmission no one has yet audited.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, future_generation_descendants, beneficiary,
    powerless, generational, trapped, regional).

% Attend, fast, process, rebuild memorial structures, and rehearse protective sequences on the community's calendar. They bear the hours, the yearly re-entry into catastrophe memory, the dues, and the forgone alternatives. The competence the rite promises accrues mainly to children and grandchildren; their own payoff is contingent, deferred, and unverifiable. Skipping brings censure; leaving the rite cycle effectively means leaving the community of memory — family ties, burial rights, mutual aid.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, present_generation_participants, payer,
    moderate, biographical, constrained, regional).

% Younger members who question whether the rehearsals teach anything real. They attend anyway under family and communal pressure, absorbing shaming, marriage-market penalties, and occasional formal sanction. Their objections circulate informally; none holds a seat where rite design is decided.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, noncompliant_youth_members, payer,
    powerless, immediate, constrained, local).

% Regional civil-protection agencies run standardized hazard drills and publish preparedness curricula. They regard the rite's protective claims as unevidenced and would audit or replace them, but they hold no seat in rite governance, and their offers of joint exercises have been declined as a profanation of mourning.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, secular_emergency_planners, excluded,
    institutional, biographical, mobile, national).

% Former participants who left for cities abroad. They keep private anniversaries without the rehearsal obligations, demonstrating in practice that commemoration and drill are separable — an exit that is hard to see from inside the constrained membership. They pay nothing into the rite and receive nothing from its enforcement.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, emigrated_former_members, payer,
    moderate, biographical, mobile, continental).

% Researchers comparing catastrophe rites across communities. They archive variants, track participation rates, and design the cohort studies that could settle whether operational transfer occurs. They neither pay nor collect; their analyses are unwelcome at rite councils.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, ritual_studies_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_preservation__survival_competence_reading, ritual_officiants).
narrative_ontology:fixing_cost_class(catastrophe_memory_preservation__survival_competence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Compresses catastrophic experience into repeatable public enactment so that later generations inherit threat-recognition patterns — evacuation thresholds, storage discipline, warning literacy — that no living member has paid experiential tuition for; simultaneously binds the community across generational turnover by giving each cohort a shared bodily practice.
% TRANSFER_FUNCTION: Moves present time, labor, wealth, and autonomous self-direction from current participants toward (a) encoded procedural memory held in common for descendants and (b) the officiant class's standing, support, and continuity; additionally conscripts grief-expression into disciplined collective form.
% ABSENT_VOICES: Secular emergency-management planners would demand efficacy evidence and offer standardized drills as a substitute; dissenting younger members raise the objection internally and are absorbed by sanction; neighboring communities running rival rites each claim superior transmission. None of these voices holds a seat in rite governance.
% DISAPPEARANCE_RATIONALE: If the rite vanished overnight: the officiant class dissolves with its office; the community calendar, mutual-aid expectations, and household preparedness habits keyed to the rite cycle lapse; encoded practices such as stores rotation, route memorization, and signal response lose their rehearsal schedule and fade within roughly one generation; mourning migrates to private forms; whoever inherits the community's safety must reconstruct preparedness through ad hoc or professional channels.
% FOUNDING_PROBLEM: After the founding catastrophe, the community faced certain recurrence risk with survivor knowledge dying at generational turnover: the people who knew which signs precede the flood, which routes burn, which stores fail first would not live long enough to teach everyone who would need it.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: post-catastrophe municipal archives and chronicles independently record the founding wave of rite-institution; disaster-sociology studies of knowledge attrition after survivor-generation mortality attest the underlying problem; secular civil-protection agencies building drill infrastructure for the same hazards confirm the problem is real. No external source attests that the CURRENT rite still delivers the competence — external attestation covers the founding problem, not present efficacy, which remains open.
narrative_ontology:disappearance_verdict(catastrophe_memory_preservation__survival_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_preservation__survival_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_preservation__survival_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_preservation__survival_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_preservation__survival_competence_reading, 0.63, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_preservation__survival_competence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_preservation__survival_competence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_preservation__survival_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high but bounded (0.63 at interval end): the cost side is concrete and compulsory (hours, dues, grief-labor, forgone alternatives, sanction exposure) while the benefit side is deferred, contingent, and unaudited — and a measurable slice of the flow lands on the officiant class rather than on anyone's future safety. Suppression is moderate-high (0.55) and structural-plus-social: attendance compulsion, shaming, marriage and burial consequences, not physical force; the suppression_requirement series deliberately traces enforcement-capacity change (this story's enforcement picture is dynamic — survivor-era rigor at t=0 decaying as memory normalizes — which is exactly the enforcement-decay trajectory the temporal apparatus exists to catch, so the series is authored rather than left to the static scalar). Theater rises from 0.12 to 0.30: at founding, nearly all rite activity was recognizably functional rehearsal; as the catastrophe recedes, pageantry share grows while drill fidelity thins. Accessibility_collapse 0.45: alternatives do not fully collapse — private mourning, secular courses, and professional drills exist and are taken up by emigrants — but inside the community the rite crowds out rivals for the same calendar slot and moral authority. Resistance 0.55: attendance decline, youth dissent, and periodic reform movements that propose separating the mourning half from the drill half. One shared time grid ({0,10,20,30,40,50,60}) carries all three tracked metrics; no metric borrows another's endpoint.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and should. From the officiant seat the arrangement is sacred duty and inherited wisdom — a coordination structure they staff at personal cost to their own freedom. From the participant seat the same structure operates as conscripted grief-labor: they are drilled in someone else's memory on a schedule they did not set. From the descendant seat the cost is invisible entirely — they see only whatever competence arrives or does not. Youth dissenters experience the sanction layer directly; emigrants, looking back, see the whole package as optional. The engine derives these divergent classifications from the declared power/exit/role data; nothing in this file adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive d: future_generation_descendants sit at the beneficiary extreme (d near 0.0 — the arrangement subsidizes them, contingent on transmission being real); present_generation_participants and noncompliant_youth_members sit near the target extreme (d near 0.8-0.9 — they bear the transfer with constrained exits). One override is authored: the organized power atom (ritual_officiants). Structural derivation from role=agenda_setter plus beneficiary declaration would hand officiants a near-full-beneficiary d (~0.10), but their true relationship is not pure subsidy: they are identity_locked into the office they administer, they carry the enforcement labor the arrangement requires, and their gain is standing and continuity rather than material windfall. Effective extraction for them is real but small — d overridden to 0.22. No override is authored for the powerless atom even though it spans two opposite seats (descendants near-beneficiary, youth near-target): the derivation already separates them via role and exit declarations, and a single atom-keyed override would flatten exactly the asymmetry the story is about.
 *
 * MANDATROPHY ANALYSIS:
 *   Claiming tangled_rope is what keeps both halves legible. Reading the rite as pure rope (the officiant view) erases the asymmetric incidence: present participants pay for benefits that accrue elsewhere in time, and a rent-collecting administrator class sits atop the transfer. Reading it as pure snare (the cynic's view) erases the genuine collective-action problem — catastrophic knowledge dies with its experiencers, and no market or individual choice supplies intergenerational transmission. The tangled_rope frame forces both the coordination function and the extraction channel into the same account, which is where they in fact live: the same ceremony that binds the community extracts from it. On obsolescence: the founding problem (recurrence risk plus knowledge mortality) is live, so no resolved-mandatrophy declaration is authored; but the rising theater series is the drift signature to watch — if the drill half finishes hollowing into pageantry while cost and enforcement persist, this constraint migrates toward the atrophied sibling reading's territory, and the temporal record here dates that transition if it comes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is the survival_competence_reading of kernel catastrophe_memory_preservation; sibling readings (mourning_practice_reading, hybrid_atrophy_reading) instantiate different constraints over the same rite — which functional description of the shared practice is structurally accurate?',
    'Cohort efficacy studies and practice ethnography conducted by parties outside the officiant class; whichever reading wins emits its OWN constraint file with its own epsilon, beneficiary structure, and type — the resolution replaces this story indexically rather than revising it in place.',
    'If the mourning reading is accurate, this file''s epsilon overstates extraction-as-price-of-competence (the cost purchases identity goods, not capability) and the type drifts snare-ward; if the hybrid reading is accurate, the drift series here should be re-read as dating a transition toward atrophied persistence. This file''s classification holds only under the survival reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Indexical routing among sibling readings of the shared catastrophe-memory kernel; disagreement located on whether costly participation yields operational transfer.').

omega_variable(
    operational_transfer_efficacy,
    'Do high-participation cohorts measurably outperform low-participation and emigrated cohorts on hazard-recognition tasks — evacuation timing judgments, stores-maintenance discipline, warning-sign identification — controlling for schooling and direct disaster exposure?',
    'Matched-cohort comparison across participating, non-participating, and emigrated members, ideally exploiting the emigrants as a natural low-cost-rehearsal control group.',
    'Positive differential confirms this reading''s foundational axiom and anchors the tangled_rope classification; null differential dissolves the survival reading into the mourning sibling and re-prices the entire extraction as payment for symbolism alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_transfer_efficacy, empirical, 'Whether the rite actually transmits threat-recognition capacity across generations.').

omega_variable(
    cost_necessity_for_encoding,
    'Is the costly, affect-laden character of the enactment functionally necessary for encoding fidelity, or would low-cost scheduled rehearsal transmit the same recognition patterns?',
    'Training-transfer studies contrasting emotionally intense embodied rehearsal with low-affect procedural drills on retention intervals of a decade or more.',
    'If cheap rehearsal suffices, the grief-drill entanglement is gratuitous — extraction riding a separable coordination core, pushing the classification toward the snare flank; if cost is necessary, the entanglement is the price of function and the tangled_rope reading stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cost_necessity_for_encoding, empirical, 'Whether ritual costliness is load-bearing for the transmission function or exploitable slack.').

omega_variable(
    cross_generational_consent_legitimacy,
    'May the present generation impose compulsory cost on its members to purchase protection for persons who cannot yet consent — and does the answer change the weight given to participant-seat extraction versus descendant-seat benefit?',
    'Normative analysis within the community''s own tradition (does the tradition contain a doctrine of owed obligations to the unborn?) plus revealed-preference evidence from voluntary high-cost participation by childless members.',
    'If the cross-generational transfer is judged legitimate, participant-seat effective extraction discounts sharply and the rope component dominates; if judged illegitimate, the same arithmetic prices the arrangement as imposed extraction with a coordination alibi.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cross_generational_consent_legitimacy, preference, 'Value-dependence of the extraction ledger across the consent boundary between generations.').

omega_variable(
    suppression_internalization_ambiguity,
    'How much of the measured suppression is structural (sanctions, marriage and burial consequences, communal censure) versus internalized (raised-from-birth piety that persists after exit)?',
    'Post-exit trajectory of emigrated_former_members: whether they resume full observance voluntarily, keep private compromise forms, or abandon the practice entirely — and whether guilt symptoms persist after the sanctioning community is geographically out of reach.',
    'If internalized, effective suppression exceeds the structural measure — the constraint travels with the payer after exit and the emigrant ''exit'' is partial; if purely structural, the emigrant comparator is a clean counterfactual and measured suppression stands as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_ambiguity, empirical, 'Structural versus internalized share of the enforcement burden keeping participation high.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_preservation__survival_competence_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 30, 0.23).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 40, 0.26).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 50, 0.28).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 60, 0.3).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 0, 0.46).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 20, 0.54).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 30, 0.57).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 40, 0.6).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 50, 0.62).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 60, 0.63).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 30, 0.66).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 40, 0.62).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 50, 0.59).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 60, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_preservation__survival_competence_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__survival_competence_reading, catastrophe_memory_preservation__mourning_practice_reading).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__survival_competence_reading, catastrophe_memory_preservation__hybrid_atrophy_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'catastrophe memory ritual'. The label covers at least three structurally distinct claims — the rite transfers survival competence now (this file), the rite is symbolic-only, the rite transferred once and has atrophied — with materially different epsilon values and beneficiary/victim structures. Per the epsilon-invariance principle these are three files sharing one kernel, each with stable epsilon; this file's network edges link to both siblings. Upstream/downstream citation runs from the officiants' publicly endorsed reading (whichever it is) down into the practice's justification structure: a community that preaches transfer justifies its compulsion differently than one that preaches remembrance, and the sibling files capture that divergence rather than averaging it away.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_preservation__survival_competence_reading, organized, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
