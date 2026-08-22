% ============================================================================
% CONSTRAINT STORY: total_war_possibility_space__nuclear_taboo_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_possibility_space__nuclear_taboo_reading, []).

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
 *   constraint_id: total_war_possibility_space__nuclear_taboo_reading
 *   human_readable: Nuclear Taboo Reading of the Total-War Possibility Space
 *   domain: international relations/strategic studies/institutional history
 *
 * SUMMARY:
 *   Since 1945 the major powers have not employed nuclear weapons in war, and
 *   this story instantiates the nuclear_taboo_reading of the
 *   total_war_possibility_space kernel: the non-use is held in place by a
 *   constructed normative prohibition — built by norm entrepreneurs,
 *   reproduced through ritual and declaratory practice, and enforced through
 *   the non-proliferation regime — operating independently of material
 *   capability. The arrangement is genuinely coordinative (every seat
 *   benefits from non-use) while simultaneously extracting asymmetrically
 *   (the recognized weapon states retain exclusive arsenals under legal cover
 *   while non-possessors bear safeguards, permanent weaponlessness, and the
 *   enforcement machinery's coercive edge). Constraint family: this kernel
 *   decomposes into three readings — deterrence_equilibrium_reading (non-use
 *   as material deterrence outcome), space_contraction_reading (non-use as
 *   cognitive foreclosure), and this file. Their epsilon profiles differ
 *   sharply: a deterrence-reading story would author low epsilon (the
 *   arrangement reduces to the physics of vulnerability, no constructed
 *   machinery needed); a space-contraction story would author near-zero
 *   agentic extraction (nothing is maintained; options simply never arise);
 *   this taboo reading authors moderate epsilon because the standing
 *   arrangement includes real enforcement machinery whose burdens fall
 *   asymmetrically. The readings are linked via network.affects_constraints
 *   and cs_structure.reading_relations. Within this reading a further
 *   decomposition is conceivable — the bare use-taboonorm (near-pure
 *   coordination, epsilon well below 0.2) versus the NPT bargain layer
 *   (concentrated asymmetry, epsilon above 0.6) — but this story deliberately
 *   takes the combined standing arrangement as its referent, because the
 *   reading's own structural delta treats the enforcement mechanisms as
 *   generated by the taboo. KEY AGENTS (by structural relationship): -
 *   nuclear_weapon_states: Agenda-setting collector
 *   (institutional/identity_locked) — administers the regime, retains
 *   exclusive arsenals under its legal cover, and simultaneously bears the
 *   taboo's binding on use - non_nuclear_treaty_parties: Primary coordinated
 *   payers (organized/constrained) — safeguards, permanent weaponlessness,
 *   declaratory-only disarmament - extended_deterrence_allies: Sheltered
 *   beneficiaries (powerful/constrained) — protection without arsenals -
 *   threshold_hedger_states: Foreclosed-option payers (powerful/constrained)
 *   — dormant capability priced by the hierarchy -
 *   norm_entrepreneur_epistemic_community: Institutional beneficiaries
 *   (organized/identity_locked) — careers constituted by the regime's
 *   persistence - proliferation_defier_states: Enforcement-target payers
 *   (moderate/constrained) — sanctions and isolation -
 *   ictpnw_coalition_states: Excluded dissenters (organized/mobile) — built a
 *   parallel treaty the possessors boycott - hibakusha_survivor_communities:
 *   Moral witnesses (powerless/analytical) — anchor the norm, hold no vote -
 *   international_security_scholarship: Analytical observer
 *   (analytical/analytical) — adjudicates between the readings
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__nuclear_taboo_reading, 0.5).
domain_priors:suppression_score(total_war_possibility_space__nuclear_taboo_reading, 0.6).
domain_priors:theater_ratio(total_war_possibility_space__nuclear_taboo_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, extractiveness, 0.5).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__nuclear_taboo_reading, tangled_rope).
narrative_ontology:human_readable(total_war_possibility_space__nuclear_taboo_reading, "Nuclear Taboo Reading of the Total-War Possibility Space").
narrative_ontology:topic_domain(total_war_possibility_space__nuclear_taboo_reading, "international relations/strategic studies/institutional history").

domain_priors:requires_active_enforcement(total_war_possibility_space__nuclear_taboo_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__nuclear_taboo_reading, '031cc415-9b4b-4ce4-b957-209766fedb9a').
narrative_ontology:cs_kernel_codification('031cc415-9b4b-4ce4-b957-209766fedb9a', distributed).
narrative_ontology:cs_authority_grounding('031cc415-9b4b-4ce4-b957-209766fedb9a', distributed).
narrative_ontology:cs_reading_relation('031cc415-9b4b-4ce4-b957-209766fedb9a', total_war_possibility_space__deterrence_equilibrium_reading, coexists_with).
narrative_ontology:cs_reading_relation('031cc415-9b4b-4ce4-b957-209766fedb9a', total_war_possibility_space__space_contraction_reading, influences).
narrative_ontology:cs_axiom('031cc415-9b4b-4ce4-b957-209766fedb9a', foundational, nonuse_obligation_independent_of_capability).
narrative_ontology:cs_axiom_status(nonuse_obligation_independent_of_capability, holdable).
narrative_ontology:cs_axiom_grounding('031cc415-9b4b-4ce4-b957-209766fedb9a', nonuse_obligation_independent_of_capability, deontological).
narrative_ontology:cs_axiom('031cc415-9b4b-4ce4-b957-209766fedb9a', foundational, normative_foreclosure_requires_active_maintenance).
narrative_ontology:cs_axiom_status(normative_foreclosure_requires_active_maintenance, holdable).
narrative_ontology:cs_axiom_grounding('031cc415-9b4b-4ce4-b957-209766fedb9a', normative_foreclosure_requires_active_maintenance, empirically_contingent).
narrative_ontology:cs_reference_frame('031cc415-9b4b-4ce4-b957-209766fedb9a', taboo_governed_nonuse_order).
narrative_ontology:cs_drift_state('031cc415-9b4b-4ce4-b957-209766fedb9a', contemporary_nuclear_signaling_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('031cc415-9b4b-4ce4-b957-209766fedb9a', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__nuclear_taboo_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, extended_deterrence_allies).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, norm_entrepreneur_epistemic_community).
narrative_ontology:constraint_victim(total_war_possibility_space__nuclear_taboo_reading, non_nuclear_treaty_parties).
narrative_ontology:constraint_victim(total_war_possibility_space__nuclear_taboo_reading, threshold_hedger_states).
narrative_ontology:constraint_victim(total_war_possibility_space__nuclear_taboo_reading, proliferation_defier_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, non_nuclear_treaty_parties).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Nine states hold the weapons; the five recognized by the NPT set the regime's terms through the review process and Security Council enforcement, and their arsenals operate under the legal cover the treaty provides. At the same time, political leaderships since 1950 have struck nuclear options from war plans, relieved or overruled commanders, and declined militarily usable moments — Korea, Vietnam, and later crises — because employing the weapons would forfeit the legitimacy and stability their position rests on. Leaving the arrangement would mean either using the weapons, shattering the order that shelters the arsenal, or disarming, surrendering primacy; neither is a live path, so the restraint has become part of what these establishments are.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, nuclear_weapon_states, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__nuclear_taboo_reading, nuclear_weapon_states, beneficiary).

% More than 180 states without weapons accept comprehensive safeguards, intrusive inspections, and export-control limits on their nuclear industries, and forgo acquisition permanently. In return they receive neighborhoods without new arsenals, promised peaceful-use cooperation, and security assurances of varying firmness. They press the recognized five on disarmament promises at review conferences and leave with declaratory text. Withdrawal is legally available, but the North Korean precedent shows the price: sanctions and isolation.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, non_nuclear_treaty_parties, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__nuclear_taboo_reading, non_nuclear_treaty_parties, beneficiary).

% States such as Japan, South Korea, and the NATO members rely on a patron's weapons rather than building their own. They receive protection below the cost and stigma of national arsenals. Their hedge is industrial latency; domestic debates about going nuclear recur whenever trust in the patron wavers, but activating it would rupture alliances and invite regional arms races.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, extended_deterrence_allies, beneficiary,
    powerful, generational, constrained, regional).

% States with the industrial capacity to build weapons quickly — Japan, South Korea, Germany, Saudi Arabia, Brazil — keep the capability dormant. The arrangement prices their abstention: they accept being outranked in the status hierarchy of force and forgo the deterrent their capability could buy, in exchange for technology cooperation, market access, and patron protection where available.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, threshold_hedger_states, payer,
    powerful, biographical, constrained, regional).

% Arms-control officials, IAEA and CTBTO staff, NGO networks, and the academic nonproliferation field staff the arrangement's daily maintenance: drafting review documents, running inspections, commemorating Hiroshima, training each diplomatic generation in the vocabulary of restraint. Their careers, funding, and professional purpose are constituted by the regime's persistence; leaving the field means abandoning the identity the work built.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, norm_entrepreneur_epistemic_community, beneficiary,
    organized, biographical, identity_locked, global).

% States that tested or pursued weapons against the regime's terms — North Korea above all, with Iraq and Iran as earlier objects of enforcement — absorb sanctions, interdiction, and isolation. North Korea's exit demonstrated the door exists and showed its price. The enforcement machinery's coercive edge lands on these states and their populations.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, proliferation_defier_states, payer,
    moderate, biographical, constrained, regional).

% A coalition of small and middle powers — Austria, Ireland, Mexico and partners — judged the NPT bargain's indefinite hierarchy unacceptable and built a parallel treaty banning the weapons outright. Every possessor boycotted the negotiation. Inside the operative bargain they remain outsiders: their objection is recorded and their votes counted in forums the possessors do not treat as authoritative.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, ictpnw_coalition_states, excluded,
    organized, generational, mobile, global).

% Survivors of Hiroshima and Nagasaki testify as the founding events' direct witnesses. Their testimony anchors the norm's moral content and is invoked at every review conference, but they hold no vote in the bargain conducted in their name. The cohort is aging and its living memory is finite.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, hibakusha_survivor_communities, observer,
    powerless, biographical, analytical, global).

% The international-security field adjudicates between rival accounts of why the weapons have not been used since 1945 — deterrence logic, normative taboo, cognitive foreclosure — working from archives, interviews, and formal modeling. Its verdicts feed back into policy training and delegitimation politics but command no enforcement power.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, international_security_scholarship, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_possibility_space__nuclear_taboo_reading, nuclear_weapon_states).
narrative_ontology:fixing_cost_class(total_war_possibility_space__nuclear_taboo_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of catastrophic-war avoidance among armed, mutually distrustful rivals who cannot verify intentions: converts non-use from a contingent strategic choice re-decided each crisis into a settled normative default, coordinating expectations so that no party plans around nuclear employment and no crisis bargaining occurs on the assumption it might happen.
% TRANSFER_FUNCTION: Moves decision autonomy — strike options — out of the hands of political leaderships and planning staffs; moves technological autonomy and deterrent option-space away from non-nuclear states through safeguards and export controls; moves status and legal cover toward the recognized weapon states, whose arsenals the bargain legitimizes exclusively; moves residual risk onto populations who never consented to bear it — test-site and downwind communities, prospective target cities, future generations.
% ABSENT_VOICES: Populations of prospective target cities, Pacific test-site and downwind communities, and future generations bear the regime's residual risks without a seat anywhere in the bargain. The TPNW coalition states were marginalized in NPT bargaining until they built a parallel forum. Hibakusha are accorded ceremonial voice without vote. They sit outside the P5-centered arrangement — admitted to speak, not to decide.
% DISAPPEARANCE_RATIONALE: If the normative prohibition and its enforcement machinery vanished overnight, war plans would re-admit nuclear options within planning cycles; crisis bargaining would lose the shared expectation of restraint; hedging states would activate latency programs; alliance structures premised on the umbrella would renegotiate; and the nonproliferation institutions would lose their object. The world rearranges because this reading's claim is precisely that arrangements, not physics, hold non-use in place.
% FOUNDING_PROBLEM: After August 1945 the problem was how armed, mutually distrustful states could make non-use of society-destroying weapons durable — converting a one-off wartime decision into a standing expectation, when verification of intent was impossible and every major power retained or sought the capability.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration comes from outside the beneficiary set: hibakusha testimony — victims of the founding events holding no seat in the bargain — attests the founding problem's reality and gravity; declassified deliberation records (the Korean War relief of MacArthur, the ExComm tapes, the Vietnam reviews) attest that the norm operated as a binding constraint in moments where capability calculations pointed the other way; the Bulletin of the Atomic Scientists' continuing assessments attest the problem remains live. No weapon-state self-attestation is relied upon.
narrative_ontology:disappearance_verdict(total_war_possibility_space__nuclear_taboo_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_possibility_space__nuclear_taboo_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__nuclear_taboo_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(total_war_possibility_space__nuclear_taboo_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_possibility_space__nuclear_taboo_reading, 0.5, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_possibility_space__nuclear_taboo_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(total_war_possibility_space__nuclear_taboo_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(total_war_possibility_space__nuclear_taboo_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.50: compliance with non-use itself is cheap for every seat — not destroying societies costs nothing anyone wanted — so the taboo core is nearly free coordination; the extraction concentrates in the regime layer the taboo generates, where safeguards, export controls, permanent second-class status, and sanctions fall on non-possessors while the recognized five collect legitimizing cover. Suppression is authored at 0.60 as a raw structural property, unscaled by power or scope (only extractiveness is scaled downstream): the machinery includes IAEA inspections, UNSC-sanctioned sanctions, interdiction, and assistance conditionality — substantial but not crushing, since the DPRK demonstrated exit is survivable at high cost. Theater ratio 0.48 and rising: Article VI disarmament promises have been ritually renewed for over half a century without delivery, NFU pledges arrive heavily caveated, review conferences produce consensus text that changes nothing, and the TPNW staged a ban ceremony no possessor attended — a growing share of the arrangement's activity is performative maintenance of the bargain's image rather than operation of its function, though the day-to-day restraint machinery (presidential review of strike options, inspections) remains functional. Accessibility collapse is low at 0.35, and this is the reading's defining signature: the constraint is constructed, not natural — use remains physically available, breakout remains demonstrated, hedging remains live — so alternatives do not collapse the way they would under a genuine natural law. Resistance 0.55 reflects the Indian, Pakistani, and Israeli refusals of the NPT, the DPRK exit, Iranian hedging, and the TPNW revolt against the bargain's hierarchy. The measurement series runs on one shared nine-point grid (1945-2025) with all three metrics authored at every point. The underlying dynamic is cyclical — crisis, reaffirmation, relaxation, accumulation (Cuba producing the NPT, the South Asian tests producing sanctions then engagement, Ukraine producing signaling then reassurance) — superimposed on a secular rise in enforcement capacity; the 1991 dip in suppression_requirement marks the post-Cold War relaxation, and the base_properties scalars reflect the 2025 phase of the cycle, with elevated great-power nuclear signaling keeping enforcement anxiety high.
 *
 * PERSPECTIVAL GAP:
 *   Four seats should compute divergent types from identical structural data. From the nuclear_weapon_states seat the arrangement presents as self-binding wisdom — a rope they wove and maintain, whose occasional binding of their own options is the premium on the stability that shelters their arsenal. From the non_nuclear_treaty_parties seat the same structure operates as enforced hierarchy — coordination whose costs they bear and whose disarmament half never arrives. From the proliferation_defier_states seat it is a coercive blockade — snare-flavored enforcement landing on bodies and economies. From the extended_deterrence_allies seat it is shelter — the best available purchase of security. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations map to real subsidy: the recognized weapon states collect legitimizing cover and allied deference; the allies collect protection below the cost of arsenals; the epistemic community collects careers and institutional purpose. The victim declarations map to real burden: treaty parties bear safeguards and permanent weaponlessness; hedgers bear foregone deterrent option-space; defiers bear the enforcement edge. Derived directionalities follow: beneficiaries near the subsidized pole, payers near the target pole, with the allies' constrained exit and the defiers' demonstrated-but-priced exit modulating the magnitudes. One override is declared: the derivation chain would read the nuclear_weapon_states' beneficiary declaration and land them near the beneficiary pole (d roughly 0.1-0.2), but this reading's core evidence — Korea 1950-53, the Vietnam reviews, repeated presidential vetoes of militarily usable options — shows the recognized weapon states pay a real, recurring option-cost under the same structure that subsidizes them. The override moves the institutional atom to d=0.38, encoding the mixed position of subsidized administrator and bound party; no other stakeholder occupies the institutional atom, so the correction is effectively agent-specific. The excluded and observer seats (ictpnw_coalition_states, hibakusha_survivor_communities, international_security_scholarship) inform the absent-voices and consensus-provenance picture; per the R3 ruling their authored absence is commentary-grade and never drives classification corrections.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents mislabeling in both directions. A pure-rope reading would treat the safeguards, export controls, and status hierarchy as neutral coordination overhead and miss the concentrated asymmetry — the bargain's indefinite extension in 1995 entrenched the five's privilege while deleting the non-possessors' principal leverage, which is extraction riding the coordination. A snare reading would miss that the coordination good is real and delivered: eighty years of non-use is a benefit every seat, including the payers, has actually collected, and the payers consented to the bargain (however grudgingly) in a way snare victims do not. Mandatrophy status: the founding problem — durable non-use among armed rivals — is live, so the mandate has not outlived its function and no zombie flag is warranted; the live founding problem paired with the world_rearranges verdict is the coherent configuration. The watch item is the theater series: rising performative share (declaratory disarmament without delivery, caveated pledges, empty-chair ceremonies) is early Goodhart drift toward a piton profile in the bargain layer specifically, even while the restraint core remains functional. If the enforcement machinery decays while the declaratory performance continues, the arrangement would migrate toward theatrical maintenance of a norm whose substance has leaked away — the trajectory the entrepreneur_exit omega is designed to detect.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_attribution_ambiguity,
    'This story is one reading of the kernel total_war_possibility_space — the nuclear_taboo_reading, which attributes eight decades of non-use to a constructed normative prohibition operating independently of material capability. The sibling deterrence_equilibrium_reading attributes the same observation to mutual-vulnerability deterrence. Where the disagreement is located is the causal attribution of non-use: normative foreclosure versus material deterrence. Which attribution is correct?',
    'Counterfactual and archival analysis of cases where deterrence calculus favored use but non-use obtained — Korea 1950-53, the Vietnam reviews, Cuban crisis sub-crises — asking whether restraint exceeded what vulnerability-based deterrence alone predicts; cross-checked against episodes where deterrence was thin or absent (early US monopoly, regional crises) yet non-use held.',
    'If deterrence suffices to explain the record, the taboo is epiphenomenal and this constraint collapses into the deterrence_equilibrium_reading''s structure — the enforcement machinery becomes redundant decoration and measured extraction reattributes to pure security competition. If the norm carries independent weight, this story''s structure stands and the sibling reading under-specifies the mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_attribution_ambiguity, conceptual, 'Whether observed non-use is caused by constructed taboo or by material deterrence — the core committer dispute of the kernel.').

omega_variable(
    maintenance_mechanism_ambiguity,
    'Is the prohibition sustained by active normative maintenance (entrepreneurship, ritual, institutional reproduction — this reading''s mechanism) or by internalized cognitive foreclosure of the kind the sibling space_contraction_reading describes, where total war is filtered out before options are ever generated?',
    'Process-tracing inside planning organizations: if usable options are repeatedly generated and then struck by named decisions (the documentary record so far suggests this), maintenance is active and normative; if options never reach paper across generational turnover of staffs, foreclosure is cognitive and automatic.',
    'If internalized, the constraint''s effective suppression is higher than the structural enforcement measure suggests — the restraint travels inside personnel and survives enforcement decay — and the space_contraction_reading absorbs this story''s explanatory work. If actively maintained, the constraint is fragile to entrepreneur exit and the enforcement series measures the real load-bearing structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maintenance_mechanism_ambiguity, conceptual, 'Active normative maintenance versus internalized cognitive foreclosure as the binding mechanism.').

omega_variable(
    entrepreneur_exit_prediction_test,
    'This reading predicts the taboo weakens if norm entrepreneurs exit. Does taboo strength in fact track the health and continuity of the arms-control epistemic community and its institutional hosts?',
    'Longitudinal comparison of declaratory-policy erosion, crisis signaling, and planning-language change against measurable indicators of entrepreneur-community attrition: budget cycles, staffing collapse, generational turnover without Hiroshima-memory anchoring, closure of dedicated institutions.',
    'Confirmation validates the reading''s causal mechanism and makes the norm_entrepreneur_epistemic_community seat the constraint''s critical dependency; disconfirmation would mean the restraint is over-determined by deeper structures (identity, deterrence, or cognition) and the reading''s maintenance claim demotes to secondary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(entrepreneur_exit_prediction_test, empirical, 'Testable prediction: taboo strength tracks norm-entrepreneur continuity.').

omega_variable(
    nnws_constraint_structure_delta,
    'This reading predicts non-nuclear powers face a different constraint structure than nuclear powers under the same taboo. Is the prohibition experienced symmetrically by possessors and non-possessors, or does the regime generate structurally distinct positions?',
    'Comparative analysis of compliance dynamics, hedging behavior, and crisis conduct across possessor and non-possessor states; natural experiments from states that crossed the threshold (DPRK, and the 1998 South Asian tests) showing how the constraint''s shape changed on the far side.',
    'If the structures differ as predicted, the regime''s asymmetric burdens are functional to the coordination it delivers and the tangled structure is confirmed; if symmetric, the asymmetric extraction documented in the victim declarations is gratuitous hierarchy rather than regime necessity, and the extraction score should be read as pure rent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(nnws_constraint_structure_delta, empirical, 'Whether possessors and non-possessors face structurally different versions of the prohibition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__nuclear_taboo_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1945, 0.05).
narrative_ontology:measurement(tota_tr_t1953, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1953, 0.08).
narrative_ontology:measurement(tota_tr_t1962, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1962, 0.14).
narrative_ontology:measurement(tota_tr_t1968, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1968, 0.22).
narrative_ontology:measurement(tota_tr_t1980, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1980, 0.26).
narrative_ontology:measurement(tota_tr_t1991, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1991, 0.31).
narrative_ontology:measurement(tota_tr_t2003, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 2003, 0.36).
narrative_ontology:measurement(tota_tr_t2017, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 2017, 0.45).
narrative_ontology:measurement(tota_tr_t2025, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 2025, 0.48).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1945, 0.15).
narrative_ontology:measurement(tota_be_t1953, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1953, 0.2).
narrative_ontology:measurement(tota_be_t1962, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1962, 0.28).
narrative_ontology:measurement(tota_be_t1968, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1968, 0.38).
narrative_ontology:measurement(tota_be_t1980, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1980, 0.42).
narrative_ontology:measurement(tota_be_t1991, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1991, 0.44).
narrative_ontology:measurement(tota_be_t2003, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 2003, 0.51).
narrative_ontology:measurement(tota_be_t2017, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 2017, 0.53).
narrative_ontology:measurement(tota_be_t2025, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 2025, 0.5).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1945, 0.12).
narrative_ontology:measurement(tota_su_t1953, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1953, 0.18).
narrative_ontology:measurement(tota_su_t1962, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1962, 0.3).
narrative_ontology:measurement(tota_su_t1968, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1968, 0.45).
narrative_ontology:measurement(tota_su_t1980, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1980, 0.52).
narrative_ontology:measurement(tota_su_t1991, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1991, 0.4).
narrative_ontology:measurement(tota_su_t2003, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 2003, 0.54).
narrative_ontology:measurement(tota_su_t2017, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 2017, 0.57).
narrative_ontology:measurement(tota_su_t2025, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 2025, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_possibility_space__nuclear_taboo_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, deterrence_equilibrium_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, space_contraction_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'why no nuclear war since 1945' conflates three structurally distinct constraints — three readings of one kernel, total_war_possibility_space. This file instantiates the nuclear_taboo_reading (constructed normative prohibition, independent of material capability, generating enforcement machinery). The sibling files instantiate deterrence_equilibrium_reading (mutual vulnerability suffices; the norm is epiphenomenal) and space_contraction_reading (cognitive foreclosure; nothing is maintained because nothing is thinkable). The readings' epsilon values differ by construction: the deterrence reading authors low epsilon over a materially determined arrangement; the space-contraction reading authors near-zero agentic extraction over an automatic one; this reading authors moderate epsilon over a maintained arrangement with asymmetric burdens. A further within-reading decomposition is available if corpus data demands it — the bare use-taboo norm (near-pure coordination, epsilon < 0.2) versus the NPT bargain layer (concentrated asymmetry, epsilon > 0.6) — but this story takes the combined standing arrangement as referent because the reading's structural delta treats the enforcement mechanisms as generated by the taboo rather than separable from it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(total_war_possibility_space__nuclear_taboo_reading, institutional, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
