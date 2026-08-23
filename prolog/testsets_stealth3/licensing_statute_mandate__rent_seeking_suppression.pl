% ============================================================================
% CONSTRAINT STORY: licensing_statute_mandate__rent_seeking_suppression
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_licensing_statute_mandate__rent_seeking_suppression, []).

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
 *   constraint_id: licensing_statute_mandate__rent_seeking_suppression
 *   human_readable: Occupational Licensing Statutes as Incumbent Supply Restriction (Rent-Seeking Reading)
 *   domain: economic/political/labor-regulatory
 *
 * SUMMARY:
 *   Statutory credential requirements now govern roughly a fifth to a quarter
 *   of the U.S. workforce across more than a thousand occupations. This story
 *   instantiates the rent_seeking_suppression reading of the
 *   licensing_statute_mandate kernel: it models the standing licensing
 *   arrangement as it presents itself under the hypothesis that its operative
 *   function is supply restriction serving incumbent practitioners, with
 *   consumers paying scarcity prices and entrants paying barrier costs. Per
 *   the epsilon-invariance decomposition, the colloquial label 'occupational
 *   licensing' covers three structurally distinct claims; the sibling
 *   readings (public_safety_coordination, graduated_access_filter) are
 *   separate constraint files with their own epsilon values, beneficiary
 *   sets, and classifications, linked through network.affects_constraints.
 *   The claimed_type (snare) is stated from this reading's authoring seat;
 *   the metrics are authored as independent descriptive judgments of the
 *   arrangement's actual operation — where the engine's per-seat computations
 *   diverge from the claim, that divergence is the datum. KEY AGENTS (by
 *   structural relationship): - licensed_incumbents: Primary beneficiary
 *   (organized/identity_locked) — collects the wage premium from restricted
 *   supply - professional_associations: Agenda setter
 *   (institutional/arbitrage) — drafts model legislation, staffs boards, owns
 *   exams - state_licensing_boards: Enforcement arm
 *   (institutional/constrained) — prosecutes unlicensed practice, interprets
 *   scope rules - aspiring_practitioners: Primary target
 *   (powerless/constrained) — bears training, exam, and delay costs -
 *   consumers_of_licensed_services: Cost bearer with partial offset
 *   (moderate/constrained) — pays scarcity prices, receives some assurance -
 *   interstate_mobile_workers: Secondary target (moderate/constrained) — pays
 *   the re-licensure toll on movement - examination_and_ceu_vendors:
 *   Parasitic beneficiary (organized/arbitrage) — sells statutorily mandated
 *   products - unlicensed_parallel_providers: Excluded voice
 *   (powerless/trapped) — criminalized for practicing without a credential -
 *   state_legislatures: Statutory author (institutional/mobile) — could
 *   repeal but faces concentrated opposition - labor_economists: Analytical
 *   observer (analytical/analytical) — measures premia, entry effects,
 *   quality deltas
 *
 * KEY AGENTS:
 *   - licensed_incumbents: primary beneficiary (organized/identity_locked)
 *   - professional_associations: agenda setter (institutional/arbitrage)
 *   - state_licensing_boards: enforcement arm and secondary beneficiary (institutional/constrained)
 *   - aspiring_practitioners: primary target (powerless/constrained)
 *   - consumers_of_licensed_services: cost bearer with partial offset (moderate/constrained)
 *   - interstate_mobile_workers: secondary target (moderate/constrained)
 *   - examination_and_ceu_vendors: parasitic beneficiary (organized/arbitrage)
 *   - unlicensed_parallel_providers: excluded voice (powerless/trapped)
 *   - state_legislatures: statutory author (institutional/mobile)
 *   - labor_economists: analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(licensing_statute_mandate__rent_seeking_suppression, 0.82).
domain_priors:suppression_score(licensing_statute_mandate__rent_seeking_suppression, 0.72).
domain_priors:theater_ratio(licensing_statute_mandate__rent_seeking_suppression, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, extractiveness, 0.82).
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(licensing_statute_mandate__rent_seeking_suppression, snare).
narrative_ontology:human_readable(licensing_statute_mandate__rent_seeking_suppression, "Occupational Licensing Statutes as Incumbent Supply Restriction (Rent-Seeking Reading)").
narrative_ontology:topic_domain(licensing_statute_mandate__rent_seeking_suppression, "economic/political/labor-regulatory").

domain_priors:requires_active_enforcement(licensing_statute_mandate__rent_seeking_suppression).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(licensing_statute_mandate__rent_seeking_suppression, 'b80e1eb3-e88d-411e-ab57-a884ad6ccc61').
narrative_ontology:cs_kernel_codification('b80e1eb3-e88d-411e-ab57-a884ad6ccc61', formalized).
narrative_ontology:cs_authority_grounding('b80e1eb3-e88d-411e-ab57-a884ad6ccc61', extraction).
narrative_ontology:cs_interpretation_layer_present('b80e1eb3-e88d-411e-ab57-a884ad6ccc61').
narrative_ontology:cs_reading_relation('b80e1eb3-e88d-411e-ab57-a884ad6ccc61', licensing_statute_mandate__public_safety_coordination, coexists_with).
narrative_ontology:cs_reading_relation('b80e1eb3-e88d-411e-ab57-a884ad6ccc61', licensing_statute_mandate__graduated_access_filter, influences).
narrative_ontology:cs_axiom('b80e1eb3-e88d-411e-ab57-a884ad6ccc61', foundational, licensing_operates_as_artificial_scarcity).
narrative_ontology:cs_axiom_status(licensing_operates_as_artificial_scarcity, holdable).
narrative_ontology:cs_axiom_grounding('b80e1eb3-e88d-411e-ab57-a884ad6ccc61', licensing_operates_as_artificial_scarcity, empirically_contingent).
narrative_ontology:cs_axiom('b80e1eb3-e88d-411e-ab57-a884ad6ccc61', secondary, stringency_tracks_organization_not_hazard).
narrative_ontology:cs_axiom_status(stringency_tracks_organization_not_hazard, holdable).
narrative_ontology:cs_axiom_grounding('b80e1eb3-e88d-411e-ab57-a884ad6ccc61', stringency_tracks_organization_not_hazard, empirically_contingent).
narrative_ontology:cs_reference_frame('b80e1eb3-e88d-411e-ab57-a884ad6ccc61', incumbent_rent_preservation_regime).
narrative_ontology:cs_drift_state('b80e1eb3-e88d-411e-ab57-a884ad6ccc61', contemporary_reform_wave, gap(repudiation_pressure, minor, true)).
narrative_ontology:cs_created_at('b80e1eb3-e88d-411e-ab57-a884ad6ccc61', '').
narrative_ontology:cs_kernel_id(licensing_statute_mandate__rent_seeking_suppression, licensing_statute_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__rent_seeking_suppression, licensed_incumbents).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__rent_seeking_suppression, professional_associations).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__rent_seeking_suppression, examination_and_ceu_vendors).
narrative_ontology:constraint_victim(licensing_statute_mandate__rent_seeking_suppression, aspiring_practitioners).
narrative_ontology:constraint_victim(licensing_statute_mandate__rent_seeking_suppression, consumers_of_licensed_services).
narrative_ontology:constraint_victim(licensing_statute_mandate__rent_seeking_suppression, interstate_mobile_workers).
narrative_ontology:constraint_victim(licensing_statute_mandate__rent_seeking_suppression, unlicensed_parallel_providers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__rent_seeking_suppression, state_licensing_boards).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__rent_seeking_suppression, consumers_of_licensed_services).
narrative_ontology:constraint_vindicates(licensing_statute_mandate__rent_seeking_suppression, stigler_capture_hypothesis).
narrative_ontology:constraint_vindicates(licensing_statute_mandate__rent_seeking_suppression, artificial_scarcity_rent_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold credentials that cap the number of legal competitors in their occupation. Their wages run above comparable unlicensed occupations, and their training investments are protected from undercutting. They staff board seats, respond to association calls, and fund campaigns against scope reductions. Leaving the occupation would strand their credential investment, so their livelihoods are bound to the system's continuation.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, licensed_incumbents, beneficiary,
    organized, biographical, identity_locked, national).

% Draft model licensing bills, testify in legislatures, nominate board members, and own the examinations and continuing-education requirements. Dues and exam fees flow to them; their staffing and budgets scale with the number of regulated occupations and registrants. They operate across many occupations and jurisdictions, so they can shift effort to wherever regulation expands next.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, professional_associations, agenda_setter,
    institutional, generational, arbitrage, national).

% Administer the statutes day to day: issuing credentials, investigating complaints, and prosecuting practice without a license. Board seats are filled disproportionately from the licensed occupation itself. Their budget and personnel exist only because the statutes exist, and several boards collect fee revenue that funds their own operations.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, state_licensing_boards, agenda_setter,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(licensing_statute_mandate__rent_seeking_suppression, state_licensing_boards, beneficiary).

% Face tuition for mandated schooling, examination fees, supervised-hour requirements, and waiting periods before earning income in their chosen trade. Many finance training on debt. Those who fail or drop out bear the cost without the credential; those who persist arrive years later and poorer than peers in unregulated trades. Switching careers forfeits sunk training costs.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, aspiring_practitioners, payer,
    powerless, biographical, constrained, national).

% Pay prices elevated by restricted practitioner supply for haircuts, childcare, medical care, electrical work, and dozens of other services. They receive in exchange a standardized credential signal and a disciplinary body to complain to, though they cannot personally verify how much protection the credential adds. Individually they have little leverage; their main alternatives are self-supply, informal providers, or going without.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, consumers_of_licensed_services, payer,
    moderate, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(licensing_statute_mandate__rent_seeking_suppression, consumers_of_licensed_services, beneficiary).

% Licensed in one state, they discover their credential does not travel: new state, new coursework, new exam, new fees, sometimes years of delay. Military spouses and telehealth practitioners hit this wall repeatedly. Portability compacts cover a handful of occupations; most movers simply pay the re-entry toll or leave the occupation.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, interstate_mobile_workers, payer,
    moderate, biographical, constrained, continental).

% Sell mandatory products: preparatory courses, examination administration, and continuing-education units required for renewal. Their revenue is written into the statutes as a requirement, so demand is guaranteed by law rather than earned in competition. Multi-state testing companies consolidate as requirements multiply.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, examination_and_ceu_vendors, beneficiary,
    organized, immediate, arbitrage, global).

% Practice or would practice the same trades without credentials — informal braiders, teeth-whitening technicians, peer counselors, handymen performing tasks inside a licensed scope. They serve customers the formal market prices out, usually off the books, and face cease-and-desist letters, fines, or prosecution when caught. They are rarely invited to the board hearings that set the rules governing them.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, unlicensed_parallel_providers, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(licensing_statute_mandate__rent_seeking_suppression, unlicensed_parallel_providers, excluded).

% Enact, amend, or repeal the statutes. Each bill arrives with organized support from the affected profession and little organized opposition, since costs fall diffusely on consumers and outsiders. Repeal attempts draw intense campaign-pressure responses; expansion draws little attention. Individual legislators face election cycles measured in years.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, state_legislatures, agenda_setter,
    institutional, biographical, mobile, national).

% Measure wage premia across licensed and comparable unlicensed occupations, estimate entry and mobility effects, and run quality comparisons across state regulatory boundaries. They publish outside the system and hold no stake in any occupation's credential value.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, labor_economists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(licensing_statute_mandate__rent_seeking_suppression, licensed_incumbents).
narrative_ontology:fixing_cost_class(licensing_statute_mandate__rent_seeking_suppression, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real information problem: consumers cannot cheaply observe practitioner competence before purchase, and licensing supplies a uniform, state-backed signal plus a disciplinary venue for complaints. It also coordinates reciprocal trust between states for the few occupations covered by compacts.
% TRANSFER_FUNCTION: Moves money from consumers (scarcity-elevated prices) and aspiring practitioners (tuition, exam fees, supervised hours, delayed earnings) to licensed incumbents (wage premium), professional associations (dues and exam administration), and mandated-course vendors; moves enforcement attention toward unlicensed competitors.
% ABSENT_VOICES: Unlicensed parallel providers and priced-out aspiring practitioners are absent from board hearings, which seat incumbents disproportionately; low-income consumers of licensed services hold no seat either. Public board members exist in some states but hold a minority of votes.
% DISAPPEARANCE_RATIONALE: Overnight repeal would reprice licensed services downward as entry opened, strand training-school economics and board budgets, force associations to compete for voluntary membership, and redistribute the wage premia toward consumers and new entrants — millions of careers, budgets, and business models are arranged around the statutes.
% FOUNDING_PROBLEM: Progressive-era reformers faced demonstrable harms: patent-medicine quackery, unsafe electrical wiring, building failures, unsanitary barbering. Where consumers could not evaluate technical quality and failure was catastrophic or invisible, minimum standards promised protection.
% FOUNDING_PROBLEM_CORROBORATION: Consumer-protection histories and injury statistics corroborate liveness for high-hazard trades (medicine, electrical, structural work); labor-economic studies and FTC testimony attest the problem is thin or absent for low-hazard occupations (cosmetology, interior design, floristry). No external source attests uniform liveness across the roughly thousand-plus regulated occupations; the strongest corroboration outside the beneficiary set comes from state sunset-review audits, several of which found no measurable harm reduction from repeal candidates.
narrative_ontology:disappearance_verdict(licensing_statute_mandate__rent_seeking_suppression, world_rearranges).
narrative_ontology:founding_problem_status(licensing_statute_mandate__rent_seeking_suppression, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(licensing_statute_mandate__rent_seeking_suppression, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(licensing_statute_mandate__rent_seeking_suppression, 'none', 1).
narrative_ontology:epsilon_provenance(licensing_statute_mandate__rent_seeking_suppression, 0.82, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(licensing_statute_mandate__rent_seeking_suppression_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(licensing_statute_mandate__rent_seeking_suppression, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(licensing_statute_mandate__rent_seeking_suppression_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.82) because the arrangement's measurable outputs — wage premia of roughly 10-15 percent in licensed occupations, suppressed entry rates, elevated service prices — flow asymmetrically to credentialed incumbents while costs concentrate on entrants and consumers. Suppression (0.72) records the raw coercive machinery: boards prosecuting practice without a license, scope-of-practice walls criminalizing adjacent work, and portability barriers taxing movement; it is authored as an unscaled structural property, with only extractiveness subject to directionality and scope modification downstream. Theater (0.48) reflects an examination-and-continuing-education apparatus that is real but whose stringency is poorly coupled to occupational hazard — mandated CEU hours in low-risk trades being the clearest performative segment. Accessibility_collapse (0.58): alternatives persist — informal provision, adjacent unregulated occupations, interstate variation — but each carries penalty or forfeiture, so understood alternatives collapse only partially. Resistance (0.6): litigation, sunset commissions, portability compacts, and recurring repeal bills. All three tracked metrics share one eight-point grid (t=0..70 by decade); the series are smoothed decade-resolution renditions of a ratchet punctuated by episodic reform waves (the 1970s deregulation push, post-2015 recognition acts) whose local dips are too small to register at this resolution. Suppression decomposes predominantly structural (legal prohibition and enforcement, roughly four fifths) with a smaller internalized component — consumers' learned equation of licensure with competence, which persists as a reputational penalty on unlicensed providers even where statutes relax. Coalition note: aspiring practitioners and unlicensed providers could in principle form a reform coalition outnumbering incumbents, but each faces private sunk costs and diffuse prospects — the classic collective-action gap that keeps the payer class disorganized; coalition formation is the live contingency behind the reform_failure_mechanism omega.
 *
 * PERSPECTIVAL GAP:
 *   The seats should classify differently. From the incumbent seat the arrangement is a purchased asset: dues paid, exams passed, premium collected — a coordination good they fund and staff, computing with low effective extraction. From the entrant seat the same statutes are a wall with a toll: identical nominal rule, opposite sign on every cash flow. From the legislature seat the statutes are neutral instruments revisable at will, obscuring that revision is politically priced. From the vendor seat the arrangement is demand guaranteed by law. The engine computes these divergences from power, exit, and directional position; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive d toward zero: licensed_incumbents (identity_locked — their credential investment binds them to the system's continuation, reinforcing maintenance rather than exit), professional_associations (arbitrage — they profit across whichever occupation regulates next), and examination_and_ceu_vendors (arbitrage — statutorily guaranteed demand). Victim declarations drive d toward one: aspiring_practitioners (constrained — sunk training costs penalize exit), consumers_of_licensed_services (constrained, moderated by their secondary beneficiary position — they do receive a usable signal and a complaint venue), interstate_mobile_workers (constrained, with continental scope amplifying effective extraction through verification difficulty), and unlicensed_parallel_providers (trapped — their trade choice is unlawful without the credential). No directionality overrides were needed: the derivation from declarations plus exit atoms reproduces these positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding safety problem was live at founding for high-hazard trades and remains genuinely live there, which blocks any blanket zombie verdict; but the arrangement's growth has been least coupled to hazard where it grew fastest. The classification machinery separates three failure modes that casual labels conflate: a degraded-inertia reading would require no concentrated maintainer, yet incumbents and associations actively fund defense of the statutes, so inertia is not the load-bearing explanation; a pure-coordination reading would require costs to track coordination benefit, yet premium and stringency decouple from measured hazard; the extraction reading survives because persistence depends on enforcement against exits (unlicensed-practice prosecutions) and on foreclosing the alternative of entry. The contested founding_problem_status paired with a world_rearranges disappearance verdict correctly refuses both the 'pure protection' and 'dead letter' verdicts: the world is arranged around the statutes, and the arrangement is maintained because maintenance pays.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story is one reading (rent_seeking_suppression) of the licensing_statute_mandate kernel; which structural elements would flip under the sibling readings (public_safety_coordination, graduated_access_filter), and where exactly is the disagreement located?',
    'Cross-reading comparison of the sibling constraint files: adopting public_safety_coordination collapses the beneficiary/victim asymmetry into a coordination-cost account (epsilon falls toward the identity_coordination floor); adopting graduated_access_filter expands the victim set to class-stratified entrants while incumbent collections remain. The disagreement is located in operative-purpose attribution — whether the statutes'' persistence is explained by incumbent benefit or by consumer protection.',
    'Classification is reading-indexed: this file authors the snare verdict for the rent-seeking reading only; sibling files carry their own epsilon, victim sets, and types. No reading-independent classification of the kernel exists.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: reading-indexed classification of the licensing kernel; siblings are separate constraints linked by network edges.').

omega_variable(
    stringency_vs_hazard_correlation,
    'Does licensing stringency (training hours, exam difficulty, scope breadth) track occupational hazard rates or incumbent political organization?',
    'Cross-state regression of stringency measures on injury and fatality rates versus professional-association density and lobbying expenditure.',
    'If stringency tracks organization rather than hazard, the rent-extraction reading is confirmed and epsilon stays high; if it tracks hazard, part of the measured extraction is reattributed to genuine screening cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stringency_vs_hazard_correlation, empirical, 'Whether barrier stringency prices hazard or political organization.').

omega_variable(
    quality_effect_null_result,
    'Do stricter licensing regimes produce measurably better consumer outcomes than looser ones?',
    'Natural experiments: within-occupation cross-state comparisons and deregulation episodes (nurse-practitioner scope expansions, natural-hair-braider delicensing) with outcome tracking.',
    'Null or negative quality deltas support the snare classification; large positive deltas would force reattribution toward tangled_rope with a genuine coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quality_effect_null_result, empirical, 'Whether the safety function is empirically delivered.').

omega_variable(
    reform_failure_mechanism,
    'Does licensing reform fail because voters cannot monitor the issue (rational ignorance) or because incumbents wield veto power over legislators?',
    'Comparative study of reform episodes where incumbent opposition was neutralized (sunset commissions, budget-reform vehicles) versus ordinary legislative channels.',
    'If veto-power-driven, fixing_cost stays prohibitive and the arrangement persists; if ignorance-driven, information interventions could cheapen reform and shift the constraint toward a contestable coordination account.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reform_failure_mechanism, preference, 'Political mechanism sustaining the arrangement against majority-favorable reform.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(licensing_statute_mandate__rent_seeking_suppression, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lice_tr_t0, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 0, 0.22).
narrative_ontology:measurement(lice_tr_t10, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 10, 0.26).
narrative_ontology:measurement(lice_tr_t20, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 20, 0.3).
narrative_ontology:measurement(lice_tr_t30, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 30, 0.33).
narrative_ontology:measurement(lice_tr_t40, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 40, 0.37).
narrative_ontology:measurement(lice_tr_t50, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 50, 0.41).
narrative_ontology:measurement(lice_tr_t60, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 60, 0.45).
narrative_ontology:measurement(lice_tr_t70, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 70, 0.48).

% Extraction over time
narrative_ontology:measurement(lice_be_t0, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(lice_be_t10, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(lice_be_t20, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 20, 0.64).
narrative_ontology:measurement(lice_be_t30, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(lice_be_t40, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 40, 0.71).
narrative_ontology:measurement(lice_be_t50, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 50, 0.75).
narrative_ontology:measurement(lice_be_t60, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 60, 0.79).
narrative_ontology:measurement(lice_be_t70, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 70, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(lice_su_t0, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(lice_su_t10, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 10, 0.46).
narrative_ontology:measurement(lice_su_t20, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 20, 0.52).
narrative_ontology:measurement(lice_su_t30, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 30, 0.57).
narrative_ontology:measurement(lice_su_t40, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 40, 0.61).
narrative_ontology:measurement(lice_su_t50, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 50, 0.65).
narrative_ontology:measurement(lice_su_t60, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 60, 0.69).
narrative_ontology:measurement(lice_su_t70, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 70, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(licensing_statute_mandate__rent_seeking_suppression, identity_coordination).
narrative_ontology:affects_constraint(licensing_statute_mandate__rent_seeking_suppression, licensing_statute_mandate__public_safety_coordination).
narrative_ontology:affects_constraint(licensing_statute_mandate__rent_seeking_suppression, licensing_statute_mandate__graduated_access_filter).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'occupational licensing' decomposes per the epsilon-invariance principle into three structurally distinct claims. This file authors the rent_seeking_suppression reading (high epsilon, incumbent beneficiaries, entrant/consumer victims). The public_safety_coordination sibling is the upstream legitimating claim — its protective framing is the cover under which stringency expands, which is why this reading contests its legitimacy conditions rather than foreclosing it. The graduated_access_filter sibling is downstream of this one: the rent-seeking/capture literature supplies the mechanism (artificial scarcity) whose distributional incidence the class-filter empirical program traces. Each member carries its own epsilon, beneficiaries, and victims; no story in the family hedges across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
