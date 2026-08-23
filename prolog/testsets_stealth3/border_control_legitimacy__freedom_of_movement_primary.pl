% ============================================================================
% CONSTRAINT STORY: border_control_legitimacy__freedom_of_movement_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_control_legitimacy__freedom_of_movement_primary, []).

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
 *   constraint_id: border_control_legitimacy__freedom_of_movement_primary
 *   human_readable: Border-Closure Arrangement Assessed from the Freedom-of-Movement-Primary Reading
 *   domain: political philosophy/international law/migration studies
 *
 * SUMMARY:
 *   A standing international arrangement grants states authority to close
 *   territorial borders: to refuse entry, detain, remove, and interdict
 *   persons who are not citizens, sustained by patrols, detention estates,
 *   externalized processing, and a global document regime. This story
 *   assesses that standing arrangement from one reading of the contested
 *   border-control-legitimacy kernel - the reading on which freedom of
 *   movement is a fundamental human right held by persons rather than
 *   citizens, and territorial sovereignty therefore confers no closure
 *   authority. On that reading the referent of epsilon is the closure
 *   arrangement itself, assessed by the reading's own lights - not the
 *   open-admission alternative the reading endorses. The arrangement takes
 *   movement-liberty, labor at suppressed wages, family unity, and
 *   life-chances from millions of people who cannot vote anywhere their fate
 *   is decided, while a smaller set of seats collects budgets, contracts,
 *   fees, wage margins, and diffuse insider insulation. Claimed type and
 *   metrics are independent authored facts: the claim states what is
 *   structurally true from this reading's seat; the metrics describe the
 *   arrangement's actual operation; the engine computes per-seat
 *   classifications from the structural data and owns any divergence. KEY
 *   AGENTS (by structural relationship): -
 *   destination_immigration_enforcement_agencies: agenda-setting
 *   administrator (institutional/identity_locked) - runs and expands the
 *   machinery - private_detention_border_security_contractors: concentrated
 *   beneficiary (powerful/arbitrage) - sells custody and surveillance
 *   capacity - destination_employers_of_deportable_labor: primary receipt
 *   seat (powerful/arbitrage) - harvests the deportability wage margin -
 *   people_smuggling_networks: parasitic beneficiary (organized/arbitrage) -
 *   closure creates their market - destination_citizen_labor_insiders:
 *   diffuse beneficiary (moderate/constrained) - origin_state_governments:
 *   dual-positioned (institutional/constrained) - receives remittances and
 *   cooperation deals; bears skill drain and mortality -
 *   irregularized_migrant_workers: primary target (powerless/trapped) -
 *   asylum_seekers_and_refugees: primary target (powerless/trapped) -
 *   transnational_families_separated_by_borders: target
 *   (moderate/constrained) - documented_noncitizen_residents: target
 *   (moderate/constrained) - climate_displaced_populations: excluded
 *   prospective target (powerless/trapped) - bound by the regime, absent from
 *   its categories - migrant_rights_advocacy_networks: excluded objectors
 *   (organized/constrained) - un_human_rights_treaty_bodies: analytical
 *   observer (institutional/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_control_legitimacy__freedom_of_movement_primary, 0.84).
domain_priors:suppression_score(border_control_legitimacy__freedom_of_movement_primary, 0.8).
domain_priors:theater_ratio(border_control_legitimacy__freedom_of_movement_primary, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, extractiveness, 0.84).
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_control_legitimacy__freedom_of_movement_primary, snare).
narrative_ontology:human_readable(border_control_legitimacy__freedom_of_movement_primary, "Border-Closure Arrangement Assessed from the Freedom-of-Movement-Primary Reading").
narrative_ontology:topic_domain(border_control_legitimacy__freedom_of_movement_primary, "political philosophy/international law/migration studies").

domain_priors:requires_active_enforcement(border_control_legitimacy__freedom_of_movement_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_control_legitimacy__freedom_of_movement_primary, '34914fa9-45cb-4743-9053-ed74e20ba7a2').
narrative_ontology:cs_kernel_codification('34914fa9-45cb-4743-9053-ed74e20ba7a2', distributed).
narrative_ontology:cs_authority_grounding('34914fa9-45cb-4743-9053-ed74e20ba7a2', lineage).
narrative_ontology:cs_interpretation_layer_present('34914fa9-45cb-4743-9053-ed74e20ba7a2').
narrative_ontology:cs_reading_relation('34914fa9-45cb-4743-9053-ed74e20ba7a2', border_control_legitimacy__sovereignty_primary, forecloses).
narrative_ontology:cs_reading_relation('34914fa9-45cb-4743-9053-ed74e20ba7a2', border_control_legitimacy__jurisdictional_sovereignty, forecloses).
narrative_ontology:cs_axiom('34914fa9-45cb-4743-9053-ed74e20ba7a2', foundational, free_movement_fundamental_inviolable_right).
narrative_ontology:cs_axiom_status(free_movement_fundamental_inviolable_right, holdable).
narrative_ontology:cs_axiom_grounding('34914fa9-45cb-4743-9053-ed74e20ba7a2', free_movement_fundamental_inviolable_right, deontological).
narrative_ontology:cs_axiom('34914fa9-45cb-4743-9053-ed74e20ba7a2', foundational, authority_limited_to_jurisdictional_regulation).
narrative_ontology:cs_axiom_status(authority_limited_to_jurisdictional_regulation, holdable).
narrative_ontology:cs_axiom_grounding('34914fa9-45cb-4743-9053-ed74e20ba7a2', authority_limited_to_jurisdictional_regulation, conventional).
narrative_ontology:cs_axiom('34914fa9-45cb-4743-9053-ed74e20ba7a2', secondary, closure_enforcement_apparatus_delegitimized).
narrative_ontology:cs_axiom_status(closure_enforcement_apparatus_delegitimized, holdable).
narrative_ontology:cs_axiom_grounding('34914fa9-45cb-4743-9053-ed74e20ba7a2', closure_enforcement_apparatus_delegitimized, deontological).
narrative_ontology:cs_reference_frame('34914fa9-45cb-4743-9053-ed74e20ba7a2', mobility_as_fundamental_right_baseline).
narrative_ontology:cs_drift_state('34914fa9-45cb-4743-9053-ed74e20ba7a2', contemporary_externalization_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('34914fa9-45cb-4743-9053-ed74e20ba7a2', '').
narrative_ontology:cs_kernel_id(border_control_legitimacy__freedom_of_movement_primary, border_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_control_legitimacy__freedom_of_movement_primary, destination_immigration_enforcement_agencies).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__freedom_of_movement_primary, private_detention_border_security_contractors).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__freedom_of_movement_primary, destination_employers_of_deportable_labor).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__freedom_of_movement_primary, people_smuggling_networks).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__freedom_of_movement_primary, destination_citizen_labor_insiders).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__freedom_of_movement_primary, origin_state_governments).
narrative_ontology:constraint_victim(border_control_legitimacy__freedom_of_movement_primary, irregularized_migrant_workers).
narrative_ontology:constraint_victim(border_control_legitimacy__freedom_of_movement_primary, asylum_seekers_and_refugees).
narrative_ontology:constraint_victim(border_control_legitimacy__freedom_of_movement_primary, transnational_families_separated_by_borders).
narrative_ontology:constraint_victim(border_control_legitimacy__freedom_of_movement_primary, documented_noncitizen_residents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(border_control_legitimacy__freedom_of_movement_primary, origin_state_governments).
narrative_ontology:constraint_vindicates(border_control_legitimacy__freedom_of_movement_primary, territorial_exclusion_premise).
narrative_ontology:constraint_vindicates(border_control_legitimacy__freedom_of_movement_primary, state_mobility_monopoly_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Run the admission, custody, and removal machinery: patrol frontiers, operate detention estates, process asylum claims, execute removals. Budgets, headcount, and statutory mandates expand with each enforcement surge, and successive leaderships inherit and defend the apparatus as the organization's reason to exist. Exit would mean dismantling themselves; no leadership proposes it, and bloc-level variants pool the same mission across borders.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, destination_immigration_enforcement_agencies, agenda_setter,
    institutional, generational, identity_locked, national).

% Sell custody beds, surveillance towers, biometric screening systems, deportation charter flights, and frontier-surveillance services under multi-year government contracts. Revenue scales with apprehension and detention volumes, and diversified portfolios let them follow enforcement demand wherever externalization funding flows, bidding across jurisdictions.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, private_detention_border_security_contractors, beneficiary,
    powerful, biographical, arbitrage, global).

% Staff agriculture, construction, care work, meatpacking, and hospitality with workers whose presence is legally precarious. Precarity suppresses wage demands and discourages complaints about conditions, and the gap between what documented and undocumented labor costs recurs as a margin. They voice support for enforcement rhetoric while quietly resisting worksite sweeps that would remove their workforce, and can relocate capital if a jurisdiction's enforcement turns hostile to their staffing model.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, destination_employers_of_deportable_labor, beneficiary,
    powerful, biographical, arbitrage, global).

% Sell passage, forged documents, and route guidance at prices that rise with every enforcement tightening. Each new barrier raises their fee and the danger premium their customers pay. They shift routes within weeks of any crackdown and have no interest in legal channels that would erase their revenue; their fortunes are made by the very closures they circumvent.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, people_smuggling_networks, beneficiary,
    organized, immediate, arbitrage, global).

% Hold jobs and welfare entitlements inside the closed perimeter. Reduced competition for lower-wage work and insulated public services are diffuse gains most never attribute to the border. They may emigrate, but leaving surrenders the protected position, so exit is nominal rather than real.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, destination_citizen_labor_insiders, beneficiary,
    moderate, biographical, constrained, national).

% Cross or overstay without authorization to feed families; work below documented wages under threat of removal; cannot approach authorities to report abuse without risking deportation. Savings go to passage fees; injury and wage theft carry no recourse. Returning means going back to the conditions that drove them out; staying means living in permanent deportability. There is no legal channel available to most of them at any price they could pay.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, irregularized_migrant_workers, payer,
    powerless, immediate, trapped, global).

% Flee persecution or collapse toward protection channels that narrow yearly: safe-third-country rules push them onward, externalized processing strands them in transit states, and deterrence-by-danger policies price the journey in lives. Recognition rates vary by route and nationality more than by case; years in legal limbo awaiting determination are common, and rejection can mean return to the danger fled.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, asylum_seekers_and_refugees, payer,
    powerless, immediate, trapped, global).

% Span the line the regime draws: citizens and residents petition for spouses, parents, children, and siblings through queues that run decades, income thresholds that exclude the poor, and refusal rates that vary by nationality. Mixed-status households organize daily life around one member's deportability. Petition fees and wait times function as a second wall that only money and patience climb.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, transnational_families_separated_by_borders, payer,
    moderate, biographical, constrained, global).

% Live, work, and pay taxes under renewable permits they do not control: tied visas bind them to employers or partners, permanent status recedes behind periodically raised bars, and naturalization waits lengthen. They bear the arrangement's everyday friction - renewals, biometric enrollment, travel limits, benefit ineligibility - while holding no vote over any of it. Departure forfeits accumulated tenure toward settlement.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, documented_noncitizen_residents, payer,
    moderate, biographical, constrained, national).

% Move because water, crops, and coastlines fail, yet hold no visa category anywhere: no treaty recognizes climate flight, no queue exists to join, and arrival is treated as a category error by destination politics. They are governed by the closure machinery without appearing in any of its legal categories - the clearest case of people bound by rules in whose making they have no seat at all.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, climate_displaced_populations, excluded,
    powerless, generational, trapped, global).

% Export labor and receive remittances that exceed development aid; sign readmission and border-cooperation agreements in exchange for aid, visas, and tariff access; publicly protest deaths and deportations while policing departures on destination states' behalf. Skill drain, stranded transit populations, and returning coffins are the cost side of the same ledger, and they cannot exit the interstate bargaining table where these trades are struck.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, origin_state_governments, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(border_control_legitimacy__freedom_of_movement_primary, origin_state_governments, payer).

% Litigate, document deaths and abuses, run search-and-rescue operations, and campaign for regularization. They hold standing in courts and media but no seat where admission quotas, enforcement budgets, and externalization deals are decided; their objections register as ambient noise in the legislative conversation that sets the terms.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, migrant_rights_advocacy_networks, excluded,
    organized, generational, constrained, global).

% Interpret the movement-related articles of the treaties they monitor, issue country findings and general comments, and document violations states decline to remedy. Their findings carry no enforcement force; states engage selectively, and externalized enforcement migrates beyond the practical reach of treaty supervision.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, un_human_rights_treaty_bodies, observer,
    institutional, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_control_legitimacy__freedom_of_movement_primary, destination_employers_of_deportable_labor).
narrative_ontology:fixing_cost_class(border_control_legitimacy__freedom_of_movement_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides states a shared system for verifying identity across borders, adjudicating claims to presence (protection, family, work categories), and removing persons deemed unauthorized; standardizes travel documents worldwide so every polity can distinguish authorized presence from unauthorized presence. The documentation-and-adjudication layer solves identification and due-process problems at scale; a separate closure layer decides who may arrive at all.
% TRANSFER_FUNCTION: Moves liberty of movement, labor at suppressed wages, and life-chances from would-be movers and irregularized residents toward destination-state insiders and employers; moves enforcement spending from taxpayers to custody and surveillance contractors; moves passage fees to smuggling networks; and places mortality risk on the people moving, so the arrangement's heaviest costs fall on those least able to refuse them.
% ABSENT_VOICES: The people the arrangement binds are mostly outside the polities that bind them: would-be migrants cannot vote anywhere their fate is decided; noncitizen residents pay taxes under rules they cannot elect; climate-displaced people appear in no legal category at all. Advocacy networks speak but hold no quota pen; origin-state objections are priced into aid-for-enforcement bargains rather than weighed as objections.
% DISAPPEARANCE_RATIONALE: Overnight removal of closure authority would rearrange labor markets as mobility normalized, empty the custody-contracting sector, collapse smuggling premiums, reunify split families on paper already filed, and force welfare, housing, and municipal planning onto new demographic baselines; remittance corridors and origin-state labor export would reprice. The world's arrangements depend on the thing existing - that dependence is precisely what the payer set finances.
% FOUNDING_PROBLEM: Wartime filtering: when passports became quasi-universal under 1914-1918 emergency measures, the problem was separating spies, deserters, and conscription-evaders from ordinary travelers. Later layers accreted: Depression-era labor-market protection amid mass unemployment, Cold War ideological screening, and older racial-exclusion projects that shaped the machinery long before the passport era.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: historical scholarship on the passport system's emergency origins and the state monopoly on legitimate mobility; migration economists documenting that destination economies actively recruit in origin countries while closure blocks legal entry for the same workers - a contradiction the insider beneficiaries do not attest; and treaty-body reviews finding security rationales invoked without proportionate evidence. No corroborating voice outside the beneficiary set attests that the founding problem remains live in its original form; that attestation gap is itself signal.
narrative_ontology:disappearance_verdict(border_control_legitimacy__freedom_of_movement_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_control_legitimacy__freedom_of_movement_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_control_legitimacy__freedom_of_movement_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(border_control_legitimacy__freedom_of_movement_primary, 'none', 1).
narrative_ontology:epsilon_provenance(border_control_legitimacy__freedom_of_movement_primary, 0.84, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_control_legitimacy__freedom_of_movement_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_control_legitimacy__freedom_of_movement_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_control_legitimacy__freedom_of_movement_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.84 because, from this reading's seat, the standing arrangement takes the largest goods at stake - movement, labor power, family unity, life itself on the routes - from a population with no recourse and no vote, and the take has grown with every enforcement layer. Suppression is 0.80 as a RAW STRUCTURAL property (unscaled by power or scope; only extractiveness is scaled downstream): walls, custody, interdiction, externalized processing, and document precarity are the arrangement's load-bearing machinery, not side effects. Theater ratio is 0.32: exclusion demonstrably functions, but a growing share of activity is symbolic - wall construction as signal, removal statistics as performance, enforcement surges timed to political calendars - superimposed on a monotonic ratchet rather than replacing it. Accessibility collapse is 0.62: once a poor-country national understands the legal channel is closed to them, formal alternatives are effectively nil, but irregular channels persist at lethal price, so alternatives degrade rather than vanish. Resistance is 0.50: episodic coalition surges (mass crossings that temporarily convert trapped individuals into an organized bloc, undocumented-worker strikes, sanctuary jurisdictions, rescue flotillas, strategic litigation) meet the arrangement and are then fragmented by externalization and deterrence-by-danger tactics; coalition potential is real but repeatedly dissolved, which is itself part of the mechanism. Identity_coordination is declared with the corpus gaming-warning attached: the identity framing ('demographic anxiety,' 'who we are') is precisely the cover story the FNL check flags, and the coupling signature here is the flagged pathological one - extraction concentrated on powerless agents at global scope. Measurement series run on ONE SHARED TIME GRID (all three tracked metrics authored at all eight points, 1948-2025); base_properties reflect the interval end-state. The trajectory is a ratchet with electoral pulses smoothed by the grid; the base values were measured in the post-2015 externalization phase, the arrangement's hardest period to date.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. The enforcement-agency seat experiences the arrangement as mission it built and staffs - a self-understanding closer to necessary governance - because its identity is fused with the machinery (institutional identity-lock: the organization has become its function; break the frame and the classification of that seat shifts sharply). Citizen-insider beneficiaries experience quiet subsidy: insulation they rarely attribute to the border, experienced as ordinary market conditions. The payer seats experience the same structure as confinement priced in money and lives. Contractors and smuggling networks experience it as demand - the first through appropriations, the second directly from customers, which is why their exposure to political swings differs despite equal power atoms. Among same-level actors, citizen insiders and documented noncitizen residents share a polity and comparable formal power, but the membership boundary the arrangement maintains differentiates their exits: insider exit is nominal (leaving surrenders the protected position), resident exit forfeits accumulated tenure toward settlement. Cross-kernel divergence - the same arrangement computing as coordination from the sovereignty_primary seat - belongs to the sibling files, not this one.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionalities: enforcement agencies (agenda-setter, mandate capture rather than direct financial receipt), contractors and smuggling networks (direct receipts, arbitrage-grade exits - nearest the beneficiary end), employers (continuous wage-margin receipt), citizen insiders (diffuse insulation), origin governments (remittances and cooperation deals, with the ambivalence routed to omega origin_state_net_position because directionality overrides key on power atoms and cannot isolate one institutional agent). Victim declarations drive high directionalities: irregularized workers, asylum seekers, separated families, and disenfranchised residents, all trapped or constrained, all global-scope. One override is authored: power_atom=powerless at d=0.88, because the derivation from victim-plus-trapped would undershoot the structural truth under this reading - formal alternatives are not merely costly but EMPTY (most of the powerless set has no legal channel at any price), exit is lethally priced, and family anchors hold people in place; that sits nearer the full-target end than a generic trapped-exit derivation reaches. Spatial scope is global, which the engine reflects as harder verification and amplified effective extraction; suppression contributes unscaled.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - wartime filtering plus Depression-era labor protection - is substantially dead: the war passed, the emergency passport measures were never lifted, and each successor era (Cold War screening, drug-war framing, terror securitization, externalization) repurposed inherited machinery for new rationales rather than retiring it. Hence founding_problem_status is contested: defenders attest live successor rationales; corroboration from outside the beneficiary set attests the original problem's obsolescence and the current rationales' evidentiary thinness. Disappearance is nevertheless world_rearranges, because dependence on the arrangement is real regardless of founding obsolescence - the two facts together are the mandatrophy signature without the zombie-flag trigger (which requires status=dead outright). Classification discipline: naming the victim set blocks the defender's rope-framing ('orderly migration coordination') from absorbing the asymmetry; naming the surviving documentation-and-adjudication layer blocks a lazy piton reduction, because the paperwork shell is functional and survives under this reading's own endorsed alternative - the extraction is the closure core, not the forms. Mandatrophy is resolved in the founding-function sense and contested in the successor-rationale sense, and the story records both rather than reconciling them.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_entailment_contestation,
    'Does territorial sovereignty entail border-closure authority? This file instantiates the reading that answers ''no''; the sibling readings answer ''yes, absolutely'' (border_control_legitimacy__sovereignty_primary) and ''not necessarily - balance decides'' (border_control_legitimacy__jurisdictional_sovereignty).',
    'No intra-framework resolution exists - the readings occupy different normative frameworks, so resolution occurs only at the level of which framework a polity adopts. Track convergence over time in court decisions, treaty interpretation, and public justification.',
    'Under sovereignty_primary the victim set dissolves (closure becomes legitimate statehood-expression) and epsilon collapses toward coordination cost; under jurisdictional_sovereignty epsilon lands intermediate with a conditional victim set. This file''s epsilon is indexed to THIS reading''s lights over the standing closure arrangement - the endorsed open-admission alternative is not the referent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_entailment_contestation, conceptual, 'Which reading of the border-control-legitimacy kernel governs; determines victim set and epsilon indexing.').

omega_variable(
    constructed_vs_civilizational_constant,
    'Is binding mobility restriction a constructed regime (a passport-era artifact roughly a century old) or a civilizational constant toward which every polity converges?',
    'Comparative-historical analysis of pre-1914 mobility norms, colonial pass systems, and the wartime emergency measures that generalized the passport and were never lifted; anthropological record of large unregulated migration zones.',
    'If artifact, the arrangement is contingent and dismantlable, supporting the snare reading; if constant, part of the measured burden is tragic-structural and this reading''s ambition narrows to channel-opening rather than regime abolition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constructed_vs_civilizational_constant, empirical, 'Whether the closure regime is a contingent construct or a recurring civilizational pattern.').

omega_variable(
    diffuse_insider_gain_vs_concentrated_capture,
    'How much of the arrangement''s benefit accrues diffusely to citizen insiders (wage and service insulation) versus concentrating in named seats (custody contractors, smuggling networks, deportable-labor employers, enforcement budgets)?',
    'Distributional accounting: enforcement appropriation flows, detention per-diems, smuggling fee volumes, and documented-versus-undocumented wage differentials.',
    'Concentrated capture strengthens the snare reading and predicts pro-enforcement lobbying by receipt seats; purely diffuse benefit would push toward a hybrid with a stronger coordination core.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diffuse_insider_gain_vs_concentrated_capture, empirical, 'Distribution of gains between diffuse insiders and concentrated capturers.').

omega_variable(
    origin_state_net_position,
    'Do origin-state governments net-benefit from the closure arrangement (remittances, aid-for-enforcement partnerships, offshored enforcement) or net-lose (skill drain, stranded transit populations, migrant mortality)?',
    'Remittance-flow accounting set against enforcement-cooperation disbursements and demographic drain data.',
    'Flips origin-state position between beneficiary and target and reshapes coalition geometry: net-beneficiary origin states stabilize the arrangement; net-losing ones join reform coalitions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(origin_state_net_position, empirical, 'Ambivalent structural position of origin-state governments.').

omega_variable(
    screening_function_separability,
    'Are identity-document verification and claims adjudication structurally separable from closure and exclusion, or does the documentation layer depend on the closure it currently rides on?',
    'Observe jurisdictions retaining registration and adjudication layers under open internal movement (intra-bloc free movement with retained registries), or decompose enforcement budgets between screening lines and interdiction lines.',
    'If separable, the measured burden is attributable to closure proper and this reading''s snare assessment sharpens; if inseparable, part of the burden is the price of residual coordination and the honest structural picture acquires a hybrid residue.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(screening_function_separability, conceptual, 'Whether coordination and exclusion components are structurally separable.').

omega_variable(
    deportability_internalization,
    'What share of the measured coercive pressure operates through internalized anticipation - self-limiting behavior in the absence of any enforcement contact - versus physical barriers, custody, and interdiction?',
    'Ethnographic labor-market studies combined with administrative-contact rates: if compliant behavior persists where contact probability is negligible, internalization carries the load.',
    'Internally carried pressure persists after barrier removal, raising effective suppression above the structural measure and slowing any post-relaxation normalization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deportability_internalization, empirical, 'Structural versus internalized share of the coercive mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_control_legitimacy__freedom_of_movement_primary, 0, 77).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 0, 0.15).
narrative_ontology:measurement(bord_tr_t11, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 11, 0.16).
narrative_ontology:measurement(bord_tr_t22, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 22, 0.18).
narrative_ontology:measurement(bord_tr_t33, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 33, 0.21).
narrative_ontology:measurement(bord_tr_t44, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 44, 0.24).
narrative_ontology:measurement(bord_tr_t55, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 55, 0.28).
narrative_ontology:measurement(bord_tr_t66, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 66, 0.31).
narrative_ontology:measurement(bord_tr_t77, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 77, 0.32).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(bord_be_t11, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 11, 0.58).
narrative_ontology:measurement(bord_be_t22, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 22, 0.61).
narrative_ontology:measurement(bord_be_t33, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 33, 0.64).
narrative_ontology:measurement(bord_be_t44, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 44, 0.67).
narrative_ontology:measurement(bord_be_t55, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 55, 0.74).
narrative_ontology:measurement(bord_be_t66, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 66, 0.79).
narrative_ontology:measurement(bord_be_t77, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 77, 0.84).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(bord_su_t11, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 11, 0.37).
narrative_ontology:measurement(bord_su_t22, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 22, 0.41).
narrative_ontology:measurement(bord_su_t33, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 33, 0.46).
narrative_ontology:measurement(bord_su_t44, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 44, 0.52).
narrative_ontology:measurement(bord_su_t55, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 55, 0.68).
narrative_ontology:measurement(bord_su_t66, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 66, 0.75).
narrative_ontology:measurement(bord_su_t77, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 77, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_control_legitimacy__freedom_of_movement_primary, identity_coordination).
narrative_ontology:affects_constraint(border_control_legitimacy__freedom_of_movement_primary, border_control_legitimacy__sovereignty_primary).
narrative_ontology:affects_constraint(border_control_legitimacy__freedom_of_movement_primary, border_control_legitimacy__jurisdictional_sovereignty).

% DUAL FORMULATION NOTE:
% 'Border control' as a colloquial label conflates three structurally distinct claims about one kernel: whether territorial sovereignty entails closure authority never (this file), absolutely (border_control_legitimacy__sovereignty_primary), or contingently-after-balancing (border_control_legitimacy__jurisdictional_sovereignty). The claims carry different epsilon (high / near-zero / intermediate), different victim sets (millions of movers / none / conditional), and different enforcement dependencies, so each is a separate story linked to the others via affects_constraints. Upstream-downstream: sovereignty_primary is the historically entrenched upstream claim, treated as self-evident and cited as ground; this reading is the downstream challenger whose litigation and treaty interpretation shift the sibling's operating environment. All three files link one another.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(border_control_legitimacy__freedom_of_movement_primary, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
