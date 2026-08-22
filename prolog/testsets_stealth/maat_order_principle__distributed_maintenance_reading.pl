% ============================================================================
% CONSTRAINT STORY: maat_order_principle__distributed_maintenance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maat_order_principle__distributed_maintenance_reading, []).

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
 *   constraint_id: maat_order_principle__distributed_maintenance_reading
 *   human_readable: Ma'at as Distributed Maintenance Obligation Across All Stations
 *   domain: ancient history/political philosophy/religious studies
 *
 * SUMMARY:
 *   This story instantiates one reading of the Ma'at kernel: that cosmic and
 *   social order are sustained by every actor's proper conduct in station,
 *   from Pharaoh to commoner, with authority grounded in demonstrated
 *   maintenance rather than inherent status. The standing arrangement under
 *   assessment is the Ma'at-maintenance regime of the Nile valley as this
 *   reading itself sees it — a flood-plain civilization's collective-action
 *   problems (flood response, grain storage and redistribution, dispute
 *   resolution, public-works labor) coordinated under a single normative
 *   frame in which no station is exempt from obligation and none stands
 *   outside order's benefit. The reading's distinctive structure against its
 *   siblings is universal accountability: the record shows kings failing
 *   maintenance and losing legitimacy for it (the First Intermediate Period
 *   collapse, the wisdom literature's laments, the Eloquent Peasant's
 *   successful complaint against a predatory official). The extractive edge
 *   is authored honestly rather than dissolved into the frame's
 *   self-presentation: corvée weight, grain tax, and the total prescription
 *   of the captive laborer's conduct are real transfers through the same
 *   structure that coordinates. The claimed type and the metrics are
 *   independent authored facts; where the engine's per-seat computation
 *   diverges from the aggregate claim — most likely at the captive-laborer
 *   seat — that divergence is the measurement this story exists to take.
 *
 * KEY AGENTS:
 *   - pharaoh_of_the_two_lands: chief maintainer and agenda-setter (institutional/identity_locked) — commands the maintenance apparatus, collects legitimacy and surplus, and bears the accountability burden that is this reading's defining feature
 *   - vizier_and_high_magistrates: administrative beneficiaries (powerful/constrained) — collect station rents and are bound by the just-judgment standard their own tomb inscriptions advertise
 *   - temple_priesthood: ritual beneficiaries (powerful/constrained) — maintain cosmic reciprocity through the cult economy and accumulate its offerings
 *   - scribal_officialdom: literate beneficiaries (moderate/constrained) — administer the arrangement and author its self-audit
 *   - peasant_laboring_communities: primary payers (powerless/constrained) — bear corvée and tax as station obligations and receive flood management, adjudication, and grain storage in return
 *   - necropolis_tomb_workmen: specialized payers (moderate/constrained) — royal-tomb crews with rations, housing, and enough collective capacity to halt work and negotiate
 *   - foreign_captive_laborers: prescribed payers (powerless/trapped) — bear the heaviest burdens with the least return; their conduct is set entirely by others
 *   - wisdom_literature_scribes: analytical observers (moderate/analytical) — articulate the standard every station is judged by, kings included
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maat_order_principle__distributed_maintenance_reading, 0.36).
domain_priors:suppression_score(maat_order_principle__distributed_maintenance_reading, 0.42).
domain_priors:theater_ratio(maat_order_principle__distributed_maintenance_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, extractiveness, 0.36).
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maat_order_principle__distributed_maintenance_reading, rope).
narrative_ontology:human_readable(maat_order_principle__distributed_maintenance_reading, "Ma'at as Distributed Maintenance Obligation Across All Stations").
narrative_ontology:topic_domain(maat_order_principle__distributed_maintenance_reading, "ancient history/political philosophy/religious studies").

domain_priors:requires_active_enforcement(maat_order_principle__distributed_maintenance_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(maat_order_principle__distributed_maintenance_reading, '648855a4-2f41-44e9-a6bb-2acd3dcba4f3').
narrative_ontology:cs_kernel_codification('648855a4-2f41-44e9-a6bb-2acd3dcba4f3', distributed).
narrative_ontology:cs_authority_grounding('648855a4-2f41-44e9-a6bb-2acd3dcba4f3', practice).
narrative_ontology:cs_interpretation_layer_present('648855a4-2f41-44e9-a6bb-2acd3dcba4f3').
narrative_ontology:cs_reading_relation('648855a4-2f41-44e9-a6bb-2acd3dcba4f3', maat_order_principle__divine_mandate_reading, influences).
narrative_ontology:cs_reading_relation('648855a4-2f41-44e9-a6bb-2acd3dcba4f3', maat_order_principle__reciprocity_reading, coexists_with).
narrative_ontology:cs_axiom('648855a4-2f41-44e9-a6bb-2acd3dcba4f3', foundational, all_stations_bound_to_maat_maintenance).
narrative_ontology:cs_axiom_status(all_stations_bound_to_maat_maintenance, holdable).
narrative_ontology:cs_axiom_grounding('648855a4-2f41-44e9-a6bb-2acd3dcba4f3', all_stations_bound_to_maat_maintenance, deontological).
narrative_ontology:cs_axiom('648855a4-2f41-44e9-a6bb-2acd3dcba4f3', foundational, authority_earned_by_demonstrated_maintenance).
narrative_ontology:cs_axiom_status(authority_earned_by_demonstrated_maintenance, holdable).
narrative_ontology:cs_axiom_grounding('648855a4-2f41-44e9-a6bb-2acd3dcba4f3', authority_earned_by_demonstrated_maintenance, instrumental).
narrative_ontology:cs_reference_frame('648855a4-2f41-44e9-a6bb-2acd3dcba4f3', distributed_station_accountability).
narrative_ontology:cs_drift_state('648855a4-2f41-44e9-a6bb-2acd3dcba4f3', ramesside_theological_concentration, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('648855a4-2f41-44e9-a6bb-2acd3dcba4f3', '').
narrative_ontology:cs_kernel_id(maat_order_principle__distributed_maintenance_reading, maat_order_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, pharaoh_of_the_two_lands).
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, vizier_and_high_magistrates).
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, temple_priesthood).
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, scribal_officialdom).
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, peasant_laboring_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(maat_order_principle__distributed_maintenance_reading, pharaoh_of_the_two_lands).
narrative_ontology:constraint_victim(maat_order_principle__distributed_maintenance_reading, vizier_and_high_magistrates).
narrative_ontology:constraint_victim(maat_order_principle__distributed_maintenance_reading, scribal_officialdom).
narrative_ontology:constraint_victim(maat_order_principle__distributed_maintenance_reading, peasant_laboring_communities).
narrative_ontology:constraint_victim(maat_order_principle__distributed_maintenance_reading, necropolis_tomb_workmen).
narrative_ontology:constraint_victim(maat_order_principle__distributed_maintenance_reading, foreign_captive_laborers).
narrative_ontology:constraint_vindicates(maat_order_principle__distributed_maintenance_reading, distributed_conduct_sustains_cosmic_order).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Leads the maintenance of order: commands the corvée, appoints the vizier, performs the rites that present the land as orderly, and sets the calendar of festivals and levies. Collects the surplus the arrangement concentrates at the royal treasuries and the legitimacy it grants conditional on conduct. The office and the person are fused — leaving the role is not an option — and the record shows kings whose maintenance failed losing the allegiance the arrangement confers.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, pharaoh_of_the_two_lands, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__distributed_maintenance_reading, pharaoh_of_the_two_lands, payer).

% Hears petitions, supervises the granaries and the courts, and advertises in tomb autobiography that he judged the strong and the weak alike. Draws status, land, and grain from his station; is bound by the just-judgment standard his own inscriptions proclaim, and can be removed when judged to have failed it.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, vizier_and_high_magistrates, beneficiary,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__distributed_maintenance_reading, vizier_and_high_magistrates, payer).

% Maintains the daily cult through which the gods are fed and the cosmos holds, administers temple estates, and interprets oracles that settle disputes and occasionally check royal decisions. Offerings and endowments flow through temple hands; the priestly station thickened considerably across the interval as endowments accumulated.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, temple_priesthood, beneficiary,
    powerful, generational, constrained, regional).

% Keeps the records, drafts the levies, and transmits the instruction literature that states the standard of proper conduct for every station. Draws rank and ration from the administration it serves; its literary seat also judges that administration, kings included.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, scribal_officialdom, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__distributed_maintenance_reading, scribal_officialdom, payer).

% Work the flood-plain fields, owe corvée service and grain tax to royal and temple granaries, and receive in return the flood management, stored grain, and village adjudication that keep the valley habitable. Their obligations are set above them; their recourse is petition, and the record shows petitions both granted and ignored.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, peasant_laboring_communities, payer,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__distributed_maintenance_reading, peasant_laboring_communities, beneficiary).

% Cut and decorate the royal tombs for ration wages in a state work village. Skilled and concentrated, they could halt the work and negotiate — the recorded strike over delayed rations ended in their favor — but leaving royal service meant losing the rations and housing their families lived on.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, necropolis_tomb_workmen, payer,
    moderate, biographical, constrained, local).

% Foreign captives settled into state and temple labor: the heaviest levies, the least return, conduct prescribed entirely by others. The arrangement's station language covers them — every actor has a station — but they had no part in setting what their station owed.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, foreign_captive_laborers, payer,
    powerless, biographical, trapped, local).

% Compose and transmit the teachings that state what proper conduct is for every station, from king to laborer, and that record what happens when stations fail. In their literary seat they hold no levy and collect no surplus; the standard they articulate is the one every other seat is measured by.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, wisdom_literature_scribes, observer,
    moderate, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(maat_order_principle__distributed_maintenance_reading, pharaoh_of_the_two_lands).
narrative_ontology:fixing_cost_class(maat_order_principle__distributed_maintenance_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the flood-plain civilization's collective-action problems — Nile flood response, grain storage and redistribution, dispute resolution, public-works labor mobilization — by assigning every actor a maintenance duty proper to their station, so that order is produced by universal conduct rather than by one actor's command.
% TRANSFER_FUNCTION: Moves corvée labor, grain tax, and offerings upward from laboring and productive stations to royal, administrative, and temple stations; moves adjudication, protection, ritual order, and redistribution downward. The frame codes the upward flow as each station's contribution to cosmic maintenance rather than as tribute.
% ABSENT_VOICES: The stations whose conduct was prescribed most completely — captive foreign laborers and the landless — had no seat where station obligations were set; their duties were authored by the scribal and priestly stations the frame also benefited. Village petitioners could reach officials case by case, and the record shows petitions granted, but the terms of the frame itself were set above them. Their objection, had they held a seat, would be that 'proper conduct in your station' prescribes obedience to arrangements they never agreed to.
% DISAPPEARANCE_RATIONALE: The material functions — flood response, storage, adjudication — would not vanish with the frame, but they would lose their authorization structure: who owes labor to whom, who judges, who interprets proper conduct would all be renegotiated. The specific distribution of burdens and the legitimacy of every station's authority are this arrangement's work; the world would rearrange around whatever replaced them.
% FOUNDING_PROBLEM: A flood-plain civilization whose survival requires coordinated response to the Nile's annual cycle and credible dispute resolution needed a legitimation structure that could command contribution from every stratum without permanent coercion — the distributed-maintenance frame was built to make order-maintenance everyone's station-bound obligation, binding elites by the same standard that bound laborers.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set by the wisdom literature itself — texts in the scribal station's own voice (the instruction attributed to Ptahhotep, the Eloquent Peasant's successful appeal, the Admonitions of Ipuwer) attest order's fragility and the cost of its failure; by the First Intermediate Period's documented order-collapse, which the frame's own tradition recorded as maintenance failure rather than denying; and by modern Egyptological analysis of the flood-plain coordination problem. No attestation comes from the laboring stations in their own voice — their stake is reconstructed from the obligations levied on them and from the strike record — which is itself signal about who authored the frame's terms.
narrative_ontology:disappearance_verdict(maat_order_principle__distributed_maintenance_reading, world_rearranges).
narrative_ontology:founding_problem_status(maat_order_principle__distributed_maintenance_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(maat_order_principle__distributed_maintenance_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(maat_order_principle__distributed_maintenance_reading, 'none', 1).
narrative_ontology:epsilon_provenance(maat_order_principle__distributed_maintenance_reading, 0.36, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maat_order_principle__distributed_maintenance_reading_tests).
:- end_tests(maat_order_principle__distributed_maintenance_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.36: above the coordination-cost floor because the station hierarchy moves real surplus upward and prescribes the lowest stations completely, but well below rent extraction because the reading's defining mechanism — universal accountability, elites included — caps what any station can collect without maintaining. Suppression is authored raw and unscaled at 0.42: the arrangement enforces through state machinery (levy, corvée muster) and through an internalized cosmic conscience (the negative confession's afterlife audit) while leaving genuine complaint channels — petition, oracle, the wisdom corpus's protest, the recorded strike — that the machinery tolerated. Only extraction is scaled by directionality and scope in the engine's computation; suppression enters as the structural property authored here. Theater ratio is 0.25: the ritual cycle is functionally load-bearing (redistribution, calendar, cohesion) with a performative surplus around royal display. Accessibility collapse is 0.50 — alternatives existed (flight, and heterodoxy attempted inside the frame's own concept in the Amarna episode) and the frame characteristically absorbed rather than suppressed them. Resistance is 0.40 — real and recorded, bounded in effect. The measurement series run on one shared grid (T0–24, seven points, all three metrics at every point): extraction, theater, and enforcement rise together through the imperial middle of the interval as royal and temple stations thicken and more extraction rides up the frame, then ease in the late period as the ethics democratize and station language universalizes downward. gain_flow names the pharaonic seat because the surplus above coordination cost lands at the royal treasuries, monuments, and grants (temple endowments were royal grants); the late-interval ascent of the temple economy is the drift that most threatens that receipt pattern and is visible in the series. fixing_cost is prohibitive: the seat that could alter the frame — the royal station — is itself constituted by it; dissolving the distributed-maintenance structure dissolves the legitimation of every station including its own. The identity_coordination declaration is made with the gaming risk in view: 'proper conduct in your station' is exactly the identity narrative that can cover extraction, and omega station_clause_extraction_edge is the standing check on it.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. From the pharaonic seat the frame is the constitution of legitimate rule that the occupant personally sustains — the accountability that defines this reading is a live occupational risk from that seat, and the fusion of office and person (identity-locked exit) means the risk cannot be exited, only managed. From the magisterial and priestly seats the frame authorizes station rents while demanding conduct — coordination with an attached bill. From the peasant seat the frame levies labor and grain while delivering the flood management, storage, and adjudication that keep the valley habitable: the benefit is real and the levy is not negotiable. From the captive laborer's seat the frame is prescription without return. The wisdom seat sees the standard itself and audits every other seat, kings included. Same-level lateral divergence: the necropolis workmen and the field peasants hold adjacent laboring stations at the same nominal standing, but skill scarcity and village concentration gave the workmen episodic collective leverage — their recorded strike over delayed rations succeeded — while the field peasantry's recourse was petition alone. Inter-institutionally, the royal and temple stations both collect through the frame while competing for its gains; the temple's endowment accumulation across the interval is the quiet transfer of the frame's receipts from one beneficiary station to another.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations (royal, magisterial, priestly, and scribal stations, plus the peasant communities whose order-benefit is material and existential) derive low directionality for those seats; the payer positions derive high directionality for the laboring seats. Two corrections matter. First, the pharaonic seat carries an explicit override (institutional, d 0.45): the derivation reads the beneficiaries array and would seat the king at the beneficiary extreme, but under this reading the king is also a payer — the accountability burden is the reading's defining feature, and the record of kings losing legitimacy on maintenance failure is the structural fact the derivation cannot see from the array alone. Second, the peasant communities are declared beneficiaries despite payer-primary roles because their benefit is not nominal — flood management and stored grain were existential — and that declaration is what separates their derived directionality from the captive laborers', who appear in no array, hold trapped exit, and derive nearest the full-target end. The captive seat is where this reading's universal-station claim is most strained: the frame prescribes their conduct completely and they had no part in setting it. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by directionality and scope, in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The rope claim protects the genuine coordination function from a snare misread: the frame really does solve flood-plain collective-action problems, really does deliver adjudication and storage, and really does bind elites — the feature that distinguishes this reading from the divine-mandate sibling, where no accountability exists and extraction is bounded by nothing but the ruler's own discretion. The honest extractiveness score does the opposite work: it prevents the frame's self-presentation (all contribute, all benefit) from erasing the captive-laborer seat, where the station clause prescribes without returning. Mandatrophy: the founding problem — coordinating order maintenance in a flood-plain economy without permanent coercion — stayed live across the whole interval, so the arrangement is neither a scaffold with a sunset nor a mandate outliving its function; it has not decayed toward piton because its function and enforcement remained load-bearing throughout. The atrophy risk this reading carries is interpretive rather than functional: if Ma'at's interpretation monopolizes in the royal and priestly stations (omega interpretive_pluralism_reality), the distributed frame's accountability mechanism goes theatrical while the station hierarchy persists — that is the rope-to-piton pathway, and the mid-interval rise in the theater_ratio series is its early signature. The late-interval easing, with the ethics universalizing downward into demotic wisdom, is the counter-pressure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This story instantiates only the distributed_maintenance_reading of the maat_order_principle kernel — how would the classification change under the sibling readings?',
    'Generate the sibling stories (maat_order_principle__divine_mandate_reading, maat_order_principle__reciprocity_reading) as separate constraints and compare computed types across the family; the kernel contest is resolved by structural comparison, not within this story.',
    'Under the divine_mandate_reading the royal station is exempt by definition: beneficiaries collapse toward the royal-priestly axis, the accountability surface disappears, and extraction rises toward snare territory. Under the reciprocity_reading obligations concentrate on the royal-societal axis and accountability becomes bilateral rather than universal. This story''s rope-level profile is specific to the distributed reading''s universal accountability and should not be generalized to the kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: this is one reading of the Ma''at kernel; siblings would restructure beneficiaries, accountability, and extraction.').

omega_variable(
    locus_of_kernel_disagreement,
    'Where exactly do the three readings disagree — is the contest over who is bound (universal versus royal exemption), how authority attaches (demonstrated maintenance versus inherent status), or who may interpret Ma''at (many versus one)?',
    'Structural comparison of the three family stories'' beneficiary sets, accountability surfaces, and directionality profiles: the axis that moves classification across the family is the disagreement''s true locus. Note that the historical framework held royal infallibility (ritual texts) and royal accountability (wisdom literature) simultaneously through the office/person distinction, so the readings coexist in one tradition rather than eliminating each other.',
    'If the locus is authority-attachment, this reading''s distinguishing axiom (authority earned by demonstrated maintenance) is the classification driver. If the locus is interpretive distribution, the reading''s low extraction depends on the interpretive layer being genuinely plural, and a monopolized interpretation would collapse this reading toward the divine-mandate shape.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(locus_of_kernel_disagreement, conceptual, 'Locates the structural element on which the kernel''s readings actually diverge.').

omega_variable(
    station_clause_extraction_edge,
    'Does ''proper conduct in their station'' function as pure coordination (each station contributes what order requires) or does it legitimate asymmetric extraction at the lowest stations — captive laborers and the landless — whose burdens were set without their participation?',
    'Compare burden-to-return ratios across stations in the documentary record (corvée rosters, ration lists, tax receipts, the Deir el-Medina strike record) against the frame''s own complaint channels: if the frame''s mechanisms repeatedly failed to relieve the lowest stations, the clause is extraction-legitimating for them.',
    'If extraction-legitimating at the bottom, the commoner and captive seats compute as extracted-from seats inside a rope-aggregate — the aggregate classification holds only because accountability binds the elite seats, and the reading''s rope character is seat-relative rather than global. If the clause coordinates all stations genuinely, the low aggregate extraction is attributable to the distributed accountability itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(station_clause_extraction_edge, empirical, 'Whether the station clause coordinates all stations or extracts from the lowest ones.').

omega_variable(
    internalized_cosmic_suppression,
    'Is the arrangement''s measured suppression structural (state machinery for tax and corvée) or internalized (the afterlife-judgment conscience — the negative confession — that enforces conduct without police)?',
    'Post-coercion conduct trajectories: where enforcement capacity weakened (First Intermediate Period local autonomy, late-period fragmentation), did conduct norms persist under the internalized mechanism alone? The moral vocabulary''s persistence through order-collapses suggests partial internalization.',
    'If largely internalized, the constraint''s effective suppression exceeds its structural enforcement record — exit from the normative frame was psychologically costlier than the institutional record shows, and trapped-seat computations understate the binding. If structural, suppression tracks enforcement capacity directly and the measured series is the whole story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_cosmic_suppression, empirical, 'Structural versus internalized suppression mechanism in the Ma''at frame.').

omega_variable(
    interpretive_pluralism_reality,
    'Was interpretation of Ma''at genuinely distributed — could any literate householder, village elder, or workman appeal to the standard — or did scribal and priestly stations monopolize it while the distributed frame was nominal?',
    'Documentary spread of Ma''at vocabulary and complaint-forms: if they appear in non-elite artifacts (workmen''s graffiti, village letters, stelae of minor officials) at rates comparable to elite texts, pluralism was real; elite-only usage indicates a monopolized standard beneath a pluralist veneer. This is also the framing check on this story''s declared kernel and authority: the less-obvious framing — a monopolized interpretive layer beneath a distributed legitimacy claim — would classify differently.',
    'If monopolized, the reading''s ''multiple legitimate interpreters'' structure is performance and the arrangement drifts toward the divine-mandate reading''s shape with distributed decoration; if genuinely plural, the accountability mechanism is real and the low extraction is attributable to it. The alternative framing would move authority_grounding from practice toward extraction and raise the theater profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_pluralism_reality, empirical, 'Whether Ma''at interpretation was genuinely plural or elite-monopolized beneath the distributed frame.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maat_order_principle__distributed_maintenance_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maat_tr_t0, maat_order_principle__distributed_maintenance_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(maat_tr_t4, maat_order_principle__distributed_maintenance_reading, theater_ratio, 4, 0.22).
narrative_ontology:measurement(maat_tr_t8, maat_order_principle__distributed_maintenance_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement(maat_tr_t12, maat_order_principle__distributed_maintenance_reading, theater_ratio, 12, 0.28).
narrative_ontology:measurement(maat_tr_t16, maat_order_principle__distributed_maintenance_reading, theater_ratio, 16, 0.3).
narrative_ontology:measurement(maat_tr_t20, maat_order_principle__distributed_maintenance_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement(maat_tr_t24, maat_order_principle__distributed_maintenance_reading, theater_ratio, 24, 0.25).

% Extraction over time
narrative_ontology:measurement(maat_be_t0, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(maat_be_t4, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 4, 0.33).
narrative_ontology:measurement(maat_be_t8, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 8, 0.36).
narrative_ontology:measurement(maat_be_t12, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 12, 0.39).
narrative_ontology:measurement(maat_be_t16, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 16, 0.42).
narrative_ontology:measurement(maat_be_t20, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 20, 0.39).
narrative_ontology:measurement(maat_be_t24, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 24, 0.36).

% Suppression requirement over time
narrative_ontology:measurement(maat_su_t0, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(maat_su_t4, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 4, 0.37).
narrative_ontology:measurement(maat_su_t8, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 8, 0.4).
narrative_ontology:measurement(maat_su_t12, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 12, 0.43).
narrative_ontology:measurement(maat_su_t16, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 16, 0.45).
narrative_ontology:measurement(maat_su_t20, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 20, 0.43).
narrative_ontology:measurement(maat_su_t24, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 24, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maat_order_principle__distributed_maintenance_reading, identity_coordination).
narrative_ontology:affects_constraint(maat_order_principle__distributed_maintenance_reading, maat_order_principle__divine_mandate_reading).
narrative_ontology:affects_constraint(maat_order_principle__distributed_maintenance_reading, maat_order_principle__reciprocity_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Ma'at' covers three structurally distinct arrangements: the distributed-maintenance arrangement (this story — universal station accountability, lowest extraction), the divine-mandate arrangement (ruler infallible by definition — extraction unbounded by accountability), and the reciprocity arrangement (bilateral royal-societal obligations). Each has its own epsilon, beneficiary structure, and classification; this story authors only the distributed reading. The kernel contest is carried by the omegas kernel_reading_commitment and locus_of_kernel_disagreement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(maat_order_principle__distributed_maintenance_reading, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
