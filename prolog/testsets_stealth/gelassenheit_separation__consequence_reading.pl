% ============================================================================
% CONSTRAINT STORY: gelassenheit_separation__consequence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gelassenheit_separation__consequence_reading, []).

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
 *   constraint_id: gelassenheit_separation__consequence_reading
 *   human_readable: Gelassenheit Separation — Consequence Reading (Practice-Preserving Technology Evaluation)
 *   domain: religious/technological/commitment-systems
 *
 * SUMMARY:
 *   Old Order Anabaptist technology governance, instantiated under the
 *   consequence reading of the gelassenheit_separation kernel: separation is
 *   defined as the preservation of community practices, and each technology
 *   is admitted or refused by its measured effect on visiting, mutual aid,
 *   and geographic rootedness. The arrangement produces famously
 *   fine-grained, context-sensitive rules — a telephone is refused in the
 *   kitchen (it erodes visiting) but permitted in a shanty at the lane's end
 *   (it preserves the farm's economic rootedness); a tractor may drive a belt
 *   in the barn but not pull in the field (horse-team field work is the
 *   occasion of the mutual-aid labor exchange); the automobile is refused but
 *   the hired driver is permitted. Assumptions stated: this story models the
 *   general Old Order pattern across districts and decades, not one specific
 *   affiliation; the enforcement layer (Meidung/shunning of leavers) is
 *   treated as a linked but distinct constraint with its own higher
 *   extraction, per the decomposition omega below. KEY AGENTS (by structural
 *   relationship): - district_ministry: Agenda-setter
 *   (institutional/identity_locked) — administers the Ordnung, rules on each
 *   technology case, leads discipline - old_order_households: Primary
 *   beneficiary and payer (organized/constrained) — receive the preserved
 *   practices, bear the foregone technologies - rumspringa_youth: Payer
 *   (powerless/mobile) — bear the option-foreclosure at the commitment point
 *   - amish_business_owners: Payer with secondary benefit
 *   (moderate/constrained) — bear negotiation friction, gain market access -
 *   elderly_and_infirm_members: Concentrated beneficiary (powerless/trapped)
 *   — most dependent on the mutual-aid web - former_members_under_shunning:
 *   Excluded voice (moderate/arbitrage) — bear the exit sanction, outside the
 *   council - anabaptist_scholars: Analytical observer
 *   (analytical/analytical) — see the comparative record across settlements
 *
 * KEY AGENTS:
 *   - district_ministry: agenda_setter (institutional/identity_locked) — rules on each technology case, leads discipline, bears enforcement labor
 *   - old_order_households: beneficiary + payer (organized/constrained) — receive preserved practices, pay foregone technologies
 *   - rumspringa_youth: payer (powerless/mobile) — bear option-foreclosure at the baptism decision
 *   - amish_business_owners: payer + secondary beneficiary (moderate/constrained) — pay negotiation friction, gain market access
 *   - elderly_and_infirm_members: beneficiary (powerless/trapped) — most dependent on the mutual-aid web the rulings preserve
 *   - former_members_under_shunning: excluded (moderate/arbitrage) — bear the exit sanction, no longer in the conversation
 *   - anabaptist_scholars: observer (analytical/analytical) — comparative view across settlements and decades
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gelassenheit_separation__consequence_reading, 0.15).
domain_priors:suppression_score(gelassenheit_separation__consequence_reading, 0.34).
domain_priors:theater_ratio(gelassenheit_separation__consequence_reading, 0.14).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, suppression_requirement, 0.34).
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, theater_ratio, 0.14).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gelassenheit_separation__consequence_reading, rope).
narrative_ontology:human_readable(gelassenheit_separation__consequence_reading, "Gelassenheit Separation — Consequence Reading (Practice-Preserving Technology Evaluation)").
narrative_ontology:topic_domain(gelassenheit_separation__consequence_reading, "religious/technological/commitment-systems").

domain_priors:requires_active_enforcement(gelassenheit_separation__consequence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gelassenheit_separation__consequence_reading, '723720df-1e20-45b5-bb8d-7b920cd6d9ce').
narrative_ontology:cs_kernel_codification('723720df-1e20-45b5-bb8d-7b920cd6d9ce', distributed).
narrative_ontology:cs_authority_grounding('723720df-1e20-45b5-bb8d-7b920cd6d9ce', practice).
narrative_ontology:cs_interpretation_layer_present('723720df-1e20-45b5-bb8d-7b920cd6d9ce').
narrative_ontology:cs_reading_relation('723720df-1e20-45b5-bb8d-7b920cd6d9ce', gelassenheit_separation__artifact_reading, coexists_with).
narrative_ontology:cs_reading_relation('723720df-1e20-45b5-bb8d-7b920cd6d9ce', gelassenheit_separation__principle_reading, influences).
narrative_ontology:cs_axiom('723720df-1e20-45b5-bb8d-7b920cd6d9ce', foundational, community_practices_constitute_separation).
narrative_ontology:cs_axiom_status(community_practices_constitute_separation, holdable).
narrative_ontology:cs_axiom_grounding('723720df-1e20-45b5-bb8d-7b920cd6d9ce', community_practices_constitute_separation, theological).
narrative_ontology:cs_axiom('723720df-1e20-45b5-bb8d-7b920cd6d9ce', foundational, technology_judged_by_practice_effects).
narrative_ontology:cs_axiom_status(technology_judged_by_practice_effects, holdable).
narrative_ontology:cs_axiom_grounding('723720df-1e20-45b5-bb8d-7b920cd6d9ce', technology_judged_by_practice_effects, instrumental).
narrative_ontology:cs_reference_frame('723720df-1e20-45b5-bb8d-7b920cd6d9ce', practice_preserving_covenant_community).
narrative_ontology:cs_drift_state('723720df-1e20-45b5-bb8d-7b920cd6d9ce', contemporary_digital_technology_surge, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('723720df-1e20-45b5-bb8d-7b920cd6d9ce', '').
narrative_ontology:cs_kernel_id(gelassenheit_separation__consequence_reading, gelassenheit_separation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gelassenheit_separation__consequence_reading, old_order_households).
narrative_ontology:constraint_beneficiary(gelassenheit_separation__consequence_reading, elderly_and_infirm_members).
narrative_ontology:constraint_beneficiary(gelassenheit_separation__consequence_reading, district_ministry).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gelassenheit_separation__consequence_reading, amish_business_owners).
narrative_ontology:constraint_victim(gelassenheit_separation__consequence_reading, old_order_households).
narrative_ontology:constraint_victim(gelassenheit_separation__consequence_reading, rumspringa_youth).
narrative_ontology:constraint_victim(gelassenheit_separation__consequence_reading, amish_business_owners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bishops and ministers of each district convene the semiannual Ordnung council, hear proposals and grievances about specific technologies, and announce rulings: a telephone shanty at the lane's end may stand while a kitchen extension line may not; a tractor may turn a belt in the barn but not pull a plow. They visit households, mediate disputes, and lead the confession process when a ruling is breached. Their standing rests on the community's continued assent; a ruling that loses the council can split the district, as the telephone and tractor controversies did historically. Stepping down from the office returns them to ordinary membership; leaving the community entirely would cost them family, livelihood, and the identity they were raised into.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, district_ministry, agenda_setter,
    institutional, generational, identity_locked, regional).

% Member families farm with horse teams, gather for barn raisings and silo fillings, and share a care rota for the sick. The rulings preserve the labor exchanges and visiting rounds their days are built around; the same rulings deny them household telephones, automobiles, and grid power, and require them to submit a proposed purchase to the council's judgment before bringing it home. A household that finds a ruling intolerable can appeal at council, move to a more permissive district, or leave — the last costing them table contact with baptized relatives under the shunning practice.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, old_order_households, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(gelassenheit_separation__consequence_reading, old_order_households, payer).

% Teenagers run cars, phones, and town jobs for a season before deciding on baptism. Whatever they taste in that window, choosing baptism means handing decision rights back: after commitment the household rulings bind them as adults. Some leave instead and do not return; those who stay carry the memory of foreclosed options into a lifetime under rulings they did not write and cannot individually revise.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, rumspringa_youth, payer,
    powerless, biographical, mobile, regional).

% Shopkeepers and dairy operators negotiate each expansion with the ministry: a phone shanty, a diesel generator, compressed-air tools, a catalog maintained by a nephew living off-farm. Each accommodation lets the business reach outside markets while keeping the shop floor inside the community's practices; each negotiation costs time, limits tooling, and leaves the owner dependent on ministerial goodwill for the next request. Closing the business or relocating to another district are the realistic exits; keeping the customers while dropping the community's rules is not on offer.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, amish_business_owners, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(gelassenheit_separation__consequence_reading, amish_business_owners, beneficiary).

% The oldest members live out their lives on the farmstead rather than in distant facilities because the mutual-aid web comes to them: meals on rotation, nursing shifts, neighbors finishing the chores after a stroke. They have little voice in technology rulings anymore and would lose the most if the labor exchanges thinned; their fallback, were the web to fail, is reliance on outside institutions they neither trust nor navigate easily.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, elderly_and_infirm_members, beneficiary,
    powerless, biographical, trapped, local).

% Those who joined another church or left outright are avoided by baptized relatives: no meals at the family table, no business dealings, guarded contact. They hold the sharpest objections to how far the rulings reach and what departure costs, but they no longer sit in the council where such things are weighed; their testimony reaches the community only through grandchildren's questions and the rare reconciliation.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, former_members_under_shunning, excluded,
    moderate, biographical, arbitrage, national).

% Historians and sociologists of Anabaptism reconstruct the deliberative record — ministers' conference minutes, district Ordnungs, schism genealogies — and compare rulings across uncoordinated settlements. They can see which rulings track practice-effects and which have hardened into mere custom, and they publish what they find without any standing in the councils they study.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, anabaptist_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gelassenheit_separation__consequence_reading, diffuse).
narrative_ontology:fixing_cost_class(gelassenheit_separation__consequence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the recurring collective-action problem of preserving interdependent community practices — visiting rounds, mutual-aid labor exchanges, geographic rootedness — against individually rational technology adoption that would unravel them, using fine-grained case-by-case rulings rather than blanket prohibitions.
% TRANSFER_FUNCTION: Moves foregone technological convenience and efficiency from individual households into preserved common goods (available neighbor labor, visiting time, local embeddedness), and moves decision authority over technology adoption from each household to the district council.
% ABSENT_VOICES: Former members under shunning and disciplined dissenters would object to enforcement severity and the breadth of ministerial discretion, but they no longer sit in the council; unbaptized youth have voice only before commitment. Outside the community, technology vendors and market actors have no standing at all.
% DISAPPEARANCE_RATIONALE: If the evaluation rule vanished overnight, technology adoption would proceed household by household; mechanized farms would need fewer neighbors, the labor exchanges would thin, visiting would decline, and within a generation the settlements would converge toward their assimilated cousins — the mutual-aid economy, and the demographic retention that staffs it, would unravel.
% FOUNDING_PROBLEM: How a covenant community committed to Gelassenheit — yieldedness to God and to one another — can remain 'in the world but not of it' when each new industrial technology is individually advantageous yet collectively dissolves the visiting rounds, labor exchanges, and place-rootedness that constitute the community.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: Anabaptist historiography built on ministers' conference minutes from 1878 onward (Hostetler, Kraybill, Nolt); the Supreme Court record in Wisconsin v. Yoder (1972), where the practice-preservation account was presented and tested under adversarial conditions; and the independent recurrence of near-identical rulings across uncoordinated settlements, which is difficult to explain as elite manufacture. No corroborating source attests that the founding problem is dead.
narrative_ontology:disappearance_verdict(gelassenheit_separation__consequence_reading, world_rearranges).
narrative_ontology:founding_problem_status(gelassenheit_separation__consequence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gelassenheit_separation__consequence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gelassenheit_separation__consequence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gelassenheit_separation__consequence_reading, 0.15, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gelassenheit_separation__consequence_reading_tests).
:- end_tests(gelassenheit_separation__consequence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is low (0.15 at interval end) because the costs are foregone conveniences borne by consenting participants, no seat collects material rent, and the fine-grained case law keeps each refusal targeted (kitchen phone refused, barn shanty permitted) rather than function-blind. Suppression (0.34) is real but bounded: confession discipline and the shunning of leavers are genuine sanctions, yet exit exists, internal appeal exists, and the suppressive force needed to hold the rules has declined steadily as negotiated-accommodation mechanisms matured — hence the falling suppression_requirement series. Theater is low (0.14): the rulings do visible work every season; only the annual reaffirmation ritual carries a partly ceremonial share, which creeps up slowly. Accessibility collapse is moderate (0.40): understanding the rule does not close alternatives — the sibling readings remain live positions held by neighboring affiliations, districts differ, and exceptions are negotiable. Resistance is moderate (0.48): episodic but real — rumspringa friction, the historic telephone and tractor schisms, and continuous case-level pushback from business owners. Claim/metric independence: the claimed type (rope) is authored from structural belief — overlapping beneficiary/payer sets, no rent collector, schism as release valve — while the metrics are authored independently from the descriptive record; if the engine computes a harder type for the youth or business-owner seats, that divergence is the measurement the corpus exists to take. All three tracked series share one eight-point time grid; the trajectories are monotone (no cyclical pattern), so no intermittent-reinforcement reading applies.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the ministry's position the arrangement is the community's own accumulated wisdom operating as intended; from the elderly members' position it is near-pure provision (everything flows in, little is asked); from the youth's position it is a wall of foreclosed options encountered at the moment of maximum temptation; from the business owners' position it is a standing tax of negotiation paid for market access; from the former members' position it is the sharp edge of the same structure. Same rulings, four different lived realities — the engine derives this per-seat divergence from power and exit atoms, not from the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Households are declared beneficiaries with constrained exit: costs and benefits land on the same families, placing them near the symmetric midpoint. Elderly and infirm members are pure beneficiaries with no exit — nearest the subsidized end. The ministry is declared a beneficiary but also bears enforcement labor and conflict mediation, tempering its derived subsidy. Youth are payers with (pre-commitment) mobility — pushed toward the target end, though their exit option dampens effective extraction relative to trapped payers. Business owners straddle payer and beneficiary — mid-range. Former members are excluded rather than coordinated; their situation feeds the absent-voices record, not the directionality arithmetic. Regional scope keeps verification cheap at district scale, damping the scope amplification — consistent with the low effective extraction this structure should compute.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live: every technology wave renews it, and the case-by-case machinery exists precisely because the problem never closes. No sunset clause, no mandatrophy declaration. The classification guards against two opposite errors. Reading the arrangement as pure extraction (snare) would erase the genuine mutual-aid coordination, the consent structure ratified twice yearly by the baptized membership, and the fact that the heaviest beneficiaries are the least powerful members. Reading it as costless harmony (pure rope with zero suppression) would erase the real burdens carried by youth, dissenters, and leavers — burdens this story attributes chiefly to the enforcement layer and routes to the decomposition omega and the linked meidung story rather than inflating this file's epsilon.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the gelassenheit_separation kernel; would the same technology portfolio (kitchen phone, barn shanty, field tractor, belt tractor, automobile, hired driver) classify differently under artifact_reading or principle_reading?',
    'Cross-reading audit: author the sibling stories and compare per-object rulings and epsilon on identical objects; divergent verdicts on the same objects locate the disagreement in the evaluation criterion rather than in the objects.',
    'Under artifact_reading epsilon rises (function-blind bans refuse harmless tools); under principle_reading the cost structure shifts toward entanglement-bearing agents and more technology is admitted. Only the consequence reading yields the low-epsilon fine-grained profile authored here; the three stories must be held side by side, never averaged.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed classification: same objects, three criteria, three distinct constraints.').

omega_variable(
    suppression_internalization_ambiguity,
    'Is the suppression that holds the rulings in place structural (council jurisdiction, shunning sanctions, livelihood coupling) or internalized (members raised from birth within the practice who cannot conceive of choosing otherwise)?',
    'Post-exit trajectory studies of leavers: if rule-obedience habits and foreclosed-option experience persist after exit with no sanction within reach, a substantial share of the measured suppression is internalized.',
    'If largely internalized, effective suppression exceeds the structural 0.34 and the youth seat''s computed classification hardens; if largely structural, the negotiated-accommodation trend should continue driving the suppression_requirement series downward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_ambiguity, empirical, 'Structural versus internalized suppression mechanism in a birth-recruited community.').

omega_variable(
    observer_epsilon_relativity,
    'Epsilon 0.15 assesses the standing arrangement by the consequence reading''s own lights; how would an autonomy-indexed observer (a liberal-individualist accounting of foreclosed options) value the identical arrangement?',
    'Author a parallel story with the autonomy-indexed observable over the same referent arrangement; per epsilon-invariance a different resulting value indicates a different constraint story, not a measurement error.',
    'An autonomy-indexed epsilon would plausibly land well above 0.4, moving computed seat classifications toward the hybrid category for the payer seats; the corpus should hold both stories as distinct rather than reconcile them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(observer_epsilon_relativity, preference, 'Value-relative epsilon over a fixed referent arrangement.').

omega_variable(
    meidung_decomposition_boundary,
    'Is the shunning (Meidung) enforcement separable from the technology-evaluation rule as a distinct constraint with its own, higher epsilon?',
    'Author meidung_shunning_enforcement as its own story; test whether this file''s epsilon stays low when enforcement extraction is attributed separately, and whether the evaluation rule would survive formally under a hypothetically sanction-free regime.',
    'If separable, this story''s low epsilon stands and the enforcement story carries the extraction; if inseparable, this epsilon understates the arrangement''s total extraction and the claimed rope should be re-read as a hybrid coordination/extraction structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meidung_decomposition_boundary, conceptual, 'Decomposition boundary between the evaluation rule and its sanction layer.').

omega_variable(
    gendered_cost_distribution,
    'Do the rulings distribute their costs evenly across genders, or do home-sited refusals (kitchen phone, household appliances, car denial) fall disproportionately on the members whose daily work is home-sited?',
    'Ethnographic time-use and workload comparison across districts before and after specific adoptions, disaggregated by gender and task site.',
    'A concentrated home-site burden would differentiate a payer seat that the current household-level aggregation flattens, supporting a hybrid computation for that seat despite the aggregate profile.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(gendered_cost_distribution, empirical, 'Whether household-level aggregation masks a gendered payer seat.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gelassenheit_separation__consequence_reading, 1920, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gelassenheit_sep_consequence_tr_t1920, gelassenheit_separation__consequence_reading, theater_ratio, 1920, 0.08).
narrative_ontology:measurement_basis(gelassenheit_sep_consequence_tr_t1920, observed).
narrative_ontology:measurement(gelassenheit_sep_consequence_tr_t1935, gelassenheit_separation__consequence_reading, theater_ratio, 1935, 0.09).
narrative_ontology:measurement_basis(gelassenheit_sep_consequence_tr_t1935, observed).
narrative_ontology:measurement(gelassenheit_sep_consequence_tr_t1950, gelassenheit_separation__consequence_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement_basis(gelassenheit_sep_consequence_tr_t1950, observed).
narrative_ontology:measurement(gelassenheit_sep_consequence_tr_t1965, gelassenheit_separation__consequence_reading, theater_ratio, 1965, 0.1).
narrative_ontology:measurement_basis(gelassenheit_sep_consequence_tr_t1965, observed).
narrative_ontology:measurement(gelassenheit_sep_consequence_tr_t1980, gelassenheit_separation__consequence_reading, theater_ratio, 1980, 0.11).
narrative_ontology:measurement_basis(gelassenheit_sep_consequence_tr_t1980, observed).
narrative_ontology:measurement(gelassenheit_sep_consequence_tr_t1995, gelassenheit_separation__consequence_reading, theater_ratio, 1995, 0.12).
narrative_ontology:measurement_basis(gelassenheit_sep_consequence_tr_t1995, observed).
narrative_ontology:measurement(gelassenheit_sep_consequence_tr_t2010, gelassenheit_separation__consequence_reading, theater_ratio, 2010, 0.13).
narrative_ontology:measurement_basis(gelassenheit_sep_consequence_tr_t2010, observed).
narrative_ontology:measurement(gelassenheit_sep_consequence_tr_t2020, gelassenheit_separation__consequence_reading, theater_ratio, 2020, 0.14).
narrative_ontology:measurement_basis(gelassenheit_sep_consequence_tr_t2020, observed).

% Extraction over time
narrative_ontology:measurement(gelassenheit_sep_consequence_be_t1920, gelassenheit_separation__consequence_reading, base_extractiveness, 1920, 0.26).
narrative_ontology:measurement_basis(gelassenheit_sep_consequence_be_t1920, observed).
narrative_ontology:measurement(gelassenheit_sep_consequence_be_t1935, gelassenheit_separation__consequence_reading, base_extractiveness, 1935, 0.23).
narrative_ontology:measurement_basis(gelassenheit_sep_consequence_be_t1935, observed).
narrative_ontology:measurement(gelassenheit_sep_consequence_be_t1950, gelassenheit_separation__consequence_reading, base_extractiveness, 1950, 0.2).
narrative_ontology:measurement_basis(gelassenheit_sep_consequence_be_t1950, observed).
narrative_ontology:measurement(gelassenheit_sep_consequence_be_t1965, gelassenheit_separation__consequence_reading, base_extractiveness, 1965, 0.18).
narrative_ontology:measurement_basis(gelassenheit_sep_consequence_be_t1965, observed).
narrative_ontology:measurement(gelassenheit_sep_consequence_be_t1980, gelassenheit_separation__consequence_reading, base_extractiveness, 1980, 0.17).
narrative_ontology:measurement_basis(gelassenheit_sep_consequence_be_t1980, observed).
narrative_ontology:measurement(gelassenheit_sep_consequence_be_t1995, gelassenheit_separation__consequence_reading, base_extractiveness, 1995, 0.16).
narrative_ontology:measurement_basis(gelassenheit_sep_consequence_be_t1995, observed).
narrative_ontology:measurement(gelassenheit_sep_consequence_be_t2010, gelassenheit_separation__consequence_reading, base_extractiveness, 2010, 0.16).
narrative_ontology:measurement_basis(gelassenheit_sep_consequence_be_t2010, observed).
narrative_ontology:measurement(gelassenheit_sep_consequence_be_t2020, gelassenheit_separation__consequence_reading, base_extractiveness, 2020, 0.15).
narrative_ontology:measurement_basis(gelassenheit_sep_consequence_be_t2020, observed).

% Suppression requirement over time
narrative_ontology:measurement(gelassenheit_sep_consequence_su_t1920, gelassenheit_separation__consequence_reading, suppression_requirement, 1920, 0.58).
narrative_ontology:measurement_basis(gelassenheit_sep_consequence_su_t1920, observed).
narrative_ontology:measurement(gelassenheit_sep_consequence_su_t1935, gelassenheit_separation__consequence_reading, suppression_requirement, 1935, 0.54).
narrative_ontology:measurement_basis(gelassenheit_sep_consequence_su_t1935, observed).
narrative_ontology:measurement(gelassenheit_sep_consequence_su_t1950, gelassenheit_separation__consequence_reading, suppression_requirement, 1950, 0.5).
narrative_ontology:measurement_basis(gelassenheit_sep_consequence_su_t1950, observed).
narrative_ontology:measurement(gelassenheit_sep_consequence_su_t1965, gelassenheit_separation__consequence_reading, suppression_requirement, 1965, 0.46).
narrative_ontology:measurement_basis(gelassenheit_sep_consequence_su_t1965, observed).
narrative_ontology:measurement(gelassenheit_sep_consequence_su_t1980, gelassenheit_separation__consequence_reading, suppression_requirement, 1980, 0.42).
narrative_ontology:measurement_basis(gelassenheit_sep_consequence_su_t1980, observed).
narrative_ontology:measurement(gelassenheit_sep_consequence_su_t1995, gelassenheit_separation__consequence_reading, suppression_requirement, 1995, 0.39).
narrative_ontology:measurement_basis(gelassenheit_sep_consequence_su_t1995, observed).
narrative_ontology:measurement(gelassenheit_sep_consequence_su_t2010, gelassenheit_separation__consequence_reading, suppression_requirement, 2010, 0.36).
narrative_ontology:measurement_basis(gelassenheit_sep_consequence_su_t2010, observed).
narrative_ontology:measurement(gelassenheit_sep_consequence_su_t2020, gelassenheit_separation__consequence_reading, suppression_requirement, 2020, 0.34).
narrative_ontology:measurement_basis(gelassenheit_sep_consequence_su_t2020, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gelassenheit_separation__consequence_reading, attachment_coordination).
narrative_ontology:affects_constraint(gelassenheit_separation__consequence_reading, gelassenheit_separation__artifact_reading).
narrative_ontology:affects_constraint(gelassenheit_separation__consequence_reading, gelassenheit_separation__principle_reading).
narrative_ontology:affects_constraint(gelassenheit_separation__consequence_reading, meidung_shunning_enforcement).

% DUAL FORMULATION NOTE:
% The colloquial label 'Amish separation from the world' decomposes into three structurally distinct evaluation criteria with materially different epsilon profiles: artifact_reading bans by resemblance (highest epsilon — function-blind refusals), consequence_reading (this file) evaluates by practice-effect (lowest epsilon — targeted refusals), and principle_reading evaluates by entanglement (intermediate — admits more technology under isolation conditions). Upstream/downstream structure: the consequence reading's accumulated case law supplies factual predicates both siblings cite — artifact-leaning communities cite its refusals, principle-leaning communities cite its permissions — hence the influences edge toward principle_reading. The enforcement layer (Meidung/shunning) is decomposed into meidung_shunning_enforcement with its own higher epsilon; this file's epsilon covers the evaluation rule only.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
