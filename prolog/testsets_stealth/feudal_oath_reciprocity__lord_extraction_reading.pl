% ============================================================================
% CONSTRAINT STORY: feudal_oath_reciprocity__lord_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feudal_oath_reciprocity__lord_extraction_reading, []).

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
 *   constraint_id: feudal_oath_reciprocity__lord_extraction_reading
 *   human_readable: Feudal Oath as Authorization for Capacity-Bounded Maximal Extraction (Lord-Extraction Reading)
 *   domain: economic/political/legal-historical
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel 'feudal oath
 *   reciprocity': the lord-extraction reading, on which the act of homage
 *   conveys an open-ended claim to the vassal's service and, symmetrically,
 *   authorizes the lord to demand up to the full measure of what the vassal
 *   can bear — the only operative bound being the threshold at which the
 *   vassal rebels, flees, or dies. On this reading the
 *   protection-and-fidelity story is the cover under which the levy apparatus
 *   runs; written custom records past liberality but does not cap future
 *   demand. Per the epsilon-referent rule for kernel readings, epsilon (0.80)
 *   is authored for the STANDING arrangement under contest — the
 *   oath-governed feudo-vassalic order as this reading sees it — assessed by
 *   this reading's own lights; it is NOT the epsilon of the arrangement this
 *   reading would prefer, and it is NOT averaged with the sibling readings,
 *   which are separate constraints in separate files. Claim and metrics are
 *   independent authored facts: the claimed type (snare) states what this
 *   reading holds to be structurally true; the metric series state what the
 *   historical record descriptively shows; the engine computes per-seat
 *   classifications from the structural data and measures any divergence.
 *
 * KEY AGENTS:
 *   - - manorial_lords: agenda-setting beneficiary (powerful/mobile) — sets dues, collects surplus, commands enforcement
 *   - - castellan_officers: administering beneficiary (organized/constrained) — runs collection and the manorial court, takes a cut
 *   - - serf_tenantry: primary target (powerless/trapped) — bears labor dues, banalities, tallages; bound to soil
 *   - - lesser_vassals: dual-positioned target-beneficiary (organized/constrained) — squeezed from above, squeezing below
 *   - - peasant_sworn_communes: excluded coalition voice (organized/trapped) — petitions, fixes custom, revolts
 *   - - royal_courts: analytical observer (institutional/analytical) — converts open-ended demand into recorded custom
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feudal_oath_reciprocity__lord_extraction_reading, 0.8).
domain_priors:suppression_score(feudal_oath_reciprocity__lord_extraction_reading, 0.55).
domain_priors:theater_ratio(feudal_oath_reciprocity__lord_extraction_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feudal_oath_reciprocity__lord_extraction_reading, snare).
narrative_ontology:human_readable(feudal_oath_reciprocity__lord_extraction_reading, "Feudal Oath as Authorization for Capacity-Bounded Maximal Extraction (Lord-Extraction Reading)").
narrative_ontology:topic_domain(feudal_oath_reciprocity__lord_extraction_reading, "economic/political/legal-historical").

domain_priors:requires_active_enforcement(feudal_oath_reciprocity__lord_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feudal_oath_reciprocity__lord_extraction_reading, '12834911-6d51-4f99-bb12-554c825e97e4').
narrative_ontology:cs_kernel_codification('12834911-6d51-4f99-bb12-554c825e97e4', distributed).
narrative_ontology:cs_authority_grounding('12834911-6d51-4f99-bb12-554c825e97e4', extraction).
narrative_ontology:cs_interpretation_layer_present('12834911-6d51-4f99-bb12-554c825e97e4').
narrative_ontology:cs_reading_relation('12834911-6d51-4f99-bb12-554c825e97e4', feudal_oath_reciprocity__vassal_coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('12834911-6d51-4f99-bb12-554c825e97e4', feudal_oath_reciprocity__ecclesiastical_mediation_reading, forecloses).
narrative_ontology:cs_axiom('12834911-6d51-4f99-bb12-554c825e97e4', foundational, demand_bounded_only_by_vassal_capacity).
narrative_ontology:cs_axiom_status(demand_bounded_only_by_vassal_capacity, holdable).
narrative_ontology:cs_axiom_grounding('12834911-6d51-4f99-bb12-554c825e97e4', demand_bounded_only_by_vassal_capacity, conventional).
narrative_ontology:cs_axiom('12834911-6d51-4f99-bb12-554c825e97e4', secondary, written_custom_records_but_does_not_bind).
narrative_ontology:cs_axiom_status(written_custom_records_but_does_not_bind, holdable).
narrative_ontology:cs_axiom_grounding('12834911-6d51-4f99-bb12-554c825e97e4', written_custom_records_but_does_not_bind, conventional).
narrative_ontology:cs_reference_frame('12834911-6d51-4f99-bb12-554c825e97e4', unilateral_homage_service_bond).
narrative_ontology:cs_drift_state('12834911-6d51-4f99-bb12-554c825e97e4', high_medieval_charter_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('12834911-6d51-4f99-bb12-554c825e97e4', '').
narrative_ontology:cs_kernel_id(feudal_oath_reciprocity__lord_extraction_reading, feudal_oath_reciprocity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__lord_extraction_reading, manorial_lords).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__lord_extraction_reading, castellan_officers).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__lord_extraction_reading, serf_tenantry).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__lord_extraction_reading, lesser_vassals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__lord_extraction_reading, lesser_vassals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold fiefs worked by dependent tenants; convene homage, receive fealty, and set the labor days, payments in kind, banalities, and court fines their tenants owe. Collect the surplus directly and command the armed retinues that make collection stick. Their alternatives are comparatively good: commute services to cash rents, sell or mortgage holdings, marry upward, or place sons in church and court careers.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, manorial_lords, agenda_setter,
    powerful, generational, mobile, continental).

% Garrison and administer the lord's castles, run day-to-day collection of dues, and preside over the manorial court. Take a share of receipts and the perquisites of office. Their standing depends on the lord's favor; they can move to another lord's service but carry household ties, local enemies, and honor obligations that make movement costly.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, castellan_officers, agenda_setter,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(feudal_oath_reciprocity__lord_extraction_reading, castellan_officers, beneficiary).

% Work the lord's demesne days, pay for the use of mill, oven, and press, and owe tallages whenever the lord levies them. Bound to the soil by birth and by the lord's right of pursuit. Flight to a chartered town risks forfeiture of goods and separation from family, though a year and a day behind town walls can extinguish the claim — an exit that exists but is priced in everything the fugitive owns and loves.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, serf_tenantry, payer,
    powerless, generational, trapped, local).

% Hold smaller fiefs under greater lords; owe mounted service, castle guard, and court attendance, and pass a share of their own tenants' dues upward. Receive protection, land, and standing in return. They may renounce homage and transfer to another lord, at the cost of forfeiting the fief and their honor-price, and they are squeezed from above by the same open-ended demands they impose below.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, lesser_vassals, payer,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(feudal_oath_reciprocity__lord_extraction_reading, lesser_vassals, beneficiary).

% Village and parish associations formed by mutual oath among the tenants themselves. They petition for written customs, fix dues in charters, and coordinate collective refusal and revolt when levies exceed living memory. They sit outside the homage ceremony that disposes of their labor and are summoned to it only as witnesses.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, peasant_sworn_communes, excluded,
    organized, generational, trapped, regional).

% Kings' justices and chancery clerks hearing disputes between lords and tenants, registering charters, and defining what custom the oath obliges. They gain jurisdiction and revenue from adjudicating the arrangement and, across generations, convert open-ended demands into recorded, enforceable terms.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, royal_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(feudal_oath_reciprocity__lord_extraction_reading, manorial_lords).
narrative_ontology:fixing_cost_class(feudal_oath_reciprocity__lord_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In a landscape stripped of public defense after the Carolingian collapse, the oath network mobilized mounted military service and garrison duty: it matched armed protectors with land capable of supporting them and with populations that needed defense against raiders, and it gave disputes a venue in the lord's court.
% TRANSFER_FUNCTION: Moves agricultural surplus — labor services, payments in kind, milling and baking dues, arbitrary tallages — and military service upward from peasant tenants and lesser vassals to lords; moves protection, land tenure, and adjudication downward.
% ABSENT_VOICES: The tenants whose labor funded the arrangement were not party to the oaths that disposed of them; peasant sworn communes and village spokesmen objected from outside the ceremony, first in petitions for written custom, later in revolt. Royal clerks recording charters heard them only when disorder forced the issue.
% DISAPPEARANCE_RATIONALE: If the oath-governed order vanished overnight, defense mobilization, landholding, inheritance, and dispute resolution would all have to be rebuilt around some other authority — paid soldiery, royal bailiffs, chartered towns — because in this period nothing else yet performs those functions at scale.
% FOUNDING_PROBLEM: Post-Carolingian state collapse: no public authority could provide defense against Viking, Magyar, and Saracen raiding or adjudicate disputes between armed men, so protection capacity had to be assembled privately through land granted for sworn service.
% FOUNDING_PROBLEM_CORROBORATION: Lords and their chroniclers attest the defensive problem as still live, citing raid and war. Outside the benefiting parties, royal chancery records, canonist commentary, and commune charter petitions attest that by the later interval royal justice, walled towns, and paid troops had taken over the protective function, leaving the levy apparatus running on its own momentum — the obsolescence reading is corroborated from the bench, the cloister, and the commune, not from the castle.
narrative_ontology:disappearance_verdict(feudal_oath_reciprocity__lord_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(feudal_oath_reciprocity__lord_extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feudal_oath_reciprocity__lord_extraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(feudal_oath_reciprocity__lord_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feudal_oath_reciprocity__lord_extraction_reading, 0.8, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feudal_oath_reciprocity__lord_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(feudal_oath_reciprocity__lord_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(feudal_oath_reciprocity__lord_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high and rising across most of the interval (0.56 to a peak of 0.84 around 1250) because the reading's premise removes every bound except capacity: banalities, tallages, and court fines expand wherever the lord's coercive reach allows, and the record of 'evil customs' shows levies invented rather than inherited. Suppression_requirement traces an inverted U: it climbs from 0.44 to 0.79 as the castle revolution and private war mature (the enforcement machinery is literally built in stone), then falls to 0.55 by 1350 as royal courts, chartered towns, and commutation to cash rent erode the lord's private enforcement capacity — this is a genuine enforcement-capacity arc, which is why suppression_requirement is tracked at all rather than left as the static scalar. Theater_ratio rises steadily from 0.08 to 0.42: the homage ceremony and the protection justification persist and grow more ornate even as the protective function migrates to walls and wages, tracking the risk that the arrangement decays toward inert performance if the levy function also drains away. Accessibility_collapse is 0.60, not higher, because real alternatives existed — flight to chartered towns under the year-and-a-day rule, church careers for younger sons, commuted rents — but each was priced in forfeiture, family, or status. Resistance is 0.65: revolt, flight, charter agitation, and communal oath-making against the lord's oath are constant features of the record, and the coalition path mattered — the peasant sworn communes are precisely the coalition formation by which individually powerless tenants gained leverage, which is why they are seated as excluded voices rather than ignored. Compliance decomposes roughly into two-thirds structural coercion (pursuit, forfeiture, castle, manorial court) and one-third internalized acceptance (the three-orders doctrine taught by the very clergy the arrangement endowed), with the uncertainty carried in the suppression_internalization_share omega. All three metric series run on one shared time grid (850/950/1050/1150/1250/1350) and their endpoint values match the base_properties scalars.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and should. From the manorial_lords seat the arrangement looks like a coordination achievement they built and personally guarantee: protection delivered, disputes settled, order kept — a rope-shaped world with themselves as its indispensable center. From the serf_tenantry seat the same structure is experienced as unbounded levy backed by pursuit and forfeiture — the snare shape. The lesser_vassals seat splits down the middle: subsidized from above (land, protection, standing) and squeezed by the same open-ended logic they apply to their own tenants. The castellan_officers seat experiences the arrangement as career and income, with enforcement duties it did not design. The royal_courts seat sees the whole surface and monetizes adjudicating it. The engine derives these divergent classifications from power, exit, and directional position; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation: manorial_lords and castellan_officers sit at the beneficiary end (low d, extraction damped or inverted into subsidy for them); serf_tenantry sits nearest the full-target end (high d amplified by trapped exit and local scope, where verification of 'customary' dues is weakest); lesser_vassals derive a high d from their victim-listing, but their genuine receipt of fief, protection, and standing makes pure target-status inaccurate, so their effective position is mixed — the dual payer/beneficiary role declaration encodes this without an override, since the structural data (victim listing plus secondary beneficiary role plus constrained-but-real homage-transfer exit) already yields a mid-to-high d. No directionality overrides are authored: the derivation chain produces the right relationships from the declared structure, and the one candidate case (lesser_vassals) is handled by the dual-role declaration rather than a numeric patch.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification discipline prevents two opposite mislabelings. Reading the arrangement as pure coordination (the lords' own account) would erase the victims the record shows; reading it as coordination-free predation would erase the real protective function that made tenants accept homage in the first place and that still partially operates at interval end. The snare claim keeps both facts ordered correctly: a genuine coordination shell with the extraction running through it, sustained by active enforcement. The mandatrophy question — has the founding problem outlived the arrangement — is answered 'contested' rather than resolved, because the protective function was genuinely migrating to royal and municipal substitutes during the interval without having fully departed; the rising theater_ratio is the leading indicator that would mark the transition to inertial persistence if the levy function drained next. The R5 mismatch consumer should note the deliberate tension: founding_problem_status 'contested' paired with disappearance_verdict 'world_rearranges' flags exactly the zombie-risk zone this story occupies at 1350.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of the kernel feudal_oath_reciprocity — the lord_extraction_reading. Which kernel and reading does this file instantiate, and what do the sibling readings (vassal_coordination_reading, ecclesiastical_mediation_reading) hold?',
    'Read the three sibling files together; each authors its own epsilon, beneficiary/victim structure, and classification for the same historical arrangement under a different premise about the oath''s binding content.',
    'Classifications are reading-indexed: this file''s high-epsilon snare verdict is a verdict on the lord-extraction reading''s constraint, not on ''the feudal oath'' simpliciter; cross-reading comparison requires the family, not any single file.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame position: one of three readings of the feudal-oath kernel, with the disagreement located in what fixes the oath''s binding content.').

omega_variable(
    sibling_structural_delta,
    'What would adopting a sibling reading change structurally? Under vassal_coordination_reading, the victim set shrinks to charter-breach cases and epsilon falls toward coordination-cost levels; under ecclesiastical_mediation_reading, extraction acquires a moral ceiling enforced by penance and excommunication rather than by charter or capacity.',
    'Compile and classify the sibling stories; compare victim sets, epsilon, and computed types across the family.',
    'If the vassal reading computes as rope or tangled_rope and the ecclesiastical reading as tangled_rope, the kernel contest is empirically a contest between a snare and two coordination-shaped constraints over the same ceremonies — the corpus''s classification divergence IS the finding.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_structural_delta, conceptual, 'Structural consequences of sibling readings on victim sets, epsilon, and type.').

omega_variable(
    rebellion_threshold_location,
    'Where exactly did the effective bound on extraction — the rebellion/flight threshold — sit, by decade and by estate? The reading defines maximal extraction as capacity minus the revolt margin, but the margin is not directly observable.',
    'Manorial account rolls, revolt chronology, flight and forfeiture records, and charter terms fixed after unrest: correlate levy levels with subsequent disturbance to estimate the threshold band.',
    'A narrow margin confirms the capacity-bound premise and the high epsilon; a wide, stable margin would suggest self-limiting norms the lord-extraction reading denies, pulling the classification toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rebellion_threshold_location, empirical, 'Calibration of the revolt/flight threshold that operationally bounds extraction under this reading.').

omega_variable(
    suppression_internalization_share,
    'How much of tenant compliance was structural (pursuit, forfeiture law, castle, manorial court) versus internalized (the three-orders doctrine preached by endowed clergy, habituated deference, identity fusion with servile status)?',
    'Compare compliance and resistance across regions with comparable coercive infrastructure but different depths of clerical penetration and doctrinal teaching; post-emancipation behavior where structural coercion lifted at different dates.',
    'If internalization carried a large share, effective suppression exceeds the structural measure and persisted after enforcement decayed — raising the true suppression of the later interval above the authored 0.55 and explaining why commutation did not immediately dissolve deference.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_share, empirical, 'Structural versus internalized share of the suppression sustaining the arrangement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feudal_oath_reciprocity__lord_extraction_reading, 850, 1350).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t850, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 850, 0.08).
narrative_ontology:measurement(feud_tr_t950, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 950, 0.11).
narrative_ontology:measurement(feud_tr_t1050, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 1050, 0.17).
narrative_ontology:measurement(feud_tr_t1150, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 1150, 0.23).
narrative_ontology:measurement(feud_tr_t1250, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 1250, 0.32).
narrative_ontology:measurement(feud_tr_t1350, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 1350, 0.42).

% Extraction over time
narrative_ontology:measurement(feud_be_t850, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 850, 0.56).
narrative_ontology:measurement(feud_be_t950, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 950, 0.64).
narrative_ontology:measurement(feud_be_t1050, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 1050, 0.73).
narrative_ontology:measurement(feud_be_t1150, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 1150, 0.81).
narrative_ontology:measurement(feud_be_t1250, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 1250, 0.84).
narrative_ontology:measurement(feud_be_t1350, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 1350, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t850, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 850, 0.44).
narrative_ontology:measurement(feud_su_t950, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 950, 0.57).
narrative_ontology:measurement(feud_su_t1050, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 1050, 0.71).
narrative_ontology:measurement(feud_su_t1150, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 1150, 0.79).
narrative_ontology:measurement(feud_su_t1250, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 1250, 0.68).
narrative_ontology:measurement(feud_su_t1350, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 1350, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feudal_oath_reciprocity__lord_extraction_reading, resource_allocation).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__lord_extraction_reading, vassal_coordination_reading).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__lord_extraction_reading, ecclesiastical_mediation_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'feudal oath reciprocity' conflates three structurally distinct claims about what fixes the oath's binding content. This file authors the lord-extraction instantiation (open-ended demand, capacity-bounded, high epsilon, snare claim). The sibling files author the charter-fixed instantiation (lower epsilon, coordination-shaped) and the charity-limited instantiation (morally capped extraction). The upstream/downstream structure runs from the ecclesiastical reading (which supplied the ceremonial form and the sacral legitimacy all readings inherit) through this reading (which operated the extraction) to the vassal reading (whose charters were largely reactive devices forcing this reading's demands into text). Each file links the others via affects_constraints; no single file averages across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
