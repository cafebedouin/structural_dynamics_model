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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Feudal Oath as Open-Ended Extraction Authorization (Lord's Reading)
 *   domain: economic/political/legal-historical
 *
 * SUMMARY:
 *   Under the lord's reading, the oath of homage and fealty is not a
 *   fixed-price contract but an open-ended grant: the vassal swears
 *   unquantified faith, and what the lord may take is measured by nothing
 *   except the vassal's remaining capacity to fight, pay, and work — and the
 *   point at which he revolts. The ceremony of reciprocity (protection
 *   professed downward, aid and counsel professed upward) legitimates the
 *   flow; the castle, the lord's court, and the armed retinue enforce it.
 *   This file instantiates ONE reading of the contested kernel
 *   feudal_oath_reciprocity — the lord_extraction_reading — and authors
 *   epsilon for the standing feudal arrangement as that reading sees it:
 *   high, because under this reading maximal collection is the arrangement's
 *   operative point and the reciprocal duties are its legitimation surface.
 *   The sibling readings (vassal_coordination_reading,
 *   ecclesiastical_mediation_reading) are separate constraints with their own
 *   epsilon, beneficiary/victim structure, and classification; the contest is
 *   recorded in the omega variables and commentary.kernel_context, never
 *   averaged into this file. The claim/metric relationship is deliberate:
 *   claimed_type is authored from this reading's structure; the metrics are
 *   authored from what the historical record shows the arrangement doing.
 *
 * KEY AGENTS:
 *   - seigneurial_lords: agenda-setting beneficiary (institutional/constrained) — convene courts, set dues, collect; the arrangement's gains land here
 *   - castellans: enforcement-layer beneficiary (organized/constrained) — run the day-to-day coercion and take a share
 *   - enfeoffed_vassals: primary payer among the oath's own parties (moderate/identity_locked) — owe service, aids, reliefs; bound by honor, lineage, and livelihood at once
 *   - serf_tenants: ultimate payer (powerless/trapped) — bear labor, tallages, and banalities; parties to no oath themselves
 *   - town_burghers: excluded seat (organized/mobile) — outside the oath network; would contest unfree tenure but have no place in the conversation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feudal_oath_reciprocity__lord_extraction_reading, 0.85).
domain_priors:suppression_score(feudal_oath_reciprocity__lord_extraction_reading, 0.74).
domain_priors:theater_ratio(feudal_oath_reciprocity__lord_extraction_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feudal_oath_reciprocity__lord_extraction_reading, snare).
narrative_ontology:human_readable(feudal_oath_reciprocity__lord_extraction_reading, "Feudal Oath as Open-Ended Extraction Authorization (Lord's Reading)").
narrative_ontology:topic_domain(feudal_oath_reciprocity__lord_extraction_reading, "economic/political/legal-historical").

domain_priors:requires_active_enforcement(feudal_oath_reciprocity__lord_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feudal_oath_reciprocity__lord_extraction_reading, 'd9e3ceaf-6158-4834-8d81-e3721402f5a1').
narrative_ontology:cs_kernel_codification('d9e3ceaf-6158-4834-8d81-e3721402f5a1', distributed).
narrative_ontology:cs_authority_grounding('d9e3ceaf-6158-4834-8d81-e3721402f5a1', extraction).
narrative_ontology:cs_interpretation_layer_present('d9e3ceaf-6158-4834-8d81-e3721402f5a1').
narrative_ontology:cs_reading_relation('d9e3ceaf-6158-4834-8d81-e3721402f5a1', feudal_oath_reciprocity__vassal_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('d9e3ceaf-6158-4834-8d81-e3721402f5a1', feudal_oath_reciprocity__ecclesiastical_mediation_reading, coexists_with).
narrative_ontology:cs_axiom('d9e3ceaf-6158-4834-8d81-e3721402f5a1', foundational, unquantified_faith_grant).
narrative_ontology:cs_axiom_status(unquantified_faith_grant, holdable).
narrative_ontology:cs_axiom_grounding('d9e3ceaf-6158-4834-8d81-e3721402f5a1', unquantified_faith_grant, conventional).
narrative_ontology:cs_axiom('d9e3ceaf-6158-4834-8d81-e3721402f5a1', secondary, capacity_is_sole_just_bound).
narrative_ontology:cs_axiom_status(capacity_is_sole_just_bound, holdable).
narrative_ontology:cs_axiom_grounding('d9e3ceaf-6158-4834-8d81-e3721402f5a1', capacity_is_sole_just_bound, instrumental).
narrative_ontology:cs_reference_frame('d9e3ceaf-6158-4834-8d81-e3721402f5a1', open_ended_fealty_discretion_order).
narrative_ontology:cs_drift_state('d9e3ceaf-6158-4834-8d81-e3721402f5a1', charter_codification_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d9e3ceaf-6158-4834-8d81-e3721402f5a1', '').
narrative_ontology:cs_kernel_id(feudal_oath_reciprocity__lord_extraction_reading, feudal_oath_reciprocity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__lord_extraction_reading, seigneurial_lords).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__lord_extraction_reading, castellans).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__lord_extraction_reading, enfeoffed_vassals).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__lord_extraction_reading, serf_tenants).
narrative_ontology:constraint_vindicates(feudal_oath_reciprocity__lord_extraction_reading, seigneurial_ban_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold fiefs with the rights of command attached: they convene courts, set and collect dues, aids, and reliefs, summon military service, and build and garrison castles. Their income and armed power come from what their tenants owe. They could in principle grant or sell land away, but the family patrimony and the standing of lordship are the substance of their position, so they do not leave; they administer the arrangement and receive what it collects.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, seigneurial_lords, agenda_setter,
    institutional, generational, constrained, regional).

% Garrison and command castles for the lords or hold minor castellanies of their own; they run the day-to-day coercion — tolls, compulsory use of the lord's mill and oven, court fines, and armed escort of collectors. They take a share of what is gathered and their standing depends on the arrangement continuing; their prospects outside castle service are thin.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, castellans, beneficiary,
    organized, biographical, constrained, local).

% Hold land in return for mounted service, court attendance, aids, reliefs, and wardship payments. The fief is the family's honor, livelihood, and marriage-market position all at once; surrendering it means leaving the knightly class they were trained into from boyhood. They resist particular demands by bargaining, litigation, withholding service, or rebellion, but leaving the relationship altogether means ceasing to be what they are.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, enfeoffed_vassals, payer,
    moderate, generational, identity_locked, regional).

% Work the lord's demesne days as well as their own land, pay tallages set at the lord's will, grind at his mill and bake at his oven for fees, and answer in his court before his steward. Their status is heritable and bound to the soil; a fugitive can be pursued and reclaimed. Some run to towns, where a year and a day can make them free, but for most there is no practical way out.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, serf_tenants, payer,
    powerless, generational, trapped, local).

% Live in chartered towns under their own law, outside the oath network's personal bonds. They buy grain, hire labor, and shelter runaways; they would argue that tenure and labor should be free and contractable, but they have no seat in the lord-vassal conversation where the terms everyone else lives under are set.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, town_burghers, excluded,
    organized, generational, mobile, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(feudal_oath_reciprocity__lord_extraction_reading, seigneurial_lords).
narrative_ontology:fixing_cost_class(feudal_oath_reciprocity__lord_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Mobilizes mounted military force and adjudication through a hierarchy of personal bonds in a landscape with no effective public authority: each man swears faith to his lord, who professes protection and judgment in return, so that defense, dispute settlement, and land tenure are organized through the oath network rather than through a state.
% TRANSFER_FUNCTION: Moves labor, grain, and money upward — demesne work days, tallages, aids, reliefs, wardship payments, court fines, mill and oven fees — from peasant households to lords, and military service and court attendance from vassals to lords; moves land tenure and the profession of protection downward.
% ABSENT_VOICES: The unfree peasantry: bound through their lords' sub-oaths, they swore nothing and were owed nothing by oath, yet bore the heaviest dues — their objection survives only as flight and revolt. Town burghers, living under their own chartered law, would argue tenure and labor should be free and contractable; they have no seat where the terms are set. Tenant women appear in the record chiefly through guardianship and widow's claims, mediated by men.
% DISAPPEARANCE_RATIONALE: If the oath network vanished overnight, the collection pyramid loses its authorization and its enforcement at once: demesne labor and tallages go ungathered without the courts and castles that back them, mounted defense dissolves into whatever local retinues can hold together, land tenure reverts to possession backed by force, and the chartered towns and royal courts already edging into the arrangement would absorb jurisdiction and labor rapidly. Every named seat's situation depends on the structure; nothing about it persists by inertia.
% FOUNDING_PROBLEM: After the Carolingian state fragmented in the ninth century, raiders struck repeatedly at a countryside with no royal army, no coinage, and no courts; local strongmen built castles and mounted retinues and needed a bond that would hold armed men to service and tenants to the fields that fed them.
% FOUNDING_PROBLEM_CORROBORATION: Ninth-century annals (St-Bertin, Fulda) and monastic charters — seats that were themselves later exaction victims — attest the raiding crisis and the rise of castle retinues. Fulbert of Chartres' letter on the mutual duties of lord and vassal (c. 1020) attests the reciprocity framing, but from a bishop's seat that was itself a lordly beneficiary, which limits its independence. No source outside the lordly class attests that collection beyond reciprocal service was ever the oath's purpose; the charter record from the late eleventh century onward — tenants paying to have their dues fixed in writing — is the strongest outside attestation that the bound this reading denies was what tenants understood the oath to promise.
narrative_ontology:disappearance_verdict(feudal_oath_reciprocity__lord_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(feudal_oath_reciprocity__lord_extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feudal_oath_reciprocity__lord_extraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(feudal_oath_reciprocity__lord_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feudal_oath_reciprocity__lord_extraction_reading, 0.85, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored high (0.85 at interval end) because under this reading the dues, aids, reliefs, tallages, court fines, and labor services are set by the lord's will and rise with his needs and his tenants' capacity, with no textual ceiling; the historical arc — consolidation of banal lordship from the mid-tenth century, multiplication of aids and banalities through the twelfth and thirteenth — is a record of the bound being sought. Suppression (0.74) is the enforcement machinery: castles, retinues, seigneurial courts, and the legal binding of tenants to land; its series rises with the castle-building and court-institutionalization of 950-1270 and falls by 1350 as royal courts, chartered towns, and post-plague labor scarcity erode seigneurial enforcement capacity. Theater (0.55) is the reciprocity surface: homage ritual, professed protection, counsel — real enough to be cited, thin enough that tenants increasingly paid to have their dues fixed in charters. Accessibility collapse (0.6) and resistance (0.6) reflect a constraint whose exits partly exist (town charters, ecclesiastical careers, frontier clearance, commutation) and whose payers fought back continuously — vassal rebellion, peasant flight, charter litigation, the Peace of God — without dissolving the arrangement inside the interval. The three series share one time grid (950/1030/1110/1190/1270/1350) so every metric is authored at every examined point. Coalition note: the payer seats' power is collective, not individual — individually powerless serfs and individually moderate vassals each acted as bodies (communes, leagues, general revolts), and the rebellion threshold that bounds collection is precisely the point where that coalition capacity activates; the dispersion of tenants and the lords' cavalry edge kept that threshold high for most of the interval.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat and the payer seats should compute differently. From the lord's seat the arrangement is the defense-and-order system he personally embodies: the oath is the bond that holds armed society together, his exactions are the price of the protection he is obliged to deliver, and the charter movement is a breach of faith by tenants. From the vassal's seat the same structure is an open-ended claim on his household's military and fiscal capacity that no text bounds and only his ability to resist limits. From the serf's seat it is a heritable liability to labor and payment in which he was never a contracting party at all. The engine computes these per-seat classifications from the structural data (power, exit, role); the authored claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations map to low directionality: seigneurial_lords (institutional power, constrained exit, generational horizon) sit near the beneficiary end — they set the terms and receive the flow; castellans (organized, local) sit near them as the paid enforcement layer. The victim declarations map to high directionality: enfeoffed_vassals are near-full targets whose identity_locked exit places them at the locked end of the derivation — the fief fuses honor, lineage, and livelihood, so exit means leaving the knightly class rather than changing landlords; serf_tenants are full targets with trapped exit and no contractual seat at all, sitting nearest the full-target end the derivation produces. town_burghers, excluded from the arrangement, derive a low-to-moderate directionality through their indirect gain from tenant flight and their freedom from the oath's personal bonds. Scope amplification is modest here: the arrangement runs at regional-to-national scope, and its collection is verified locally by the lord's own court — the engine owns that arithmetic. No directionality overrides are authored: the beneficiary/victim declarations plus exit options produce the correct relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification discipline matters here because the coordination cover is strong: the arrangement genuinely mobilized defense and adjudication in a stateless landscape, and a naive reading of that function would classify it as rope. Reading the structure — who set the terms, who bore them, what enforcement required, and what the actual bound on collection was — separates the two: the coordination function was real, but the terms were set unilaterally by the seat that collected, with exits suppressed and victims identifiable, which is what the snare claim records. On mandatrophy: the founding problem (mobilizing defense against the raiding era) was real in the tenth century and dead by the thirteenth, when raiding had ended, royal justice was rising, and the arrangement's operative content was collection; this story declares mandatrophy_resolved true from that seat, while founding_problem_status is recorded as contested because the lordly seat still professed — and sometimes performed — the protective duty through the interval's end. The analysis also blocks the opposite mislabel: the post-1270 enforcement decay is not the arrangement dissolving into a harmless vestige, because extraction held near its peak while enforcement strained — a snare under pressure, not a piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_location,
    'This constraint is one reading (lord_extraction_reading) of the kernel feudal_oath_reciprocity; the siblings locate the oath''s binding force in charter text (vassal_coordination_reading) or in charity and sacrament (ecclesiastical_mediation_reading) — what structural change follows if a sibling reading is adopted instead?',
    'Not resolvable within this story: the readings are separate constraints in separate files. Resolution proceeds by authoring the sibling stories and comparing their epsilon, victim sets, and enforcement structure against the same historical record.',
    'All metrics, the victim set, and the classification of this story are reading-indexed. Adopting vassal_coordination_reading converts the exposure from capacity-bound to text-bound and cuts epsilon sharply; adopting ecclesiastical_mediation_reading adds operative church-enforced limits and shifts part of enforcement to ecclesiastical courts. Neither adoption corrects this file — it replaces it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Reading-indexed classification: the contest over what measures the lord''s due is located between readings, not inside this one.').

omega_variable(
    rebellion_threshold_location,
    'Where does the effective bound on collection actually sit — at what point does the vassal''s or peasantry''s capacity-and-tolerance threshold bind the lord''s exaction, and did that threshold move over the interval?',
    'Rebellion and revolt frequency, flight and fugitive-reclamation rates, commutation pressure, and litigation over aids and tallages, plotted against recorded exaction levels across the interval.',
    'A low, falling threshold means the arrangement behaves as a contested bargain with a real ceiling, shading the classification toward tangled_rope; a high, rising threshold means near-unlimited collection and confirms the snare profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rebellion_threshold_location, empirical, 'Location and drift of the capacity/rebellion bound on lordly collection.').

omega_variable(
    protection_delivery_rate,
    'What fraction of the protection the oath nominally purchased was actually delivered — as against tenants being defended chiefly from the lord''s own exaction, or protection simply not rendered?',
    'Case comparison of lordly defense performance (sieges met, raiders engaged, courts kept) against exaction records and tenant testimony in charter disputes.',
    'Substantial delivery would make part of the measured theater functional coordination and shade the classification toward tangled_rope; minimal delivery confirms the reciprocity surface as legitimation cover and strengthens the snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protection_delivery_rate, empirical, 'Whether the reciprocal half of the bond was delivered or performed.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (binding to land, seigneurial courts, armed retinues) or internalized (customary acceptance of the three-orders worldview that makes exit unthinkable for tenants and dishonorable for vassals)?',
    'Post-emancipation and post-flight trajectories: where tenants escaped the arrangement (town charters, cleared districts), did dependent relationships re-form around them, and did former vassal families cling to lapsed obligations as honor?',
    'If substantially internalized, effective suppression exceeds the structural measure and persists after institutional exit opens; the classification would understate the arrangement''s grip on the payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized component of the arrangement''s hold on its payers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feudal_oath_reciprocity__lord_extraction_reading, 950, 1350).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t950, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 950, 0.25).
narrative_ontology:measurement(feud_tr_t1030, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 1030, 0.32).
narrative_ontology:measurement(feud_tr_t1110, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 1110, 0.38).
narrative_ontology:measurement(feud_tr_t1190, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 1190, 0.44).
narrative_ontology:measurement(feud_tr_t1270, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 1270, 0.5).
narrative_ontology:measurement(feud_tr_t1350, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 1350, 0.55).

% Extraction over time
narrative_ontology:measurement(feud_be_t950, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 950, 0.6).
narrative_ontology:measurement(feud_be_t1030, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 1030, 0.68).
narrative_ontology:measurement(feud_be_t1110, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 1110, 0.75).
narrative_ontology:measurement(feud_be_t1190, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 1190, 0.8).
narrative_ontology:measurement(feud_be_t1270, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 1270, 0.86).
narrative_ontology:measurement(feud_be_t1350, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 1350, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t950, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 950, 0.55).
narrative_ontology:measurement(feud_su_t1030, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 1030, 0.68).
narrative_ontology:measurement(feud_su_t1110, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 1110, 0.75).
narrative_ontology:measurement(feud_su_t1190, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 1190, 0.8).
narrative_ontology:measurement(feud_su_t1270, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 1270, 0.82).
narrative_ontology:measurement(feud_su_t1350, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 1350, 0.74).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feudal_oath_reciprocity__lord_extraction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__lord_extraction_reading, vassal_coordination_reading).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__lord_extraction_reading, ecclesiastical_mediation_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'feudal oath reciprocity' decomposes into three structurally distinct claims per the epsilon-invariance principle. This file is the lord_extraction_reading (high epsilon; vassals and serfs in the victim set; bound only by service capacity). vassal_coordination_reading and ecclesiastical_mediation_reading instantiate the charter-bound and charity/sacrament-bound claims respectively, each with its own epsilon, beneficiaries, victims, and classification. The upstream claim in each sibling is cited as evidence against this one (charters and canon law as the oath's true terms), which is why the family is linked: contamination and legitimacy pressure run across the readings, not within this one.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
