% ============================================================================
% CONSTRAINT STORY: feudal_oath_reciprocity__lord_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   human_readable: Feudal Oath as Authorization for Maximal Seigneurial Demand (Lord Extraction Reading)
 *   domain: economic/political/legal-historical
 *
 * SUMMARY:
 *   This story instantiates one reading of the feudal_oath_reciprocity
 *   kernel: that the oath of homage and fealty functioned, from the lord's
 *   seat, as an authorization to demand — reliefs, aids, wardship and
 *   marriage profits, tallages, hospitality — up to the point where tenants
 *   could bear no more and resisted. On this reading the only effective bound
 *   on what moved up the tenurial ladder was vassal service capacity and the
 *   rebellion threshold; charter text and sacramental scruple registered
 *   limits without setting them. The epsilon referent is the standing
 *   oath-governed tenurial arrangement as this reading sees it, not any
 *   charter-bound alternative. Per the epsilon-invariance principle, the
 *   sibling readings (vassal_coordination_reading,
 *   ecclesiastical_mediation_reading) are separate constraint files linked
 *   through the network section; their structural deltas are carried in the
 *   omega variables rather than averaged into this story's numbers. KEY
 *   AGENTS (by structural relationship): - seigneurial_lords: agenda-setting
 *   beneficiary (institutional/arbitrage) — pronounces custom, collects
 *   reliefs, wardship, aids - crown_feudal_administration: apex-lord
 *   agenda-setter and collector (institutional/arbitrage) -
 *   enfeoffed_vassals: primary organized payer (organized/constrained) —
 *   bears demands, resists by league and appeal - mesne_subvassal_knights:
 *   secondary payer (moderate/trapped) - warded_heirs_and_widows: captive
 *   payer (powerless/trapped) - demesne_peasantry: downstream payer
 *   (powerless/trapped) - episcopal_authorities: external check, partly an
 *   interested landlord (institutional/constrained) - royal_justices:
 *   analytical observer reshaping terms case by case
 *   (institutional/analytical)
 *
 * KEY AGENTS:
 *   - seigneurial_lords: agenda-setting beneficiary (institutional/arbitrage) — pronounces custom, collects reliefs, wardship, aids
 *   - crown_feudal_administration: apex-lord agenda-setter and collector (institutional/arbitrage)
 *   - enfeoffed_vassals: primary organized payer (organized/constrained) — bears demands, resists by league and appeal
 *   - mesne_subvassal_knights: secondary payer (moderate/trapped)
 *   - warded_heirs_and_widows: captive payer (powerless/trapped)
 *   - demesne_peasantry: downstream payer (powerless/trapped)
 *   - episcopal_authorities: external check, partly an interested landlord (institutional/constrained)
 *   - royal_justices: analytical observer reshaping terms case by case (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feudal_oath_reciprocity__lord_extraction_reading, 0.76).
domain_priors:suppression_score(feudal_oath_reciprocity__lord_extraction_reading, 0.75).
domain_priors:theater_ratio(feudal_oath_reciprocity__lord_extraction_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, extractiveness, 0.76).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feudal_oath_reciprocity__lord_extraction_reading, snare).
narrative_ontology:human_readable(feudal_oath_reciprocity__lord_extraction_reading, "Feudal Oath as Authorization for Maximal Seigneurial Demand (Lord Extraction Reading)").
narrative_ontology:topic_domain(feudal_oath_reciprocity__lord_extraction_reading, "economic/political/legal-historical").

domain_priors:requires_active_enforcement(feudal_oath_reciprocity__lord_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feudal_oath_reciprocity__lord_extraction_reading, '1553014c-f85e-427f-a0c7-af637181bf3b').
narrative_ontology:cs_kernel_codification('1553014c-f85e-427f-a0c7-af637181bf3b', distributed).
narrative_ontology:cs_authority_grounding('1553014c-f85e-427f-a0c7-af637181bf3b', extraction).
narrative_ontology:cs_interpretation_layer_present('1553014c-f85e-427f-a0c7-af637181bf3b').
narrative_ontology:cs_reading_relation('1553014c-f85e-427f-a0c7-af637181bf3b', feudal_oath_reciprocity__vassal_coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('1553014c-f85e-427f-a0c7-af637181bf3b', feudal_oath_reciprocity__ecclesiastical_mediation_reading, coexists_with).
narrative_ontology:cs_axiom('1553014c-f85e-427f-a0c7-af637181bf3b', foundational, oath_conveys_unquantified_demand_right).
narrative_ontology:cs_axiom_status(oath_conveys_unquantified_demand_right, holdable).
narrative_ontology:cs_axiom_grounding('1553014c-f85e-427f-a0c7-af637181bf3b', oath_conveys_unquantified_demand_right, conventional).
narrative_ontology:cs_axiom('1553014c-f85e-427f-a0c7-af637181bf3b', secondary, charter_bounds_are_prudential_not_constitutive).
narrative_ontology:cs_axiom_status(charter_bounds_are_prudential_not_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('1553014c-f85e-427f-a0c7-af637181bf3b', charter_bounds_are_prudential_not_constitutive, conventional).
narrative_ontology:cs_reference_frame('1553014c-f85e-427f-a0c7-af637181bf3b', full_proprietary_discretion_grant).
narrative_ontology:cs_drift_state('1553014c-f85e-427f-a0c7-af637181bf3b', charter_reaction_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1553014c-f85e-427f-a0c7-af637181bf3b', '').
narrative_ontology:cs_kernel_id(feudal_oath_reciprocity__lord_extraction_reading, feudal_oath_reciprocity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__lord_extraction_reading, seigneurial_lords).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__lord_extraction_reading, crown_feudal_administration).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__lord_extraction_reading, enfeoffed_vassals).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__lord_extraction_reading, mesne_subvassal_knights).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__lord_extraction_reading, warded_heirs_and_widows).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__lord_extraction_reading, demesne_peasantry).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold honors of manors and castles granted in return for service. Summon tenants to their own courts, pronounce what custom requires, and take reliefs when heirs inherit, profits from the wardship and marriage of tenants' children, aids for knighting sons and marrying daughters, and hospitality dues when they travel. When a tenant resists they distrain his chattels, besiege his house, or sue him in their own court. Their own position is mobile: they can press one estate harder while easing another, marry heirs into other networks, and litigate the same dispute in several jurisdictions.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, seigneurial_lords, agenda_setter,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(feudal_oath_reciprocity__lord_extraction_reading, seigneurial_lords, beneficiary).

% Acts as apex lord: takes reliefs and wardships from tenants-in-chief, sells custody of heirs to the highest bidder, levies scutage when it prefers cash to service, and licenses or demolishes private castles. It also hears appeals from lower lords' courts, so it both runs its own demand machinery and sets some of the rules governing everyone else's.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, crown_feudal_administration, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(feudal_oath_reciprocity__lord_extraction_reading, crown_feudal_administration, beneficiary).

% Hold land by oath of homage and fealty; owe mounted service, castle guard, and attendance at the lord's court. Beyond the fixed services they face demands — reliefs, aids, tallages — sized by the lord's need and their apparent ability to pay. They resist by withholding service, forming leagues with fellow tenants, appealing to the overlord's court, or rebelling; outright departure means forfeiting the estate that anchors the family's standing, so most fight from inside rather than leave.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, enfeoffed_vassals, payer,
    organized, generational, constrained, regional).

% Hold single manors or fractions of manors from greater tenants; owe knight service they often commute for cash. They attend their immediate lord's court, pay what it assesses, and have no realistic forum above it short of the crown. Giving up the holding means giving up the family's livelihood and rank, so they absorb demands up to the point of ruin.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, mesne_subvassal_knights, payer,
    moderate, biographical, trapped, local).

% Minor heirs fall into the lord's custody, and the income of the estate accrues to the guardian until majority; widows and heiresses are married to whom the lord chooses, or fined for the privilege of choosing themselves. They appear in no court that sets these terms and cannot decline guardianship; their protection is whatever an overlord's writ or a well-placed kinsman obtains.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, warded_heirs_and_widows, payer,
    powerless, biographical, trapped, local).

% Work the lord's home farms part of the week, pay tallages set at the lord's will, grind at his mill, bake at his oven, and render boon works at harvest. Legally bound to the soil, they resist by slow work, flight to chartered towns, and occasional revolt; success in flight depends on distance to a town and the reach of the lord's officers.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, demesne_peasantry, payer,
    powerless, biographical, trapped, local).

% Teach that oaths sworn on relics bind under pain of perjury before God, threaten excommunication and interdict against notorious oppressors, and shelter fugitives on church land. They record grievances in synods and chronicles. Their sanctions arrive slowly and unevenly, and they hold extensive tenures of their own, which makes their position partly that of a fellow landlord.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, episcopal_authorities, observer,
    institutional, civilizational, constrained, continental).

% Itinerant and central courts hear pleas between lords and tenants, gradually treating some tenant protections as enforceable royal law. Their possessory writs give dispossessed tenants a venue outside the lord's own court. They reshape the terms of the arrangement case by case without abolishing it.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, royal_justices, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(feudal_oath_reciprocity__lord_extraction_reading, seigneurial_lords).
narrative_ontology:fixing_cost_class(feudal_oath_reciprocity__lord_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converts scattered armed landholders into a legible hierarchy: annual service periods, castle guard, court attendance, and hereditary succession to tenures are settled once in the homage ceremony instead of renegotiated per campaign, and the lord's court resolves disputes among his tenants.
% TRANSFER_FUNCTION: Moves surplus from tenant households up the tenurial ladder to lords and the crown — labor on demesne, cash reliefs at inheritance, aids for the lord's ceremonies, wardship and marriage profits from tenants' children, mill and oven monopolies, hospitality dues — in amounts the lord can press before resistance organizes.
% ABSENT_VOICES: Warded heirs and widows have no standing in the court that prices their custody and marriages; the peasantry appears only in its lord's own court; rival lords who might bid for a resisting tenant's homage are barred by the exclusivity of the oath itself. The strongest organized voice — the baronial league — speaks only in crisis, not in the ordinary running of honors.
% DISAPPEARANCE_RATIONALE: If the oath-bound demand structure vanished overnight, tenures would lose their hereditary glue, military service would need explicit contract or wages, seigneurial court revenue and incident income would collapse, and the landed class would reorganize around salaried retainer and rental contracts over the following generations — the shape of rural power depends on it.
% FOUNDING_PROBLEM: After the Carolingian fiscal-military state dissolved, kings could neither pay armored cavalry nor police local order: land granted in return for service sustained defense, succession, and dispute settlement without a salaried bureaucracy.
% FOUNDING_PROBLEM_CORROBORATION: Royal judicial records and itinerant-justice rolls — outside the seigneurial beneficiary set — attest that tenant protections were being enforced as royal law by the interval's end, indicating the original justification no longer covered observed practice; monastic chronicles, themselves landlords but independent of the magnate beneficiaries, record demands far exceeding any protective return. Lords themselves attest the problem still live. No fully disinterested attestation exists; the closest is the crown's, which is itself a beneficiary at the apex.
narrative_ontology:disappearance_verdict(feudal_oath_reciprocity__lord_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(feudal_oath_reciprocity__lord_extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feudal_oath_reciprocity__lord_extraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(feudal_oath_reciprocity__lord_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feudal_oath_reciprocity__lord_extraction_reading, 0.76, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is high (0.76 at interval end) because the demand streams — reliefs taken at inheritance, wardship and marriage sold, aids and tallages assessed in the lord's own court — track the lord's need and the tenant's visible liquidity, not the cost of protection rendered. Suppression (0.75) is the cost of holding the arrangement open-ended: private castles, distraint of chattels, forfeiture proceedings, and the lord's court as sole first-instance forum. Theater (0.38) is moderate: the homage ceremony and invocations of ancient custom still do legal work, but a rising share of ritual legitimates demands with no protective counterpart. Accessibility collapse (0.60): exit routes exist — appeal to the overlord, flight to towns, sale of claims — but every route costs the holder his tenurial anchor, so understanding the arrangement does not dissolve it. Resistance (0.70) is high and organized: withheld service, baronial leagues, the 1215 charter crisis. Coordination type is resource_allocation: the dominant function whose failure would break the arrangement is converting land tenure into sustained military and administrative service. The three measured series share one time grid (points 0-100); the oscillation in extractiveness tracks royal strength — regularization under strong kings, spikes under weak ones — riding an upward ratchet as incident-taking intensified. Base properties are authored at the interval end (t=100), just past the charter reaction's first partial restraint. The claimed type (snare) and the metrics are independently authored: the claim states what this reading holds structurally true; the metrics state what the record descriptively shows.
 *
 * PERSPECTIVAL GAP:
 *   From the lord's seat the arrangement is the constitution of ordered society: his court pronounces custom, his protection is real, and the ceremony binds both sides before witnesses — a seat from which the structure computes as burden-bearing coordination. From the trapped seats — warded heirs, villeins, single-manor knights — the same structure is open-ended taking with no forum that sets a bound. Organized vassals sit between: strong enough to force written limits in crisis, too weak to hold them between crises. The engine computes these per-seat classifications from the structural data; the divergence between the lord-seat and payer-seat computations is the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Seigneurial lords and the crown's incident machinery are declared beneficiaries: they collect the reliefs, wardships, aids, and tallages, and their wide exits (arbitrage across estates and courts) place them near the beneficiary end of d. The four payer groups are declared victims: enfeoffed vassals (organized, constrained exit) sit high on d but somewhat damped by coalition leverage; mesne knights, warded heirs, and the peasantry (trapped exits) sit nearest the full-target end, with the wards and villeins effectively at it. Episcopal authorities and royal justices are observers: neither collected from nor bound by the flows, they shape the constraint's environment without bearing it. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled by directionality and scope in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — sustaining armored defense and local order after the collapse of public taxation and salaried administration — was real, and a pure-extraction verdict that erased the coordination layer would misread why tenants ever entered. Equally, a coordination verdict that erased the asymmetry would misread why the arrangement needed castles. Classifying the standing arrangement as a snare keeps both facts: the coordination function is the enabling frame, the open-ended transfer is the operative dynamic. The founding problem's status is contested at interval end: royal courts and growing monetized administration attest its fading, while lords attest continuing raid and war; the mismatch consumer reads that contested status against the world_rearranges verdict, and the charter-reaction measurements show the first forced partial restraint rather than resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates the lord_extraction_reading of the feudal_oath_reciprocity kernel; for a given honor and decade, which reading — this one, vassal_coordination_reading, or ecclesiastical_mediation_reading — describes the operative constraint?',
    'Per-honor archival comparison: charter clauses fixing reliefs and services versus recorded exactions exceeding them; court rolls showing whether the lord''s court treated charter bounds as enforceable or ignorable.',
    'Where the coordination reading is operative, epsilon falls sharply and the computed type moves toward tangled_rope; where this reading is operative, the high-extraction profile stands. Sibling readings are separate constraint files, not hedges folded into this story''s numbers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Which sibling reading of the feudal-oath kernel is operative for a given tenancy.').

omega_variable(
    sibling_delta_quantum_fixity,
    'What would change structurally if the vassal_coordination_reading were adopted instead: is the obligation quantum fixed by charter text, or set by lordly demand bounded only by vassal service capacity?',
    'Statistical comparison of recorded exactions against charter-stated quanta across honors; if exaction distributions cluster at charter bounds the coordination reading governs, if they track lordly need and tenant liquidity this reading governs.',
    'Adopting the sibling reading removes the open-endedness of the victim asymmetry: lords become bound parties, classification shifts toward tangled_rope, and the rebellion threshold stops being the operative limiter.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_delta_quantum_fixity, conceptual, 'Located disagreement: whether the oath''s obligation quantum is text-fixed or capacity-bounded.').

omega_variable(
    sibling_delta_sacramental_binding,
    'What would change if the ecclesiastical_mediation_reading were adopted: does sacramental obligation and charity doctrine impose an effective cap on secular demands, or does it register protest without altering what lords take?',
    'Case tracing of excommunication and interdict episodes: did threatened or imposed censures precede measurable reductions in exactions, or did lords settle with Rome while practice at home stayed unchanged?',
    'If sacramental binding effectively caps demands, the church seat becomes a co-enforcer and the suppression profile splits between secular coercion and moral sanction, shifting classification toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_delta_sacramental_binding, conceptual, 'Whether the ecclesiastical limit on the oath is operative or declaratory.').

omega_variable(
    rebellion_threshold_calibration,
    'Where exactly does the capacity bound sit — the ceiling this reading claims is the only effective limit on demands — and how does it move with harvests, war burdens, and the lord''s military position?',
    'Reconstruct exaction series around known revolt triggers (the 1173-74 revolt, the 1215 crisis) and test whether demands preceding revolts cluster at a reconstructable fraction of tenant surplus.',
    'A stable, estimable threshold supports the high-extraction profile with a sharp bound; a highly volatile threshold suggests demands are bounded by negotiation rather than raw capacity, softening the classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rebellion_threshold_calibration, empirical, 'Calibrating the capacity bound this reading treats as the sole limiter.').

omega_variable(
    villein_exit_softening,
    'Does flight to chartered towns soften the demesne peasantry''s trapped exit enough to move their directionality below the full-target end?',
    'Manor-level records of villein departures, lordly pursuit litigation, and recapture rates versus distance to a chartered town.',
    'If flight is materially available, peasant effective extraction is damped and the arrangement''s aggregate epsilon sits below the trapped-exit estimate; if pursuit and recapture dominate, the trapped profile stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(villein_exit_softening, empirical, 'Whether peasant exit is truly trapped or partially open via town flight.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feudal_oath_reciprocity__lord_extraction_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lord_extraction_reading_tr_t0, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 0, 0.24).
narrative_ontology:measurement(lord_extraction_reading_tr_t20, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement(lord_extraction_reading_tr_t40, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 40, 0.26).
narrative_ontology:measurement(lord_extraction_reading_tr_t60, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 60, 0.31).
narrative_ontology:measurement(lord_extraction_reading_tr_t80, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 80, 0.36).
narrative_ontology:measurement(lord_extraction_reading_tr_t100, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 100, 0.38).

% Extraction over time
narrative_ontology:measurement(lord_extraction_reading_be_t0, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 0, 0.64).
narrative_ontology:measurement(lord_extraction_reading_be_t20, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 20, 0.69).
narrative_ontology:measurement(lord_extraction_reading_be_t40, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 40, 0.66).
narrative_ontology:measurement(lord_extraction_reading_be_t60, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 60, 0.73).
narrative_ontology:measurement(lord_extraction_reading_be_t80, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 80, 0.79).
narrative_ontology:measurement(lord_extraction_reading_be_t100, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 100, 0.76).

% Suppression requirement over time
narrative_ontology:measurement(lord_extraction_reading_su_t0, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 0, 0.56).
narrative_ontology:measurement(lord_extraction_reading_su_t20, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 20, 0.61).
narrative_ontology:measurement(lord_extraction_reading_su_t40, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 40, 0.58).
narrative_ontology:measurement(lord_extraction_reading_su_t60, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 60, 0.67).
narrative_ontology:measurement(lord_extraction_reading_su_t80, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 80, 0.72).
narrative_ontology:measurement(lord_extraction_reading_su_t100, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 100, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feudal_oath_reciprocity__lord_extraction_reading, resource_allocation).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__lord_extraction_reading, vassal_coordination_reading).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__lord_extraction_reading, ecclesiastical_mediation_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the feudal oath' conflates three structurally distinct claims about one kernel: what the oath obligates (this file — whatever the lord can press, bounded by vassal capacity), what the charter fixes (vassal_coordination_reading), and what heaven enforces (ecclesiastical_mediation_reading). Each is a separate constraint story with its own epsilon, beneficiaries, and victims; they are linked here because the same homage ceremony is the kernel each reading instantiates, and charter and canon developments feed back into what lords can press.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
