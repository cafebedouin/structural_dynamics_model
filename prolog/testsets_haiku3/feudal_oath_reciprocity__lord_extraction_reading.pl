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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Feudal Oath as Maximal Extraction Authority (Lord Reading)
 *   domain: medieval/political_economy/institutional
 *
 * SUMMARY:
 *   This constraint story instantiates the lord_extraction_reading of the
 *   feudal_oath_reciprocity kernel. It models the feudal oath as a unilateral
 *   instrument that authorizes the lord to extract labor, military service,
 *   and surplus value from oath-bound vassals, constrained only by the
 *   vassal's capacity to maintain livelihood and military readiness. The oath
 *   is presented not as a reciprocal compact but as a binding authorization
 *   of extraction that persists across generations. This reading emphasizes
 *   the lord's structural power to set oath terms unilaterally, interpret
 *   them expansively, and enforce them through dispossession and legal
 *   supremacy. The measurement series traces rising extractiveness and
 *   suppression requirement over the interval (0–400, representing ~100 to
 *   ~500 CE feudal development), suggesting that as the oath mechanism
 *   matures, extraction becomes more systematized and the enforcement
 *   machinery more costly to maintain. This is a sibling constraint to
 *   vassal_coordination_reading (which emphasizes fixed, bounded reciprocal
 *   obligations) and ecclesiastical_mediation_reading (which emphasizes
 *   Christian charity constraints). All three share the same referent—the
 *   feudal oath—but authored with distinct ε values and different structural
 *   beneficiary/victim sets. The divergence is intentional: measuring which
 *   reading's ε is empirically true is the point of the constraint family.
 *
 * KEY AGENTS:
 *   - territorial_lord: Structural beneficiary; sets oath terms, interprets scope, collects extractions; power=institutional, exit=arbitrage (can relocate or reallocate vassals across holdings).
 *   - oath_bound_vassals: Structural victims; bound by sacramental and legal force; power=moderate (individually), exit=identity_locked (oath-breaking is social death in feudal frame); their labor and surplus flow to the lord.
 *   - competing_lords: Excluded; would benefit from access to extraction mechanism but barred by territorial closure and norm homogeneity.
 *   - church_authorities: Observer/complicit beneficiary; sanctions oath, benefits from stability, does not effectively limit extraction (from this reading's perspective).
 *   - royal_authority: Observer; enforces property rights that make extraction viable; guarantees oath-breaking is legal violation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feudal_oath_reciprocity__lord_extraction_reading, 0.82).
domain_priors:suppression_score(feudal_oath_reciprocity__lord_extraction_reading, 0.79).
domain_priors:theater_ratio(feudal_oath_reciprocity__lord_extraction_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feudal_oath_reciprocity__lord_extraction_reading, snare).
narrative_ontology:human_readable(feudal_oath_reciprocity__lord_extraction_reading, "Feudal Oath as Maximal Extraction Authority (Lord Reading)").
narrative_ontology:topic_domain(feudal_oath_reciprocity__lord_extraction_reading, "medieval/political_economy/institutional").

domain_priors:requires_active_enforcement(feudal_oath_reciprocity__lord_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feudal_oath_reciprocity__lord_extraction_reading, '26b9df8e-e9f6-4eb7-8556-68fd701ae304').
narrative_ontology:cs_kernel_codification('26b9df8e-e9f6-4eb7-8556-68fd701ae304', fixed_text).
narrative_ontology:cs_authority_grounding('26b9df8e-e9f6-4eb7-8556-68fd701ae304', extraction).
narrative_ontology:cs_interpretation_layer_present('26b9df8e-e9f6-4eb7-8556-68fd701ae304').
narrative_ontology:cs_reading_relation('26b9df8e-e9f6-4eb7-8556-68fd701ae304', feudal_oath_reciprocity__vassal_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('26b9df8e-e9f6-4eb7-8556-68fd701ae304', feudal_oath_reciprocity__ecclesiastical_mediation_reading, influences).
narrative_ontology:cs_axiom('26b9df8e-e9f6-4eb7-8556-68fd701ae304', foundational, lord_unilateral_oath_interpretation).
narrative_ontology:cs_axiom_status(lord_unilateral_oath_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('26b9df8e-e9f6-4eb7-8556-68fd701ae304', lord_unilateral_oath_interpretation, deontological).
narrative_ontology:cs_axiom('26b9df8e-e9f6-4eb7-8556-68fd701ae304', foundational, extraction_bounded_capacity_not_consent).
narrative_ontology:cs_axiom_status(extraction_bounded_capacity_not_consent, holdable).
narrative_ontology:cs_axiom_grounding('26b9df8e-e9f6-4eb7-8556-68fd701ae304', extraction_bounded_capacity_not_consent, empirically_contingent).
narrative_ontology:cs_reference_frame('26b9df8e-e9f6-4eb7-8556-68fd701ae304', lord_legal_supremacy_extraction_authority).
narrative_ontology:cs_drift_state('26b9df8e-e9f6-4eb7-8556-68fd701ae304', high_medieval_fully_systemized, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('26b9df8e-e9f6-4eb7-8556-68fd701ae304', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(feudal_oath_reciprocity__lord_extraction_reading, feudal_oath_reciprocity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__lord_extraction_reading, territorial_lord).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__lord_extraction_reading, oath_bound_vassals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__lord_extraction_reading, church_authorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the oath as a binding instrument authorizing extraction of labor, military service, and surplus value up to the vassal's capacity to maintain livelihood. Sets the oath terms unilaterally, interprets its scope through enforcement actions, and collects the extractions directly. The oath's language ('fealty,' 'service,' 'due aid') is read permissively to justify whatever demands the lord believes the vassal can bear. Maintains enforcement through legal supremacy and the threat of dispossession.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, territorial_lord, agenda_setter,
    institutional, generational, arbitrage, regional).

% Bound by oath to render military service, labor service, and payment of customary dues (and 'other aids' as demanded). The oath's binding force—sacramental and legal—makes exit unthinkable within the feudal frame itself. Exit means oath-breaking, forfeiture of tenure, and loss of social identity as a sworn vassal. The capacity constraint (they cannot be extracted beyond their ability to work and fight) is the only limit; extraction is otherwise open-ended. Resistance takes the form of evasion, slowdown, and localized rebellion, not formal contract renegotiation.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, oath_bound_vassals, payer,
    moderate, biographical, identity_locked, regional).

% Are structurally barred from offering alternative oath terms that would attract vassals away—the regional homogeneity of extraction norms and the absence of a labor market mean vassals cannot arbitrage between lords. Competing lords would benefit from access to the same extraction apparatus if they could, but territorial boundaries and military parity keep the system stable across regions.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, competing_lords, excluded,
    institutional, generational, trapped, regional).

% The Church sanctions the oath and benefits from both the lord's piety and the stability the oath provides to the feudal order. They also perform the sacramental function that lends the oath its binding force. From this reading's perspective, they are complicit in the extraction but do not adjudicate its limits—their blessing legitimates the structure rather than constraining it.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, church_authorities, observer,
    institutional, generational, analytical, regional).
narrative_ontology:stakeholder_secondary_role(feudal_oath_reciprocity__lord_extraction_reading, church_authorities, beneficiary).

% Stands outside the feudal dyad but enforces the property rights that make oath-bound tenure viable. From this reading, the crown is a guarantor of the lord's extraction authority, not a limiter of it. Royal courts enforce oath-breaking as legal violation (dispossession, damages) rather than moderating extraction claims.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, royal_authority, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(feudal_oath_reciprocity__lord_extraction_reading, territorial_lord).
narrative_ontology:fixing_cost_class(feudal_oath_reciprocity__lord_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The oath stabilizes a regional military-economic hierarchy: it creates predictable obligation chains that allow lords to plan military campaigns, vassals to know their tenure is conditional on service, and the feudal order to operate without constant renegotiation. The coordination solves the problem of assembling military force and labor without markets.
% TRANSFER_FUNCTION: Moves military service, labor service (cultivation of demesne, fortification work, road maintenance), and monetary payments (customary dues, marriage aids, relief payments, 'gracious aids') from oath-bound vassals to the territorial lord. The quantum of transfer is set unilaterally by the lord and bounded only by the vassal's capacity to sustain themselves as a warrior-farmer.
% ABSENT_VOICES: Vassals who might propose bounded, fixed-rate oaths (analogous to the vassal_coordination_reading); ecclesiastical voices arguing for charity-bounded extraction (the ecclesiastical_mediation_reading); competing lords in neighboring regions who might benefit from attracting vassals through more favorable terms (but are barred by territorial closure). None of these voices shape the lord's interpretation of the oath in his own domain.
% DISAPPEARANCE_RATIONALE: If the oath apparatus vanished, the lord would lose the binding claim to vassal labor and surplus; vassals would disperse or negotiate fixed tenures; the military pyramid would collapse into petty warlordism or territorial consolidation under a different legal mechanism. The feudal order's stability rides on the oath's extraction authority.
% FOUNDING_PROBLEM: The post-Roman political collapse left regional lords without reliable mechanisms to field armies and work fortified holdings. The oath transformed a personal promise into a heritable, enforceable obligation that could hold across generations and provide predictable military and labor mobilization.
% FOUNDING_PROBLEM_CORROBORATION: The lord and his scribes attest the oath is necessary for regional security and estate management. Historians and ecclesiastical critics (from the ecclesiastical_mediation_reading seat) attest the founding problem was solved by the 12th century and the oath thereafter persists as a mechanism of surplus extraction, documented in charters showing escalating 'aids' and royal courts enforcing oath-breaking as property violation rather than mediating rate disputes. The vassal_coordination_reading sources (charter texts listing fixed dues) corroborate that the 'bounded obligation' alternative existed and was chosen against.
narrative_ontology:disappearance_verdict(feudal_oath_reciprocity__lord_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(feudal_oath_reciprocity__lord_extraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feudal_oath_reciprocity__lord_extraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(feudal_oath_reciprocity__lord_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feudal_oath_reciprocity__lord_extraction_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.82) because the constraint transfers material surplus (labor, crops, military service) from vassals to the lord with no reciprocal bounded obligation—the lord's interpretation of 'due aid' expands with need and pressure. The measurement series shows rising extractiveness over the interval (0.58 → 0.82), reflecting the systematization of extraction mechanisms (feudal aids, relief payments, marriage aids, military levies) as the feudal order matures; initially ad hoc, extraction becomes increasingly formalized and predictable from the lord's perspective, enabling more intensive demands. Suppression is high (0.79) because the oath's binding force—sacramental oath-breaking is sin, legal oath-breaking is forfeiture—eliminates exit options and makes resistance costly; any effective vassal resistance is covert (evasion, slowdown, rent seeking) rather than open challenge. Theater ratio is low-moderate (0.28) because while the lord's scribal apparatus maintains elaborate charter records and the Church performs sacramental ceremonies, these are genuine infrastructure, not pure performance; the extractive function does most of the work. Accessibility collapse is substantial (0.72) because once the oath is sworn, alternatives collapse: a vassal cannot simply declare fealty to a competing lord (territorial closure enforces monopoly), cannot appeal to a vassal's charter for rate limits (in the lord_extraction_reading, no such charter exists), and cannot exit without oath-breaking (identity_locked). Resistance is moderate (0.68) because vassals do mount real resistance—through localized rebellion, evasion, and strategic foot-dragging—but the sacramental and legal force of the oath prevents unified, sustained challenge. The asymmetry of power (institutional lord, moderate vassals) and the identity lock mean resistance is episodic rather than structural. The claim/metric gap is intentional: the constraint is CLAIMED as snare (pure extraction, no coordination story) while vassals might claim (in the vassal_coordination_reading) that the oath coordinates a reciprocal relationship. The engine computes which claim the metrics support; the authored divergence is signal.
 *
 * PERSPECTIVAL GAP:
 *   The lord and the vassal should compute radically different constraint types from the engine: From the lord's seat, the oath is a coordination mechanism (assembles military force, organizes labor) with bounded extraction (capacity limit). The lord's anchor is the coordination function—the oath solves the real problem of regional military mobilization—and the extraction is the price of that solution. The engine, reading high extractiveness + high suppression + the lord's power/arbitrage_exit, computes snare (from the lord's seat, snare with beneficiary role). From the vassal's seat, the oath is pure extraction masquerading as reciprocal obligation. The vassal's anchor is the asymmetry of power and the unilaterality of oath interpretation. The engine, reading high extractiveness + high suppression + vassal power/identity_locked_exit, computes snare (from the vassal's seat, snare with payer role). Both seats see snare, but for different reasons: the lord sees it as justified coordination, the vassal sees it as exploitation dressed in reciprocal language. The engine assigns snare to both and flags the consensus as diagnostic—two different seats, same terminal type, but opposite structural readings.
 *
 * DIRECTIONALITY LOGIC:
 *   The territorial_lord has directionality near 0.0 (full beneficiary): they collect the extraction directly, set the oath terms, and exit freely (can reallocate vassals, negotiate with rivals, or switch holdings). The oath_bound_vassals have directionality near 1.0 (full target): they bear the extraction, have identity_locked exit (oath-breaking forecloses livelihood and status), and cannot renegotiate terms. The church and royal authority sit near 0.5 (symmetric): they benefit from the stability the oath provides and bear the cost of enforcing it through sacramental and legal machinery, but do not directly transfer surplus. The competing_lords have identity_trapped directionality (excluded seats don't have d values in the standard sense, but if modeled as constrained, they would be near the target end—they want access to extraction they are barred from). No directionality overrides are needed: the structural data (beneficiaries=[territorial_lord], victims=[oath_bound_vassals], power atoms, exit options) drive the derivation chain cleanly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem was regional military mobilization post-Roman collapse (founding_problem_status=contested). By the measurement interval's end (t=400, representing feudalism fully matured, 10th–12th centuries), the founding problem is DEAD by most external corroboration: regional military mobilization has stabilized (no more chaos from the lack of armies), fortifications are permanent, succession is hereditary and largely predictable. Yet the oath persists, with extraction actually rising (base_extractiveness increases from 0.58 to 0.82). This is textbook mandatrophy: the constraint's founding justification is obsolete, but extraction continues and accelerates. The disappearance_verdict is world_rearranges, meaning the world does depend on the oath—but the dependence is the extraction itself, not the original coordination function. Mandatrophy is resolved by naming this divergence explicitly: the oath authorizes extraction that persists because it serves the lord, not because it solves any live founding problem. The vassal_coordination_reading and ecclesiastical_mediation_reading offer alternative framings where the oath either solves a different founding problem (stable tenure, bounded obligation) or persists under constraints (charity, mediation) that limit extraction. From this reading (lord_extraction_reading), mandatrophy is full: the oath is a zombie mechanism, dead as a solution to its original problem but ambulatory as an extraction machine.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    oath_binding_mechanism_interpretation,
    'Is the oath''s binding force grounded in sacramental obligation (ecclesiastical mediation reading) or in the lord''s unilateral will enforced by legal supremacy (this extraction reading)?',
    'Analysis of charter language, ecclesiastical commentary, and vassal resistance patterns over time. If ecclesiastical authorities successfully limit extraction demands, the sacramental reading gains traction; if lords override ecclesiastical counsel and extract at will, the legal supremacy reading dominates.',
    'If sacramental constraint is real and operative, extraction ceilings exist; if it is ceremonial cover, extraction is bounded only by capacity. Classification diverges from snare to tangled_rope if mediation is genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oath_binding_mechanism_interpretation, conceptual, 'The true source of the oath''s binding force and whether it constrains extraction.').

omega_variable(
    capacity_constraint_as_structural_limit,
    'Is the ''capacity'' limit a genuine floor (vassals can only be extracted from up to subsistence and military readiness), or is it a fiction—a theoretical floor lords breach when military threat rises or succession uncertainty triggers predatory exaction?',
    'Longitudinal analysis of vassal household records (demesne crop yields, military mobilization frequency, tenant flight, and secondary oath-taking by desperate vassals). If capacity is systematically exceeded during crises, it is a myth; if enforced, it is real.',
    'If the capacity constraint is mythical, extraction is purely arbitrary and the snare classification is strengthened; if real and enforced, extraction sits closer to tangled_rope (constrained by a structural fact, not benevolence). This bears on the founding_problem_status: if capacity is fiction, the founding problem (predictable obligation chains) failed early.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(capacity_constraint_as_structural_limit, empirical, 'Whether vassal capacity functions as an actual extraction ceiling or is breached routinely.').

omega_variable(
    reading_kernel_contest,
    'Is the feudal oath a unilateral extraction mechanism (lord_extraction_reading) or does it instantiate reciprocal bounded obligations (vassal_coordination_reading) or mediated constraints (ecclesiastical_mediation_reading)?',
    'This omega documents the contested kernel itself. The three readings are one-constraint-family phenomenon: sibling constraints sharing a fixed referent (the feudal oath) but authored from reading-distinct seats. Resolution requires judging which reading''s ε is empirically true of the standing arrangement.',
    'The classification diverges radically: lord_extraction_reading => snare (high ε, victims, one-way extraction); vassal_coordination_reading => tangled_rope or rope (bounded ε, coordination function, enforcement shared or absent); ecclesiastical_mediation_reading => tangled_rope (mediated extraction, charity constraint). The engine computes per-reading type from per-reading ε; divergence is the measurement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_contest, conceptual, 'The contested kernel: three readings of the feudal oath, three ε values, three classification outcomes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feudal_oath_reciprocity__lord_extraction_reading, 0, 400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t0, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(feud_tr_t0, observed).
narrative_ontology:measurement(feud_tr_t80, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 80, 0.15).
narrative_ontology:measurement_basis(feud_tr_t80, observed).
narrative_ontology:measurement(feud_tr_t160, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 160, 0.18).
narrative_ontology:measurement_basis(feud_tr_t160, observed).
narrative_ontology:measurement(feud_tr_t240, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 240, 0.23).
narrative_ontology:measurement_basis(feud_tr_t240, observed).
narrative_ontology:measurement(feud_tr_t320, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 320, 0.26).
narrative_ontology:measurement_basis(feud_tr_t320, observed).
narrative_ontology:measurement(feud_tr_t400, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 400, 0.28).
narrative_ontology:measurement_basis(feud_tr_t400, observed).

% Extraction over time
narrative_ontology:measurement(feud_be_t0, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(feud_be_t0, observed).
narrative_ontology:measurement(feud_be_t80, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 80, 0.65).
narrative_ontology:measurement_basis(feud_be_t80, observed).
narrative_ontology:measurement(feud_be_t160, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 160, 0.71).
narrative_ontology:measurement_basis(feud_be_t160, observed).
narrative_ontology:measurement(feud_be_t240, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 240, 0.78).
narrative_ontology:measurement_basis(feud_be_t240, observed).
narrative_ontology:measurement(feud_be_t320, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 320, 0.81).
narrative_ontology:measurement_basis(feud_be_t320, observed).
narrative_ontology:measurement(feud_be_t400, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 400, 0.82).
narrative_ontology:measurement_basis(feud_be_t400, observed).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t0, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 0, 0.61).
narrative_ontology:measurement_basis(feud_su_t0, observed).
narrative_ontology:measurement(feud_su_t80, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 80, 0.68).
narrative_ontology:measurement_basis(feud_su_t80, observed).
narrative_ontology:measurement(feud_su_t160, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 160, 0.73).
narrative_ontology:measurement_basis(feud_su_t160, observed).
narrative_ontology:measurement(feud_su_t240, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 240, 0.76).
narrative_ontology:measurement_basis(feud_su_t240, observed).
narrative_ontology:measurement(feud_su_t320, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 320, 0.78).
narrative_ontology:measurement_basis(feud_su_t320, observed).
narrative_ontology:measurement(feud_su_t400, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 400, 0.79).
narrative_ontology:measurement_basis(feud_su_t400, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feudal_oath_reciprocity__lord_extraction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(feudal_oath_reciprocity__lord_extraction_reading, 0.12).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__lord_extraction_reading, feudal_oath_reciprocity__vassal_coordination_reading).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__lord_extraction_reading, feudal_oath_reciprocity__ecclesiastical_mediation_reading).

% DUAL FORMULATION NOTE:
% This constraint (lord_extraction_reading) is one reading of the feudal_oath_reciprocity kernel. The sibling constraints (vassal_coordination_reading and ecclesiastical_mediation_reading) are structurally distinct instantiations of the same feudal oath commitment, each with its own ε, beneficiary/victim structure, and classification. The kernel contest is documented in omega variables and cs_structure.reading_relations. The three constraints are linked by shared kernel identity, not causal dependency; network edges record influence at the legitimacy level—each reading, if ascendant, changes the structural conditions that make the other readings viable.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
