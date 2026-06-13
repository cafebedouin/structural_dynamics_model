% ============================================================================
% CONSTRAINT STORY: feudal_oath_reciprocity__ecclesiastical_mediation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feudal_oath_reciprocity__ecclesiastical_mediation_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: feudal_oath_reciprocity__ecclesiastical_mediation_reading
 *   human_readable: Feudal Oath Reciprocity: Ecclesiastical Mediation Reading
 *   domain: medieval_political_economy/institutional_analysis
 *
 * SUMMARY:
 *   In the ecclesiastical mediation reading of the feudal oath, the
 *   constraint's structure is: lords swear oaths bound not only by mutual
 *   military and economic interest but by Christian sacramental obligation
 *   and the theological principle that all authority (including lordly power)
 *   must answer to moral law as interpreted by the church. This reading
 *   places the church as the authoritative interpreter of what constitutes
 *   just extraction—what is permissible under Christian charity and what
 *   constitutes sinful overreach. The reading establishes a tangled_rope: the
 *   church gains interpretive authority and the power to sanction lords
 *   (beneficiary); vassals gain a theological defense against unlimited
 *   extraction (beneficiary); but lords are constrained from maximal
 *   extraction and must accept ecclesiastical judgment over their personal
 *   power (payer/victim). The constraint persists through active
 *   ecclesiastical enforcement: bishops hear disputes, pronounce judgments,
 *   threaten excommunication, and condition absolution on compliance. The
 *   claim/metric independence rule applies here: this reading is CLAIMED as
 *   genuine tangled_rope (coordination + enforcement + asymmetric extraction)
 *   while the metrics show moderate extraction (0.58), rising over time, with
 *   theater increasing. The measured theater (0.41 at interval end, rising to
 *   0.44 by midpoint) suggests that ecclesiastical performance of the moral
 *   limit is becoming increasingly theatrical—the judgment is delivered but
 *   the extraction keeps rising, and ecclesiastical authority is spending
 *   more energy on ritual pronouncement than on actual constraint. This
 *   divergence between claim and measurement is the point: the engine detects
 *   whether the reading's structure matches its operation.
 *
 * KEY AGENTS:
 *   - ecclesiastical_authority (institutional agenda_setter): church as interpreter of Christian charity, enforcer of sacramental oath limits, beneficiary of interpretive monopoly
 *   - lord_warrior_class (powerful payer): extraction capacity constrained by theological limits, exit options limited by legitimacy and vassal rebellion risk
 *   - vassal_subject_class (moderate beneficiary/payer): gains protection against unlimited extraction via ecclesiastical appeal, remains obligated to provide rents and service
 *   - king_or_paramount_lord (institutional observer): holds ambivalent position—ecclesiastical limits on sub-lords serve paramount interest but also constrain paramount power
 *   - secular_legal_tradition (moderate excluded): pushed aside when ecclesiastical frame becomes authoritative; secular charter law loses precedence over theological reasoning
 *   - heterodox_or_dissenting_voices (powerless excluded): suppressed via heresy charges and censorship when they offer alternative readings of the oath
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.58).
domain_priors:suppression_score(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.62).
domain_priors:theater_ratio(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feudal_oath_reciprocity__ecclesiastical_mediation_reading, tangled_rope).
narrative_ontology:human_readable(feudal_oath_reciprocity__ecclesiastical_mediation_reading, "Feudal Oath Reciprocity: Ecclesiastical Mediation Reading").
narrative_ontology:topic_domain(feudal_oath_reciprocity__ecclesiastical_mediation_reading, "medieval_political_economy/institutional_analysis").

domain_priors:requires_active_enforcement(feudal_oath_reciprocity__ecclesiastical_mediation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feudal_oath_reciprocity__ecclesiastical_mediation_reading, '58792980-8666-446e-a6b5-830453d6aa1a').
narrative_ontology:cs_kernel_codification('58792980-8666-446e-a6b5-830453d6aa1a', fixed_text).
narrative_ontology:cs_authority_grounding('58792980-8666-446e-a6b5-830453d6aa1a', lineage).
narrative_ontology:cs_interpretation_layer_present('58792980-8666-446e-a6b5-830453d6aa1a').
narrative_ontology:cs_reading_relation('58792980-8666-446e-a6b5-830453d6aa1a', feudal_oath_reciprocity__lord_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('58792980-8666-446e-a6b5-830453d6aa1a', feudal_oath_reciprocity__vassal_coordination_reading, coexists_with).
narrative_ontology:cs_axiom('58792980-8666-446e-a6b5-830453d6aa1a', foundational, sacramental_oath_theological_subordination).
narrative_ontology:cs_axiom_status(sacramental_oath_theological_subordination, holdable).
narrative_ontology:cs_axiom_grounding('58792980-8666-446e-a6b5-830453d6aa1a', sacramental_oath_theological_subordination, theological).
narrative_ontology:cs_axiom('58792980-8666-446e-a6b5-830453d6aa1a', foundational, ecclesiastical_authority_moral_adjudication).
narrative_ontology:cs_axiom_status(ecclesiastical_authority_moral_adjudication, holdable).
narrative_ontology:cs_axiom_grounding('58792980-8666-446e-a6b5-830453d6aa1a', ecclesiastical_authority_moral_adjudication, deontological).
narrative_ontology:cs_reference_frame('58792980-8666-446e-a6b5-830453d6aa1a', christian_charity_subordination_to_authority).
narrative_ontology:cs_drift_state('58792980-8666-446e-a6b5-830453d6aa1a', high_medieval_consolidation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('58792980-8666-446e-a6b5-830453d6aa1a', '').
narrative_ontology:cs_kernel_id(feudal_oath_reciprocity__ecclesiastical_mediation_reading, feudal_oath_reciprocity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__ecclesiastical_mediation_reading, ecclesiastical_authority).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__ecclesiastical_mediation_reading, vassal_subject_class).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__ecclesiastical_mediation_reading, lord_warrior_class).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__ecclesiastical_mediation_reading, vassal_subject_class).
narrative_ontology:constraint_vindicates(feudal_oath_reciprocity__ecclesiastical_mediation_reading, theological_limits_secular_power).
narrative_ontology:constraint_vindicates(feudal_oath_reciprocity__ecclesiastical_mediation_reading, sacramental_oath_binding).
narrative_ontology:constraint_vindicates(feudal_oath_reciprocity__ecclesiastical_mediation_reading, christian_charity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The church interprets the feudal oath through theological frameworks: Christian charity, sacramental obligation, and the principle that all authority derives from God and must answer to moral law. Bishops and papal legates sit as arbiters in disputes, asserting that lords who extract beyond theological limits (those permissible under Christian charity) violate their oath before God. The church gains authority by being the keeper of moral standards and the interpreter of what constitutes just extraction.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, ecclesiastical_authority, agenda_setter,
    institutional, generational, analytical, continental).

% Lords swear oaths bound by Christian charity and sacramental obligation. They are constrained from extracting beyond what the ecclesiastical interpretation deems theologically permissible. Their exit options are limited: they can defy the church (risking excommunication, loss of legitimacy, and vassal rebellion), renegotiate the oath (costly and publicly humiliating), or accept the constraint. The theological framing limits their freedom to maximize extraction even where military or economic power would permit it.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, lord_warrior_class, payer,
    powerful, biographical, constrained, regional).

% Vassals and commons gain a theological defense against unlimited extraction. If a lord exceeds the bounds of Christian charity, they can appeal to ecclesiastical authority for redress. They still pay obligations, but the ceiling on extraction is lower than it would be under pure lord power. Their benefit is asymmetric: they escape the worst predation but remain obligated to provide service and rents. Exit is trapped—flight or rebellion is the only alternative—but ecclesiastical mediation offers some protection within the framework.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, vassal_subject_class, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(feudal_oath_reciprocity__ecclesiastical_mediation_reading, vassal_subject_class, payer).

% The paramount authority (king, emperor, or highest feudal lord) observes the church's interpretive role with ambivalence. Ecclesiastical limits on lords' extraction can serve the paramount power's interest in preventing sub-lords from becoming too rich or powerful, but it also constrains the paramount power itself. The observer seat holds no direct stake but influences how the constraint evolves through recognition or challenge of ecclesiastical authority.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, king_or_paramount_lord, observer,
    institutional, generational, analytical, national).

% Secular law—customary law, charter law, Roman law traditions—are pushed aside when ecclesiastical interpretation of the oath becomes the authoritative frame. Secular law specialists and charter keepers who might argue for fixed, written bounds on extraction find their claims superseded by theological reasoning. They are excluded from the conversation when the church frames the oath as a matter of spiritual obligation.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, secular_legal_tradition, excluded,
    moderate, generational, analytical, regional).

% Heterodox clergy, popular preachers, or dissidents who read the oath differently—who argue that vassals owe nothing beyond survival subsistence, or that lords owe active charity—are suppressed. Their exclusion is structural: heresy charges, censorship, and loss of pulpit access prevent their reading of the oath from reaching the population.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, heretical_or_heterodox_voices, excluded,
    powerless, biographical, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a three-party arrangement in which lords swear oaths to provide justice and protection to vassals; vassals swear oaths to provide service and rents; and the church sits as the authoritative interpreter of what constitutes just extraction and charitable obligation. This solves the problem of how to prevent a lord from simply seizing all vassals' surplus while claiming feudal right.
% TRANSFER_FUNCTION: Moves a bounded share of vassal surplus to the lord (as rents and service), as the price of justice and protection. The bounds are set by ecclesiastical interpretation of Christian charity doctrine. Additional extraction beyond the theological limit is classified as sinful and is subject to ecclesiastical sanction and potential vassal appeal.
% ABSENT_VOICES: Vassals without access to ecclesiastical ear, peasants below the feudal hierarchy, heterodox clergy or preachers who read the oath differently, and secular legal specialists who argue for written, fixed bounds instead of interpretive bounds.
% DISAPPEARANCE_RATIONALE: If ecclesiastical mediation of the oath vanished, the constraint on extraction would collapse. Lords would face only the physical and military limit of vassal rebellion, which is a much higher ceiling. Extraction would rise sharply unless replaced by another enforcement mechanism (written law, royal enforcement, stronger vassal coalitions). The ecclesiastical frame is essential to maintaining the current ceiling; without it, the arrangement reverts to pure power.
% FOUNDING_PROBLEM: Early feudal arrangements lacked a principle limiting lordly extraction. Lords with military power could seize as much as vassals could survive producing. The lack of an agreed-upon ceiling created chronic instability: vassals would rebel or flee if extraction became intolerable, lords would lose revenue and military support, and the whole social order became fragile. The church offered a solution: frame the oath as a Christian sacrament and subordinate extraction to theological limits (Christian charity, just price, subsistence rights).
% FOUNDING_PROBLEM_CORROBORATION: Church authorities (bishops, canonists, papal legates) attest the founding problem as persistent: lords continually press extraction beyond theological bounds and must be reminded of Christian limits. Vassals and subject peoples attest the problem is real and the ecclesiastical frame provides some protection. Lords and secular law keepers attest the founding problem has been solved by custom and charter; the ecclesiastical interpretation is painted as overreach. Independent historical analysis supports that ecclesiastical mediation did reduce extraction volatility in regions where church authority was strong, though the effect was partial and contested.
narrative_ontology:disappearance_verdict(feudal_oath_reciprocity__ecclesiastical_mediation_reading, world_rearranges).
narrative_ontology:founding_problem_status(feudal_oath_reciprocity__ecclesiastical_mediation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feudal_oath_reciprocity__ecclesiastical_mediation_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feudal_oath_reciprocity__ecclesiastical_mediation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(feudal_oath_reciprocity__ecclesiastical_mediation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(feudal_oath_reciprocity__ecclesiastical_mediation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58 at interval end, rising from 0.48) because the ecclesiastical reading does impose a ceiling on extraction—lords cannot simply seize all vassal surplus—but the ceiling is permeable. The church gains authority to interpret and enforce the limit, but that authority is exercised selectively and is subject to capture by lordly patronage. The measurement series shows extraction rising over the interval despite ecclesiastical mediation, suggesting the constraint's limiting power weakens over time or that extraction gradually exceeds the theological bounds while maintaining theological cover. Suppression is measured at 0.62 and relatively stable, reflecting the ongoing need for ecclesiastical enforcement machinery (hearing disputes, pronouncing judgment, managing dissent). Theater ratio rises from 0.30 to 0.44 (interval peak) before declining to 0.41 at the interval end, suggesting an oscillating cycle: periods of heightened ecclesiastical performance (theater peak) alternate with periods of reduced enforcement intensity. This oscillation itself may be an extraction mechanism: intermittent enforcement allows lords to test the boundary, extract temporarily, face ecclesiastical response, then retrench temporarily—a rhythm of tension and appeasement that keeps the population off-balance. The accessibility_collapse metric (0.68) reflects that once the ecclesiastical framing becomes hegemonic, alternatives (secular contract law, lord-vassal direct negotiation, vassal coalition power) collapse—vassals are locked into appeal-to-church as their only recourse. Resistance remains moderate (0.54) because resistance exists (lords push boundaries, vassals occasionally rebel, secular lawyers argue for written law) but is not sufficient to dislodge the constraint's core structure. The time grid is shared across all metrics; no metric has a per-metric subinterval.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between seats is substantial and deliberate. From the ecclesiastical seat, the constraint is coordination with enforcement: the church solves the problem of unchecked lordly power by providing an authoritative moral frame and the institutional power to sanction violation. From the lord seat, the constraint is extraction—the church appropriates authority over what the lord can extract and uses that authority to extract concessions, endowments, and deference. From the vassal seat, the constraint is a partial protection: better than facing a lord with no limit, but the limit is soft and the church can be captured or can reinterpret doctrine upward. From the secular legal tradition seat, the constraint is displacement: theological reasoning crowds out written law, and the contract-like specificity of charter law loses precedence to ecclesiastical judgment. The engine computes these divergent types from the structural data (beneficiary/victim, power, exit_options, enforcement requirement); the perspectival gap explains why the same structural arrangement looks like different things from different seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Ecclesiastical authority is the clear beneficiary: it gains interpretive monopoly over the oath's meaning, earns concessions and endowments from lords seeking favorable interpretations, and gains the power to sanction non-compliance. Its directionality is d ≈ 0.15 (beneficiary end): it collects from the arrangement without being constrained by it. Lords are the primary payers: they lose extraction capacity and must accept ecclesiastical judgment over disputes. Their directionality is d ≈ 0.78 (target end): they bear costs (constrained extraction, ecclesiastical interference, loss of sovereignty) in exchange for legitimacy and the coordination benefit of stable vassal relationships. Vassals are mixed: they benefit from protection against unlimited extraction (d ≈ 0.35, beneficiary-leaning) but still pay rents and service (d also includes payer costs). Paramountcy (king) is observer: no direct stake but indirect benefit from preventing sub-lord consolidation. The beneficiary declaration includes both ecclesiastical_authority (clear) and vassal_subject_class (secondary, because they escape the worst predation—this is not a standard beneficiary-of-collection situation but a beneficiary-from-constraint-limiting-someone-else's-power). The victim declaration is lord_warrior_class (constrained, loses extraction capacity). No directionality overrides are needed: the derivation chain (beneficiary/victim + power + exit_options) produces accurate d values without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids false classification by correctly naming both the coordination function (ecclesiastical adjudication of just extraction limits) AND the asymmetric extraction function (church gains authority and extracts concessions). A false mandatrophy scenario would claim the constraint is purely coordination (rope) based on the founding problem (need to limit extraction), but the metrics show enforcement, theater, and asymmetry (extraction from lords via ecclesiastical authority). The tangled_rope classification prevents this error. The measurement series showing rising extraction despite ecclesiastical mediation suggests the constraint may be approaching mandatrophy: the founding problem (unchecked lordly extraction) persists, but the ecclesiastical solution is increasingly theatrical (theater ratio rising). The contested founding_problem_status and absence of corroboration from outside the benefiting parties (ecclesiastical authority's own attestation carries no weight; only secular lawyers and vassal testimony matter) flags a mandatrophy risk. If extraction continues rising while ecclesiastical enforcement becomes more theatrical, the constraint may be reclassified as piton or snare depending on whether the church still collects rents (snare) or merely performs enforcement (piton).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ecclesiastical_authority_capture,
    'Does ecclesiastical authority genuinely enforce theological limits on extraction, or does it become captured by lords who offer land and endowments in exchange for favorable interpretations?',
    'Historical analysis of ecclesiastical judgments in disputes: do bishops rule systematically in favor of lords who have granted them land? Do their interpretations of Christian charity drift toward greater permissiveness when lords are wealthy patrons?',
    'If capture is substantial, the constraint devolves into theater: the church''s interpretive authority becomes a cover for continued high extraction (moving toward snare or piton classification). If ecclesiastical independence holds, the constraint operates as genuinely limiting (tangled_rope holds).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecclesiastical_authority_capture, empirical, 'Whether ecclesiastical authority is independent or captured by lordly patronage.').

omega_variable(
    theological_charity_doctrine_elasticity,
    'Is Christian charity doctrine sufficiently specific and stable to function as a binding limit on extraction, or is it so elastic that lords can reinterpret it to justify any extraction they choose?',
    'Comparison of ecclesiastical rulings over time: do interpretations of what constitutes Christian charity remain consistent, or do they shift to accommodate rising extraction? Analysis of canonical texts and glosses for definitional precision.',
    'High elasticity would reduce the constraint''s effective limiting power: extraction could rise while claiming theological justification (theater ratio rising, suppression increasing). Low elasticity would support the constraint''s function as a genuine ceiling.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theological_charity_doctrine_elasticity, conceptual, 'Whether Christian charity doctrine provides a stable interpretive standard for bounding extraction.').

omega_variable(
    sibling_reading_contestation,
    'Within a single diocese or realm, do lords, vassals, and church authorities all agree this reading of the oath (ecclesiastical mediation via charity) is the authoritative one, or are the sibling readings (lord_extraction_reading, vassal_coordination_reading) actively contested?',
    'Documentary evidence: do chronicles, charters, and dispute records show consistent application of this reading, or do different parties invoke different frames? Are disputes framed in ecclesiastical charity language or in secular contract language?',
    'If all parties recognize the ecclesiastical reading as authoritative, the constraint''s structure is stable. If sibling readings are live alternatives, the constraint''s operation is contested and may be unstable: lords deploy lord_extraction_reading, vassals deploy vassal_coordination_reading, church deploys this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_contestation, empirical, 'Whether the ecclesiastical mediation reading is hegemonic or contested against sibling readings in actual practice.').

omega_variable(
    suppression_internalization_mechanism,
    'Is the measured suppression (0.62) structural (ecclesiastical sanctions like excommunication, loss of legitimacy) or internalized (lords and vassals genuinely believe in Christian charity doctrine and self-suppress extraction)?',
    'Post-contestation analysis: where ecclesiastical authority weakens or a lord explicitly rejects the church''s interpretation, does suppression persist? If lords who escape ecclesiastical oversight maintain moderate extraction, suppression is partly internalized; if they immediately maximize extraction, suppression is mostly structural.',
    'Structural suppression would weaken if ecclesiastical authority declines. Internalized suppression is more durable but harder to enforce. Mixed suppression (most likely) means the constraint''s durability depends on maintaining ecclesiastical authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_mechanism, empirical, 'Whether suppression is enforced by external ecclesiastical sanction or internalized belief.').

omega_variable(
    secular_law_displacement,
    'Does this reading''s dominance (ecclesiastical mediation via charity) displace secular law (charter texts, customary law, written obligations) as the operative frame for the oath, or do both frames coexist?',
    'Document analysis: are disputes resolved by appeal to the charter''s written terms, by ecclesiastical judgment, or by both? Does one frame systematically override the other?',
    'If ecclesiastical framing displaces secular law, the church gains monopoly interpretive authority and the constraint is highly dependent on ecclesiastical strength. If both coexist, there are multiple frames available (reducing the church''s gatekeeping power and potentially enabling lord or vassal escape). Coexistence weakens this reading''s dominance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secular_law_displacement, empirical, 'Whether ecclesiastical mediation replaces or coexists with secular legal frames.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t0, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement_basis(feud_tr_t0, observed).
narrative_ontology:measurement(feud_tr_t5, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 5, 0.33).
narrative_ontology:measurement_basis(feud_tr_t5, observed).
narrative_ontology:measurement(feud_tr_t10, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement_basis(feud_tr_t10, observed).
narrative_ontology:measurement(feud_tr_t15, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement_basis(feud_tr_t15, observed).
narrative_ontology:measurement(feud_tr_t20, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(feud_tr_t20, observed).
narrative_ontology:measurement(feud_tr_t25, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(feud_tr_t25, observed).
narrative_ontology:measurement(feud_tr_t30, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 30, 0.44).
narrative_ontology:measurement_basis(feud_tr_t30, observed).
narrative_ontology:measurement(feud_tr_t40, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(feud_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(feud_be_t0, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(feud_be_t0, observed).
narrative_ontology:measurement(feud_be_t5, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(feud_be_t5, observed).
narrative_ontology:measurement(feud_be_t10, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement_basis(feud_be_t10, observed).
narrative_ontology:measurement(feud_be_t15, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 15, 0.57).
narrative_ontology:measurement_basis(feud_be_t15, observed).
narrative_ontology:measurement(feud_be_t20, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 20, 0.59).
narrative_ontology:measurement_basis(feud_be_t20, observed).
narrative_ontology:measurement(feud_be_t25, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 25, 0.6).
narrative_ontology:measurement_basis(feud_be_t25, observed).
narrative_ontology:measurement(feud_be_t30, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 30, 0.61).
narrative_ontology:measurement_basis(feud_be_t30, observed).
narrative_ontology:measurement(feud_be_t40, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement_basis(feud_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t0, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(feud_su_t0, observed).
narrative_ontology:measurement(feud_su_t5, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement_basis(feud_su_t5, observed).
narrative_ontology:measurement(feud_su_t10, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement_basis(feud_su_t10, observed).
narrative_ontology:measurement(feud_su_t15, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement_basis(feud_su_t15, observed).
narrative_ontology:measurement(feud_su_t20, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 20, 0.63).
narrative_ontology:measurement_basis(feud_su_t20, observed).
narrative_ontology:measurement(feud_su_t25, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 25, 0.64).
narrative_ontology:measurement_basis(feud_su_t25, observed).
narrative_ontology:measurement(feud_su_t30, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 30, 0.65).
narrative_ontology:measurement_basis(feud_su_t30, observed).
narrative_ontology:measurement(feud_su_t40, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 40, 0.62).
narrative_ontology:measurement_basis(feud_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feudal_oath_reciprocity__ecclesiastical_mediation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.18).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__ecclesiastical_mediation_reading, feudal_oath_reciprocity__lord_extraction_reading).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__ecclesiastical_mediation_reading, feudal_oath_reciprocity__vassal_coordination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the feudal oath kernel. The sibling readings (lord_extraction_reading, vassal_coordination_reading) instantiate the same oath through different frames: lord extraction authorizes maximal extraction bounded only by vassal capacity (high ε snare); vassal coordination frames the oath as fixed reciprocal obligations in charter text (low ε rope). All three readings are live in medieval political practice—different parties invoke different frames in the same disputes. The ecclesiastical reading's dominance depends on church authority, which varies geographically and temporally. Where church authority is strong, this reading's tangled_rope structure holds. Where secular law dominates, the vassal_coordination_reading supplants it. Where lordly power dominates enforcement, the lord_extraction_reading emerges. This is not contradiction within a single constraint—it is three distinct constraints instantiating one contested kernel. See commentary.kernel_context for the full decomposition rationale.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
