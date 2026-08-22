% ============================================================================
% CONSTRAINT STORY: nicene_christological_kernel__homoousios_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nicene_christological_kernel__homoousios_reading, []).

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
 *   constraint_id: nicene_christological_kernel__homoousios_reading
 *   human_readable: Homoousios Settlement: Enforced Consubstantiality of the Son
 *   domain: historical_theology/ecclesiastical_authority
 *
 * SUMMARY:
 *   After Nicaea (325) defined the Son as homoousios with the Father, the
 *   formula became the enforced boundary of legitimate Christianity within
 *   the Roman Empire: anathema against contrary confessions, deposition and
 *   exile of dissenting bishops, confiscation of their churches, and finally
 *   the Edict of Thessalonica (380) making Nicene Christianity the state
 *   religion. The arrangement solved a genuine coordination problem — one
 *   confession for a movement no regional formula could hold together — while
 *   extracting conformity through machinery whose benefits concentrated in
 *   the episcopal-imperial establishment and whose costs fell on Homoian
 *   clergy, Gothic Christian communities, regional congregations, and the
 *   laity caught between factions. The ε referent is the standing enforced
 *   arrangement itself, not the creed's endorsed ideal: this story measures
 *   the settlement as it operated. Claim/metric independence is preserved:
 *   claimed_type tangled_rope is authored from the structural judgment that
 *   genuine coordination and asymmetric extraction coexist in one enforced
 *   structure; the metrics are authored descriptively from the historical
 *   record. This file is ONE READING of the nicene_christological_kernel; the
 *   sibling homoiousios_reading is a separate constraint with its own ε,
 *   victim set, and enforcement history, linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - - pro_nicene_episcopal_establishment: Agenda-setting beneficiary (institutional/identity_locked) — sets the doctrinal standard, administers enforcement, collects sees and property
 *   - - imperial_government: Co-agenda-setter and indirect beneficiary (institutional/arbitrage) — supplies coercive force, switches formulas when politically convenient
 *   - - homoian_clergy: Primary payer (organized/trapped) — bears deposition, exile, and confiscation inside the empire
 *   - - gothic_arian_communities: Payer outside the imperial core (organized/identity_locked) — frozen into a heretical caste by the confessional divide
 *   - - north_african_provincial_communities: Regional payer (moderate/constrained) — absorbs uniformity costs and metropolitan discipline
 *   - - ordinary_laity: Diffuse payer and incidental beneficiary (powerless/trapped) — receives the shared creed, bears compelled subscription and factional violence
 *   - - ecumenical_historians: Analytical observer (analytical/analytical) — sees the full structure across both readings' tenures
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_christological_kernel__homoousios_reading, 0.72).
domain_priors:suppression_score(nicene_christological_kernel__homoousios_reading, 0.76).
domain_priors:theater_ratio(nicene_christological_kernel__homoousios_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_christological_kernel__homoousios_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(nicene_christological_kernel__homoousios_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(nicene_christological_kernel__homoousios_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nicene_christological_kernel__homoousios_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(nicene_christological_kernel__homoousios_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_christological_kernel__homoousios_reading, tangled_rope).
narrative_ontology:human_readable(nicene_christological_kernel__homoousios_reading, "Homoousios Settlement: Enforced Consubstantiality of the Son").
narrative_ontology:topic_domain(nicene_christological_kernel__homoousios_reading, "historical_theology/ecclesiastical_authority").

domain_priors:requires_active_enforcement(nicene_christological_kernel__homoousios_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_christological_kernel__homoousios_reading, '5019e1f2-1abc-463b-bfad-56cf40350e59').
narrative_ontology:cs_kernel_codification('5019e1f2-1abc-463b-bfad-56cf40350e59', formalized).
narrative_ontology:cs_authority_grounding('5019e1f2-1abc-463b-bfad-56cf40350e59', lineage).
narrative_ontology:cs_interpretation_layer_present('5019e1f2-1abc-463b-bfad-56cf40350e59').
narrative_ontology:cs_reading_relation('5019e1f2-1abc-463b-bfad-56cf40350e59', nicene_christological_kernel__homoiousios_reading, forecloses).
narrative_ontology:cs_axiom('5019e1f2-1abc-463b-bfad-56cf40350e59', foundational, son_consubstantial_with_father).
narrative_ontology:cs_axiom_status(son_consubstantial_with_father, holdable).
narrative_ontology:cs_axiom_grounding('5019e1f2-1abc-463b-bfad-56cf40350e59', son_consubstantial_with_father, theological).
narrative_ontology:cs_axiom('5019e1f2-1abc-463b-bfad-56cf40350e59', secondary, salvation_requires_divine_savior).
narrative_ontology:cs_axiom_status(salvation_requires_divine_savior, holdable).
narrative_ontology:cs_axiom_grounding('5019e1f2-1abc-463b-bfad-56cf40350e59', salvation_requires_divine_savior, theological).
narrative_ontology:cs_reference_frame('5019e1f2-1abc-463b-bfad-56cf40350e59', consensual_apostolic_confession).
narrative_ontology:cs_drift_state('5019e1f2-1abc-463b-bfad-56cf40350e59', post_theodosian_coercive_settlement, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5019e1f2-1abc-463b-bfad-56cf40350e59', '').
narrative_ontology:cs_kernel_id(nicene_christological_kernel__homoousios_reading, nicene_christological_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_christological_kernel__homoousios_reading, pro_nicene_episcopal_establishment).
narrative_ontology:constraint_beneficiary(nicene_christological_kernel__homoousios_reading, imperial_government).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoousios_reading, homoian_clergy).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoousios_reading, gothic_arian_communities).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoousios_reading, north_african_provincial_communities).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoousios_reading, ordinary_laity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nicene_christological_kernel__homoousios_reading, ordinary_laity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafts the creedal formula at Nicaea and Constantinople, defines the anathemas, and administers the boundary of communion: dissenting bishops are deposed, their sees and basilicas transferred to compliant clergy, their persons exiled by imperial rescript. The establishment's own office-warrant is fused to the formula it enforces — a bishop who conceded mere similarity of substance would dissolve the ground of his own authority. Its episodic costs (Athanasius's five exiles) were borne for the formula's content while out of power, not by the arrangement's operation once seated.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, pro_nicene_episcopal_establishment, agenda_setter,
    institutional, generational, identity_locked, continental).

% Supplies the coercive force the conciliar verdicts lack: exile decrees, confiscation orders, and the Edict of Thessalonica making Nicene Christianity the state religion. Collects a unified cult as an instrument of administrative cohesion. Demonstrates arbitrage exit repeatedly — Constantius II reversed enforcement and backed Homoian formulas for two decades, then Theodosius reversed it back — treating the formula's content as negotiable so long as some uniform confession serves the state.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, imperial_government, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(nicene_christological_kernel__homoousios_reading, imperial_government, beneficiary).

% Carries the rival reading institutionally: bishops and presbyters around Eudoxius, Demophilus, and the wider Homoian network who confess similarity rather than identity of substance. Inside the empire after 381 they face deposition, exile, and loss of churches with no tolerated place to stand; before 381 they held the enforcement seat themselves and directed the same machinery at pro-Nicene holders. Their organizational networks were real but could not survive dispossession inside imperial territory.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, homoian_clergy, payer,
    organized, biographical, trapped, continental).

% Converted by Ulfila's mission to Homoian Christianity before the Nicene settlement hardened, then frozen outside the imperial church's communion for two centuries. Their Gothic-language liturgy and clergy constitute Gothic peoplehood itself, so conforming to the homoousios standard would mean cultural dissolution, not mere doctrinal adjustment. They bear the costs of the divide: exclusion from Catholic office, Justinian's persecution in Italy and Africa, and finally coerced conversion at Toledo in 589. Their military-political power was real but could not purchase doctrinal recognition.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, gothic_arian_communities, payer,
    organized, generational, identity_locked, continental).

% Regional congregations whose local liturgical customs, disciplinary traditions, and clerical appointments are subordinated to metropolitan courts operating under the conciliar standard. They absorb the uniformity costs — loss of regional variation, subjection to distant adjudication — while receiving the shared creed's benefits only incidentally. Exit means schism, which the enforcement machinery treats as heresy or disorder and answers with the same instruments used against doctrinal dissent.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, north_african_provincial_communities, payer,
    moderate, biographical, constrained, regional).

% Receive the coordinated goods: one baptismal confession, one liturgy, legible membership boundaries across a vast movement. Bear the costs diffusely: compelled creedal subscription, neighborhoods split by factional allegiance, and episodic violence between confessional mobs in cities like Alexandria and Constantinople. Baptismal membership leaves no denominational exit; their latent coalition power surfaced only as riot, which authorities read as disorder rather than voice.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, ordinary_laity, payer,
    powerless, immediate, trapped, continental).
narrative_ontology:stakeholder_secondary_role(nicene_christological_kernel__homoousios_reading, ordinary_laity, beneficiary).

% Reconstruct the full structure from surviving acta, imperial rescripts, exile lists, and both parties' polemics. They see the coordination function and the extraction machinery in the same documents, and see the sibling reading's tenure of the same machinery during the Homoian ascendancy, which neither party's self-account records.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, ecumenical_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nicene_christological_kernel__homoousios_reading, pro_nicene_episcopal_establishment).
narrative_ontology:fixing_cost_class(nicene_christological_kernel__homoousios_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real dispute-resolution and boundary problem: a movement spanning Greek, Latin, Coptic, Syriac, and Gothic speech needed one answer to what it confesses about Christ — one baptismal formula, one criterion of communion, one procedure (the ecumenical council) for settling christological questions no regional church could settle alone.
% TRANSFER_FUNCTION: Moves doctrinal authority, sees, basilicas, and imperial patronage toward the conciliar-episcopal center: dissenting clergy lose office, property, and civic standing; conformity and institutional resources flow inward; the imperial state receives a unified cult as an instrument of cohesion.
% ABSENT_VOICES: Dissenting presbyters and the laypeople expelled from communion with them had no seat at any council; Gothic Christians were condemned by assemblies they never attended; women and non-clerics — the majority of the baptized — voted nowhere. They would object that the settlements' unanimity was manufactured by excluding them, and their absence is located in the conciliar process itself: episcopal, male, and imperial-chartered.
% DISAPPEARANCE_RATIONALE: If the enforced settlement vanished overnight, the church would reorganize around competing regional formulas — as it visibly did during the Homoian ascendancy of 337–380, when sees, liturgies, and alliances rearranged along confessional lines within a decade. Imperial religious policy would lose its cohesion instrument, and the later confessional map of Europe and North Africa — including the very existence of a Gothic Homoian church — presupposes the divide this settlement drew.
% FOUNDING_PROBLEM: The Arian crisis: whether the Son is God or creature. The church needed one answer to preserve the worship of Christ within monotheism, the coherence of baptismal invocation of Father, Son, and Spirit, and communal unity across a linguistically diverse and rapidly expanding movement.
% FOUNDING_PROBLEM_CORROBORATION: Sources outside the benefiting parties corroborate the problem's reality and severity: Ammianus Marcellinus, a pagan soldier, records the disputes convulsing entire cities; modern patristic scholarship documents the soteriological argument being made before imperial involvement; the Homoian parties' own counter-formulas attest the dispute was live. What no outside source corroborates is that coercion was the necessary remedy — that inference comes only from the parties that collected the enforcement's benefits.
narrative_ontology:disappearance_verdict(nicene_christological_kernel__homoousios_reading, world_rearranges).
narrative_ontology:founding_problem_status(nicene_christological_kernel__homoousios_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nicene_christological_kernel__homoousios_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nicene_christological_kernel__homoousios_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nicene_christological_kernel__homoousios_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nicene_christological_kernel__homoousios_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nicene_christological_kernel__homoousios_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nicene_christological_kernel__homoousios_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72) because the enforcement's costs were decoupled from the creed's pastoral function: deposition, exile, and confiscation fell on dissent as such, not on any harm dissent produced. Suppression is higher still (0.76) because the arrangement's persistence depended on imperial coercion and exclusion from communion and civic life, not on participant preference — every attempt to hold the church together without coercion failed within a decade. Theater is modest (0.28): the stakes were real and the enforcement functional, though the performative share rises late in the interval as ritualized creedal recitation outlives active opposition. Accessibility collapse is 0.62: alternatives collapsed almost completely inside the empire but survived for two centuries in Gothic kingdoms, so the constraint never achieved natural-law closure. Resistance is 0.68: decades of open episcopal defiance, urban riots, and armed Gothic refusal to convert. The measurement series runs on one shared grid (325, 355, 380, 450, 520, 589) with every tracked metric authored at every point. The mid-interval dip (355) is not noise: under Constantius II the enforcement seat inverted and this reading's machinery stood down while its holders were persecuted — an abeyance phase driven by imperial arbitration of the dispute, not intermittent reinforcement; the oscillation is a side effect of the arbitrage exit available to the imperial seat. End-state values match base_properties.
 *
 * PERSPECTIVAL GAP:
 *   From the episcopal seat the arrangement computes as defense of the gospel: the coordination function dominates, and the extraction reads as necessary discipline against corruption of the saving truth. From the Homoian and Gothic seats the identical structure operates as coerced conformity that destroyed legitimate theology and froze their communities outside the church's communion for generations. From the imperial seat it is an instrument of cohesion whose doctrinal content is negotiable — hence the arbitrage exit and the repeated reversals. The engine computes this per-seat divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The episcopal establishment sits nearest the beneficiary end: it drafts the standard, runs the machinery, and receives the confiscated sees — its identity lock amplifies rather than dampens its stake. The imperial government is dual-positioned: it collects the cohesion benefit (low-d pull) while paying administrative and legitimacy costs (mid-d pull), landing well short of full beneficiary. Homoian clergy, Gothic communities, provincial congregations, and the laity sit near the target end — trapped or identity-locked exits push them toward full-target treatment. The laity's secondary beneficiary role (they do receive the coordinated creed) damps their directionality slightly below the clerical payers'. Scope is continental, which scales effective extraction upward for the trapped target seats: verification of conformity across the empire's breadth is exactly what the enforcement machinery existed to do.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents two symmetric mislabels. Coding this as pure snare would erase the genuine coordination function: a single confession really did solve a dispute-resolution problem that fragmented the movement whenever left to regional formulas, and the creed's liturgical and catechetical work was real. Coding it as pure rope would erase the asymmetric extraction: the same structure that coordinated worship confiscated the property of those who confessed differently, and the coordination story cannot account for why similarity-of-substance Christians owed exile. On mandatrophy: the founding mandate — hold the fourth-century church together against christological dissolution — was substantially accomplished by 381; thereafter the enforcement machinery persisted by inertia and acquired new targets (the Germanic kingdoms), the classic gradient toward piton in the post-450 stretch. mandatrophy_resolved is declared accordingly: the enforcement-specific mandate outlived the crisis that justified it even as the creed itself remained live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment_structure,
    'This constraint is one reading (homoousios_reading) of the nicene_christological_kernel; what would the sibling homoiousios_reading change structurally if it had held the enforcement seat?',
    'Counterfactual institutional analysis comparing enforcement patterns under regimes where the Homoian formula held the imperial seat (Constantius II, Valens) with regimes where homoousios held it.',
    'If the sibling reading enforced comparable uniformity when seated, the measured extraction attaches to the enforcement seat rather than to the consubstantiality content, and this story''s epsilon is partly seat-attributed rather than reading-attributed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_commitment_structure, conceptual, 'Committer structure: one reading of the Nicene kernel; sibling-reading counterfactual for extraction attribution.').

omega_variable(
    doctrine_content_vs_enforcement_form,
    'Is the measured extraction attributable to the homoousios content itself, or to the imperial-enforcement form that any victorious reading of the kernel would have taken?',
    'Compare extraction profiles of the same reading with versus without imperial backing (pre-325 Alexandrian debates versus the post-380 settlement), and of the rival reading during its tenure of the machinery.',
    'If extraction rides the enforcement form, the constraint''s effective extraction is overstated relative to the doctrine; if the content required the form — because full consubstantiality admitted no negotiated middle — the extraction is intrinsic to this reading and the tangled_rope reading trends toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_content_vs_enforcement_form, empirical, 'Whether epsilon belongs to the doctrine''s content or to the enforcement machinery''s form.').

omega_variable(
    soteriological_sincerity_ambiguity,
    'Did the enforcing establishment sincerely hold that salvation itself was at stake — making suppression a proportionate defense of a truth-claim — or was uniformity pursued primarily for institutional advantage?',
    'Analysis of pre-imperial theological literature (before 325) for independent development of the consubstantiality concern, and of enforcement behavior in settings where no institutional benefit accrued to the enforcers.',
    'If sincere, part of the measured suppression is truth-defense cost rather than extraction overhead, lowering net extraction; if strategic, the coordination story moves toward cover and the classification trends toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(soteriological_sincerity_ambiguity, conceptual, 'Sincere truth-defense versus institutional rent-seeking in the enforcement motive.').

omega_variable(
    gothic_confessional_divide_origins,
    'Was the Gothic Homoian victim class created by the homoousios settlement, or by missionary timing — Ulfila''s mission preceding the Nicene victory — that the settlement merely froze in place?',
    'Chronology of Gothic conversion (330s–350s) against the settlement timeline, compared with peoples converted after 381 who faced no equivalent freeze.',
    'If timing-driven, the Gothic victim class is a path-dependence artifact rather than a product of this reading''s enforcement, reducing victim-set attribution; if the settlement actively froze the divide by refusing communion to Homoian Goths, attribution stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(gothic_confessional_divide_origins, empirical, 'Origin of the Gothic Arian victim class: settlement product or missionary-timing artifact.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_christological_kernel__homoousios_reading, 325, 589).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nice_tr_t325, nicene_christological_kernel__homoousios_reading, theater_ratio, 325, 0.14).
narrative_ontology:measurement(nice_tr_t355, nicene_christological_kernel__homoousios_reading, theater_ratio, 355, 0.31).
narrative_ontology:measurement(nice_tr_t380, nicene_christological_kernel__homoousios_reading, theater_ratio, 380, 0.1).
narrative_ontology:measurement(nice_tr_t450, nicene_christological_kernel__homoousios_reading, theater_ratio, 450, 0.17).
narrative_ontology:measurement(nice_tr_t520, nicene_christological_kernel__homoousios_reading, theater_ratio, 520, 0.26).
narrative_ontology:measurement(nice_tr_t589, nicene_christological_kernel__homoousios_reading, theater_ratio, 589, 0.28).

% Extraction over time
narrative_ontology:measurement(nice_be_t325, nicene_christological_kernel__homoousios_reading, base_extractiveness, 325, 0.48).
narrative_ontology:measurement(nice_be_t355, nicene_christological_kernel__homoousios_reading, base_extractiveness, 355, 0.34).
narrative_ontology:measurement(nice_be_t380, nicene_christological_kernel__homoousios_reading, base_extractiveness, 380, 0.64).
narrative_ontology:measurement(nice_be_t450, nicene_christological_kernel__homoousios_reading, base_extractiveness, 450, 0.72).
narrative_ontology:measurement(nice_be_t520, nicene_christological_kernel__homoousios_reading, base_extractiveness, 520, 0.7).
narrative_ontology:measurement(nice_be_t589, nicene_christological_kernel__homoousios_reading, base_extractiveness, 589, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(nice_su_t325, nicene_christological_kernel__homoousios_reading, suppression_requirement, 325, 0.42).
narrative_ontology:measurement(nice_su_t355, nicene_christological_kernel__homoousios_reading, suppression_requirement, 355, 0.22).
narrative_ontology:measurement(nice_su_t380, nicene_christological_kernel__homoousios_reading, suppression_requirement, 380, 0.7).
narrative_ontology:measurement(nice_su_t450, nicene_christological_kernel__homoousios_reading, suppression_requirement, 450, 0.8).
narrative_ontology:measurement(nice_su_t520, nicene_christological_kernel__homoousios_reading, suppression_requirement, 520, 0.74).
narrative_ontology:measurement(nice_su_t589, nicene_christological_kernel__homoousios_reading, suppression_requirement, 589, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nicene_christological_kernel__homoousios_reading, identity_coordination).
narrative_ontology:affects_constraint(nicene_christological_kernel__homoousios_reading, homoiousios_reading).
narrative_ontology:affects_constraint(nicene_christological_kernel__homoousios_reading, chalcedonian_two_natures_definition).
narrative_ontology:affects_constraint(nicene_christological_kernel__homoousios_reading, gothic_homoian_church_settlement).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'Nicene Christology' covers two structurally distinct enforced arrangements. This story (homoousios_reading) authors the same-substance settlement with its victim set (Homoian clergy, Gothic communities, regional congregations); the sibling homoiousios_reading authors the similar-substance arrangement, whose victim set inverts — during its tenure the machinery fell on pro-Nicene holders. The upstream story (this one, higher empirical consolidation after 381) influences the downstream Chalcedonian definition, which inherits the conciliar-enforcement template; the Gothic settlement story records the divide's persistence outside imperial reach. Each member links the others via affects_constraints; no single story carries both readings' epsilon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
