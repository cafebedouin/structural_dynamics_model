% ============================================================================
% CONSTRAINT STORY: feud_obligation_kernel__christianized_pacification_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-07
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feud_obligation_kernel__christianized_pacification_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: feud_obligation_kernel__christianized_pacification_reading
 *   human_readable: Blood-feud obligation under Christianized pacification
 *   domain: legal_anthropology/medieval_history/comparative_political_systems
 *
 * SUMMARY:
 *   This constraint story instantiates the christianized_pacification_reading
 *   of the feud_obligation_kernel. The reading holds that blood-feud
 *   obligations — kinship-based duties to avenge wrongs through lethal
 *   violence — violate divine law's prohibition on private vengeance.
 *   Legitimate violence authority resides exclusively with God and is
 *   delegated to ecclesiastical institutions (penitential discipline,
 *   sacramental gatekeeping) and royal institutions (king's peace, judicial
 *   courts). The constraint operates by declaring feud participation a
 *   spiritual peril (mortal sin, excommunication risk) while simultaneously
 *   offering the only authorized path to legitimate violence resolution
 *   through Church and Crown courts. This creates a tangled_rope structure: a
 *   genuine coordination function (replacing chaotic private violence with
 *   ordered judicial process) fused with asymmetric extraction
 *   (ecclesiastical jurisdiction expands via penitential fees, commutations,
 *   and jurisdictional claims; royal courts extract fines, forfeitures, and
 *   political loyalty from pacified populations).
 *
 * KEY AGENTS:
 *   - church_hierarchy: Primary beneficiary (institutional/identity_locked) — claims interpretive monopoly on legitimate violence, expands jurisdictional reach through anti-feud canons, collects penitential revenue
 *   - royal_courts: Secondary beneficiary (institutional/arbitrage) — monopolizes violence authority, extracts judicial profits and political submission from pacified populations
 *   - blood_feud_participants: Primary victims (powerless/identity_locked) — face spiritual peril (excommunication, damnation) and legal penalties; exit requires surrendering kin-honor identity
 *   - kinship_group_members: Secondary victims (moderate/identity_locked) — collective liability for feud acts; spiritual and legal sanctions extend to entire kin-group
 *   - local_lords: Ambiguous position (organized/constrained) — lose private justice revenue but gain royal delegation; some resist, some collaborate
 *   - analytical_observer: Observer (analytical/analytical) — sees full structural transformation from stateless coordination to institutionalized extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feud_obligation_kernel__christianized_pacification_reading, 0.68).
domain_priors:suppression_score(feud_obligation_kernel__christianized_pacification_reading, 0.82).
domain_priors:theater_ratio(feud_obligation_kernel__christianized_pacification_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feud_obligation_kernel__christianized_pacification_reading, tangled_rope).
narrative_ontology:human_readable(feud_obligation_kernel__christianized_pacification_reading, "Blood-feud obligation under Christianized pacification").
narrative_ontology:topic_domain(feud_obligation_kernel__christianized_pacification_reading, "legal_anthropology/medieval_history/comparative_political_systems").

domain_priors:requires_active_enforcement(feud_obligation_kernel__christianized_pacification_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feud_obligation_kernel__christianized_pacification_reading, '3c597ba6-bdc3-4fd0-9e34-279c1a1ee5e1').
narrative_ontology:cs_kernel_codification('3c597ba6-bdc3-4fd0-9e34-279c1a1ee5e1', formalized).
narrative_ontology:cs_authority_grounding('3c597ba6-bdc3-4fd0-9e34-279c1a1ee5e1', lineage).
narrative_ontology:cs_interpretation_layer_present('3c597ba6-bdc3-4fd0-9e34-279c1a1ee5e1').
narrative_ontology:cs_reading_relation('3c597ba6-bdc3-4fd0-9e34-279c1a1ee5e1', feud_obligation_kernel__stateless_coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('3c597ba6-bdc3-4fd0-9e34-279c1a1ee5e1', feud_obligation_kernel__extraction_cycle_reading, coexists_with).
narrative_ontology:cs_axiom('3c597ba6-bdc3-4fd0-9e34-279c1a1ee5e1', foundational, vengeance_is_divine_prerogative).
narrative_ontology:cs_axiom_status(vengeance_is_divine_prerogative, holdable).
narrative_ontology:cs_axiom_grounding('3c597ba6-bdc3-4fd0-9e34-279c1a1ee5e1', vengeance_is_divine_prerogative, deontological).
narrative_ontology:cs_axiom('3c597ba6-bdc3-4fd0-9e34-279c1a1ee5e1', foundational, church_holds_keys_to_legitimate_violence).
narrative_ontology:cs_axiom_status(church_holds_keys_to_legitimate_violence, holdable).
narrative_ontology:cs_axiom_grounding('3c597ba6-bdc3-4fd0-9e34-279c1a1ee5e1', church_holds_keys_to_legitimate_violence, conventional).
narrative_ontology:cs_reference_frame('3c597ba6-bdc3-4fd0-9e34-279c1a1ee5e1', christian_pacification_mandate).
narrative_ontology:cs_drift_state('3c597ba6-bdc3-4fd0-9e34-279c1a1ee5e1', high_medieval_institutionalization, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3c597ba6-bdc3-4fd0-9e34-279c1a1ee5e1', '').
narrative_ontology:cs_kernel_id(feud_obligation_kernel__christianized_pacification_reading, feud_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__christianized_pacification_reading, church_hierarchy).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__christianized_pacification_reading, royal_courts).
narrative_ontology:constraint_victim(feud_obligation_kernel__christianized_pacification_reading, blood_feud_participants).
narrative_ontology:constraint_victim(feud_obligation_kernel__christianized_pacification_reading, kinship_group_members).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__christianized_pacification_reading, local_lords).
narrative_ontology:constraint_victim(feud_obligation_kernel__christianized_pacification_reading, local_lords).
narrative_ontology:constraint_vindicates(feud_obligation_kernel__christianized_pacification_reading, divine_monopoly_on_vengeance).
narrative_ontology:constraint_vindicates(feud_obligation_kernel__christianized_pacification_reading, ecclesiastical_jurisdiction_over_violence).
narrative_ontology:constraint_vindicates(feud_obligation_kernel__christianized_pacification_reading, royal_peace_as_divine_mandate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claims divine authority to define legitimate violence; promulgates anti-feud canons (e.g., Lateran councils); operates penitential system that commutes feud-related sins for fees, pilgrimages, or land grants; expands ecclesiastical court jurisdiction over violence cases. Can shift between spiritual and temporal justification. Collects material and jurisdictional benefits from the constraint.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, church_hierarchy, beneficiary,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(feud_obligation_kernel__christianized_pacification_reading, church_hierarchy, agenda_setter).

% Asserts king's peace as divine mandate; establishes royal courts as sole legitimate venue for violence adjudication; extracts judicial profits (fines, forfeitures, wardships) and political loyalty from pacified populations. Adopts ecclesiastical anti-feud rhetoric to legitimize monopolization of violence. Can shift between divine-right and pragmatic-order justification.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, royal_courts, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(feud_obligation_kernel__christianized_pacification_reading, royal_courts, agenda_setter).

% Bound by kinship honor to avenge wrongs through lethal violence; now face excommunication, denial of sacraments, eternal damnation rhetoric, and royal outlawry/execution. Exit requires publicly surrendering the feud obligation — which means surrendering kin-honor identity and admitting ancestral duties were sinful. No viable alternative justice system exists locally; royal courts are distant, expensive, and culturally alien.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, blood_feud_participants, payer,
    powerless, biographical, identity_locked, local).

% Collective liability for feud acts of any member; entire kin-group faces spiritual sanctions (interdict) and legal penalties (collective fines, land seizure). Group identity is fused with feud obligation — 'we are the people who avenge our own.' Exit requires collective surrender of identity, which is structurally nearly impossible without external coercion or generational collapse.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, kinship_group_members, payer,
    moderate, biographical, identity_locked, regional).

% Traditionally held private justice rights (court profits, vengeance mediation fees). Lose these to royal/ecclesiastical courts but may gain delegated royal authority (sheriff, bailiff) or ecclesiastical protection. Caught between kinship obligations to their own kin-groups and institutional pressures from Church/Crown. Some resist centralization violently; others collaborate for office.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, local_lords, payer,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(feud_obligation_kernel__christianized_pacification_reading, local_lords, beneficiary).

% Sees the full structural transformation: a stateless coordination mechanism (feud) being replaced by an institutionalized extraction-coordination hybrid (Church/Crown courts). Observes that the coordination function (predictable violence resolution) is genuine but the extraction layer (jurisdictional monopolies, revenue streams, identity destruction) is asymmetric and persistent beyond the coordination necessity.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Replaces unpredictable, escalatory private vengeance with ordered judicial process under divine authority. Provides a single legitimate venue for violence adjudication, standardized penalties, and spiritual assurance that justice aligns with divine will. Solves the coordination problem of 'who may legitimately kill whom' by answering: only God's delegates.
% TRANSFER_FUNCTION: Moves three things from feud participants/kinship groups to Church and Crown: (1) jurisdictional authority over violence cases — from kin-groups to courts; (2) material revenue — penitential commutation fees, judicial fines, forfeited lands; (3) political loyalty — submission to royal/ecclesiastical authority as condition of legal personhood.
% ABSENT_VOICES: The dead ancestors whose honor the feud obligates the living to avenge — they cannot consent to the surrender of their vengeance. The unborn kin-group members whose identity is being restructured without their input. Rival pagan/customary legal practitioners (brehons, lagmen, thingmen) who are excluded from the new court system. Women in kin-groups who often bore the social cost of feud violence but had no voice in either the feud or its suppression.
% DISAPPEARANCE_RATIONALE: If the Christianized pacification constraint vanished overnight, kinship groups would revert to private vengeance within months — the coordination problem it solved (stateless violence) remains real in structural memory. Royal courts would lose their divine mandate for violence monopoly; ecclesiastical courts would lose jurisdictional basis. The medieval order of 'king's peace under God' would collapse into competing lordships.
% FOUNDING_PROBLEM: Early medieval Europe lacked centralized enforcement capacity. Kinship groups filled the vacuum with self-enforcing vengeance obligations that provided deterrence and dispute resolution but at terrible cost: escalatory violence, destruction of productive capacity, impossibility of territorial consolidation, and spiritual danger (from the Church's perspective). The constraint was built to replace kin-justice with divine-justice-administered-by-institutions.
% FOUNDING_PROBLEM_CORROBORATION: The Church attests the problem is still live (human sinfulness requires constant pacification). Royal courts attest the problem is substantially solved (king's peace functions). Historians outside the benefiting parties (e.g., Marxist historians of state formation, anthropologists of stateless societies) attest the coordination problem was real but the institutional solution extracted far beyond coordination necessity. No single corroboration settles it — the founding problem's status is the kernel's contested ground.
narrative_ontology:disappearance_verdict(feud_obligation_kernel__christianized_pacification_reading, world_rearranges).
narrative_ontology:founding_problem_status(feud_obligation_kernel__christianized_pacification_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feud_obligation_kernel__christianized_pacification_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(feud_obligation_kernel__christianized_pacification_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feud_obligation_kernel__christianized_pacification_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feud_obligation_kernel__christianized_pacification_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(feud_obligation_kernel__christianized_pacification_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(feud_obligation_kernel__christianized_pacification_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The claimed_type is tangled_rope because the constraint genuinely solves a coordination problem (private violence is unpredictable, escalatory, and destroys productive capacity) while simultaneously extracting asymmetric benefits for Church and Crown. Extraction (0.68) is substantial: penitential commutations, judicial fees, forfeited lands, and political loyalty flows to the institutions. Suppression (0.82) is high: the constraint seeks complete elimination of feud violence through spiritual sanctions (excommunication, interdict) and legal penalties (outlawry, execution). Theater_ratio (0.28) is moderate-low: the spiritual justification is sincerely held by the enforcing institutions, but the jurisdictional and revenue benefits are real and pursued. Accessibility_collapse (0.71) is high: once the divine prohibition is accepted, private vengeance becomes conceptually illegitimate, not just illegal. Resistance (0.55) is significant: kinship groups resist surrendering honor-justice for generations; local lords resist centralization.
 *
 * PERSPECTIVAL GAP:
 *   From the Church/Crown seat (agenda_setter/beneficiary), the constraint appears as rope: a divinely mandated pacification that brings order. From the feud participant seat (victim/identity_locked), it appears as snare: spiritual terror and legal coercion stripping away the only effective justice they knew. From the local lord seat (ambiguous), it appears as scaffold: a transitional arrangement that centralizes authority they once held. The engine computes these per-seat divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Church hierarchy and royal courts are beneficiaries: they gain jurisdictional authority, revenue streams (penitential fees, court profits), and political control. Their exit_options are arbitrage (they can shift between spiritual/secular justification). Blood feud participants and kinship groups are victims: they bear spiritual peril, legal penalties, and loss of autonomous justice. Their exit_options are identity_locked — surrendering feud obligation means surrendering kin-honor identity, which is existentially costly. Local lords are constrained: they lose private justice revenue but may gain royal delegation; exit is constrained by competing institutional pressures.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (stateless violence coordination) was live in the early medieval period but substantially resolved by the high medieval period as royal courts developed functional judicial systems. The constraint persists (mandatrophy) because the institutional beneficiaries (Church, Crown) extract value from maintaining the anti-feud framework even after its coordination function could be served by less extractive means. The spiritual justification becomes a cover for jurisdictional and revenue extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint one reading of the feud_obligation_kernel (christianized_pacification_reading), and how do the sibling readings stateless_coordination_reading and extraction_cycle_reading differ structurally?',
    'Comparative structural analysis: stateless_coordination_reading treats feud as self-enforcing coordination (rope-like, low extraction); extraction_cycle_reading treats feud as destructive depletion (snare-like, high extraction). This reading treats feud as violation of divine order requiring active suppression (tangled_rope, coordination+extraction).',
    'If the kernel decomposes cleanly into three distinct constraints with stable ε, each gets its own story. If the readings are merely perspectival slices of one constraint, the ε-invariance principle is violated and the framework must treat them as one story with observer-dependent classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Structural independence of the three declared readings of feud_obligation_kernel').

omega_variable(
    spiritual_peril_as_extraction,
    'Does the spiritual peril faced by feud participants (eternal damnation, sacramental exclusion) function as extractive leverage for the Church, or is it an independent theological reality that the Church merely declares?',
    'Historical analysis of penitential practice: did the Church use feud-related penances as revenue source (commutation fees, pilgrimage levies, land grants)? Did jurisdictions with stronger ecclesiastical courts show higher feud suppression correlated with ecclesiastical enrichment?',
    'If spiritual peril is extractive leverage, the Church''s beneficiary status is strengthened and χ rises for feud participants. If purely declarative, the Church is a vindicated_proposition beneficiary only, not a material extractor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(spiritual_peril_as_extraction, empirical, 'Whether spiritual sanctions against feuding function as extraction mechanism for ecclesiastical institutions').

omega_variable(
    royal_church_competition,
    'Did royal courts and ecclesiastical courts compete or cooperate in suppressing feud violence, and how did this affect the extraction profile?',
    'Comparative institutional history: jurisdictions with strong royal courts (England, France) vs. fragmented imperial territories (HRE) vs. ecclesiastical principalities. Did royal courts adopt ecclesiastical anti-feud rhetoric to legitimize their own monopolization of violence?',
    'If cooperative, the beneficiary set is unified (church+royal). If competitive, each institution''s extraction from feud participants is moderated by the other''s competing jurisdiction, potentially lowering effective χ for both.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(royal_church_competition, empirical, 'Institutional relationship between Church and Crown in feud suppression').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feud_obligation_kernel__christianized_pacification_reading, 500, 1300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_christianized_tr_t500, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 500, 0.15).
narrative_ontology:measurement(feud_christianized_tr_t800, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 800, 0.18).
narrative_ontology:measurement(feud_christianized_tr_t1000, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 1000, 0.22).
narrative_ontology:measurement(feud_christianized_tr_t1100, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 1100, 0.25).
narrative_ontology:measurement(feud_christianized_tr_t1200, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 1200, 0.27).
narrative_ontology:measurement(feud_christianized_tr_t1300, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 1300, 0.28).

% Extraction over time
narrative_ontology:measurement(feud_christianized_be_t500, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 500, 0.45).
narrative_ontology:measurement(feud_christianized_be_t800, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 800, 0.52).
narrative_ontology:measurement(feud_christianized_be_t1000, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 1000, 0.58).
narrative_ontology:measurement(feud_christianized_be_t1100, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 1100, 0.63).
narrative_ontology:measurement(feud_christianized_be_t1200, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 1200, 0.67).
narrative_ontology:measurement(feud_christianized_be_t1300, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 1300, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(feud_christianized_su_t500, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 500, 0.55).
narrative_ontology:measurement(feud_christianized_su_t800, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 800, 0.65).
narrative_ontology:measurement(feud_christianized_su_t1000, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 1000, 0.72).
narrative_ontology:measurement(feud_christianized_su_t1100, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 1100, 0.78).
narrative_ontology:measurement(feud_christianized_su_t1200, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 1200, 0.8).
narrative_ontology:measurement(feud_christianized_su_t1300, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 1300, 0.82).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=500, tn=1300
narrative_ontology:measurement(feud_christianized_grid_01, feud_obligation_kernel__christianized_pacification_reading, accessibility_collapse(class), 500, 0.5).
narrative_ontology:measurement(feud_christianized_grid_02, feud_obligation_kernel__christianized_pacification_reading, accessibility_collapse(class), 1300, 0.72).
narrative_ontology:measurement(feud_christianized_grid_03, feud_obligation_kernel__christianized_pacification_reading, accessibility_collapse(individual), 500, 0.55).
narrative_ontology:measurement(feud_christianized_grid_04, feud_obligation_kernel__christianized_pacification_reading, accessibility_collapse(individual), 1300, 0.75).
narrative_ontology:measurement(feud_christianized_grid_05, feud_obligation_kernel__christianized_pacification_reading, accessibility_collapse(organizational), 500, 0.5).
narrative_ontology:measurement(feud_christianized_grid_06, feud_obligation_kernel__christianized_pacification_reading, accessibility_collapse(organizational), 1300, 0.68).
narrative_ontology:measurement(feud_christianized_grid_07, feud_obligation_kernel__christianized_pacification_reading, accessibility_collapse(structural), 500, 0.45).
narrative_ontology:measurement(feud_christianized_grid_08, feud_obligation_kernel__christianized_pacification_reading, accessibility_collapse(structural), 1300, 0.7).
narrative_ontology:measurement(feud_christianized_grid_09, feud_obligation_kernel__christianized_pacification_reading, resistance(class), 500, 0.6).
narrative_ontology:measurement(feud_christianized_grid_10, feud_obligation_kernel__christianized_pacification_reading, resistance(class), 1300, 0.5).
narrative_ontology:measurement(feud_christianized_grid_11, feud_obligation_kernel__christianized_pacification_reading, resistance(individual), 500, 0.65).
narrative_ontology:measurement(feud_christianized_grid_12, feud_obligation_kernel__christianized_pacification_reading, resistance(individual), 1300, 0.4).
narrative_ontology:measurement(feud_christianized_grid_13, feud_obligation_kernel__christianized_pacification_reading, resistance(organizational), 500, 0.55).
narrative_ontology:measurement(feud_christianized_grid_14, feud_obligation_kernel__christianized_pacification_reading, resistance(organizational), 1300, 0.45).
narrative_ontology:measurement(feud_christianized_grid_15, feud_obligation_kernel__christianized_pacification_reading, resistance(structural), 500, 0.5).
narrative_ontology:measurement(feud_christianized_grid_16, feud_obligation_kernel__christianized_pacification_reading, resistance(structural), 1300, 0.42).
narrative_ontology:measurement(feud_christianized_grid_17, feud_obligation_kernel__christianized_pacification_reading, stakes_inflation(class), 500, 0.3).
narrative_ontology:measurement(feud_christianized_grid_18, feud_obligation_kernel__christianized_pacification_reading, stakes_inflation(class), 1300, 0.55).
narrative_ontology:measurement(feud_christianized_grid_19, feud_obligation_kernel__christianized_pacification_reading, stakes_inflation(individual), 500, 0.4).
narrative_ontology:measurement(feud_christianized_grid_20, feud_obligation_kernel__christianized_pacification_reading, stakes_inflation(individual), 1300, 0.7).
narrative_ontology:measurement(feud_christianized_grid_21, feud_obligation_kernel__christianized_pacification_reading, stakes_inflation(organizational), 500, 0.35).
narrative_ontology:measurement(feud_christianized_grid_22, feud_obligation_kernel__christianized_pacification_reading, stakes_inflation(organizational), 1300, 0.62).
narrative_ontology:measurement(feud_christianized_grid_23, feud_obligation_kernel__christianized_pacification_reading, stakes_inflation(structural), 500, 0.25).
narrative_ontology:measurement(feud_christianized_grid_24, feud_obligation_kernel__christianized_pacification_reading, stakes_inflation(structural), 1300, 0.5).
narrative_ontology:measurement(feud_christianized_grid_25, feud_obligation_kernel__christianized_pacification_reading, suppression(class), 500, 0.5).
narrative_ontology:measurement(feud_christianized_grid_26, feud_obligation_kernel__christianized_pacification_reading, suppression(class), 1300, 0.78).
narrative_ontology:measurement(feud_christianized_grid_27, feud_obligation_kernel__christianized_pacification_reading, suppression(individual), 500, 0.5).
narrative_ontology:measurement(feud_christianized_grid_28, feud_obligation_kernel__christianized_pacification_reading, suppression(individual), 1300, 0.8).
narrative_ontology:measurement(feud_christianized_grid_29, feud_obligation_kernel__christianized_pacification_reading, suppression(organizational), 500, 0.45).
narrative_ontology:measurement(feud_christianized_grid_30, feud_obligation_kernel__christianized_pacification_reading, suppression(organizational), 1300, 0.75).
narrative_ontology:measurement(feud_christianized_grid_31, feud_obligation_kernel__christianized_pacification_reading, suppression(structural), 500, 0.4).
narrative_ontology:measurement(feud_christianized_grid_32, feud_obligation_kernel__christianized_pacification_reading, suppression(structural), 1300, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feud_obligation_kernel__christianized_pacification_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(feud_obligation_kernel__christianized_pacification_reading, 0.12).
narrative_ontology:affects_constraint(feud_obligation_kernel__christianized_pacification_reading, feud_obligation_kernel__stateless_coordination_reading).
narrative_ontology:affects_constraint(feud_obligation_kernel__christianized_pacification_reading, feud_obligation_kernel__extraction_cycle_reading).
narrative_ontology:affects_constraint(feud_obligation_kernel__christianized_pacification_reading, royal_peace_expansion).
narrative_ontology:affects_constraint(feud_obligation_kernel__christianized_pacification_reading, ecclesiastical_court_jurisdiction).
narrative_ontology:affects_constraint(feud_obligation_kernel__christianized_pacification_reading, penitential_system).

% DUAL FORMULATION NOTE:
% feud_obligation_kernel decomposes into three structurally distinct constraints with different ε values: stateless_coordination_reading (rope, ε≈0.25), extraction_cycle_reading (snare, ε≈0.75), christianized_pacification_reading (tangled_rope, ε≈0.68). The readings share the referent (blood-feud obligations) but disagree on the constraint's coordination/extraction structure. This reading's ε is higher than the coordination reading because it includes institutional extraction; lower than the extraction reading because it retains a genuine coordination function.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(feud_obligation_kernel__christianized_pacification_reading, institutional, 0.15).
constraint_indexing:directionality_override(feud_obligation_kernel__christianized_pacification_reading, powerless, 0.9).
constraint_indexing:directionality_override(feud_obligation_kernel__christianized_pacification_reading, moderate, 0.7).
constraint_indexing:directionality_override(feud_obligation_kernel__christianized_pacification_reading, organized, 0.45).
constraint_indexing:directionality_override(feud_obligation_kernel__christianized_pacification_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
