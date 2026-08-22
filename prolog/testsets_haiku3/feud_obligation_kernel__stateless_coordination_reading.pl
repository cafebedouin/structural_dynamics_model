% ============================================================================
% CONSTRAINT STORY: feud_obligation_kernel__stateless_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feud_obligation_kernel__stateless_coordination_reading, []).

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
 *   constraint_id: feud_obligation_kernel__stateless_coordination_reading
 *   human_readable: Blood-Feud Obligation Coordination (Stateless Reading)
 *   domain: legal/anthropological
 *
 * SUMMARY:
 *   This constraint instantiates the stateless-coordination reading of
 *   contested blood-feud obligations. Feud obligation, in this reading, is a
 *   self-enforcing justice mechanism that operates in the absence of
 *   centralized enforcement authority. Kinship groups benefit from the
 *   deterrent signal that feud retaliation provides and from the honor
 *   restoration that successful vengeance brings. The mechanism is genuinely
 *   coordinative: it solves the problem of justice accountability without
 *   central arbitration. However, the reading is contested: a
 *   christianized-pacification reading sees feuding as violating divine law;
 *   an extraction-cycle reading sees feuding as a destructive zero-sum trap.
 *   This story describes ONLY the stateless-coordination reading—the one that
 *   treats feud as self-enforcing coordination. The other readings are
 *   separate constraints (separate files), linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - kinship_group_members: Benefit from honor restoration and deterrent effect; identity-locked into the obligation
 *   - honor_participants (elite/powerful): Use feud as primary mechanism for defending territorial claims and dynastic legitimacy
 *   - alleged_wrongdoers and their kin: Bear the cost of retaliation cycles; options constrained by geography and kinship obligation
 *   - wergild_negotiators: Operate as alternative dispute resolution; their role depends on both sides retaining negotiation exit
 *   - defectors from obligation: Face kinship sanctions and exclusion; trapped because kinship group is only security source
 *   - externally_authorized_authorities: Observe from position of potential displacement; work to establish state monopoly on violence
 *   - christian_church: Excluded from legitimacy but present through settlement mediation; represents competing moral authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feud_obligation_kernel__stateless_coordination_reading, 0.38).
domain_priors:suppression_score(feud_obligation_kernel__stateless_coordination_reading, 0.22).
domain_priors:theater_ratio(feud_obligation_kernel__stateless_coordination_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, resistance, 0.41).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feud_obligation_kernel__stateless_coordination_reading, rope).
narrative_ontology:human_readable(feud_obligation_kernel__stateless_coordination_reading, "Blood-Feud Obligation Coordination (Stateless Reading)").
narrative_ontology:topic_domain(feud_obligation_kernel__stateless_coordination_reading, "legal/anthropological").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feud_obligation_kernel__stateless_coordination_reading, '36af884d-7748-4436-9e7a-01c1aefa2948').
narrative_ontology:cs_kernel_codification('36af884d-7748-4436-9e7a-01c1aefa2948', implicit).
narrative_ontology:cs_authority_grounding('36af884d-7748-4436-9e7a-01c1aefa2948', practice).
narrative_ontology:cs_interpretation_layer_present('36af884d-7748-4436-9e7a-01c1aefa2948').
narrative_ontology:cs_reading_relation('36af884d-7748-4436-9e7a-01c1aefa2948', feud_obligation_kernel__extraction_cycle_reading, coexists_with).
narrative_ontology:cs_reading_relation('36af884d-7748-4436-9e7a-01c1aefa2948', feud_obligation_kernel__christianized_pacification_reading, coexists_with).
narrative_ontology:cs_axiom('36af884d-7748-4436-9e7a-01c1aefa2948', foundational, kinship_group_authority_legitimate).
narrative_ontology:cs_axiom_status(kinship_group_authority_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('36af884d-7748-4436-9e7a-01c1aefa2948', kinship_group_authority_legitimate, conventional).
narrative_ontology:cs_axiom('36af884d-7748-4436-9e7a-01c1aefa2948', foundational, self_executing_retaliation_enforces_accountability).
narrative_ontology:cs_axiom_status(self_executing_retaliation_enforces_accountability, holdable).
narrative_ontology:cs_axiom_grounding('36af884d-7748-4436-9e7a-01c1aefa2948', self_executing_retaliation_enforces_accountability, instrumental).
narrative_ontology:cs_reference_frame('36af884d-7748-4436-9e7a-01c1aefa2948', kinship_based_justice_authority).
narrative_ontology:cs_drift_state('36af884d-7748-4436-9e7a-01c1aefa2948', pre_state_centralization, gap(stable, minor, false)).
narrative_ontology:cs_created_at('36af884d-7748-4436-9e7a-01c1aefa2948', '').
narrative_ontology:cs_kernel_id(feud_obligation_kernel__stateless_coordination_reading, feud_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__stateless_coordination_reading, kinship_group_members).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__stateless_coordination_reading, honor_participants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__stateless_coordination_reading, wergild_negotiators).
narrative_ontology:constraint_victim(feud_obligation_kernel__stateless_coordination_reading, alleged_wrongdoers).
narrative_ontology:constraint_victim(feud_obligation_kernel__stateless_coordination_reading, defectors_from_obligation).
narrative_ontology:constraint_vindicates(feud_obligation_kernel__stateless_coordination_reading, self_enforcing_justice_doctrine).
narrative_ontology:constraint_vindicates(feud_obligation_kernel__stateless_coordination_reading, deterrence_through_reciprocal_obligation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Members of agnatic kin groups participate in feud resolution by collectively pursuing justice for slain or dishonored relatives. They receive the satisfaction of vengeance, restoration of honor within their community, and the deterrent effect that their willingness to retaliate provides against future wrongs. Exit from the obligation is theoretically possible but carries identity cost: abandoning the feud is read as personal cowardice and kinship disloyalty, risking expulsion from the group's protective structure.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, kinship_group_members, beneficiary,
    moderate, generational, identity_locked, local).

% Elite families and noble houses use feud obligation as the primary mechanism to defend territorial claims, dynastic legitimacy, and personal honor against rivals. They benefit from the deterrent signal that feuding sends (others hesitate to encroach knowing retaliation will follow), and from the public resolution mechanism the feud provides (settling disputes through blood rather than allowing grievances to accumulate unresolved).
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, honor_participants, beneficiary,
    powerful, generational, constrained, regional).

% Individuals or their kin groups who have committed a killing or dishonoring act bear the cost of the feud cycle: they face retaliation raids, counter-escalations, and the threat of endless cycles of revenge unless they negotiate settlement (wergild payment, hostage exchange, or marriage alliance). Their exit options are limited by geography (cannot easily relocate in a feudal economy) and by the structural nature of kinship obligation (their relatives are implicated whether they personally participated or not).
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, alleged_wrongdoers, payer,
    moderate, biographical, constrained, local).

% Ecclesiastical authorities, village elders, and respected mediators operate as alternative dispute-resolution seats, offering wergild (compensation payment) as a path to settle feuds without endless bloodshed. They benefit from their role as peace-brokers and receive material compensation (tithe, gift) for orchestrating settlements. Their position depends on both sides retaining the option to negotiate—if feud obligation becomes absolute, their mediation role disappears.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, wergild_negotiators, beneficiary,
    institutional, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(feud_obligation_kernel__stateless_coordination_reading, wergild_negotiators, observer).

% Individuals who refuse to participate in feud retaliation or who advocate peace-without-bloodshed face kinship sanctions: loss of protective standing within the group, social ostracism, and vulnerability to harm from rivals who now see them as weak or disloyal. They may also face internal kinship punishment. Their trapped status comes from the fact that in a stateless context, the kinship group is the only source of security; exit means undefended exposure.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, defectors_from_obligation, payer,
    powerless, biographical, trapped, local).

% Kings, emperors, and centralized legal systems observe feud coordination from a position of potential displacement: from their seat, feud obligation represents a competing coordination mechanism that must be eliminated, regulated, or captured to establish state monopoly on legitimate violence. They collect information about feud practices to calibrate policy, but do not directly participate in the feud mechanism itself.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, externally_authorized_authorities, observer,
    institutional, civilizational, analytical, continental).

% Ecclesiastical authorities (distinct from local mediators) are excluded from the core feud coordination mechanism by virtue of their alternative moral authority: they teach that vengeance belongs to God, not kinship groups, and that participation in blood-feud violates divine law. Yet they are simultaneously present through the wergild negotiation seat, creating a structural tension between exclusion from feud legitimacy and inclusion in feud settlement.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, christian_church, excluded,
    institutional, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(feud_obligation_kernel__stateless_coordination_reading, honor_participants).
narrative_ontology:fixing_cost_class(feud_obligation_kernel__stateless_coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Blood-feud obligation solves the justice problem in the absence of centralized enforcement: when one kinship group wrongs another, the obligation to retaliate (or negotiate settlement) creates a self-enforcing mechanism. The threat of costly feud retaliation deters wrongs in the first place; the institution of wergild negotiation allows the cycle to be interrupted without loss of honor. Without centralized law enforcement, kinship-based reputation for willingness to retaliate is the primary deterrent against violation.
% TRANSFER_FUNCTION: Transfers honor-restoration from the wronged party's kinship group to itself through successful retaliation or compensation. Transfers blood-debt obligation from the wronged party to the wrongdoing party's kin. In settlements, transfers wergild (money or goods) from wrongdoer's kin to wronged party's kin as a substitute for blood. The mechanism moves costs in the form of retaliation risk, negotiation effort, and potential death or permanent vendetta.
% ABSENT_VOICES: Victims of feud escalation (collateral casualties in raids, families destroyed by multi-generational vendettas) are structurally absent from the decision-making seat: they have already been killed or are viewed as necessary costs of maintaining the deterrence system. Peacemakers and merchants whose trade routes are disrupted by feud violence are also absent—they would argue for expanded wergild negotiation and reduced acceptance of blood-debt, but their economic interests do not translate into kinship obligation standing.
% DISAPPEARANCE_RATIONALE: If feud obligation disappeared, justice resolution would fragment: kinship groups would lack the primary mechanism for deterring wrongs and restoring honor. Wergild negotiation might expand to fill some gaps, but without the credible threat of retaliation backing it, settlements would become unreliable. Alternatively (the contestation): if centralized enforcement developed, the feud mechanism would become redundant and society would reorganize around state law. Historians dispute whether feuding societies are equilibria awaiting state development or whether state development actively displaces working feuding systems.
% FOUNDING_PROBLEM: In the absence of centralized legal authority, how do independent kinship groups deter wrongs against each other and resolve disputes without infinite escalation? How does one establish that a killing or dishonoring has occurred and hold the responsible party accountable when there is no neutral arbiter?
% FOUNDING_PROBLEM_CORROBORATION: Medieval historians and anthropologists studying stateless societies (Iceland before royal centralization, pre-Islamic Arabia, early Germanic law codes) attest that feud obligation was the primary operative justice mechanism. Ethnographic studies of contemporary stateless societies (certain pastoral and tribal contexts) show similar mechanisms functioning to deter wrongs and establish accountability. The founding problem remains live in any context where centralized enforcement capacity is absent or weak—contemporary stateless regions and failed-state contexts exhibit kinship-based retaliation systems functionally homologous to medieval feuding. This corroboration comes from external academic sources, not from the feud participants themselves.
narrative_ontology:disappearance_verdict(feud_obligation_kernel__stateless_coordination_reading, contested).
narrative_ontology:founding_problem_status(feud_obligation_kernel__stateless_coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feud_obligation_kernel__stateless_coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(feud_obligation_kernel__stateless_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feud_obligation_kernel__stateless_coordination_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feud_obligation_kernel__stateless_coordination_reading_tests).
:- end_tests(feud_obligation_kernel__stateless_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-to-moderate (0.38 at terminal, starting at 0.22) because the mechanism is genuinely coordinative for participants—the primary function is justice restoration, not extraction. Suppression is low (0.22) because feud obligation coexists with alternative mechanisms (wergild negotiation) and does not require active suppression of exit from individual acts of revenge—participants' identity fusion with kinship obligation provides the enforcement. Theater ratio is minimal (0.12) because the functional activity is predominant: ritual elements exist, but the retaliation and settlement activities are substantive dispute resolution, not performance. Accessibility collapse is moderate (0.48): alternatives (wergild, relocation, alliance-building) exist and are used, so the constraint does not collapse alternative paths completely, yet the identity-locked nature of kinship membership means some actors cannot credibly exit. The measurement series shows extractiveness rising in the early period (0–200) as feuds compound and honor obligations accumulate, then stabilizing (200–400) as the system settles into equilibrium cycles—this plateau pattern is characteristic of a working coordination mechanism, not an extraction ratchet. Suppression requirement rises slightly (0.15–0.22) during the accumulation phase as defectors must be managed, then holds steady as the system normalizes.
 *
 * PERSPECTIVAL GAP:
 *   From the kinship-group perspective, the feud is a legitimate, self-enforcing justice mechanism—the core coordination problem is solved and participants benefit. From the external-authority perspective, feud is a competing mechanism that must be displaced by state law. From the defector's perspective, feud obligation is coercive and identity-fusing. From the wergild-negotiator's perspective, feud obligation is functional but its prevalence limits their settlement opportunities. The engine computes different types for these different seats from the same structural data. The agenda-setter (honor-participant) seat sees rope-like coordination; the payer (wrongdoer) seat sees snare-like extraction; the excluded seat (church) sees snare-like violation of divine law. This is the expected divergence when a coordination mechanism distributes costs asymmetrically.
 *
 * DIRECTIONALITY LOGIC:
 *   Kinship-group members are structural beneficiaries (d near 0.2): they receive honor restoration without running the coordination mechanism, and their exit is identity-locked (high cost). Honor-participants (elite) are also beneficiaries but with more mobile exit (d near 0.25–0.3): they actively maintain the system and benefit from its deterrent properties, but could theoretically relocate or seek alternative legitimacy. Alleged wrongdoers are targets (d near 0.75): they bear retaliation cost and constrained exit. Wergild negotiators are near-symmetric (d near 0.5): they benefit from their mediation role and have mobile exit, but they must absorb the conflict energy that the feud generates. Defectors are high-target (d near 0.9): they face kinship expulsion and trapped exit with no legitimate income stream or protective structure. The engine derives these d values from the beneficiary/victim declarations plus power and exit_options; the commentary explains why the asymmetry exists.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (justice without centralized authority) is live in the interval examined—the feud mechanism continues to operate and solves the stated problem. No mandatrophy is present in this reading: the constraint's declared function persists throughout. However, this is the crucial point of contest between readings: the extraction-cycle reading argues that feuding in fact PREVENTS territorial consolidation and generates long-term inefficiency (mandatrophy: the founding problem has been replaced by destructive rent-seeking). The christianized-pacification reading argues that feud obligation violates divine law (mandatrophy: the founding problem is superseded by a higher moral problem). In the stateless-coordination reading alone, mandatrophy is not declared—the founding problem remains the operative driving function. This divergence is captured in the omega variables below.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_extraction_boundary,
    'Does the measured extractiveness (0.38) represent genuine coordination cost or does it measure hidden extraction that serves honor-participant interests at kinship-group expense?',
    'Compare feud societies'' demographic patterns, productive output, and survivor welfare against non-feud control cases (e.g., pre-centralization Iceland vs. post-centralization Iceland; pre-state pastoral societies with blood-price systems vs. those without). If feud societies show sustained population stability and economic productivity comparable to non-feud neighbors, the extractiveness measures coordination cost; if they show elevated mortality without compensating security gains, the extractiveness measures extraction.',
    'If extracted: reclassify as tangled_rope (genuine coordination + asymmetric extraction); if coordination cost: remain rope. The classification difference pivots on whether the cost distribution serves a function or concentrates on powerless actors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, empirical, 'Whether elevated extractiveness represents necessary coordination overhead or asymmetric rent-capture.').

omega_variable(
    identity_lock_mechanism,
    'Is the identity-locked exit option for kinship-group members a structural feature of agnatic kinship (the defector faces genuine protective abandonment) or a performative norm (the group could forgive defection but chooses not to)?',
    'Ethnographic study of defector outcomes in feuding societies: do defectors who refuse feud participation actually face material harm (denied shelter, excluded from economic activity, exposed to violence from rival groups) or face primarily social ostracism that could be theoretically overcome? Post-exit trajectory data: do defectors who migrate or join other groups regain security and productivity?',
    'If structural (genuine abandonment): directionality for defectors remains high-target (d near 0.9); if performative (could be forgiven): directionality moderates to constrained-exit (d near 0.7), suggesting the constraint is more extractive than the coordination reading alone explains.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether kinship-group abandonment of defectors is material or performative.').

omega_variable(
    wergild_coexistence_vs_substitution,
    'Does wergild negotiation coexist as a viable alternative to blood-feud (low suppression of exit), or does it only function as a high-cost settlement mechanism after feud costs have already mounted?',
    'Historical analysis of dispute-resolution patterns: what proportion of disputes proceed to wergild settlement directly without blood-feud escalation? Do early-stage wrongs (minor insults, boundary disputes, property conflicts) resolve through wergild without kinship retaliation obligation engaging? Or do all disputes progress to feud first, with wergild only intervening after blood has been shed?',
    'If wergild is used at low-cost early stage: suppression of alternative mechanisms is genuinely low (0.22 is accurate); if wergild only enters post-escalation: suppression is actually higher (alternatives are foreclosed until feud proves costly), suggesting extractiveness is understated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wergild_coexistence_vs_substitution, empirical, 'Whether wergild functions as genuine alternative or only as post-escalation settlement.').

omega_variable(
    feud_as_reading_not_natural_law,
    'Is the stateless-coordination reading of feud obligation a description of how feud systems actually function, or is it a legitimate reading that can coexist with competing readings without one foreclosing the others?',
    'Textual and institutional analysis: do medieval chronicles, law codes, and ecclesiastical records show the same community simultaneously defending, condemning, and attempting to replace feud obligation? Or do different authorities (lay, ecclesiastical, royal) advance mutually exclusive readings?',
    'If coexistent readings: this constraint remains rope in its own framing, extraction-cycle remains snare in its framing, etc.—no foreclosure. If mutually exclusive: one reading''s foundational axiom directly contradicts another''s, and the axioms should be marked forecloses rather than coexists_with in cs_structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(feud_as_reading_not_natural_law, conceptual, 'Whether the three kernel readings coexist as live positions or foreclose each other.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feud_obligation_kernel__stateless_coordination_reading, 0, 400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t0, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(feud_tr_t0, observed).
narrative_ontology:measurement(feud_tr_t50, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 50, 0.09).
narrative_ontology:measurement_basis(feud_tr_t50, observed).
narrative_ontology:measurement(feud_tr_t100, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 100, 0.1).
narrative_ontology:measurement_basis(feud_tr_t100, observed).
narrative_ontology:measurement(feud_tr_t150, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 150, 0.11).
narrative_ontology:measurement_basis(feud_tr_t150, observed).
narrative_ontology:measurement(feud_tr_t200, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 200, 0.12).
narrative_ontology:measurement_basis(feud_tr_t200, observed).
narrative_ontology:measurement(feud_tr_t250, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 250, 0.12).
narrative_ontology:measurement_basis(feud_tr_t250, observed).
narrative_ontology:measurement(feud_tr_t300, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 300, 0.12).
narrative_ontology:measurement_basis(feud_tr_t300, observed).
narrative_ontology:measurement(feud_tr_t350, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 350, 0.12).
narrative_ontology:measurement_basis(feud_tr_t350, observed).
narrative_ontology:measurement(feud_tr_t400, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 400, 0.12).
narrative_ontology:measurement_basis(feud_tr_t400, observed).

% Extraction over time
narrative_ontology:measurement(feud_be_t0, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement_basis(feud_be_t0, observed).
narrative_ontology:measurement(feud_be_t50, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 50, 0.28).
narrative_ontology:measurement_basis(feud_be_t50, observed).
narrative_ontology:measurement(feud_be_t100, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 100, 0.32).
narrative_ontology:measurement_basis(feud_be_t100, observed).
narrative_ontology:measurement(feud_be_t150, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 150, 0.35).
narrative_ontology:measurement_basis(feud_be_t150, observed).
narrative_ontology:measurement(feud_be_t200, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 200, 0.38).
narrative_ontology:measurement_basis(feud_be_t200, observed).
narrative_ontology:measurement(feud_be_t250, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 250, 0.38).
narrative_ontology:measurement_basis(feud_be_t250, observed).
narrative_ontology:measurement(feud_be_t300, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 300, 0.37).
narrative_ontology:measurement_basis(feud_be_t300, observed).
narrative_ontology:measurement(feud_be_t350, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 350, 0.38).
narrative_ontology:measurement_basis(feud_be_t350, observed).
narrative_ontology:measurement(feud_be_t400, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 400, 0.38).
narrative_ontology:measurement_basis(feud_be_t400, observed).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t0, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(feud_su_t0, observed).
narrative_ontology:measurement(feud_su_t50, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 50, 0.17).
narrative_ontology:measurement_basis(feud_su_t50, observed).
narrative_ontology:measurement(feud_su_t100, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 100, 0.18).
narrative_ontology:measurement_basis(feud_su_t100, observed).
narrative_ontology:measurement(feud_su_t150, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 150, 0.19).
narrative_ontology:measurement_basis(feud_su_t150, observed).
narrative_ontology:measurement(feud_su_t200, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 200, 0.22).
narrative_ontology:measurement_basis(feud_su_t200, observed).
narrative_ontology:measurement(feud_su_t250, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 250, 0.22).
narrative_ontology:measurement_basis(feud_su_t250, observed).
narrative_ontology:measurement(feud_su_t300, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 300, 0.22).
narrative_ontology:measurement_basis(feud_su_t300, observed).
narrative_ontology:measurement(feud_su_t350, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 350, 0.22).
narrative_ontology:measurement_basis(feud_su_t350, observed).
narrative_ontology:measurement(feud_su_t400, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 400, 0.22).
narrative_ontology:measurement_basis(feud_su_t400, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feud_obligation_kernel__stateless_coordination_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(feud_obligation_kernel__stateless_coordination_reading, 0.12).
narrative_ontology:affects_constraint(feud_obligation_kernel__stateless_coordination_reading, feud_obligation_kernel__extraction_cycle_reading).
narrative_ontology:affects_constraint(feud_obligation_kernel__stateless_coordination_reading, feud_obligation_kernel__christianized_pacification_reading).

% DUAL FORMULATION NOTE:
% This story is one reading (stateless-coordination) of a contested kernel. The kernel 'blood-feud obligations' is a persisting social practice (kinship-based retaliation, honor obligation) that different parties and traditions read fundamentally differently. The three readings are: (1) stateless-coordination_reading (this file) — feud as self-enforcing justice mechanism in absence of centralized authority; (2) extraction_cycle_reading — feud as destructive zero-sum cycle that depletes productive capacity; (3) christianized_pacification_reading — feud as violation of divine law requiring ecclesiastical/royal authority. The readings share the same referent (the feud practice) but instantiate different constraints with different ε, beneficiary/victim structures, and types. This is not observer-dependent measurement; it is a reading-contest that shaped historical societies: each reading corresponded to a real faction's worldview (kinship groups defending their practices, church condemning them, emerging states displacing them). The three constraints are linked via network.affects_constraints to show their structural interdependence: each reading's legitimacy claim is contested by the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(feud_obligation_kernel__stateless_coordination_reading, moderate, 0.25).
constraint_indexing:directionality_override(feud_obligation_kernel__stateless_coordination_reading, powerful, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
