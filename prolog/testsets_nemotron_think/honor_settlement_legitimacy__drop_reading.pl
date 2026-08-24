% ============================================================================
% CONSTRAINT STORY: honor_settlement_legitimacy__drop_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_settlement_legitimacy__drop_reading, []).

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
 *   constraint_id: honor_settlement_legitimacy__drop_reading
 *   human_readable: Honor Settlement Legitimacy (Drop Reading: Niche Persistence)
 *   domain: historical_sociology/legal_history/cultural_anthropology
 *
 * SUMMARY:
 *   The drop_reading of honor_settlement_legitimacy claims that honor culture
 *   — including dueling as its ultimate enforcement — persists as a live,
 *   functional option in specific geographic and social niches (rural
 *   Southern communities, military subcultures, aristocratic remnants,
 *   certain immigrant enclaves) despite 160 years of state suppression.
 *   Dueling is driven underground but not eliminated from the normative
 *   repertoire; adherents still treat it as a legitimate, if costly,
 *   recourse. This constraint story models the honor settlement system as a
 *   tangled rope: it genuinely coordinates dispute resolution for adherents
 *   (beneficiary function) while the state's active suppression extracts
 *   compliance costs from the same adherents (victim function). The system is
 *   neither a pure coordination mechanism (rope) nor pure extraction (snare)
 *   — it is a hybrid sustained by identity-locked participation and uneven
 *   state enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_settlement_legitimacy__drop_reading, 0.58).
domain_priors:suppression_score(honor_settlement_legitimacy__drop_reading, 0.72).
domain_priors:theater_ratio(honor_settlement_legitimacy__drop_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_settlement_legitimacy__drop_reading, tangled_rope).
narrative_ontology:human_readable(honor_settlement_legitimacy__drop_reading, "Honor Settlement Legitimacy (Drop Reading: Niche Persistence)").
narrative_ontology:topic_domain(honor_settlement_legitimacy__drop_reading, "historical_sociology/legal_history/cultural_anthropology").

domain_priors:requires_active_enforcement(honor_settlement_legitimacy__drop_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_settlement_legitimacy__drop_reading, '7c3bea8a-c014-467b-94e6-9003d4b0a0a2').
narrative_ontology:cs_kernel_codification('7c3bea8a-c014-467b-94e6-9003d4b0a0a2', distributed).
narrative_ontology:cs_authority_grounding('7c3bea8a-c014-467b-94e6-9003d4b0a0a2', practice).
narrative_ontology:cs_interpretation_layer_present('7c3bea8a-c014-467b-94e6-9003d4b0a0a2').
narrative_ontology:cs_reading_relation('7c3bea8a-c014-467b-94e6-9003d4b0a0a2', honor_settlement_legitimacy__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('7c3bea8a-c014-467b-94e6-9003d4b0a0a2', honor_settlement_legitimacy__composite_reading, coexists_with).
narrative_ontology:cs_axiom('7c3bea8a-c014-467b-94e6-9003d4b0a0a2', foundational, honor_legitimacy_persists_in_niches).
narrative_ontology:cs_axiom_status(honor_legitimacy_persists_in_niches, holdable).
narrative_ontology:cs_axiom_grounding('7c3bea8a-c014-467b-94e6-9003d4b0a0a2', honor_legitimacy_persists_in_niches, empirically_contingent).
narrative_ontology:cs_axiom('7c3bea8a-c014-467b-94e6-9003d4b0a0a2', secondary, dueling_suppressed_not_eliminated).
narrative_ontology:cs_axiom_status(dueling_suppressed_not_eliminated, holdable).
narrative_ontology:cs_axiom_grounding('7c3bea8a-c014-467b-94e6-9003d4b0a0a2', dueling_suppressed_not_eliminated, empirically_contingent).
narrative_ontology:cs_reference_frame('7c3bea8a-c014-467b-94e6-9003d4b0a0a2', honor_settlement_as_live_niche_practice).
narrative_ontology:cs_drift_state('7c3bea8a-c014-467b-94e6-9003d4b0a0a2', contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7c3bea8a-c014-467b-94e6-9003d4b0a0a2', '').
narrative_ontology:cs_kernel_id(honor_settlement_legitimacy__drop_reading, honor_settlement_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__drop_reading, residual_honor_adherents).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__drop_reading, niche_kinship_networks).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__drop_reading, residual_honor_adherents).
narrative_ontology:constraint_vindicates(honor_settlement_legitimacy__drop_reading, honor_as_coordination_mechanism).
narrative_ontology:constraint_vindicates(honor_settlement_legitimacy__drop_reading, private_settlement_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain honor-based dispute resolution including dueling as ultimate enforcement in geographic/social niches (rural South, military subcultures, aristocratic remnants, certain immigrant communities). They gain functional coordination: disputes settle without state courts, reputation is protected, collective identity coheres. They bear costs: legal jeopardy for dueling, social marginalization from mainstream, internal discipline demands, risk of escalation. Exit means abandoning the honor identity that constitutes their self-concept and community belonging — not merely changing a practice but dissolving who they are.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, residual_honor_adherents, payer,
    moderate, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(honor_settlement_legitimacy__drop_reading, residual_honor_adherents, beneficiary).

% Extended kin groups in honor-persistent niches (Appalachian clans, Borderer-descended communities, Creole planter remnants, Western ranching dynasties). They benefit from the honor system's ability to resolve intra-group disputes without external intervention, protect collective reputation, and mobilize mutual defense. Their constraint is maintaining the system across generations while state law encroaches; exit would fracture the kinship solidarity the system sustains.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, niche_kinship_networks, beneficiary,
    organized, generational, constrained, local).

% Courts, legislatures, and law enforcement that criminalize dueling and suppress honor violence. They set the agenda by defining legitimate dispute resolution as exclusively state-administered. They benefit from monopoly on legitimate violence and legal uniformity. Their enforcement is active but geographically uneven — vigorous in cities, sporadic in remote niches where honor persists. They could abolish the fringe practice entirely but tolerate it as low-priority residue.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, state_legal_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% General population in areas where honor culture has receded. They view dueling as archaic and illegitimate, rely on state courts, and experience the constraint only indirectly (e.g., when honor violence spills into public view). They neither benefit from nor pay for the niche honor system; their consent to state monopoly on violence is the background condition.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, mainstream_citizens, observer,
    organized, biographical, mobile, national).

% Scholars documenting the persistence and transformation of honor cultures. They see the full structural picture: the coordination function for adherents, the extraction by state suppression, the identity-lock dynamics. Their analysis does not affect the constraint's operation but records its trajectory.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, historical_anthropologists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a self-contained dispute resolution and reputation-management system for communities that lack trust in or access to state institutions — settling insults, debts, and status conflicts through ritualized confrontation and collective enforcement, without external arbitration.
% TRANSFER_FUNCTION: Moves dispute-resolution authority and reputational capital from state courts to local honor networks; moves legal risk and social marginalization from mainstream society onto residual adherents; moves enforcement effort from state (sporadic suppression) to adherents (internal discipline, secrecy costs).
% ABSENT_VOICES: Women and junior males within honor-persistent niches who bear disproportionate costs (gendered violence, forced marriage, suppressed dissent) but lack voice in the honor system's governance. They are structurally excluded — the honor system's legitimacy depends on their silence. Also absent: state authorities in remote niches who could enforce but choose not to, effectively consenting to the fringe persistence.
% DISAPPEARANCE_RATIONALE: If the niche honor system vanished overnight, adherents would lose their primary coordination mechanism for dispute resolution and collective identity. Kinship networks would face unresolved conflicts, reputation markets would collapse, and the social fabric of these niches would reorganize — likely toward state dependence or chaotic fragmentation. The mainstream would barely notice.
% FOUNDING_PROBLEM: Post-Civil War / post-Reconstruction collapse of state legitimacy in specific regions (South, Borderlands, frontier) left a vacuum: no trusted courts, no reliable law enforcement, no shared norms with distant authorities. Honor culture filled this vacuum with a peer-enforced, kin-backed system that worked where the state did not.
% FOUNDING_PROBLEM_CORROBORATION: Adherents attest the founding problem persists: state courts remain distant, biased, or untrusted in their niches. Historians (Ayers, Wyatt-Brown, Greenberg) corroborate the original vacuum but argue it has largely closed — state capacity now reaches these areas. Criminologists note honor violence persists even where state access exists, suggesting the founding problem is no longer the sole driver. No single outside source resolves the dispute.
narrative_ontology:disappearance_verdict(honor_settlement_legitimacy__drop_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_settlement_legitimacy__drop_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_settlement_legitimacy__drop_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_settlement_legitimacy__drop_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_settlement_legitimacy__drop_reading, 0.58, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_settlement_legitimacy__drop_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_settlement_legitimacy__drop_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_settlement_legitimacy__drop_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects the real but partial costs adherents bear: legal jeopardy, social marginalization, internal discipline — costs that have risen as state capacity expanded but plateaued as enforcement stabilized at a low-intensity equilibrium. Suppression (0.72) is high because the constraint's persistence depends on state actively criminalizing dueling and honor violence; without enforcement, the system would either expand (if coordination value dominates) or collapse (if only fear sustained it). Theater ratio (0.45) is moderate: the honor system's dispute-resolution function is real, but a growing share of its practice is performative — duels are rare, replaced by ritualized apologies and mediated settlements that mimic the form without the lethal substance. Accessibility collapse (0.52) is partial: state courts exist and are accessible, but adherents perceive them as illegitimate or ineffective for their specific disputes. Resistance (0.38) is low-moderate: adherents resist through secrecy and cultural persistence, not open confrontation.
 *
 * PERSPECTIVAL GAP:
 *   From the adherent seat, the constraint feels like a rope: a working coordination system they voluntarily maintain despite costs. From the state seat, it looks like a snare to be suppressed: an illegitimate parallel justice system. From the scholar seat, it computes as tangled_rope: genuine coordination hybridized with asymmetric extraction. The engine will compute per-seat types from the structural data; this divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Residual honor adherents are structurally dual-positioned: they are beneficiaries (the system coordinates their disputes, protects reputation, constitutes identity) AND payers (they bear legal risk, marginalization, internal discipline). This dual role is the tangled rope's signature — the same agents are coordinated and extracted from. State legal authorities are agenda_setters: they define the legitimate dispute-resolution monopoly and enforce it, but their enforcement is calibrated to tolerate the fringe residue. Mainstream citizens and scholars are observers: they neither benefit nor pay. The identity_locked exit for adherents is critical — leaving the honor system means abandoning the identity that constitutes their self and community, not merely switching dispute-resolution providers. This locks them into the extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (state vacuum post-1865) is contested as still live. Adherents say yes — state courts remain untrusted. Historians say no — state capacity now reaches these niches. The constraint persists not because the founding problem is live, but because identity lock and institutional inertia sustain it. This is mandatrophy: the arrangement's original justification has atrophied, but the constraint remains because adherents' identity is fused to it and the state lacks incentive for total eradication. The drop_reading captures this by claiming honor culture remains a live option — not because it solves the original problem, but because it has become constitutive of the niche identity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'How does the drop_reading''s claim that honor legitimacy persists in niches structurally relate to the contraction_reading''s claim that honor became cognitively unthinkable, and the composite_reading''s claim of overdetermined decline?',
    'Comparative analysis of niche vs. mainstream honor discourse: if niche adherents explicitly reject the contraction narrative and maintain distinct normative vocabularies, the readings coexist as descriptions of different populations. If niche discourse shows contamination from mainstream frameworks, the readings may influence each other.',
    'If readings coexist, the kernel is distributed across populations with no single authoritative reading. If drop_reading forecloses contraction_reading in niches, the kernel has a fragmented but internally authoritative structure. This affects whether the kernel''s CS classification is distributed or fixed_text.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Structural relationship between sibling readings of the honor_settlement_legitimacy kernel').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of dueling in honor niches structural (legal criminalization, police enforcement) or internalized (adherents'' own declining willingness to duel, moral discomfort)?',
    'Post-legalization thought experiment: if dueling were decriminalized tomorrow, would niche adherence rates rise, stay flat, or fall? A rise indicates structural suppression; flat/fall indicates internalized suppression.',
    'If internalized, the constraint''s effective suppression is higher than the legal measure suggests — adherents carry the suppression with them. This would increase the extraction experienced by adherents (they self-suppress) and shift the constraint toward snare. If structural, the tangled_rope classification holds: coordination function is real, state extraction is the asymmetric component.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Whether suppression of dueling in honor niches is externally imposed or internally absorbed').

omega_variable(
    coordination_vs_identity_persistence,
    'Does the honor system persist in niches because it still solves a coordination problem (dispute resolution without state courts), or because honor identity has become self-sustaining independent of functional utility?',
    'Measure dispute-resolution outcomes in honor niches vs. comparable non-honor niches: if honor system produces better/faster/cheaper resolutions, coordination function is live. If outcomes are worse but adherence persists, identity maintenance is the driver.',
    'If coordination-driven, the constraint is a genuine tangled_rope with live rope component. If identity-driven, the rope component is atrophied and the constraint trends toward piton (theatrical maintenance of a degraded function). This distinction determines whether the system could revive if state capacity withdrew.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_identity_persistence, empirical, 'Whether niche honor persistence is functionally motivated or identity-locked').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_settlement_legitimacy__drop_reading, 1865, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(honor_settlement_legitimacy__drop_reading_tr_t1865, honor_settlement_legitimacy__drop_reading, theater_ratio, 1865, 0.15).
narrative_ontology:measurement(honor_settlement_legitimacy__drop_reading_tr_t1890, honor_settlement_legitimacy__drop_reading, theater_ratio, 1890, 0.22).
narrative_ontology:measurement(honor_settlement_legitimacy__drop_reading_tr_t1920, honor_settlement_legitimacy__drop_reading, theater_ratio, 1920, 0.31).
narrative_ontology:measurement(honor_settlement_legitimacy__drop_reading_tr_t1950, honor_settlement_legitimacy__drop_reading, theater_ratio, 1950, 0.38).
narrative_ontology:measurement(honor_settlement_legitimacy__drop_reading_tr_t1980, honor_settlement_legitimacy__drop_reading, theater_ratio, 1980, 0.42).
narrative_ontology:measurement(honor_settlement_legitimacy__drop_reading_tr_t2000, honor_settlement_legitimacy__drop_reading, theater_ratio, 2000, 0.44).
narrative_ontology:measurement(honor_settlement_legitimacy__drop_reading_tr_t2025, honor_settlement_legitimacy__drop_reading, theater_ratio, 2025, 0.45).

% Extraction over time
narrative_ontology:measurement(honor_settlement_legitimacy__drop_reading_be_t1865, honor_settlement_legitimacy__drop_reading, base_extractiveness, 1865, 0.35).
narrative_ontology:measurement(honor_settlement_legitimacy__drop_reading_be_t1890, honor_settlement_legitimacy__drop_reading, base_extractiveness, 1890, 0.42).
narrative_ontology:measurement(honor_settlement_legitimacy__drop_reading_be_t1920, honor_settlement_legitimacy__drop_reading, base_extractiveness, 1920, 0.48).
narrative_ontology:measurement(honor_settlement_legitimacy__drop_reading_be_t1950, honor_settlement_legitimacy__drop_reading, base_extractiveness, 1950, 0.52).
narrative_ontology:measurement(honor_settlement_legitimacy__drop_reading_be_t1980, honor_settlement_legitimacy__drop_reading, base_extractiveness, 1980, 0.55).
narrative_ontology:measurement(honor_settlement_legitimacy__drop_reading_be_t2000, honor_settlement_legitimacy__drop_reading, base_extractiveness, 2000, 0.57).
narrative_ontology:measurement(honor_settlement_legitimacy__drop_reading_be_t2025, honor_settlement_legitimacy__drop_reading, base_extractiveness, 2025, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(honor_settlement_legitimacy__drop_reading_su_t1865, honor_settlement_legitimacy__drop_reading, suppression_requirement, 1865, 0.55).
narrative_ontology:measurement(honor_settlement_legitimacy__drop_reading_su_t1890, honor_settlement_legitimacy__drop_reading, suppression_requirement, 1890, 0.62).
narrative_ontology:measurement(honor_settlement_legitimacy__drop_reading_su_t1920, honor_settlement_legitimacy__drop_reading, suppression_requirement, 1920, 0.68).
narrative_ontology:measurement(honor_settlement_legitimacy__drop_reading_su_t1950, honor_settlement_legitimacy__drop_reading, suppression_requirement, 1950, 0.7).
narrative_ontology:measurement(honor_settlement_legitimacy__drop_reading_su_t1980, honor_settlement_legitimacy__drop_reading, suppression_requirement, 1980, 0.71).
narrative_ontology:measurement(honor_settlement_legitimacy__drop_reading_su_t2000, honor_settlement_legitimacy__drop_reading, suppression_requirement, 2000, 0.71).
narrative_ontology:measurement(honor_settlement_legitimacy__drop_reading_su_t2025, honor_settlement_legitimacy__drop_reading, suppression_requirement, 2025, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_settlement_legitimacy__drop_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(honor_settlement_legitimacy__drop_reading, 0.08).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__drop_reading, honor_settlement_legitimacy__contraction_reading).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__drop_reading, honor_settlement_legitimacy__composite_reading).

% DUAL FORMULATION NOTE:
% This drop_reading and its siblings (contraction_reading, composite_reading) form a constraint family decomposing the honor_settlement_legitimacy kernel. The drop_reading claims niche persistence (ε=0.58); contraction_reading claims cognitive unthinkability (ε≈0.1 for mainstream); composite_reading claims overdetermined decline (ε intermediate). They differ in ε because they describe different populations and mechanisms. Linked via affects_constraints for contamination analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honor_settlement_legitimacy__drop_reading, moderate, 0.65).
constraint_indexing:directionality_override(honor_settlement_legitimacy__drop_reading, organized, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
