% ============================================================================
% CONSTRAINT STORY: feud_obligation_kernel__stateless_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: feud_obligation_kernel__stateless_coordination_reading
 *   human_readable: Blood-Feud Obligation Coordination (Stateless Reading)
 *   domain: legal/anthropological/political
 *
 * SUMMARY:
 *   This constraint story instantiates the stateless_coordination_reading of
 *   the feud_obligation_kernel. Under this reading, blood-feud obligations
 *   function as a self-enforcing justice and deterrence mechanism in the
 *   absence of centralized authority. Feud participants and their kinship
 *   groups are beneficiaries: they receive recognized status, justice-seeking
 *   legitimacy, and deterrent protection through participation in the feud
 *   system. The reading does NOT claim that feud is costless, efficient, or
 *   perpetually beneficial — it claims that feud solves a real coordination
 *   problem (who has the right to retaliate? under what conditions? with what
 *   social effect?) in circumstances where no alternative enforcement
 *   institution exists. The sibling readings (extraction_cycle_reading and
 *   christianized_pacification_reading) contest this framing by emphasizing
 *   feud's destructive accumulation and moral illegitimacy respectively. This
 *   story models feud as rope (genuine coordination) from the seats that
 *   benefit from it, while acknowledging that other readings would classify
 *   the same kernel constraint differently.
 *
 * KEY AGENTS:
 *   - feud_participants: moderate-power individuals and lineage members whose honor and security depend on feud participation
 *   - kinship_groups: moderate-power extended families that organize feuds and whose collective identity is constituted through feud practice
 *   - honor_community: organized network of lords and respected families who validate injury claims and enforce status consequences
 *   - feud_defectors: victims of the constraint who face identity-lock (expulsion) for refusing participation
 *   - wergild_practitioners: excluded alternative dispute resolution actors operating in parallel
 *   - ecclesiastical_authority: excluded critics whose theological authority later rises
 *   - nascent_centralized_authority: observer seat representing state-building forces
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feud_obligation_kernel__stateless_coordination_reading, 0.38).
domain_priors:suppression_score(feud_obligation_kernel__stateless_coordination_reading, 0.42).
domain_priors:theater_ratio(feud_obligation_kernel__stateless_coordination_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feud_obligation_kernel__stateless_coordination_reading, rope).
narrative_ontology:human_readable(feud_obligation_kernel__stateless_coordination_reading, "Blood-Feud Obligation Coordination (Stateless Reading)").
narrative_ontology:topic_domain(feud_obligation_kernel__stateless_coordination_reading, "legal/anthropological/political").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feud_obligation_kernel__stateless_coordination_reading, '4e80bcd4-fce3-4c28-abc8-749d55cf434b').
narrative_ontology:cs_kernel_codification('4e80bcd4-fce3-4c28-abc8-749d55cf434b', distributed).
narrative_ontology:cs_authority_grounding('4e80bcd4-fce3-4c28-abc8-749d55cf434b', practice).
narrative_ontology:cs_interpretation_layer_present('4e80bcd4-fce3-4c28-abc8-749d55cf434b').
narrative_ontology:cs_reading_relation('4e80bcd4-fce3-4c28-abc8-749d55cf434b', feud_obligation_kernel__extraction_cycle_reading, coexists_with).
narrative_ontology:cs_reading_relation('4e80bcd4-fce3-4c28-abc8-749d55cf434b', feud_obligation_kernel__christianized_pacification_reading, coexists_with).
narrative_ontology:cs_axiom('4e80bcd4-fce3-4c28-abc8-749d55cf434b', foundational, kinship_coordinated_retaliation_justifiable).
narrative_ontology:cs_axiom_status(kinship_coordinated_retaliation_justifiable, holdable).
narrative_ontology:cs_axiom_grounding('4e80bcd4-fce3-4c28-abc8-749d55cf434b', kinship_coordinated_retaliation_justifiable, conventional).
narrative_ontology:cs_axiom('4e80bcd4-fce3-4c28-abc8-749d55cf434b', foundational, deterrence_via_credible_retaliation_necessary).
narrative_ontology:cs_axiom_status(deterrence_via_credible_retaliation_necessary, holdable).
narrative_ontology:cs_axiom_grounding('4e80bcd4-fce3-4c28-abc8-749d55cf434b', deterrence_via_credible_retaliation_necessary, instrumental).
narrative_ontology:cs_reference_frame('4e80bcd4-fce3-4c28-abc8-749d55cf434b', acephalous_justice_framework).
narrative_ontology:cs_drift_state('4e80bcd4-fce3-4c28-abc8-749d55cf434b', rising_centralized_authority_ecclesiastical_challenge, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4e80bcd4-fce3-4c28-abc8-749d55cf434b', '').
narrative_ontology:cs_kernel_id(feud_obligation_kernel__stateless_coordination_reading, feud_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__stateless_coordination_reading, feud_participants).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__stateless_coordination_reading, kinship_groups).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__stateless_coordination_reading, honor_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(feud_obligation_kernel__stateless_coordination_reading, feud_defectors).
narrative_ontology:constraint_vindicates(feud_obligation_kernel__stateless_coordination_reading, justice_without_centralized_authority).
narrative_ontology:constraint_vindicates(feud_obligation_kernel__stateless_coordination_reading, deterrence_through_retaliation_credibility).
narrative_ontology:constraint_vindicates(feud_obligation_kernel__stateless_coordination_reading, kinship_solidarity_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Members of kinship groups who initiate or execute feuds in response to injury, insult, or killing. They derive justice through participation in the feud system — the constraint coordinates their right to retaliate and their obligation to support kinship members. Honor and standing in the community depend on fulfilling feud obligations. Exit would mean kinship expulsion and permanent status loss.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, feud_participants, beneficiary,
    moderate, biographical, identity_locked, regional).

% Extended family networks that organize and execute feuds collectively. The constraint allocates resources (warriors, wealth) to defend honor and pursue justice for group members. Group cohesion and external standing depend on reliably mounting feuds when injured. The group's identity is constituted through feud participation.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, kinship_groups, beneficiary,
    moderate, generational, identity_locked, regional).

% The broader community of lords, free persons, and respected families who maintain the norm system — who recognize honor claims, validate injury/insult categories, and enforce status consequences for feud failure or defection. They benefit from the constraint's deterrent effect (fewer unchecked injuries because the cost of inflicting injury is high and certain) and from the order it imposes on otherwise lawless interaction.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, honor_community, beneficiary,
    organized, generational, constrained, regional).

% Individuals or groups who refuse to participate in or execute a feud obligation — whether from incapacity, cowardice, or (later, under Christianization) principle. They bear permanent status loss, kinship expulsion, and loss of group protection. Their refusal creates vulnerability because others will not defend them in future conflicts.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, feud_defectors, payer,
    moderate, biographical, identity_locked, regional).

% Actors who prefer compensation-based settlement (wergild payment for injury, paying or receiving in cash/goods rather than blood). They coexist with feud practitioners and are not suppressed by the constraint — both settlement modes are live. They represent an alternative to feud that does not logically contradict it but offers different incentives.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, wergild_practitioners, excluded,
    powerful, biographical, constrained, regional).

% Church institutions that, by the reading-horizon period, actively campaign against feud participation, framing it as violation of divine law and retaliation prerogative. They are structurally excluded from the feud system's dispute resolution by their theological claims; they would replace the constraint but cannot enforce that replacement without state backing. Their voice in the feud community is initially marginal, later ascending.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, ecclesiastical_authority, excluded,
    powerful, generational, constrained, regional).

% Early state-building authorities (kings, high nobles) who observe feud systems as both useful (maintaining order without central cost) and constraining (preventing their monopoly on violence). They take the role of analytical observers initially; later they become agents of constraint replacement via law codes and enforcement.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, nascent_centralized_authority, observer,
    institutional, generational, analytical, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(feud_obligation_kernel__stateless_coordination_reading, honor_community).
narrative_ontology:fixing_cost_class(feud_obligation_kernel__stateless_coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In the absence of centralized authority to enforce contracts or judge disputes, feud obligations coordinate liability for injuries and insults: they make clear that harming another person carries a high, credible, certain cost (retaliation by the victim's kinship group). This deters casual injury and establishes a framework for recognizing and responding to harm that would otherwise remain unaddressed.
% TRANSFER_FUNCTION: The constraint transfers violence authorization and obligation: from isolated, unrecognized retaliation (a murder is only a murder if someone avenges it) to organized, socially validated feuding. Perpetrators of injuries transfer risk and reputation cost to their kinship groups. Victims and their groups transfer the burden of justice-seeking into a socially legitimated practice that confers honor on successful execution.
% ABSENT_VOICES: Victims of feuds' secondary harms — civilians caught in escalation, merchants disrupted by feud cycles, populations depleted by prolonged cycles — are structurally absent. The constraint's beneficiaries (feud participants, honor community) control the discourse. Wergild practitioners offer an alternative voice but remain marginal in this reading's period. Ecclesiastical critics are present but initially lack enforcement power.
% DISAPPEARANCE_RATIONALE: If blood-feud obligations disappeared overnight and no substitutive system emerged, injury would become unrecognized, honor violations would accumulate without remedy, kinship groups would lack a coordinated response mechanism, and deterrence through retaliation would collapse — the social world would reorganize around the absence of recognized justice, creating vacuum pressure toward either wergild dominance or state monopoly on violence.
% FOUNDING_PROBLEM: In pre-state or weakly-state-governed societies, injuries (killing, insult, resource appropriation) inflicted by one kinship group on another lack a formal remedy mechanism. Victims have no court to appeal to, no authority to levy judgment, no police to enforce restitution. Feud obligations solve this by making retaliation socially legitimate, kinship-coordinated, and self-enforcing: the constraint recognizes the injury and authorizes the response.
% FOUNDING_PROBLEM_CORROBORATION: Anthropological and historical studies of acephalous societies (Nuer, Somali pastoral societies, pre-state Germanic tribes) corroborate that injury/insult recognition and coordinated response through kinship were core features of justice in the absence of centralized authority. Medieval legal historians document feud systems functioning in this role across the early medieval period. Critics (ecclesiastical authorities, later legal theorists) do not deny the founding problem — they dispute whether blood feud is the legitimate solution and whether centralized/divine authority should replace it.
narrative_ontology:disappearance_verdict(feud_obligation_kernel__stateless_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(feud_obligation_kernel__stateless_coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feud_obligation_kernel__stateless_coordination_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(feud_obligation_kernel__stateless_coordination_reading, 'none', 1).

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
 *   Extractiveness is moderate (0.38 at endpoint) because the constraint does solve a real coordination problem and participants genuinely benefit from justice recognition — the system does not exist purely to extract value. However, extractiveness is non-trivial because defectors and collateral victims bear costs that are not balanced by benefits. Suppression is moderate (0.42) because while the constraint uses status loss and kinship expulsion as enforcement mechanisms, wergild practitioners coexist without suppression — alternative dispute settlement is not shut down by feud dominance. Theater is low (0.22) because feud practice is structurally functional, not primarily performative. Accessibility_collapse is moderate-high (0.71) because once embedded in identity and kinship obligation, exiting the feud system is nearly impossible without losing group membership and honor — but it is not total collapse because wergild remains an authorized alternative at the community level. Resistance is moderate (0.58) because over the measurement interval, ecclesiastical and state-building critiques are mounting, but have not yet won enforcement power; the system meets real opposition but continues to function. The measurement series models gradual increasing pressure from alternative authority systems (church, state) that later displace feud — at the endpoint (t=40), metrics slightly decline as the constraint's dominance begins to crack, though it persists.
 *
 * PERSPECTIVAL GAP:
 *   The gap here is reading-specific, not seat-specific. From the stateless_coordination_reading perspective (this story), feud functions as Rope — genuine coordination with real beneficiaries. From the extraction_cycle_reading, the same kernel constraint would classify as Snare — a destructive cycle that benefits warfare-dependent lineages while depleting productive populations. From the christianized_pacification_reading, it would be Snare or Piton — the reading treats feud as theologically illegitimate and structurally extractive regardless of its coordination function. These are not seat divergences; they are reading divergences. The engine computes per-seat classifications from the structural data (beneficiary/victim declarations, power, exit options). This story's authored metrics and stakeholder structure correspond to the stateless_coordination_reading's epistemic framing. A sibling reading would author different beneficiaries (perhaps replacing feud_participants with warfare_dependent_elites and treating broader populations as victims) and would measure different extractiveness (higher) and suppression (higher). The sibling stories would be separate JSON files linked by network.affects_constraints.
 *
 * DIRECTIONALITY LOGIC:
 *   Feud_participants and kinship_groups are the structural beneficiaries: they receive justice recognition, deterrence protection, and honor-based status that only exist through feud participation. Their directionality is low (beneficiary end) because the constraint subsidizes their ability to seek justice and gain social standing. Feud_defectors are the constraint's targets: they bear identity loss and expulsion without receiving the coordination benefits — their directionality is high (victim end). The honor_community sits near symmetric (d~0.5) because they both enforce the constraint's norms and benefit from its deterrent effect, but they also bear the costs of feud cycles and escalation. Wergild_practitioners are excluded rather than victims — they represent an alternative not suppressed by the constraint. Ecclesiastical_authority and nascent_centralized_authority are observers: they have not yet captured or replaced the constraint, though they are actively working to do so.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live: in acephalous or weakly-state societies, injury lacks a remedy mechanism, and feud solves it. The disappearance verdict is world_rearranges: the system is necessary to the order it maintains. However, the mandatrophy question arises in the interval: as centralized authority rises and ecclesiastical power grows (the measurement interval end), the founding problem's status begins to shift toward 'contested' — state authorities argue that they can now provide the justice function better than feud, and church authorities dispute feud's moral legitimacy. The constraint does NOT exhibit classical mandatrophy (persistence after function death) — rather, it shows the early signs of functional displacement that will accelerate in the subsequent period. Theater is low, indicating the constraint is not primarily performative in this reading. The measurement series capture the moment at which alternative authority systems are rising but have not yet achieved dominance — the constraint persists functionally because centralized alternatives are not yet capable of replacing it everywhere, and ecclesiastical authority cannot enforce its theological claims without state backing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_accumulation,
    'Does feud function primarily as a coordination mechanism for justice-seeking, or does it primarily accumulate extractive costs (blood cycles, territorial destabilization) that overwhelm its coordination benefit?',
    'Comparative analysis of feuding societies: do societies with functional feud systems show lower injury rates and greater stability (supporting coordination reading) or higher casualty rates and less territoriality (supporting extraction_cycle_reading)? Historical reconstruction of feud intervals and settlement patterns.',
    'If coordination dominates: feud is rope; the extraction measured in this story is legitimate coordination cost. If extraction dominates: feud is snare; the extraction rises and beneficiary claims are cover stories for elite war-economy benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_accumulation, empirical, 'Whether feud''s justice function outweighs its destructive accumulation or vice versa.').

omega_variable(
    identity_lock_internalization,
    'Is the identity_lock status of feud participants and kinship groups a structural lock (kinship obligation is external and enforced) or an internalized lock (participants have fused their identity with feud practice such that exit feels impossible even absent enforcement)?',
    'Post-displacement observation: when centralized authority or church successfully displaces feud (via law codes, enforcement, or theological conversion), do participants and kinship groups quickly abandon feud practice, or do some groups continue feuding despite suppression? Persistence indicates internalization; rapid abandonment indicates primarily structural lock.',
    'If structural: defectors can exit if external enforcement changes; the constraint''s persistence depends on continued state weakness. If internalized: even with centralized authority available, some participants continue feuding because their identity is constituted through it; the constraint persists as behavioral residue and cultural practice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_internalization, empirical, 'Whether identity-lock is structural or internalized in feud participants.').

omega_variable(
    reading_boundary_contest,
    'Is the boundary between stateless_coordination_reading and extraction_cycle_reading a genuine structural difference, or an observer-perspective difference imposed on a single constraint?',
    'Test whether changing the observational frame (measuring from the warrior-elite seat vs. the broader-population seat) produces a reclassification while keeping ε stable. If reclassification requires different ε values, the readings describe different constraints (supporting two-story decomposition). If the same ε can generate different readings via observer perspective alone, the boundary is perspective, not structural.',
    'If structural: the sibling readings are legitimately separate constraint stories with different ε values, different beneficiary/victim sets, and linked via network.affects_constraints. If perspective-only: the readings are the same constraint viewed from different seats, and should not spawn separate stories.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_boundary_contest, conceptual, 'Whether the three kernel readings are structurally distinct constraints or observer-relative perspectives on one constraint.').

omega_variable(
    wergild_coexistence_mechanism,
    'Why does wergild (compensation-based settlement) coexist with blood-feud in the same community without suppressing either? What structural factors allow both to remain live options?',
    'Historical evidence: under what conditions do participants choose wergild over feud, and vice versa? If wergild is chosen primarily by powerless parties (unable to mount feuds effectively) and feud by powerful parties, the coexistence reflects differential power, not genuine alternatives. If all parties have access to both and choose based on circumstance/preference, the coexistence reflects genuine alternative mechanisms.',
    'If wergild is a genuine alternative (low suppression is correct): feud''s measured extractiveness is the cost of choosing to participate in feud rather than wergild. If wergild is only available to powerless groups: the measured suppression understates the constraint''s enforcement (feud is suppressing wergild for some parties), and the feud system is more extractive than authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wergild_coexistence_mechanism, empirical, 'Whether wergild and feud are genuine coexisting alternatives or wergild is suppressed for powerful parties.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feud_obligation_kernel__stateless_coordination_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t0, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(feud_tr_t5, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 5, 0.19).
narrative_ontology:measurement(feud_tr_t10, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(feud_tr_t20, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(feud_tr_t30, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 30, 0.24).
narrative_ontology:measurement(feud_tr_t40, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(feud_be_t0, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(feud_be_t5, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 5, 0.32).
narrative_ontology:measurement(feud_be_t10, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(feud_be_t20, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(feud_be_t30, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 30, 0.41).
narrative_ontology:measurement(feud_be_t40, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 40, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t0, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(feud_su_t5, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 5, 0.38).
narrative_ontology:measurement(feud_su_t10, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(feud_su_t20, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(feud_su_t30, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 30, 0.44).
narrative_ontology:measurement(feud_su_t40, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 40, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feud_obligation_kernel__stateless_coordination_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(feud_obligation_kernel__stateless_coordination_reading, 0.25).
narrative_ontology:affects_constraint(feud_obligation_kernel__stateless_coordination_reading, feud_obligation_kernel__extraction_cycle_reading).
narrative_ontology:affects_constraint(feud_obligation_kernel__stateless_coordination_reading, feud_obligation_kernel__christianized_pacification_reading).

% DUAL FORMULATION NOTE:
% The feud_obligation_kernel decomposes into three constraint stories representing three distinct readings of blood-feud practices: (1) stateless_coordination_reading (this story) — feud as functional justice mechanism in acephalous societies; (2) extraction_cycle_reading — feud as destructive war-economy accumulation; (3) christianized_pacification_reading — feud as theologically illegitimate practice to be replaced by church/state justice. These are not seat-perspective differences on one constraint; they are structural interpretations with different ε values, different beneficiary/victim structures, and different claimed_types. Each reading produces a different classification from the same underlying practices. They are linked via affects_constraints to show the kernel contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
