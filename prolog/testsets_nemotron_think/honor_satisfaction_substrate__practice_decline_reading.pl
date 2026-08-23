% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_substrate__practice_decline_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_substrate__practice_decline_reading, []).

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
 *   constraint_id: honor_satisfaction_substrate__practice_decline_reading
 *   human_readable: Honor Satisfaction Substrate — Practice Decline Reading
 *   domain: historical_sociology/cultural_anthropology/legal_history
 *
 * SUMMARY:
 *   This constraint story captures the practice_decline_reading of the
 *   honor_satisfaction_substrate kernel: the honor code as a normative
 *   substrate persists (military honor codes, Southern culture of honor,
 *   dueling rhetoric in politics) while the practice of dueling declines due
 *   to exogenous enforcement — state legal prohibition, institutional
 *   barriers (army regulations, professional codes), and rising opportunity
 *   costs (professionalization, bureaucratic careers). The constraint is a
 *   rope: the honor code once solved a genuine coordination problem
 *   (decentralized status dispute resolution), but legal centralization
 *   creates a coordination failure by suppressing the enforcement mechanism
 *   without fully replacing its social function. The honor culture
 *   participants are payers; state institutions are beneficiaries. The engine
 *   computes per-seat types from this structural data.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_substrate__practice_decline_reading, 0.22).
domain_priors:suppression_score(honor_satisfaction_substrate__practice_decline_reading, 0.58).
domain_priors:theater_ratio(honor_satisfaction_substrate__practice_decline_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_substrate__practice_decline_reading, rope).
narrative_ontology:human_readable(honor_satisfaction_substrate__practice_decline_reading, "Honor Satisfaction Substrate — Practice Decline Reading").
narrative_ontology:topic_domain(honor_satisfaction_substrate__practice_decline_reading, "historical_sociology/cultural_anthropology/legal_history").

domain_priors:requires_active_enforcement(honor_satisfaction_substrate__practice_decline_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_substrate__practice_decline_reading, 'bb3ed3e7-2e9b-458e-a46d-c8ee943d9a6d').
narrative_ontology:cs_kernel_codification('bb3ed3e7-2e9b-458e-a46d-c8ee943d9a6d', distributed).
narrative_ontology:cs_authority_grounding('bb3ed3e7-2e9b-458e-a46d-c8ee943d9a6d', practice).
narrative_ontology:cs_interpretation_layer_present('bb3ed3e7-2e9b-458e-a46d-c8ee943d9a6d').
narrative_ontology:cs_reading_relation('bb3ed3e7-2e9b-458e-a46d-c8ee943d9a6d', honor_satisfaction_substrate__cultural_contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('bb3ed3e7-2e9b-458e-a46d-c8ee943d9a6d', honor_satisfaction_substrate__composite_overdetermined_reading, influences).
narrative_ontology:cs_axiom('bb3ed3e7-2e9b-458e-a46d-c8ee943d9a6d', foundational, honor_code_substrate_persists).
narrative_ontology:cs_axiom_status(honor_code_substrate_persists, holdable).
narrative_ontology:cs_axiom_grounding('bb3ed3e7-2e9b-458e-a46d-c8ee943d9a6d', honor_code_substrate_persists, conventional).
narrative_ontology:cs_axiom('bb3ed3e7-2e9b-458e-a46d-c8ee943d9a6d', foundational, exogenous_suppression_primary).
narrative_ontology:cs_axiom_status(exogenous_suppression_primary, holdable).
narrative_ontology:cs_axiom_grounding('bb3ed3e7-2e9b-458e-a46d-c8ee943d9a6d', exogenous_suppression_primary, empirically_contingent).
narrative_ontology:cs_reference_frame('bb3ed3e7-2e9b-458e-a46d-c8ee943d9a6d', pre_legal_centralization_honor_order).
narrative_ontology:cs_drift_state('bb3ed3e7-2e9b-458e-a46d-c8ee943d9a6d', post_legal_monopoly_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('bb3ed3e7-2e9b-458e-a46d-c8ee943d9a6d', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_substrate__practice_decline_reading, honor_satisfaction_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__practice_decline_reading, state_legal_institutions).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__practice_decline_reading, modern_bureaucracy).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__practice_decline_reading, centralized_judiciary).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__practice_decline_reading, honor_culture_participants).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__practice_decline_reading, gentry_aristocracy).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__practice_decline_reading, military_officer_corps).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__practice_decline_reading, military_officer_corps).
narrative_ontology:constraint_vindicates(honor_satisfaction_substrate__practice_decline_reading, state_monopoly_on_violence).
narrative_ontology:constraint_vindicates(honor_satisfaction_substrate__practice_decline_reading, legal_centralization_as_pacification).
narrative_ontology:constraint_vindicates(honor_satisfaction_substrate__practice_decline_reading, impersonal_dispute_resolution_superiority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts and enforces legal prohibitions on dueling; establishes courts as the sole legitimate venue for dispute resolution. Gains monopoly on legitimate violence and dispute resolution fees. Faces no meaningful exit from this role — it defines the institution.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, state_legal_institutions, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_substrate__practice_decline_reading, state_legal_institutions, beneficiary).

% Benefits from predictable, rule-bound dispute resolution that replaces unpredictable private violence. Administrative stability and tax collection are easier when honor violence is suppressed. No exit — the bureaucracy is the beneficiary of the constraint's success.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, modern_bureaucracy, beneficiary,
    institutional, generational, analytical, national).

% Gains jurisdiction and authority over status and insult disputes previously settled by duel. Professional prestige and institutional relevance expand. The constraint's enforcement is the judiciary's institutional foundation.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, centralized_judiciary, beneficiary,
    institutional, generational, analytical, national).

% Members of honor cultures (Southern planters, European aristocrats, urban gentlemen) who lose access to the satisfaction mechanism. Must either absorb insults, use courts (which they view as inadequate for honor), or risk prosecution. Exit is constrained by social identity — leaving the honor culture means status death.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, honor_culture_participants, payer,
    moderate, biographical, constrained, regional).

% Traditional elite whose status system depended on the duel as proof of courage and autonomy. Legal prohibition strips a key status-maintenance tool. They retain wealth and political influence but lose the performative mechanism of honor. Exit is constrained by class identity — they cannot 'opt out' of being gentlemen.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, gentry_aristocracy, payer,
    powerful, biographical, constrained, national).

% Officers historically used dueling to maintain corps cohesion and individual honor. Prohibition forces substitution with formal military justice and codes of conduct. They pay the cost of losing a traditional bonding mechanism but benefit from a more disciplined, legally legible force. Exit is constrained by professional identity and chain of command.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, military_officer_corps, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_substrate__practice_decline_reading, military_officer_corps, beneficiary).

% Rural and peripheral communities (American South, European borderlands, colonial frontiers) where honor culture persists informally. They are excluded from the legislative process that bans dueling and from the courts that replace it. Their continued informal practice is criminalized. Exit is trapped — geographic isolation and cultural identity prevent assimilation to legal norms.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, traditional_honor_communities, excluded,
    moderate, biographical, trapped, regional).

% Analyze the constraint from outside: Weber on monopoly of violence, Elias on civilizing process, Nisbett & Cohen on culture of honor. They neither pay nor collect; they map the structural transformation. Their exit is analytical — they can change frameworks but not the history.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, historical_sociologists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The honor code provided a decentralized coordination mechanism for status disputes and reputation management without state intervention — a self-enforcing system where the threat of duel compelled civility and honest signaling among equals.
% TRANSFER_FUNCTION: The constraint transfers the satisfaction function from private violence to state courts — moves dispute resolution from honor participants (who bear risk and cost) to legal institutions (which collect fees, authority, and monopoly legitimacy). The honor code substrate remains but its enforcement mechanism is expropriated.
% ABSENT_VOICES: Traditional honor communities, rural gentry, military traditionalists, and frontier populations who saw dueling as essential to their status system and community cohesion. They were not consulted in legislative bans; their continued practice was criminalized rather than negotiated. Their absence is structural — the constraint's beneficiaries (state institutions) defined them as obstacles to order.
% DISAPPEARANCE_RATIONALE: If legal prohibitions and institutional barriers vanished overnight, the honor code substrate — still culturally alive in military codes, Southern 'culture of honor,' and elite social norms — would likely reactivate dueling or analogous satisfaction practices. The constraint suppresses practice; it does not erase the normative grammar. Revival would be rapid where identity_locked populations remain.
% FOUNDING_PROBLEM: Managing status disputes and reputation in pre-state or weak-state societies where legal institutions could not reliably enforce contracts, protect honor, or adjudicate insults among social equals. The duel was a commitment device: only those willing to risk death for their word could claim gentlemanly status.
% FOUNDING_PROBLEM_CORROBORATION: Weber (state monopoly on violence), Elias (civilizing process), and Nisbett & Cohen (culture of honor as historical residue) all document that the original problem — absence of reliable impersonal dispute resolution — was solved by legal centralization. The honor code's persistence in military and Southern contexts is documented by historians (Wyatt-Brown, Greenberg) as cultural survival, not functional necessity. No corroborating source outside the honor cultures themselves argues the founding problem remains live.
narrative_ontology:disappearance_verdict(honor_satisfaction_substrate__practice_decline_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_substrate__practice_decline_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_substrate__practice_decline_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_satisfaction_substrate__practice_decline_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_substrate__practice_decline_reading, 0.22, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_substrate__practice_decline_reading_tests).
:- end_tests(honor_satisfaction_substrate__practice_decline_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.22) because the constraint's primary operation is coordination (honor code as status grammar) and the extraction is the state's capture of the enforcement monopoly — not a rent stream from participants. Suppression is moderate (0.58) because legal prohibition and institutional barriers actively prevent the practice, but the normative substrate remains accessible. Theater ratio is low (0.18) — the legal suppression is functional (state monopoly on violence), not performative. Accessibility collapse is moderate (0.42) — alternatives (courts, military justice) exist but are experienced as inadequate for honor by participants. Resistance is moderate (0.48) — dueling persisted illegally for decades; military and Southern cultures maintained shadow practices.
 *
 * PERSPECTIVAL GAP:
 *   From the state seat, the constraint is a rope: genuine coordination problem (dispute resolution) solved by superior mechanism (courts). From the honor participant seat, it is experienced as a snare: the coordination grammar (honor code) remains but its enforcement is coercively suppressed, leaving them with costly signals (absorbing insults, using inadequate courts). The engine computes this seat divergence — the honor participant's effective extraction is amplified by identity_locked exit; the state's is dampened by beneficiary position.
 *
 * DIRECTIONALITY LOGIC:
 *   State institutions (agenda_setter/beneficiary) sit at d ≈ 0.1 — they collect the monopoly on legitimate violence. Honor culture participants and gentry (payers) sit at d ≈ 0.8 — they lose the satisfaction mechanism, face prosecution, and have constrained exit (identity_locked by class/culture). Military officers are dual: they pay (lose traditional bonding) but benefit (gain disciplined legal force) — d ≈ 0.45. Traditional honor communities are excluded and trapped (d ≈ 0.95) — criminalized without representation. The engine derives these from beneficiary/victim declarations + exit_options.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (no reliable state dispute resolution) is dead — modern courts solve it. But the constraint persists because the honor code substrate survives as cultural grammar. This is not mandatrophy (a functionless remnant maintained by inertia) — the legal prohibition actively suppresses a live practice. The rope classification captures this: coordination function atrophied for the state (courts work), but the substrate's coordination logic remains live for identity_locked participants. The constraint would be a piton if the state maintained dueling bans purely ceremonially; it does not — enforcement is real.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Where does the practice_decline_reading''s claim (honor code persists, only practice suppressed) shade into the cultural_contraction_reading''s claim (honor code itself transforms)?',
    'Comparative historical analysis of honor discourse in military codes, Southern literature, and European aristocratic memoirs 1850-1920: if the *language* of honor changes (e.g., from ''satisfaction'' to ''dignity''), the substrate has contracted; if only the *enforcement* changes, the substrate persists.',
    'If substrate contracts, the constraint becomes a scaffold (transitional support for a dying norm) or mountain erosion (natural decay of a cultural form). If substrate persists, rope classification holds — coordination failure under legal pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether the honor code substrate is stable or transforming — the boundary between this reading and cultural_contraction_reading.').

omega_variable(
    exogenous_vs_endogenous_weight,
    'How much of the dueling decline is attributable to exogenous legal suppression versus endogenous cultural change?',
    'Counterfactual analysis: jurisdictions with similar honor cultures but different legal timelines (e.g., US South vs. France vs. Prussia). If dueling declines at similar rates regardless of legal severity, endogenous weight rises. If decline tracks legal enforcement intensity, exogenous weight rises.',
    'High endogenous weight supports composite_overdetermined_reading or cultural_contraction_reading. High exogenous weight supports this reading''s rope classification — the constraint is an external pressure on a live coordination system.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exogenous_vs_endogenous_weight, empirical, 'Causal weight of legal prohibition vs. cultural transformation in driving practice decline.').

omega_variable(
    identity_locked_exit_mechanism,
    'Is the honor_culture_participants'' constrained exit truly identity_locked (self-concept constituted through honor) or structurally trapped (no alternative status system available)?',
    'Micro-historical analysis of individual trajectories: did gentlemen who abandoned dueling suffer status death, or did they successfully translate honor capital into professional/bureaucratic status? The proportion who exited successfully vs. those who persisted illegally calibrates the identity_locked vs. trapped distinction.',
    'If identity_locked, the constraint''s effective extraction on participants is amplified (d → 1.0) — they cannot leave the game. If structurally trapped, exit_options = constrained and extraction is lower. Changes per-seat classification for honor_culture_participants and gentry_aristocracy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_exit_mechanism, empirical, 'Mechanism of exit constraint for honor culture participants — identity fusion vs. structural absence of alternatives.').

omega_variable(
    military_dual_position_nature,
    'Does the military_officer_corps'' dual role (payer/beneficiary) represent a genuine structural duality or a temporal transition (payer early, beneficiary late)?',
    'Longitudinal analysis of military codes 1750-1900: track when dueling bans were enforced vs. when formal honor courts/articles of war substituted. If the same officer cohort experiences both simultaneously, duality is structural. If cohorts shift, it is temporal.',
    'Structural duality means the constraint is a tangled_rope for this seat (coordination + extraction simultaneously). Temporal transition means the constraint''s type changes over the interval for this seat — rope early, mountain late.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(military_dual_position_nature, empirical, 'Whether military officers'' dual position is synchronic structural fact or diachronic transition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_substrate__practice_decline_reading, 1750, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(honor_sat_substrate_practice_decline_tr_t1750, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1750, 0.05).
narrative_ontology:measurement(honor_sat_substrate_practice_decline_tr_t1780, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1780, 0.07).
narrative_ontology:measurement(honor_sat_substrate_practice_decline_tr_t1810, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1810, 0.1).
narrative_ontology:measurement(honor_sat_substrate_practice_decline_tr_t1840, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1840, 0.13).
narrative_ontology:measurement(honor_sat_substrate_practice_decline_tr_t1870, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1870, 0.16).
narrative_ontology:measurement(honor_sat_substrate_practice_decline_tr_t1900, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1900, 0.18).

% Extraction over time
narrative_ontology:measurement(honor_sat_substrate_practice_decline_be_t1750, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1750, 0.08).
narrative_ontology:measurement(honor_sat_substrate_practice_decline_be_t1780, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1780, 0.1).
narrative_ontology:measurement(honor_sat_substrate_practice_decline_be_t1810, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1810, 0.14).
narrative_ontology:measurement(honor_sat_substrate_practice_decline_be_t1840, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1840, 0.18).
narrative_ontology:measurement(honor_sat_substrate_practice_decline_be_t1870, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1870, 0.2).
narrative_ontology:measurement(honor_sat_substrate_practice_decline_be_t1900, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1900, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(honor_sat_substrate_practice_decline_su_t1750, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1750, 0.15).
narrative_ontology:measurement(honor_sat_substrate_practice_decline_su_t1780, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1780, 0.22).
narrative_ontology:measurement(honor_sat_substrate_practice_decline_su_t1810, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1810, 0.35).
narrative_ontology:measurement(honor_sat_substrate_practice_decline_su_t1840, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1840, 0.48).
narrative_ontology:measurement(honor_sat_substrate_practice_decline_su_t1870, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1870, 0.54).
narrative_ontology:measurement(honor_sat_substrate_practice_decline_su_t1900, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1900, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_substrate__practice_decline_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(honor_satisfaction_substrate__practice_decline_reading, 0.08).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__practice_decline_reading, honor_satisfaction_substrate__cultural_contraction_reading).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__practice_decline_reading, honor_satisfaction_substrate__composite_overdetermined_reading).

% DUAL FORMULATION NOTE:
% This constraint (practice_decline_reading) and its siblings (cultural_contraction_reading, composite_overdetermined_reading) form a constraint family decomposing the honor_satisfaction_substrate kernel. This reading's ε = 0.22 (rope: coordination failure under legal pressure). cultural_contraction_reading would have higher ε (substrate itself eroding). composite_overdetermined_reading would have highest ε (dual causal pathways = more suppression/extraction). All three share the honor code substrate as referent but differ on whether the substrate persists, contracts, or is overdetermined.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honor_satisfaction_substrate__practice_decline_reading, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
