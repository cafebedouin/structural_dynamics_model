% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_substrate__practice_decline_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Honor Satisfaction Constraint — Practice Decline Reading (Exogenous Enforcement)
 *   domain: historical_sociology/cultural_anthropology/legal_history
 *
 * SUMMARY:
 *   This constraint story instantiates the practice_decline_reading of the
 *   honor_satisfaction_substrate kernel. The claim: the honor code persisted
 *   as a normative substrate across the long 18th and 19th centuries —
 *   surviving in military codes, Southern 'culture of honor,' dueling rituals
 *   among elites, and professional ethics — while the practice of dueling
 *   declined due to exogenous enforcement: legal prohibition (anti-dueling
 *   statutes), institutional barriers (military/academic expulsion,
 *   professional blacklisting), and rising opportunity costs
 *   (commercial/professional careers incompatible with dueling risk). The
 *   constraint is a rope: the honor code's coordination function (reputation
 *   assurance, status signaling, conflict management among equals) failed not
 *   because the code eroded endogenously, but because the state's monopoly on
 *   violence and institutional gatekeepers actively suppressed its primary
 *   enforcement mechanism (the duel). The beneficiaries are legal
 *   institutions and state authority; the victims are honor-bound actors who
 *   lost their traditional satisfaction mechanism while the normative
 *   substrate remained intact.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_substrate__practice_decline_reading, 0.15).
domain_priors:suppression_score(honor_satisfaction_substrate__practice_decline_reading, 0.82).
domain_priors:theater_ratio(honor_satisfaction_substrate__practice_decline_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, accessibility_collapse, 0.28).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_substrate__practice_decline_reading, rope).
narrative_ontology:human_readable(honor_satisfaction_substrate__practice_decline_reading, "Honor Satisfaction Constraint — Practice Decline Reading (Exogenous Enforcement)").
narrative_ontology:topic_domain(honor_satisfaction_substrate__practice_decline_reading, "historical_sociology/cultural_anthropology/legal_history").

domain_priors:requires_active_enforcement(honor_satisfaction_substrate__practice_decline_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_substrate__practice_decline_reading, '6d7174de-f960-422c-a88c-af52044625e8').
narrative_ontology:cs_kernel_codification('6d7174de-f960-422c-a88c-af52044625e8', distributed).
narrative_ontology:cs_authority_grounding('6d7174de-f960-422c-a88c-af52044625e8', practice).
narrative_ontology:cs_interpretation_layer_present('6d7174de-f960-422c-a88c-af52044625e8').
narrative_ontology:cs_reading_relation('6d7174de-f960-422c-a88c-af52044625e8', honor_satisfaction_substrate__cultural_contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('6d7174de-f960-422c-a88c-af52044625e8', honor_satisfaction_substrate__composite_overdetermined_reading, influences).
narrative_ontology:cs_axiom('6d7174de-f960-422c-a88c-af52044625e8', foundational, honor_code_persists_as_normative_substrate).
narrative_ontology:cs_axiom_status(honor_code_persists_as_normative_substrate, holdable).
narrative_ontology:cs_axiom_grounding('6d7174de-f960-422c-a88c-af52044625e8', honor_code_persists_as_normative_substrate, conventional).
narrative_ontology:cs_axiom('6d7174de-f960-422c-a88c-af52044625e8', foundational, dueling_declined_by_exogenous_enforcement).
narrative_ontology:cs_axiom_status(dueling_declined_by_exogenous_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('6d7174de-f960-422c-a88c-af52044625e8', dueling_declined_by_exogenous_enforcement, empirically_contingent).
narrative_ontology:cs_reference_frame('6d7174de-f960-422c-a88c-af52044625e8', honor_satisfaction_among_equals).
narrative_ontology:cs_drift_state('6d7174de-f960-422c-a88c-af52044625e8', post_legal_prohibition_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6d7174de-f960-422c-a88c-af52044625e8', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_substrate__practice_decline_reading, honor_satisfaction_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__practice_decline_reading, legal_institutions).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__practice_decline_reading, state_monopoly_on_violence).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__practice_decline_reading, institutional_authorities).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__practice_decline_reading, honor_bound_gentry).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__practice_decline_reading, military_officer_corps).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__practice_decline_reading, southern_planter_class).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__practice_decline_reading, military_officer_corps).
narrative_ontology:constraint_vindicates(honor_satisfaction_substrate__practice_decline_reading, state_monopoly_on_violence_doctrine).
narrative_ontology:constraint_vindicates(honor_satisfaction_substrate__practice_decline_reading, rule_of_law_supremacy).
narrative_ontology:constraint_vindicates(honor_satisfaction_substrate__practice_decline_reading, due_process_over_private_violence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enact and enforce anti-dueling statutes, prosecute participants, and establish state courts as the sole legitimate venue for dispute resolution. Gain monopoly on legitimate violence and institutional authority by suppressing private satisfaction mechanisms.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, legal_institutions, agenda_setter,
    institutional, generational, arbitrage, national).

% The state's claim to exclusive legitimate force is strengthened each time dueling is suppressed. The honor code's persistence as rhetoric does not threaten this monopoly; only the practice of private violence does.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, state_monopoly_on_violence, beneficiary,
    institutional, civilizational, arbitrage, national).

% Military academies, universities, professional guilds, and churches that expel or sanction duelists. They gain institutional legitimacy and alignment with state law by enforcing anti-dueling rules internally, while their own honor codes (e.g., military honor) persist in attenuated form.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, institutional_authorities, beneficiary,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_substrate__practice_decline_reading, institutional_authorities, agenda_setter).

% Aristocratic and elite men for whom honor satisfaction through dueling was a status requirement and conflict management tool. They bear the cost of lost mechanism: disputes must go to courts (slow, public, uncontrolled) or be swallowed (status loss). Exit options constrained: dueling risks prosecution, career ruin, and social exile; not dueling risks honor loss among peers.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, honor_bound_gentry, payer,
    powerful, biographical, constrained, regional).

% Officers whose professional identity fuses with honor codes (West Point, Sandhurst, Prussian cadet schools). They are bound by institutional anti-dueling rules but also by internal honor codes that demand satisfaction. Identity-locked exit: leaving the corps means abandoning professional identity; staying means navigating contradictory imperatives. They benefit from the honor code's persistence as professional ethic but pay for the duel's suppression.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, military_officer_corps, payer,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_substrate__practice_decline_reading, military_officer_corps, beneficiary).

% Antebellum Southern elites for whom dueling and honor culture were constitutive of social order. They resist legal suppression through jurisdictional arbitrage (dueling across state lines, coded rituals) and cultural persistence. Post-bellum, the 'culture of honor' persists in attenuated form while dueling declines under Reconstruction-era legal pressure and economic transformation.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, southern_planter_class, payer,
    powerful, biographical, constrained, regional).

% Jurists, legislators, and reform societies (e.g., anti-dueling associations) who campaigned for legal prohibition. They observe the constraint's operation from the reformist seat, documenting the gap between law on books and practice on ground.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, legal_reformers, observer,
    organized, generational, analytical, national).

% The honor code as normative substrate — not an agent, but the vindicated proposition that persists across the decline. It is excluded from the conversation because it cannot speak; its persistence is the phenomenon the constraint story explains.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, honor_code_itself, excluded,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(honor_satisfaction_substrate__practice_decline_reading, honor_code_itself).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The honor code coordinates status signaling, reputation assurance, and conflict management among status equals without state intervention. It solves the problem: how do equals resolve disputes and maintain trust when no higher authority binds them? The duel was the enforcement mechanism; the code was the coordination grammar.
% TRANSFER_FUNCTION: The arrangement transfers dispute-resolution authority from private satisfaction (dueling) to public courts, and transfers the monopoly on legitimate violence from distributed honor enforcement to the state. Honor-bound actors lose autonomous satisfaction capacity; the state gains enforcement monopoly.
% ABSENT_VOICES: Women, enslaved people, and non-elite men were structurally excluded from the honor code's protection and its satisfaction mechanisms. They would object to a system that reserved honor for propertied men while denying them standing, but they were not in the conversation. Their absence is not the focus of this reading — the constraint operates among the honor-bound.
% DISAPPEARANCE_RATIONALE: If the anti-dueling legal framework and institutional barriers vanished overnight, honor-bound actors would likely resume dueling or coded satisfaction rituals within a generation — the normative substrate persists and the coordination problem (trust among equals without state mediation) remains live. The world rearranges because the constraint actively suppresses a live coordination mechanism.
% FOUNDING_PROBLEM: How do status equals in a pre-bureaucratic, pre-police society enforce agreements, defend reputation, and manage violence without state mediation? The honor code and its duel mechanism solved this by making satisfaction a private, reciprocal obligation among peers.
% FOUNDING_PROBLEM_CORROBORATION: Historical sociologists (e.g., Rothman on dueling as 'governance,' Kingston on honor as 'social grammar') attest the founding problem was real in its era. Legal historians (e.g., Brown on state monopoly formation) attest the problem is substantially solved by modern institutions — but military ethicists and Southern cultural historians attest it persists in attenuated forms. No single consensus exists.
narrative_ontology:disappearance_verdict(honor_satisfaction_substrate__practice_decline_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_substrate__practice_decline_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_substrate__practice_decline_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(honor_satisfaction_substrate__practice_decline_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_substrate__practice_decline_reading, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Low extractiveness (0.15): the honor code itself extracts little — it is a coordination norm. High suppression (0.82): the constraint's persistence depends on active legal/institutional enforcement against dueling. Low theater (0.12): the suppression is functional, not performative. Low accessibility_collapse (0.28): alternatives (courts, reputation markets, institutional discipline) remained available and were adopted. Moderate resistance (0.45): honor-bound actors resisted through jurisdictional arbitrage (dueling in tolerant zones), coded rituals (deloping, bloodless encounters), and cultural persistence of honor discourse. The rope classification reflects a genuine coordination problem (honor satisfaction among equals) that became unsolvable because the state suppressed the mechanism, not because the problem vanished.
 *
 * PERSPECTIVAL GAP:
 *   From the state/institutional seat, the constraint is successful coordination: private violence replaced by public law. From the honor-bound seat, the constraint is extraction: their normative world's enforcement mechanism was confiscated while the norms themselves were left intact — a coordination failure imposed from outside. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Legal institutions and state monopoly on violence are structural beneficiaries (d near 0.0): they gain legitimacy, monopoly enforcement, and institutional control by suppressing private violence. Honor-bound gentry, military officers, and Southern planter class are structural targets (d near 1.0): they bear the cost of lost satisfaction mechanism, constrained exit (cannot duel without career/legal ruin), and identity friction. The honor code itself is a vindicated proposition — it persists as normative substrate but collects no rents; its persistence is the puzzle this reading explains.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate (honor satisfaction among equals) did not atrophy — the mechanism (dueling) was exogenously suppressed. The honor code's normative demand for satisfaction persists; the constraint is not a degraded piton but an actively suppressed rope. The founding problem (private enforcement of honor among status equals) remains live in attenuated forms (military honor codes, Southern culture, professional ethics), but the state's prohibition prevents its traditional resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_framing,
    'Is this constraint one reading of the honor_satisfaction_substrate kernel (practice_decline_reading), distinct from cultural_contraction_reading and composite_overdetermined_reading?',
    'Comparative structural analysis: the practice_decline_reading attributes dueling''s decline to exogenous enforcement while the honor code persists as normative substrate; cultural_contraction_reading attributes it to endogenous transformation of the honor code itself; composite_overdetermined_reading claims both operated simultaneously with non-independent pathways. These are structurally distinct constraints with different beneficiary/victim structures and different ε referents.',
    'If the readings are not structurally distinct, they collapse into one constraint with ambiguous metrics; if distinct, each gets its own ε and classification. The practice_decline_reading classifies as rope (coordination under legal pressure), while the others may classify differently.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'This constraint instantiates the practice_decline_reading of the honor_satisfaction_substrate kernel; sibling readings are separate constraint stories').

omega_variable(
    honor_code_persistence_vs_transformation,
    'Did the honor code persist as a normative substrate (this reading) or undergo foundational transformation into a dignity culture (cultural_contraction_reading)?',
    'Historical evidence tracing continuity of honor discourse in military codes, Southern culture, and professional ethics vs. evidence of semantic shift in honor vocabulary and the emergence of dignity-based moral frameworks in the same populations.',
    'If the code persisted, the constraint is coordination failure under exogenous pressure (rope); if it transformed, the constraint''s coordination function itself eroded endogenously, changing the structural classification and the reading''s axioms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(honor_code_persistence_vs_transformation, empirical, 'Whether the honor code''s normative content persisted or transformed is the structural divergence between this reading and cultural_contraction_reading').

omega_variable(
    exogenous_enforcement_causal_weight,
    'Was dueling''s decline driven primarily by exogenous enforcement (legal prohibition, institutional barriers, opportunity cost) as this reading claims, or was endogenous delegitimation a necessary co-cause (composite_overdetermined_reading)?',
    'Counterfactual legal history: in jurisdictions where dueling laws were weak or unenforced but cultural transformation was advanced (or vice versa), did dueling persist? Quantitative analysis of prosecution rates, institutional sanctions (military/academic), and opportunity cost differentials across regions and periods.',
    'If exogenous enforcement alone suffices, this reading''s rope classification (coordination under pressure) stands alone; if composite causation is required, the constraint family''s network structure shifts — this reading influences but does not fully explain the decline.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exogenous_enforcement_causal_weight, empirical, 'Causal weight of exogenous enforcement vs. endogenous transformation in driving dueling''s decline').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_substrate__practice_decline_reading, 1750, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(honor_satisfaction_substrate__practice_decline_reading_tr_t1750, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1750, 0.05).
narrative_ontology:measurement(honor_satisfaction_substrate__practice_decline_reading_tr_t1780, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1780, 0.06).
narrative_ontology:measurement(honor_satisfaction_substrate__practice_decline_reading_tr_t1810, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1810, 0.08).
narrative_ontology:measurement(honor_satisfaction_substrate__practice_decline_reading_tr_t1840, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1840, 0.1).
narrative_ontology:measurement(honor_satisfaction_substrate__practice_decline_reading_tr_t1870, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1870, 0.11).
narrative_ontology:measurement(honor_satisfaction_substrate__practice_decline_reading_tr_t1900, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1900, 0.12).

% Extraction over time
narrative_ontology:measurement(honor_satisfaction_substrate__practice_decline_reading_be_t1750, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1750, 0.08).
narrative_ontology:measurement(honor_satisfaction_substrate__practice_decline_reading_be_t1780, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1780, 0.1).
narrative_ontology:measurement(honor_satisfaction_substrate__practice_decline_reading_be_t1810, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1810, 0.12).
narrative_ontology:measurement(honor_satisfaction_substrate__practice_decline_reading_be_t1840, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1840, 0.14).
narrative_ontology:measurement(honor_satisfaction_substrate__practice_decline_reading_be_t1870, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1870, 0.15).
narrative_ontology:measurement(honor_satisfaction_substrate__practice_decline_reading_be_t1900, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1900, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(honor_satisfaction_substrate__practice_decline_reading_su_t1750, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1750, 0.45).
narrative_ontology:measurement(honor_satisfaction_substrate__practice_decline_reading_su_t1780, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1780, 0.55).
narrative_ontology:measurement(honor_satisfaction_substrate__practice_decline_reading_su_t1810, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1810, 0.65).
narrative_ontology:measurement(honor_satisfaction_substrate__practice_decline_reading_su_t1840, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1840, 0.75).
narrative_ontology:measurement(honor_satisfaction_substrate__practice_decline_reading_su_t1870, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1870, 0.8).
narrative_ontology:measurement(honor_satisfaction_substrate__practice_decline_reading_su_t1900, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1900, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_substrate__practice_decline_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(honor_satisfaction_substrate__practice_decline_reading, 0.1).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__practice_decline_reading, honor_satisfaction_substrate__cultural_contraction_reading).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__practice_decline_reading, honor_satisfaction_substrate__composite_overdetermined_reading).

% DUAL FORMULATION NOTE:
% This reading and its siblings form a constraint family decomposing the 'honor satisfaction substrate' kernel. The practice_decline_reading attributes dueling's decline to exogenous enforcement with persistent honor norms (rope). The cultural_contraction_reading attributes it to endogenous normative transformation (different ε, different type). The composite_overdetermined_reading claims non-independent dual causation (different network structure). All three are distinct constraints with distinct ε values, linked here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honor_satisfaction_substrate__practice_decline_reading, institutional, 0.08).
constraint_indexing:directionality_override(honor_satisfaction_substrate__practice_decline_reading, powerful, 0.78).
constraint_indexing:directionality_override(honor_satisfaction_substrate__practice_decline_reading, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
