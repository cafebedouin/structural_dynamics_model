% ============================================================================
% CONSTRAINT STORY: imposition_mechanism_kernel__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_mechanism_kernel__exogenous_override_reading, []).

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
 *   constraint_id: imposition_mechanism_kernel__exogenous_override_reading
 *   human_readable: State-Coerced Norm Imposition (Exogenous Override Reading)
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   This constraint instantiates the exogenous-override reading of the
 *   imposition-mechanism kernel: new norms are imposed by state force on a
 *   population that does not accept them culturally. Legitimacy is claimed
 *   through the state's monopoly on violence, not through having earned
 *   cultural acceptance. The constraint persists because resistance is
 *   suppressed, alternatives collapse (exit via emigration is rare and
 *   costly), and compliance is conditional on ongoing enforcement. This
 *   reading contrasts with the endogenous-climb reading (norms achieve
 *   legitimacy through bottom-up adoption, state mandate follows popular
 *   acceptance) and the hybrid-legitimation reading (symbolic authority
 *   transfer and institutional incentives create legitimacy jointly with
 *   enforcement). All three readings share a kernel commitment: the state
 *   claims authority to set cultural norms. They differ in the SOURCE of
 *   legitimacy — exogenous-override asserts that legitimacy is manufactured
 *   through coercion, not earned through cultural work.
 *
 * KEY AGENTS:
 *   - state_apparatus — monopoly holder on violence; sets mandate unilaterally
 *   - population_subject_to_mandate — bears compliance costs; resistance conditional on enforcement decay
 *   - pre_existing_norm_holders — identity_locked out of exit; practices criminalized
 *   - enforcement_apparatus — administers surveillance and punishment; benefits from expanded authority scope
 *   - resistance_movements — maintain clandestine practice; targeted for elimination
 *   - external_observers — witness enforcement intensity; their accounts are excluded from state narrative
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_mechanism_kernel__exogenous_override_reading, 0.81).
domain_priors:suppression_score(imposition_mechanism_kernel__exogenous_override_reading, 0.88).
domain_priors:theater_ratio(imposition_mechanism_kernel__exogenous_override_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, accessibility_collapse, 0.76).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_mechanism_kernel__exogenous_override_reading, snare).
narrative_ontology:human_readable(imposition_mechanism_kernel__exogenous_override_reading, "State-Coerced Norm Imposition (Exogenous Override Reading)").
narrative_ontology:topic_domain(imposition_mechanism_kernel__exogenous_override_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(imposition_mechanism_kernel__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_mechanism_kernel__exogenous_override_reading, 'f7c350d4-6dba-43a1-b57e-fda96b158fbd').
narrative_ontology:cs_kernel_codification('f7c350d4-6dba-43a1-b57e-fda96b158fbd', formalized).
narrative_ontology:cs_authority_grounding('f7c350d4-6dba-43a1-b57e-fda96b158fbd', extraction).
narrative_ontology:cs_interpretation_layer_present('f7c350d4-6dba-43a1-b57e-fda96b158fbd').
narrative_ontology:cs_reading_relation('f7c350d4-6dba-43a1-b57e-fda96b158fbd', imposition_mechanism_kernel__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('f7c350d4-6dba-43a1-b57e-fda96b158fbd', imposition_mechanism_kernel__hybrid_legitimation_reading, influences).
narrative_ontology:cs_axiom('f7c350d4-6dba-43a1-b57e-fda96b158fbd', foundational, state_authority_grounded_in_monopoly_on_violence).
narrative_ontology:cs_axiom_status(state_authority_grounded_in_monopoly_on_violence, holdable).
narrative_ontology:cs_axiom_grounding('f7c350d4-6dba-43a1-b57e-fda96b158fbd', state_authority_grounded_in_monopoly_on_violence, deontological).
narrative_ontology:cs_axiom('f7c350d4-6dba-43a1-b57e-fda96b158fbd', secondary, cultural_legitimacy_achievable_through_coercion_alone).
narrative_ontology:cs_axiom_status(cultural_legitimacy_achievable_through_coercion_alone, holdable).
narrative_ontology:cs_axiom_grounding('f7c350d4-6dba-43a1-b57e-fda96b158fbd', cultural_legitimacy_achievable_through_coercion_alone, empirically_contingent).
narrative_ontology:cs_reference_frame('f7c350d4-6dba-43a1-b57e-fda96b158fbd', state_monopoly_on_legitimate_violence).
narrative_ontology:cs_drift_state('f7c350d4-6dba-43a1-b57e-fda96b158fbd', contemporary_post_mandate_obsolescence, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f7c350d4-6dba-43a1-b57e-fda96b158fbd', '').
narrative_ontology:cs_kernel_id(imposition_mechanism_kernel__exogenous_override_reading, imposition_mechanism_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_victim(imposition_mechanism_kernel__exogenous_override_reading, population_subject_to_mandate).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__exogenous_override_reading, pre_existing_norm_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__exogenous_override_reading, enforcement_apparatus).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Unilaterally imposes a new norm (religious observance, legal procedure, language use, dress code, kinship practice, or other cultural arrangement) on the population it governs. Justifies the mandate as civilizing, moral improvement, or institutional necessity. Enforces compliance through punishment, surveillance, and control of institutional access (taxation, justice, employment). The state's legitimacy claim rests on monopoly of violence, not on having earned cultural acceptance of the norm itself.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Must adopt the imposed norm or face punishment: fines, imprisonment, social exclusion, property confiscation, or violence. Compliance is conditional on state surveillance and enforcement — it persists only while punishment is credible. The population continues pre-existing cultural practices in private or abandons them under duress. Their only exit is emigration (costly and often prohibited) or organized resistance (high-cost, low-success). Legitimacy of the norm remains contested in private discourse even as public compliance holds.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, population_subject_to_mandate, payer,
    powerless, biographical, trapped, national).

% Their cultural practices are declared illegitimate and prohibited. Exit is not available — the practices are identity-constituting (kinship, religious observance, language use); abandoning them means identity dissolution. They face direct punishment for maintaining what they understand as fundamental to their being. Their objections are systematically silenced — they have no institutional venue to contest the mandate.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, pre_existing_norm_holders, payer,
    powerless, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(imposition_mechanism_kernel__exogenous_override_reading, pre_existing_norm_holders, excluded).

% Administers the punishment and surveillance machinery that makes the mandate stick. Benefits from the expanded scope of their authority and resources (budgets for inspectors, prisons, courts). Their institutional survival depends on the norm remaining unenforced in practice — if voluntary compliance rises too high, enforcement budgets shrink and their power recedes.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, enforcement_apparatus, agenda_setter,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(imposition_mechanism_kernel__exogenous_override_reading, enforcement_apparatus, beneficiary).

% Organize among the population to maintain or revive forbidden practices, or to contest the mandate itself. They are systematically targeted by the enforcement apparatus. Their exclusion is the core enforcement object — maintaining clandestine practice spaces and transmission of prohibited knowledge keeps the resistance alive but under constant pressure.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, resistance_movements, excluded,
    moderate, biographical, constrained, national).

% Historians, sociologists, and other-states' diplomatic observers document the constraint's operation. They can witness the gap between mandated public practice and clandestine resistance, measure enforcement intensity, and corroborate accounts of punishment. Their testimony is often excluded from the mandate-holder's internal legitimation narrative.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, external_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imposition_mechanism_kernel__exogenous_override_reading, state_apparatus).
narrative_ontology:fixing_cost_class(imposition_mechanism_kernel__exogenous_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None. This constraint solves no collective-action problem — it does not reduce transaction costs or make coordination cheaper. It creates a single obligatory practice where plural legitimate practices existed. No party benefits from the coordination itself; the state benefits from the control.
% TRANSFER_FUNCTION: Transfers cultural authority from distributed populations (who maintained plural, decentralized norms) to the state apparatus (which monopolizes norm-setting power). Also transfers enforcement labor and surveillance costs to the enforcement apparatus, which gains institutional scope and budgets in exchange.
% ABSENT_VOICES: Resistance movements, pre-existing norm-holders, and populations whose practices are forbidden are systematically excluded. They would contest both the mandate and the state's right to impose it — their exclusion is structural, not accidental. External observers and rival norm-setting authorities (churches, communities, families) also have no institutional seat.
% DISAPPEARANCE_RATIONALE: If the mandate and enforcement apparatus vanished overnight, the population would revert to pre-existing cultural practices (or adopt new ones through bottom-up consensus) within weeks. The clandestine resistance structures that maintained forbidden practices would surface and normalize. The state's enforcement apparatus would lose its primary justification and funding. Social organization would reorganize around cultural pluralism rather than state-imposed uniformity.
% FOUNDING_PROBLEM: The state claims the imposition solves a coordination or civilizational problem: the population's existing norms are declared backward, immoral, dangerous, or inefficient — allegedly preventing commerce, enabling superstition, threatening security, or inhibiting progress. The state asserts that the population cannot solve this problem through internal deliberation and must be coerced into the solution.
% FOUNDING_PROBLEM_CORROBORATION: The state apparatus attests the founding problem is live and ongoing. Independent historical and ethnographic sources, resistance testimonies, and external observers attest that the population's pre-existing practices functioned adequately before imposition and that the alleged problem was manufactured or vastly overstated to justify state control. The claim that the population is incapable of self-correction is contradicted by successful plural norm systems in other societies and historical periods.
narrative_ontology:disappearance_verdict(imposition_mechanism_kernel__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_mechanism_kernel__exogenous_override_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_mechanism_kernel__exogenous_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(imposition_mechanism_kernel__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_mechanism_kernel__exogenous_override_reading, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_mechanism_kernel__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imposition_mechanism_kernel__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imposition_mechanism_kernel__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness reaches 0.81 because the constraint transfers cultural authority (a diffuse good) from the population to the state without providing coordination benefit — the state gains control, the population loses agency. The trajectory rises from 0.65 to 0.81 over 50 time units as enforcement machinery matures and resistance is suppressed. Suppression remains consistently high (0.82–0.88) because the constraint's persistence depends entirely on active punishment and surveillance — it has zero voluntary cultural adoption. Theater ratio rises from 0.48 to 0.62 as the state invests in legitimation narratives (civilizing mission, moral progress, necessity) to cover the extraction — early-stage enforcement is more openly violent; mature enforcement adds ceremonial justification. Accessibility collapse is high (0.76) because once the population understands the mandate is coerced and contradicts their own cultural practices, alternatives (clandestine resistance, emigration) are available in principle but foreclosed by practical cost and enforcement. Resistance remains substantial (0.72) because identity-locked populations and resistance movements sustain forbidden practices despite punishment, keeping the constraint from achieving the near-total compliance that genuine cultural norms achieve. The measurement series tracks the maturation of enforcement infrastructure — early enforcement is brutal and obvious; mature enforcement is bureaucratized and theaters legitimacy more systematically.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter (state) perceives the constraint as coordination and progress — it views the imposition as solving a real problem and the population's resistance as irrationality or backwardness. The payer (population) perceives the constraint as coerced identity destruction — they experience ongoing punishment and the impossibility of exit. The external_observer perceives the constraint as a power transfer with no legitimacy foundation — the founding problem is manufactured or trivial; the state's claimed authority is contested; the population sustains cultural resistance despite suppression. These gaps are not measurement error — they reflect genuine structural asymmetries. The engine computes per-seat type classifications that expose these gaps: the state's classification will differ from the population's because they sit at opposite ends of the directionality spectrum and the suppression is structural (not scaled), making the constraint extractive at every seat.
 *
 * DIRECTIONALITY LOGIC:
 *   The state_apparatus sits at d ≈ 0.0 (full beneficiary: gains cultural authority and enforcement budget). The population and pre_existing_norm_holders sit at d ≈ 1.0 (full targets: bear compliance costs, face punishment, lose cultural agency). The enforcement_apparatus sits at d ≈ 0.2 (beneficiary: gains institutional scope and resources; also partially target because their survival depends on the mandate remaining enforced — if cultural adoption succeeded, enforcement would become unnecessary and their budgets would shrink). Resistance_movements sit at d ≈ 0.95 (nearly full target: constantly under attack, clandestine operation, high personal risk). External_observers sit at d ≈ 0.5 (symmetric: they document the constraint without being directly harmed or benefited by its persistence). The divergence is structural: from the state's seat, the constraint is protective authority; from the population's seats, it is pure extraction. The engine derives this divergence from the beneficiary/victim declarations and power atoms — no explicit override needed.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint has NOT resolved its mandatrophy — the founding problem is dead (the state's alleged justification has been refuted by historical evidence, comparative analysis, or changed circumstances), but the constraint persists and even intensifies over time. This is the canonical mandatrophy signature: extractive capacity grows (base_extractiveness rises 0.65→0.81) while the coordination benefit vanishes and enforcement theater increases (theater_ratio rises 0.48→0.62). The constraint should dissolve when the mandate becomes obsolete, but institutional inertia, sunk enforcement infrastructure, and the state's vested interest in maintaining control prevent remedy. A true mandatrophy-resolved constraint would show one of two patterns: (1) the constraint weakens and disappears as the founding problem dissipates (base_extractiveness falls, theater_ratio stays low); or (2) the constraint is explicitly acknowledged as obsolete and formally repealed. This constraint shows neither — it is a zombie mandate maintained through pure coercion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coercion_sustainability_boundary,
    'How long can a norm imposed by pure coercion persist without any cultural drift toward acceptance?',
    'Historical comparative analysis of imposed norms across multiple state contexts (colonial administration, religious conversion mandates, legal-system imposition, language coercion) tracking how many generations enforcement can persist against clandestine resistance before either cultural adoption occurs or enforcement collapses.',
    'If coercion alone can sustain indefinitely without cultural shift, the exogenous-override reading fully explains the constraint''s persistence. If cultural adoption becomes necessary beyond a generational threshold, the constraint must be reclassified as transitional toward cultural norm (scaffold with a very long sunset) rather than pure extraction. This affects the terminal attractor: a snare that asymptotically becomes a rope (cultural adoption) versus a snare that remains pure snare indefinitely.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coercion_sustainability_boundary, empirical, 'Whether pure coercion can indefinitely sustain a norm without cultural drift.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression structural (enforcement machinery, surveillance, legal penalties) or internalized (the population has absorbed the prohibition and enforces it on themselves and their children)?',
    'Post-enforcement decay experiment: if enforcement apparatus collapses (state weakens, budget cuts eliminate surveillance), does the norm persist through internalized practice or does clandestine resistance surface immediately? Long-term generational analysis: do later generations maintain the norm voluntarily, or only under ongoing enforcement?',
    'If suppression is purely structural, the constraint''s collapse is rapid when enforcement weakens (a true snare). If suppression is internalized, later generations may maintain the norm even if external enforcement is removed — the constraint has shifted from pure snare toward a rope with internalized coordination. This affects the trajectory: structural-only suppression shows a cliff decline when enforcement fails; internalized suppression shows persistence even post-enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural or has become internalized through generational transmission.').

omega_variable(
    founding_problem_manufacture_versus_genuine,
    'Was the founding problem genuine (the state correctly identified a real coordination failure or harmful practice) or manufactured (the state invented the justification to expand control)?',
    'Ethnographic and historical evidence comparing the population''s practices before imposition (did they function adequately? did they generate complaints from within the population?) to external comparisons (did similar practices in other societies function adequately without state interference? do comparable societies with different norms show better outcomes?). Testimony from external observers and resistance movements about whether the mandated norm is actually superior.',
    'If the founding problem was genuine, the constraint may eventually shift toward cultural adoption (endogenous-climb) as the population recognizes the benefit. If the founding problem was manufactured, the constraint remains pure extraction with no foundation for legitimacy — resistance is rational, not irrationality. This affects the reading''s classification: if the problem is genuine but the state chose coercion over persuasion, the constraint is snare-classified but with some coordination potential latent in it; if the problem is manufactured, it is pure snare with zero coordination function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_manufacture_versus_genuine, empirical, 'Whether the founding problem''s justification is genuine or was manufactured post-hoc.').

omega_variable(
    kernel_reading_foreclosure_boundary,
    'Does the exogenous-override reading logically foreclose the endogenous-climb reading, or do both readings remain live positions that different parties can hold simultaneously about the same kernel?',
    'Clarify the logical relationship: if the kernel commit is ''the state has authority to set norms,'' does the exogenous-override axiom (authority derives from monopoly on violence) logically rule out the endogenous-climb axiom (authority derives from earned cultural acceptance) within a single framework? Or are these two different framings of the same phenomenon that could be held by different actors without contradiction?',
    'If exogenous-override forecloses endogenous-climb, only one reading is correct and the others are false; the constraint''s classification is determinate. If both remain live, they coexist as different seats'' perspectives on the same kernel — the constraint''s classification is per-seat and both readings describe different parties'' experience of the same arrangement. This affects the relation declaration: forecloses versus coexists_with.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_boundary, conceptual, 'Logical relationship between exogenous-override and sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_mechanism_kernel__exogenous_override_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t0, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(impo_tr_t5, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 5, 0.51).
narrative_ontology:measurement(impo_tr_t10, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 10, 0.54).
narrative_ontology:measurement(impo_tr_t20, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 20, 0.59).
narrative_ontology:measurement(impo_tr_t35, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 35, 0.61).
narrative_ontology:measurement(impo_tr_t50, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 50, 0.62).

% Extraction over time
narrative_ontology:measurement(impo_be_t0, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(impo_be_t5, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 5, 0.69).
narrative_ontology:measurement(impo_be_t10, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 10, 0.73).
narrative_ontology:measurement(impo_be_t20, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 20, 0.78).
narrative_ontology:measurement(impo_be_t35, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 35, 0.8).
narrative_ontology:measurement(impo_be_t50, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 50, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(impo_su_t0, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 0, 0.82).
narrative_ontology:measurement(impo_su_t5, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 5, 0.84).
narrative_ontology:measurement(impo_su_t10, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 10, 0.85).
narrative_ontology:measurement(impo_su_t20, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 20, 0.87).
narrative_ontology:measurement(impo_su_t35, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 35, 0.88).
narrative_ontology:measurement(impo_su_t50, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 50, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_mechanism_kernel__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(imposition_mechanism_kernel__exogenous_override_reading, 0.18).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__exogenous_override_reading, imposition_mechanism_kernel__endogenous_climb_reading).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__exogenous_override_reading, imposition_mechanism_kernel__hybrid_legitimation_reading).

% DUAL FORMULATION NOTE:
% The imposition_mechanism_kernel decomposes into three constraint stories, each instantiating a different reading of how state cultural authority is grounded. The exogenous_override_reading (this story) emphasizes coercive imposition without cultural acceptance. The endogenous_climb_reading emphasizes bottom-up cultural adoption that the state subsequently claims authority for. The hybrid_legitimation_reading emphasizes symbolic authority transfer (emperor-as-exemplar) combined with institutional incentives that shift cultural practice gradually. All three readings share the kernel commitment (state authority over norm-setting) and compete to explain the SOURCE of legitimacy. The exogenous-override reading influences the other two by establishing coercion as the baseline mechanism; if coercion is the default, the other readings must explain why and how cultural adoption or symbolic legitimacy ever occurs. Link all three via network.affects_constraints to enable comparative analysis of the same kernel across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
