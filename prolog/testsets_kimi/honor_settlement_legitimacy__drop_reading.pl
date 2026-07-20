% ============================================================================
% CONSTRAINT STORY: honor_settlement_legitimacy__drop_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Fringe Dueling Persistence in Residual Honor Cultures
 *   domain: historical_sociology/legal_history
 *
 * SUMMARY:
 *   This constraint is the drop_reading of the contested kernel
 *   honor_settlement_legitimacy. The kernel concerns the historical mechanism
 *   by which affronts among peers are settled. The drop_reading claims that
 *   dueling persisted as a live, if fringe, practice in residual
 *   honor-culture niches, distinct from the contraction_reading (cognitive
 *   unthinkability) and the composite_reading (overdetermined decline). The
 *   constraint instantiates a tangled rope: genuine coordination within the
 *   subculture (dispute settlement, status ordering) coupled with asymmetric
 *   extraction (coerced participation, lethal risk) and active enforcement
 *   through identity-locked social coercion.
 *
 * KEY AGENTS:
 *   - honor_code_arbiters: Primary agenda_setter (institutional/identity_locked) â administer the dueling code, derive authority from its persistence, and absorb deference as indispensable interpreters.
 *   - male_honor_elite: Primary beneficiary (powerful/constrained) â use the code to settle disputes and signal dominance, collecting status while bearing manageable risk.
 *   - coerced_challengers: Primary target (moderate/trapped) â bear physical and social annihilation costs, cannot decline without exiting the identity frame entirely.
 *   - bereaved_families: Secondary target (powerless/trapped) â absorb the lethal externalities of the practice with no standing to contest it inside the subculture.
 *   - excluded_subalterns: Excluded voice (powerless/trapped) â structurally barred from the honor conversation, would contest the violence and monopoly if present.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_settlement_legitimacy__drop_reading, 0.76).
domain_priors:suppression_score(honor_settlement_legitimacy__drop_reading, 0.78).
domain_priors:theater_ratio(honor_settlement_legitimacy__drop_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, extractiveness, 0.76).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_settlement_legitimacy__drop_reading, tangled_rope).
narrative_ontology:human_readable(honor_settlement_legitimacy__drop_reading, "Fringe Dueling Persistence in Residual Honor Cultures").
narrative_ontology:topic_domain(honor_settlement_legitimacy__drop_reading, "historical_sociology/legal_history").

domain_priors:requires_active_enforcement(honor_settlement_legitimacy__drop_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_settlement_legitimacy__drop_reading, '2bb4cf53-5157-4fe0-a274-7fa9a227c626').
narrative_ontology:cs_kernel_codification('2bb4cf53-5157-4fe0-a274-7fa9a227c626', distributed).
narrative_ontology:cs_authority_grounding('2bb4cf53-5157-4fe0-a274-7fa9a227c626', lineage).
narrative_ontology:cs_interpretation_layer_present('2bb4cf53-5157-4fe0-a274-7fa9a227c626').
narrative_ontology:cs_reading_relation('2bb4cf53-5157-4fe0-a274-7fa9a227c626', honor_settlement_legitimacy__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('2bb4cf53-5157-4fe0-a274-7fa9a227c626', honor_settlement_legitimacy__composite_reading, influences).
narrative_ontology:cs_axiom('2bb4cf53-5157-4fe0-a274-7fa9a227c626', foundational, residual_honor_jurisdiction_legitimate).
narrative_ontology:cs_axiom_status(residual_honor_jurisdiction_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('2bb4cf53-5157-4fe0-a274-7fa9a227c626', residual_honor_jurisdiction_legitimate, conventional).
narrative_ontology:cs_reference_frame('2bb4cf53-5157-4fe0-a274-7fa9a227c626', classical_honor_society_integrated).
narrative_ontology:cs_drift_state('2bb4cf53-5157-4fe0-a274-7fa9a227c626', modern_state_monopoly_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2bb4cf53-5157-4fe0-a274-7fa9a227c626', '').
narrative_ontology:cs_kernel_id(honor_settlement_legitimacy__drop_reading, honor_settlement_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__drop_reading, honor_code_arbiters).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__drop_reading, male_honor_elite).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__drop_reading, coerced_challengers).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__drop_reading, bereaved_families).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the dueling code within residual honor cultures: set the terms of satisfaction, appoint seconds, witness encounters, and adjudicate whether an affront warrants a challenge. Derive authority and social deference from being the indispensable interpreters of honor norms. Their social identity is fused with the subculture; renouncing the code would mean self-annihilation as an arbiter.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, honor_code_arbiters, agenda_setter,
    institutional, generational, identity_locked, regional).

% Occupy the top of the honor hierarchy and use the dueling option to protect reputation, settle interpersonal disputes, and signal willingness to bear lethal risk. Benefit from a regulated mechanism that converts potentially chaotic affronts into bounded status contests they are socially equipped to win. Exit is possible by abandoning the honor frame, but this entails status collapse within the only community that grants them standing.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, male_honor_elite, beneficiary,
    powerful, biographical, constrained, regional).

% Are challenged to duel over real or perceived affronts and cannot decline without social annihilation within the honor community. Bear the full physical risk of injury or death, plus the psychological burden of forced participation. Alternatives such as legal recourse or public apology are structurally closed off by the honor code itself, which treats refusal as cowardice.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, coerced_challengers, payer,
    moderate, biographical, trapped, regional).

% Absorb the lethal externalities of the dueling code when family members are killed or maimed in honor encounters. Lack standing to challenge the legitimacy of the practice within the subculture and receive little or no recourse from state legal institutions that either tolerate or are circumvented by the honor community.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, bereaved_families, payer,
    powerless, immediate, trapped, local).

% Women, lower-class individuals, and non-adherents are structurally barred from demanding satisfaction or adjudicating honor disputes. Their grievances are not recognized by the dueling framework, and they would contest both the violence and the monopoly over legitimate grievance if admitted to the conversation.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, excluded_subalterns, excluded,
    powerless, biographical, trapped, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Within residual honor cultures, dueling coordinates dispute settlement among elite males by ritualizing potentially escalatory affronts into bounded encounters, preventing feuds and establishing publicly recognized status outcomes without recourse to state institutions.
% TRANSFER_FUNCTION: Moves physical risk, injury, and death from challenged parties and their families to the challenger and the honor elite, while transferring social authority and deference to the code arbiters who stage and witness the encounters.
% ABSENT_VOICES: Women, lower-class individuals, and non-adherents are structurally excluded from the honor conversation; they would contest the violence and the monopoly over grievance-resolution but are not admitted as legitimate interlocutors. The modern legal state is present in the broader territory but externalized from the subculture's normative framework.
% DISAPPEARANCE_RATIONALE: If the legitimacy of dueling as an honor settlement mechanism vanished overnight within these residual niches, the internal status hierarchy would collapse; disputes would lose their regulated outlet, likely reorganizing toward either unregulated violence or state legal channels, fundamentally altering social relations among adherents.
% FOUNDING_PROBLEM: In pre-modern societies lacking centralized legal authority capable of adjudicating interpersonal affronts among peers, unregulated grievances escalated into feuds and chronic social disorder; dueling provided a bounded, peer-legitimated mechanism to settle such disputes without communal warfare.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and state institutions outside the subculture attest that the monopoly on violence has superseded private honor settlement; residual adherents attest the problem remains live because state courts fail to recognize honor-based grievances. Independent ethnographers corroborate the persistence of the practice but note its closure within shrinking social niches.
narrative_ontology:disappearance_verdict(honor_settlement_legitimacy__drop_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_settlement_legitimacy__drop_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_settlement_legitimacy__drop_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_settlement_legitimacy__drop_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_settlement_legitimacy__drop_reading, 0.76, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is high (0.76) because the constraint imposes lethal physical risk and social death on coerced participants; suppression is higher (0.78) because the constraint's persistence depends on actively closing off alternatives (legal recourse, apology, refusal) through identity-fused shame. Theater_ratio rises over the interval (0.72 at endpoint) as the practice retreats into narrower niches and becomes more ritualized and performative, though material risk never collapses to zero. Resistance is moderate (0.58) because state institutions and some internal dissent actively oppose the practice, but the subculture's closure buffers it. The measurement series share one time grid to prevent misaligned substitution artifacts.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats experience the constraint as necessary social architecture preserving order and identity; the payer seats experience it as coerced endangerment. The engine computes this divergence from the structural data â the same encounter is read as ritual by the elite and as compelled violence by the challenged party.
 *
 * DIRECTIONALITY LOGIC:
 *   The honor_code_arbiters and male_honor_elite are structural beneficiaries: they sit at low directionality because the constraint subsidizes their authority and status. The coerced_challengers and bereaved_families are structural targets: high directionality because the constraint extracts physical and social survival from them. The divergence is steep because the same practice that coordinates status for the elite terminates existence for the challenged party.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (absence of centralized legal authority) is contested: the state claims it is solved, while adherents claim state justice fails their grievances. This prevents simple mandatrophy resolution. The constraint is not a piton because a concentrated beneficiary class (the honor elite and arbiters) actively profits from its maintenance; it is not a snare because the coordination function (dispute settlement within the subculture) is genuine and not merely cover. Tangled rope captures the hybrid accurately.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fringe_persistence_theatricality,
    'Is the dueling observed in residual niches a live lethal practice or a predominantly performative ritual with negligible death rates?',
    'Archival and newspaper analysis of recorded duels in residual honor cultures measuring lethality rates over time; comparison with earlier mainstream periods.',
    'If lethality collapsed while the practice persisted, theater_ratio rises and the constraint may trend toward piton; if lethality remained material, the extraction score holds and tangled_rope is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fringe_persistence_theatricality, empirical, 'Whether fringe dueling is live violence or theatrical remnant.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the coercion maintaining dueling structural (external legal tolerance, geographic isolation of subculture) or internalized (honor-shame identity fusion)?',
    'Longitudinal study of individuals exiting honor subcultures: if coercive pressure persists after legal and geographic removal, suppression is partially internalized.',
    'Internalized suppression raises effective extraction for identity-locked agents beyond structural measures; structural-only suppression would predict easier dissolution with external enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism in honor subculture.').

omega_variable(
    drop_vs_contraction_reading_separation,
    'Does the drop_reading''s claim of residual dueling persistence describe a structurally distinct constraint from the contraction_reading''s claim of cognitive unthinkability, or are they threshold variants of the same empirical distribution?',
    'Comparative ethnography mapping documented duel frequency against normative endorsement across field sites; if no population simultaneously exhibits high unthinkability and live persistence, the readings may be threshold variants rather than distinct structures.',
    'If threshold variants, the kernel should collapse to a single constraint with a persistence parameter; if structurally distinct, separate Îµ values and types are warranted, validating the three-way decomposition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(drop_vs_contraction_reading_separation, conceptual, 'Whether drop and contraction readings are distinct constraints or threshold variants.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_settlement_legitimacy__drop_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t0, honor_settlement_legitimacy__drop_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(hono_tr_t5, honor_settlement_legitimacy__drop_reading, theater_ratio, 5, 0.38).
narrative_ontology:measurement(hono_tr_t10, honor_settlement_legitimacy__drop_reading, theater_ratio, 10, 0.46).
narrative_ontology:measurement(hono_tr_t15, honor_settlement_legitimacy__drop_reading, theater_ratio, 15, 0.54).
narrative_ontology:measurement(hono_tr_t20, honor_settlement_legitimacy__drop_reading, theater_ratio, 20, 0.62).
narrative_ontology:measurement(hono_tr_t25, honor_settlement_legitimacy__drop_reading, theater_ratio, 25, 0.67).
narrative_ontology:measurement(hono_tr_t30, honor_settlement_legitimacy__drop_reading, theater_ratio, 30, 0.72).

% Extraction over time
narrative_ontology:measurement(hono_be_t0, honor_settlement_legitimacy__drop_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(hono_be_t5, honor_settlement_legitimacy__drop_reading, base_extractiveness, 5, 0.74).
narrative_ontology:measurement(hono_be_t10, honor_settlement_legitimacy__drop_reading, base_extractiveness, 10, 0.74).
narrative_ontology:measurement(hono_be_t15, honor_settlement_legitimacy__drop_reading, base_extractiveness, 15, 0.75).
narrative_ontology:measurement(hono_be_t20, honor_settlement_legitimacy__drop_reading, base_extractiveness, 20, 0.76).
narrative_ontology:measurement(hono_be_t25, honor_settlement_legitimacy__drop_reading, base_extractiveness, 25, 0.77).
narrative_ontology:measurement(hono_be_t30, honor_settlement_legitimacy__drop_reading, base_extractiveness, 30, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t0, honor_settlement_legitimacy__drop_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(hono_su_t5, honor_settlement_legitimacy__drop_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(hono_su_t10, honor_settlement_legitimacy__drop_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(hono_su_t15, honor_settlement_legitimacy__drop_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(hono_su_t20, honor_settlement_legitimacy__drop_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(hono_su_t25, honor_settlement_legitimacy__drop_reading, suppression_requirement, 25, 0.79).
narrative_ontology:measurement(hono_su_t30, honor_settlement_legitimacy__drop_reading, suppression_requirement, 30, 0.83).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_settlement_legitimacy__drop_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
