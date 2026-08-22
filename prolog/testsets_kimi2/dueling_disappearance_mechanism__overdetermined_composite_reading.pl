% ============================================================================
% CONSTRAINT STORY: dueling_disappearance_mechanism__overdetermined_composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dueling_disappearance_mechanism__overdetermined_composite_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: dueling_disappearance_mechanism__overdetermined_composite_reading
 *   human_readable: Dueling Disappearance via Overdetermined Composite Mechanisms
 *   domain: historical/cultural/legal
 *
 * SUMMARY:
 *   The disappearance of dueling in the nineteenth-century United States is
 *   explained by the overdetermined composite reading as the simultaneous
 *   action of multiple independent sufficient conditions: legal prohibition,
 *   institutional modernization, cultural shift toward dignity, and Civil War
 *   trauma. Each mechanism alone could have suppressed the practice; together
 *   they created a non-separable causal field that eliminated aristocratic
 *   interpersonal violence. This reading treats the kernel not as a
 *   single-cause transition but as a robust, multiply-redundant social
 *   transformation in which state-builders, bourgeois modernizers,
 *   dignity-culture advocates, and postwar reconstruction elites each
 *   benefited from a different strand of the suppression, while the
 *   traditional honor elite paid diffuse costs through the erosion of status
 *   identity and autonomous dispute resolution. As a kernel reading, it
 *   instantiates one of three live historiographical positions; the sibling
 *   contraction and institutional-displacement readings are modeled as
 *   separate constraints in the same family.
 *
 * KEY AGENTS:
 *   - state_legal_apparatus: Primary agenda-setter (institutional/analytical) â enforces legal prohibition and claims monopoly on legitimate violence.
 *   - bourgeois_institutional_modernizers: Primary beneficiary (organized/mobile) â builds courts, banks, and libel law as substitutes.
 *   - postwar_state_builders: Primary beneficiary (institutional/mobile) â leverages Civil War outcome to dismantle regional honor autonomy.
 *   - dignity_culture_advocates: Primary beneficiary (organized/mobile) â substitutes emotional restraint and Christian forbearance for aggressive honor.
 *   - traditional_honor_elite: Primary payer (moderate/identity_locked) â bears loss of practice, status system, and autonomous dispute resolution.
 *   - historical_sociologists: Analytical observer (analytical/analytical) â evaluates competing causal frameworks without stake in the outcome.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.58).
domain_priors:suppression_score(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.71).
domain_priors:theater_ratio(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dueling_disappearance_mechanism__overdetermined_composite_reading, tangled_rope).
narrative_ontology:human_readable(dueling_disappearance_mechanism__overdetermined_composite_reading, "Dueling Disappearance via Overdetermined Composite Mechanisms").
narrative_ontology:topic_domain(dueling_disappearance_mechanism__overdetermined_composite_reading, "historical/cultural/legal").

domain_priors:requires_active_enforcement(dueling_disappearance_mechanism__overdetermined_composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dueling_disappearance_mechanism__overdetermined_composite_reading, 'd248960e-eff5-4bb7-8022-f9157db8192b').
narrative_ontology:cs_kernel_codification('d248960e-eff5-4bb7-8022-f9157db8192b', distributed).
narrative_ontology:cs_authority_grounding('d248960e-eff5-4bb7-8022-f9157db8192b', distributed).
narrative_ontology:cs_reading_relation('d248960e-eff5-4bb7-8022-f9157db8192b', dueling_disappearance_mechanism__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('d248960e-eff5-4bb7-8022-f9157db8192b', dueling_disappearance_mechanism__institutional_displacement_reading, coexists_with).
narrative_ontology:cs_axiom('d248960e-eff5-4bb7-8022-f9157db8192b', foundational, historical_overdetermination_as_default).
narrative_ontology:cs_axiom_status(historical_overdetermination_as_default, holdable).
narrative_ontology:cs_axiom_grounding('d248960e-eff5-4bb7-8022-f9157db8192b', historical_overdetermination_as_default, empirically_contingent).
narrative_ontology:cs_axiom('d248960e-eff5-4bb7-8022-f9157db8192b', secondary, causal_non_separability_in_social_death).
narrative_ontology:cs_axiom_status(causal_non_separability_in_social_death, holdable).
narrative_ontology:cs_axiom_grounding('d248960e-eff5-4bb7-8022-f9157db8192b', causal_non_separability_in_social_death, empirically_contingent).
narrative_ontology:cs_reference_frame('d248960e-eff5-4bb7-8022-f9157db8192b', overdetermined_causal_field).
narrative_ontology:cs_drift_state('d248960e-eff5-4bb7-8022-f9157db8192b', contemporary_historiography, gap(stable, minor, true)).
narrative_ontology:cs_created_at('d248960e-eff5-4bb7-8022-f9157db8192b', '').
narrative_ontology:cs_kernel_id(dueling_disappearance_mechanism__overdetermined_composite_reading, dueling_disappearance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, state_legal_apparatus).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, bourgeois_institutional_modernizers).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, postwar_state_builders).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, dignity_culture_advocates).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__overdetermined_composite_reading, traditional_honor_elite).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacted and enforced anti-dueling statutes and claimed monopoly on legitimate violence. Administered the legal prohibition strand of the composite suppression, consolidating state authority over interpersonal dispute resolution.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, state_legal_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Built courts, commercial banking, and libel law as formalized substitutes for honor-based arbitration. Collected institutional power and transaction-volume growth as informal aristocratic violence was displaced.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, bourgeois_institutional_modernizers, beneficiary,
    organized, generational, mobile, national).

% Leveraged Civil War trauma and federal authority to delegitimize Southern honor culture and pacify regional aristocratic violence. Collected national unification and centralized legitimacy.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, postwar_state_builders, beneficiary,
    institutional, generational, mobile, national).

% Promoted emotional restraint, Christian forbearance, and bourgeois respectability as replacements for the honor ethic. Collected cultural hegemony as aggressive masculine status-defense became socially toxic.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, dignity_culture_advocates, beneficiary,
    organized, generational, mobile, national).

% Antebellum gentry and Southern aristocrats for whom dueling was a core practice of status maintenance, masculine identity, and autonomous dispute resolution. Paid the costs of criminalization, institutional displacement, cultural ridicule, and military defeat; exit required abandoning a class identity fused to honor culture.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, traditional_honor_elite, payer,
    moderate, biographical, identity_locked, regional).

% Evaluate competing causal frameworks for dueling's decline without collecting from or paying into the constraint. Their stake is explanatory adequacy, not historical advocacy.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, historical_sociologists, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dueling_disappearance_mechanism__overdetermined_composite_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates multiple independent social mechanismsâlegal prohibition, institutional substitution, cultural shift, and traumatic military collapseâtoward the shared elimination of aristocratic interpersonal violence without relying on any single point of failure.
% TRANSFER_FUNCTION: Moves the power of violent dispute resolution from autonomous aristocratic actors to state and bourgeois institutions; moves cultural prestige from aggressive honor to restrained dignity; moves political legitimacy from regional aristocratic autonomy to centralized national authority.
% ABSENT_VOICES: Monocausal historians who champion a single explanation (pure cultural shift or pure institutional substitution) are marginalized in this composite frame; the traditional honor elite themselves were largely excluded from the modernizing discourse that authored the history of their own practice's demise.
% DISAPPEARANCE_RATIONALE: If the multiple overlapping sufficient conditions suppressing dueling were removed, the practice would likely re-emerge in some form among honor-bound subcultures, and the modern monopoly of state and institutional dispute resolution would face direct challenges.
% FOUNDING_PROBLEM: The persistence of informal aristocratic violence threatened state monopoly on legitimate force, destabilized emerging bourgeois institutions, and encoded a regional honor culture incompatible with national unification and centralized legal authority.
% FOUNDING_PROBLEM_CORROBORATION: Postwar state-builders and legal historians outside the traditional honor elite attest that the monopoly on violence was the central aim; modern criminologists and sociologists corroborate that the institutional substitution function is now complete and the founding problem no longer exists.
narrative_ontology:disappearance_verdict(dueling_disappearance_mechanism__overdetermined_composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(dueling_disappearance_mechanism__overdetermined_composite_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dueling_disappearance_mechanism__overdetermined_composite_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dueling_disappearance_mechanism__overdetermined_composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dueling_disappearance_mechanism__overdetermined_composite_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dueling_disappearance_mechanism__overdetermined_composite_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dueling_disappearance_mechanism__overdetermined_composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) is moderate-high because the simultaneous action of four suppression mechanisms imposes substantial costs on the honor elite, though the practice was already vulnerable. Suppression (0.71) is high because multiple independent sufficient conditions create a robust enforcement field with no single point of failure. Theater ratio (0.25) is low: the mechanisms were overwhelmingly functional rather than performativeâlaws were enforced, institutions actually substituted, the Civil War genuinely traumatized. Accessibility collapse (0.65) is substantial because the convergence of legal, institutional, cultural, and traumatic barriers made revival nearly unthinkable. Resistance (0.45) is moderate: the honor elite resisted culturally and politically but were overcome by the overdetermined pressure. The temporal series show extraction and suppression peaking during the Civil War and immediate postwar period (T=45) when all mechanisms were simultaneously active, then moderating as the practice died and enforcement became habitual rather than contested.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats (state apparatus, modernizers, reconstruction elites) experience the constraint as necessary modernization and state-building. The payer seat (traditional honor elite) experiences it as cultural annihilation and status degradation. The analytical observer sees the structural asymmetry: the beneficiaries' gains are specific and legible (monopoly violence, institutional expansion, cultural hegemony), while the payer's losses are diffuse and identity-fused, making resistance organizationally difficult.
 *
 * DIRECTIONALITY LOGIC:
 *   State_legal_apparatus sits near the beneficiary end because it subsidizes state power; bourgeois_institutional_modernizers, postwar_state_builders, and dignity_culture_advocates all sit on the beneficiary side because each collects a distinct form of social power from the suppression. Traditional_honor_elite sits near the target end because it bears the concentrated cultural and status extraction; its identity_locked exit amplifies effective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâautonomous aristocratic violence threatening state monopoly and institutional stabilityâis dead. Dueling no longer exists. However, the composite constraint persists as an embedded historical arrangement: legal statutes remain on books, institutional substitutes are entrenched, dignity culture is hegemonic, and Civil War memory continues to delegitimize honor violence. The mismatch between founding_problem_status=dead and disappearance_verdict=world_rearranges signals that the constraint has outlived its function but its structural residues persist. Nonetheless, the active coordination among multiple beneficiary groups and the non-theatrical nature of the mechanisms keep it classified as tangled_rope: the coordination (shared suppression of violence) and extraction (cultural dispossession of the honor elite) remain structurally coupled.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_mechanism_nonseparability,
    'Can the independent contributions of legal prohibition, institutional substitution, cultural shift, and Civil War trauma to dueling''s decline be separated empirically, or do they form a non-decomposable causal field?',
    'Counterfactual historical analysis or natural experiment from regions where some mechanisms were absent while others operated.',
    'If separable, the overdetermined reading dissolves into component constraints with distinct epsilon values; if non-separable, the composite reading is the only valid frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_mechanism_nonseparability, conceptual, 'Whether the four causal pathways can be isolated or constitute an inseparable causal field.').

omega_variable(
    victim_mechanism_attribution,
    'Which mechanism''s dominance determines the primary victim seat: legal prohibition, institutional substitution, cultural shift, or Civil War trauma?',
    'Microhistorical reconstruction of individual dueling cases and participant testimonies to identify which mechanism victims experienced as primary.',
    'If one mechanism dominated victimization, the constraint leans toward snare; if all acted equally, it remains tangled_rope with diffuse victimization.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_mechanism_attribution, empirical, 'Uncertainty about which suppression mechanism primarily extracted from the honor elite.').

omega_variable(
    kernel_reading_exclusivity,
    'Is the overdetermined reading one live historiographical position among many, or does the empirical evidence of multiple sufficient causes logically override monocausal explanations?',
    'Archival discovery of counterfactual regions where only some mechanisms operated, or historiographical consensus tracking.',
    'If the reading overrides siblings, network relations shift from coexists_with toward influences or forecloses; if not, pluralism persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_exclusivity, conceptual, 'Whether the overdetermined reading forecloses or merely coexists with monocausal sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dueling_disappearance_mechanism__overdetermined_composite_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(duel_tr_t0, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(duel_tr_t15, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 15, 0.12).
narrative_ontology:measurement(duel_tr_t30, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 30, 0.15).
narrative_ontology:measurement(duel_tr_t45, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 45, 0.2).
narrative_ontology:measurement(duel_tr_t60, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 60, 0.22).
narrative_ontology:measurement(duel_tr_t70, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 70, 0.25).

% Extraction over time
narrative_ontology:measurement(duel_be_t0, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(duel_be_t15, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 15, 0.38).
narrative_ontology:measurement(duel_be_t30, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 30, 0.52).
narrative_ontology:measurement(duel_be_t45, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 45, 0.68).
narrative_ontology:measurement(duel_be_t60, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 60, 0.61).
narrative_ontology:measurement(duel_be_t70, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 70, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(duel_su_t0, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(duel_su_t15, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 15, 0.45).
narrative_ontology:measurement(duel_su_t30, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 30, 0.6).
narrative_ontology:measurement(duel_su_t45, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 45, 0.85).
narrative_ontology:measurement(duel_su_t60, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 60, 0.75).
narrative_ontology:measurement(duel_su_t70, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 70, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dueling_disappearance_mechanism__overdetermined_composite_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__overdetermined_composite_reading, contraction_reading).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__overdetermined_composite_reading, institutional_displacement_reading).

% DUAL FORMULATION NOTE:
% The dueling disappearance mechanism decomposes into three structurally distinct readings: contraction (monocultural), institutional_displacement (monoinstitutional), and overdetermined_composite (plural causal). Each carries a distinct epsilon and stakeholder geometry; they form a constraint family linked by shared referent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
