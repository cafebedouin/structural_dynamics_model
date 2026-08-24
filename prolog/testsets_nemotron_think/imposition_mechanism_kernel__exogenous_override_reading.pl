% ============================================================================
% CONSTRAINT STORY: imposition_mechanism_kernel__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: imposition_mechanism_kernel__exogenous_override_reading
 *   human_readable: State-Coerced Norm Imposition via Monopoly on Violence
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   This constraint models the exogenous imposition of cultural norms by
 *   state power — the 'override' reading of how norms achieve dominance. The
 *   state uses its monopoly on violence to enforce uniformity, presenting the
 *   result as legitimate order. The claimed type is snare: the coordination
 *   story (legibility, unity) is cover for extraction (compliance, resources,
 *   political control). Legitimacy is contested but overridden; compliance is
 *   conditional on monitoring. The measurement series shows initial high
 *   extraction and suppression, moderate decline as norms internalize, then
 *   resurgence as bureaucratic alternatives fail to fully replace cultural
 *   coercion. Theater rises as performative legitimacy rituals (ceremonies,
 *   education, media) substitute for raw violence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_mechanism_kernel__exogenous_override_reading, 0.78).
domain_priors:suppression_score(imposition_mechanism_kernel__exogenous_override_reading, 0.87).
domain_priors:theater_ratio(imposition_mechanism_kernel__exogenous_override_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 0.87).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, resistance, 0.63).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_mechanism_kernel__exogenous_override_reading, snare).
narrative_ontology:human_readable(imposition_mechanism_kernel__exogenous_override_reading, "State-Coerced Norm Imposition via Monopoly on Violence").
narrative_ontology:topic_domain(imposition_mechanism_kernel__exogenous_override_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(imposition_mechanism_kernel__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_mechanism_kernel__exogenous_override_reading, 'ef79d649-1c39-44be-b5f3-72bab7052f30').
narrative_ontology:cs_kernel_codification('ef79d649-1c39-44be-b5f3-72bab7052f30', formalized).
narrative_ontology:cs_authority_grounding('ef79d649-1c39-44be-b5f3-72bab7052f30', extraction).
narrative_ontology:cs_interpretation_layer_present('ef79d649-1c39-44be-b5f3-72bab7052f30').
narrative_ontology:cs_reading_relation('ef79d649-1c39-44be-b5f3-72bab7052f30', imposition_mechanism_kernel__endogenous_climb_reading, influences).
narrative_ontology:cs_reading_relation('ef79d649-1c39-44be-b5f3-72bab7052f30', imposition_mechanism_kernel__hybrid_legitimation_reading, coexists_with).
narrative_ontology:cs_axiom('ef79d649-1c39-44be-b5f3-72bab7052f30', foundational, political_authority_supersedes_cultural_consent).
narrative_ontology:cs_axiom_status(political_authority_supersedes_cultural_consent, holdable).
narrative_ontology:cs_axiom_grounding('ef79d649-1c39-44be-b5f3-72bab7052f30', political_authority_supersedes_cultural_consent, conventional).
narrative_ontology:cs_axiom('ef79d649-1c39-44be-b5f3-72bab7052f30', foundational, monopoly_on_violence_legitimizes_norm_enforcement).
narrative_ontology:cs_axiom_status(monopoly_on_violence_legitimizes_norm_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('ef79d649-1c39-44be-b5f3-72bab7052f30', monopoly_on_violence_legitimizes_norm_enforcement, conventional).
narrative_ontology:cs_reference_frame('ef79d649-1c39-44be-b5f3-72bab7052f30', weberian_state_monopoly_on_violence).
narrative_ontology:cs_drift_state('ef79d649-1c39-44be-b5f3-72bab7052f30', contemporary_bureaucratic_state, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ef79d649-1c39-44be-b5f3-72bab7052f30', '').
narrative_ontology:cs_kernel_id(imposition_mechanism_kernel__exogenous_override_reading, imposition_mechanism_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__exogenous_override_reading, state_apparatus).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__exogenous_override_reading, ruling_elite).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__exogenous_override_reading, subject_population).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__exogenous_override_reading, dissident_groups).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__exogenous_override_reading, alternative_cultural_authorities).
narrative_ontology:constraint_vindicates(imposition_mechanism_kernel__exogenous_override_reading, state_monopoly_on_violence).
narrative_ontology:constraint_vindicates(imposition_mechanism_kernel__exogenous_override_reading, political_authority_over_cultural_norms).
narrative_ontology:constraint_vindicates(imposition_mechanism_kernel__exogenous_override_reading, legibility_as_state_capacity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decrees and enforces cultural norms through law, education, and bureaucracy. Collects compliance, tax revenue, and military manpower from the imposed uniformity. Maintains enforcement machinery (police, courts, schools) that would be costly to dismantle. Can shift enforcement priorities but cannot exit the role of norm-imposer without losing state capacity.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, state_apparatus, agenda_setter,
    institutional, generational, arbitrage, universal).

% Gains political stability, resource extraction, and legitimation from culturally uniform subjects. The imposed norms justify their rule and suppress challengers. They benefit from the constraint without administering its daily enforcement. Their exit would mean relinquishing power — not a live option.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, ruling_elite, beneficiary,
    powerful, generational, arbitrage, universal).

% Bears the cost of conformity: abandoning endogenous practices, paying cultural penalties for noncompliance, providing labor and taxes to the enforcing state. Compliance is conditional on monitoring — resistance emerges in unmonitored spaces. Exit options are limited: migration is costly, resistance is punished, internal dissent is policed. Collective resistance is possible but faces overwhelming state violence.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, subject_population, payer,
    organized, biographical, constrained, national).

% Actively resist imposed norms (religious minorities, ethnic communities, ideological opponents). Face direct repression: imprisonment, exile, execution. Their cultural survival depends on clandestine practice. No meaningful exit — they are the primary targets of the constraint's enforcement machinery.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, dissident_groups, payer,
    powerless, biographical, trapped, local).

% Religious leaders, customary elders, intellectual traditions that previously governed norm-setting. Displaced by state decree but retain latent authority among populations. Would contest the imposition if permitted public voice; their exclusion is maintained by state control of public discourse, education, and media.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, alternative_cultural_authorities, excluded,
    moderate, generational, constrained, national).

% Observes the constraint's operation across centuries: the shift from exogenous imposition to internalized norm, the persistence of enforcement machinery after founding problems dissolve, the contested legitimacy that never resolves to consensus. Does not collect from or pay into the constraint.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, historical_analyst, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Imposes uniform norms across diverse populations to enable legible governance, tax collection, and military mobilization — solving the state's problem of administrative illegibility.
% TRANSFER_FUNCTION: Moves cultural autonomy, resources, and compliance from subject populations to state apparatus via coercive enforcement backed by monopoly on violence.
% ABSENT_VOICES: Pre-state cultural authorities, minority communities with distinct norms, and populations in peripheral regions who would contest the imposition but are structurally excluded by state violence and epistemic control.
% DISAPPEARANCE_RATIONALE: Without state enforcement, imposed norms would lose their binding force; populations would revert to endogenous cultural practices or develop new hybrid forms; state capacity for extraction and mobilization would collapse, forcing renegotiation of the political order.
% FOUNDING_PROBLEM: Early states needed to make diverse populations legible and governable for taxation, conscription, and administration; cultural uniformity was imposed as an administrative simplification to replace opaque local practices.
% FOUNDING_PROBLEM_CORROBORATION: Historical sociologists (Tilly, Scott, Mann) document that cultural imposition served state-building; contemporary states maintain these mechanisms despite bureaucratic alternatives for legibility (census, ID systems, digital administration). No corroboration from beneficiary institutions alone — the administrative necessity has been superseded.
narrative_ontology:disappearance_verdict(imposition_mechanism_kernel__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_mechanism_kernel__exogenous_override_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_mechanism_kernel__exogenous_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(imposition_mechanism_kernel__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_mechanism_kernel__exogenous_override_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extraction (0.78) is high because the state collects compliance, cultural conformity, and political stability at gunpoint — the transfer is not voluntary. Suppression (0.87) is very high because alternatives are actively crushed, not merely disadvantaged. Theater (0.42) reflects the substantial performative layer: national rituals, civic education, manufactured consent. Accessibility collapse (0.82) is high because exit from state-imposed norms requires either migration (costly) or rebellion (lethal). Resistance (0.63) is significant — populations resist through foot-dragging, hidden transcripts, and periodic revolt — but remains below suppression. The snare classification fits: coordination function exists (legibility) but is asymmetrically extractive and enforcement-dependent.
 *
 * PERSPECTIVAL GAP:
 *   From the state's seat, the constraint appears as necessary coordination (legibility, unity) with manageable enforcement costs. From the subject population's seat, it is experienced as extractive coercion with no negotiated consent. From dissident groups' seat, it is existential threat. The engine computes these divergent seat classifications from the structural data — the exogenous override reading instantiates a constraint where the gap between agenda-setter and payer perceptions IS the structural fact.
 *
 * DIRECTIONALITY LOGIC:
 *   State apparatus and ruling elite are structural beneficiaries (d near 0.0) — they design, administer, and profit from the constraint. Subject population and dissident groups are targets (d near 1.0) — they pay the costs with constrained/trapped exit. Alternative cultural authorities are excluded (not in the coordination calculus). Historical analyst is analytical (d=0.5). The state's 'arbitrage' exit is illusory — it cannot exit its own monopoly on violence without ceasing to be a state — but the engine's derivation from declared beneficiaries/victims + power + exit will capture the asymmetry correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (administrative illegibility) is dead — modern states have bureaucratic alternatives (census, biometric ID, digital records) that achieve legibility without cultural uniformity. Yet the imposition machinery persists and expands (education systems, media regulation, language laws). This is mandatrophy: the mandate (make populations governable) has been solved, but the mechanism (cultural coercion) persists because it now serves a different function (political control, resource extraction). The constraint is not a scaffold (no sunset clause) and not a piton (theatrical maintenance is secondary; enforcement is real and costly). It is a snare whose original justification has evaporated but whose extraction continues.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (state violence, legal penalties) or internalized (subjects believing the imposed norms are legitimate/natural)?',
    'Post-state collapse trajectories: if suppression persists after the extractive mechanism is removed (e.g., post-colonial states maintaining colonial cultural laws), reclassify as partially internalized. Compare resistance levels in monitored vs. unmonitored spaces.',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest — subjects carry the suppression with them. This would increase effective extraction for payer seats and blur the snare/rope boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in state-imposed cultural norms').

omega_variable(
    kernel_reading_boundary,
    'Does the exogenous_override_reading describe a distinct historical mechanism, or is it a rhetorical framing of the same process described by endogenous_climb_reading and hybrid_legitimation_reading?',
    'Comparative historical analysis: code cases for whether state decree preceded or followed popular adoption. If the same case receives different codings from different scholars, the boundary is conceptual, not empirical.',
    'If the readings are framing differences on one process, they should be a single constraint with observer-dependent ε. If they are mechanistically distinct, the kernel decomposition is valid and each reading gets its own ε.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether the three readings of imposition_mechanism_kernel are structurally distinct constraints or framings of one constraint').

omega_variable(
    founding_problem_persistence,
    'Is the administrative legibility problem genuinely dead, or does it reappear in new forms (digital illegibility, population mobility, identity fragmentation) that revive the founding justification?',
    'Track whether states cite cultural uniformity as necessary for NEW administrative challenges (counter-terrorism, pandemic response, digital governance) rather than only maintaining old impositions.',
    'If the founding problem is live in new forms, the constraint may be a scaffold with rolling sunset, not a snare with dead mandate. This would change classification from snare to scaffold or tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_persistence, empirical, 'Whether the administrative legibility problem has truly been superseded or has mutated').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_mechanism_kernel__exogenous_override_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(imposition_exogenous_tr_t0, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(imposition_exogenous_tr_t0, observed).
narrative_ontology:measurement(imposition_exogenous_tr_t20, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement_basis(imposition_exogenous_tr_t20, observed).
narrative_ontology:measurement(imposition_exogenous_tr_t40, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(imposition_exogenous_tr_t40, observed).
narrative_ontology:measurement(imposition_exogenous_tr_t60, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 60, 0.45).
narrative_ontology:measurement_basis(imposition_exogenous_tr_t60, observed).
narrative_ontology:measurement(imposition_exogenous_tr_t80, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 80, 0.4).
narrative_ontology:measurement_basis(imposition_exogenous_tr_t80, observed).
narrative_ontology:measurement(imposition_exogenous_tr_t100, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 100, 0.42).
narrative_ontology:measurement_basis(imposition_exogenous_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(imposition_exogenous_be_t0, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 0, 0.85).
narrative_ontology:measurement_basis(imposition_exogenous_be_t0, observed).
narrative_ontology:measurement(imposition_exogenous_be_t20, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 20, 0.78).
narrative_ontology:measurement_basis(imposition_exogenous_be_t20, observed).
narrative_ontology:measurement(imposition_exogenous_be_t40, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 40, 0.72).
narrative_ontology:measurement_basis(imposition_exogenous_be_t40, observed).
narrative_ontology:measurement(imposition_exogenous_be_t60, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 60, 0.7).
narrative_ontology:measurement_basis(imposition_exogenous_be_t60, observed).
narrative_ontology:measurement(imposition_exogenous_be_t80, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 80, 0.75).
narrative_ontology:measurement_basis(imposition_exogenous_be_t80, observed).
narrative_ontology:measurement(imposition_exogenous_be_t100, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 100, 0.78).
narrative_ontology:measurement_basis(imposition_exogenous_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(imposition_exogenous_su_t0, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 0, 0.95).
narrative_ontology:measurement_basis(imposition_exogenous_su_t0, observed).
narrative_ontology:measurement(imposition_exogenous_su_t20, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 20, 0.88).
narrative_ontology:measurement_basis(imposition_exogenous_su_t20, observed).
narrative_ontology:measurement(imposition_exogenous_su_t40, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 40, 0.82).
narrative_ontology:measurement_basis(imposition_exogenous_su_t40, observed).
narrative_ontology:measurement(imposition_exogenous_su_t60, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 60, 0.78).
narrative_ontology:measurement_basis(imposition_exogenous_su_t60, observed).
narrative_ontology:measurement(imposition_exogenous_su_t80, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 80, 0.85).
narrative_ontology:measurement_basis(imposition_exogenous_su_t80, observed).
narrative_ontology:measurement(imposition_exogenous_su_t100, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 100, 0.87).
narrative_ontology:measurement_basis(imposition_exogenous_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_mechanism_kernel__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(imposition_mechanism_kernel__exogenous_override_reading, 0.12).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__exogenous_override_reading, state_formation_kernel).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__exogenous_override_reading, cultural_hegemony_kernel).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__exogenous_override_reading, bureaucratic_legibility_kernel).

% DUAL FORMULATION NOTE:
% This is the exogenous_override_reading of imposition_mechanism_kernel. The endogenous_climb_reading and hybrid_legitimation_reading are sibling constraints. All three share the kernel 'how do norms achieve binding force?' but instantiate different ε values and beneficiary/victim structures. This reading has the highest ε (coercive extraction) and most asymmetric beneficiary/victim split.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(imposition_mechanism_kernel__exogenous_override_reading, institutional, 0.15).
constraint_indexing:directionality_override(imposition_mechanism_kernel__exogenous_override_reading, powerful, 0.1).
constraint_indexing:directionality_override(imposition_mechanism_kernel__exogenous_override_reading, organized, 0.85).
constraint_indexing:directionality_override(imposition_mechanism_kernel__exogenous_override_reading, powerless, 0.95).
constraint_indexing:directionality_override(imposition_mechanism_kernel__exogenous_override_reading, moderate, 0.75).
constraint_indexing:directionality_override(imposition_mechanism_kernel__exogenous_override_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
