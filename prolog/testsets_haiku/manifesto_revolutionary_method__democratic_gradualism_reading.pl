% ============================================================================
% CONSTRAINT STORY: manifesto_revolutionary_method__democratic_gradualism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_manifesto_revolutionary_method__democratic_gradualism_reading, []).

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
 *   constraint_id: manifesto_revolutionary_method__democratic_gradualism_reading
 *   human_readable: Democratic Gradualism Reading of Revolutionary Method
 *   domain: political_philosophy/revolutionary_theory
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested kernel
 *   'manifesto_revolutionary_method' — the claim that working-class
 *   transformation is achievable through democratic electoral majorities and
 *   gradual institutional reform within existing constitutional structures.
 *   The reading asserts that the capitalist state CAN be reformed into a
 *   socialist state through votes and legislation, that this is the proper
 *   (not 'adventurist') path, and that vanguardist or council-communist
 *   alternatives are either impossible or illegitimate. Beneficiaries include
 *   social democratic parties and trade unions, whose institutional position
 *   and legitimacy depend on this reading being true and accepted. Victims
 *   include revolutionary militants and vanguardists, whose positions are
 *   delegitimized as 'sectarian' or 'premature' by the very frame that
 *   asserts gradualism is possible. The measurement series model
 *   extractiveness and theater rising over time as the reading's operation
 *   (suppression of rival readings, party bureaucratization of unions,
 *   slowing of actual transformation) accumulates, then plateauing or
 *   declining in scenarios where the reading's empirical falsifiability
 *   becomes undeniable.
 *
 * KEY AGENTS:
 *   - social_democratic_parties: institutional beneficiaries, set the frame of acceptable socialism
 *   - trade_unions: organized beneficiaries, constrained by electoral-gradualist discipline
 *   - revolutionary_militants: identity-locked victims, suppressed as 'adventurist'
 *   - vanguard_party_advocates: excluded from legitimate discourse, denied voice in the debate
 *   - council_communist_organizers: excluded on grounds their direct-democracy principle contradicts the reading's constitutionalist frame
 *   - capital_owners: paradoxical payers who benefit from the slowness of gradualism
 *   - analytical_observer: measures the structure and empirical falsifiability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(manifesto_revolutionary_method__democratic_gradualism_reading, 0.4).
domain_priors:suppression_score(manifesto_revolutionary_method__democratic_gradualism_reading, 0.35).
domain_priors:theater_ratio(manifesto_revolutionary_method__democratic_gradualism_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(manifesto_revolutionary_method__democratic_gradualism_reading, tangled_rope).
narrative_ontology:human_readable(manifesto_revolutionary_method__democratic_gradualism_reading, "Democratic Gradualism Reading of Revolutionary Method").
narrative_ontology:topic_domain(manifesto_revolutionary_method__democratic_gradualism_reading, "political_philosophy/revolutionary_theory").

domain_priors:requires_active_enforcement(manifesto_revolutionary_method__democratic_gradualism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(manifesto_revolutionary_method__democratic_gradualism_reading, 'b26d1307-af0c-4a9c-b7c1-d79bfc8bd5c9').
narrative_ontology:cs_kernel_codification('b26d1307-af0c-4a9c-b7c1-d79bfc8bd5c9', fixed_text).
narrative_ontology:cs_authority_grounding('b26d1307-af0c-4a9c-b7c1-d79bfc8bd5c9', lineage).
narrative_ontology:cs_interpretation_layer_present('b26d1307-af0c-4a9c-b7c1-d79bfc8bd5c9').
narrative_ontology:cs_reading_relation('b26d1307-af0c-4a9c-b7c1-d79bfc8bd5c9', manifesto_revolutionary_method__vanguard_rupture_reading, coexists_with).
narrative_ontology:cs_reading_relation('b26d1307-af0c-4a9c-b7c1-d79bfc8bd5c9', manifesto_revolutionary_method__council_communist_reading, coexists_with).
narrative_ontology:cs_axiom('b26d1307-af0c-4a9c-b7c1-d79bfc8bd5c9', foundational, democracy_sufficient_for_socialist_transformation).
narrative_ontology:cs_axiom_status(democracy_sufficient_for_socialist_transformation, holdable).
narrative_ontology:cs_axiom_grounding('b26d1307-af0c-4a9c-b7c1-d79bfc8bd5c9', democracy_sufficient_for_socialist_transformation, empirically_contingent).
narrative_ontology:cs_axiom('b26d1307-af0c-4a9c-b7c1-d79bfc8bd5c9', foundational, capitalist_state_reformable_through_legislation).
narrative_ontology:cs_axiom_status(capitalist_state_reformable_through_legislation, holdable).
narrative_ontology:cs_axiom_grounding('b26d1307-af0c-4a9c-b7c1-d79bfc8bd5c9', capitalist_state_reformable_through_legislation, empirically_contingent).
narrative_ontology:cs_axiom('b26d1307-af0c-4a9c-b7c1-d79bfc8bd5c9', secondary, working_class_electoral_majority_possible).
narrative_ontology:cs_axiom_status(working_class_electoral_majority_possible, holdable).
narrative_ontology:cs_axiom_grounding('b26d1307-af0c-4a9c-b7c1-d79bfc8bd5c9', working_class_electoral_majority_possible, empirically_contingent).
narrative_ontology:cs_reference_frame('b26d1307-af0c-4a9c-b7c1-d79bfc8bd5c9', democratic_majoritarianism_as_socialist_transformation_path).
narrative_ontology:cs_drift_state('b26d1307-af0c-4a9c-b7c1-d79bfc8bd5c9', post_eurocommunist_recapture_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b26d1307-af0c-4a9c-b7c1-d79bfc8bd5c9', '').
narrative_ontology:cs_kernel_id(manifesto_revolutionary_method__democratic_gradualism_reading, manifesto_revolutionary_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, social_democratic_parties).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, trade_unions).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, constitutional_legislatures).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__democratic_gradualism_reading, revolutionary_militants).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__democratic_gradualism_reading, vanguard_party_advocates).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__democratic_gradualism_reading, council_communist_organizers).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(manifesto_revolutionary_method__democratic_gradualism_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(manifesto_revolutionary_method__democratic_gradualism_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(manifesto_revolutionary_method__democratic_gradualism_reading_tests).
:- end_tests(manifesto_revolutionary_method__democratic_gradualism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.40) because the reading produces real benefits for some working-class actors (union recognition, welfare gains) while systematically suppressing others (revolutionaries). It is not pure extraction (like a snare would be) because the coordination function is genuine — it does mobilize working-class political power at scale. It is not pure coordination (like a rope) because the suppression of rival readings and the identity-locking of militants against their will introduces substantial asymmetry. Theater is moderate (0.28) because the reading requires continuous narrative maintenance — the empirical track record shows mixed results (some gradualist successes in Scandinavia, failures in Chile/Germany), so the reading must constantly re-frame outcomes as either vindications or merely 'setbacks en route.' Suppression is lower than extraction (0.35 vs. 0.40) because the reading opposes itself to other readings, not primarily to external actors — suppression is directed at internal movement rivals. The measurement series capture the historical arc: extractiveness and theater rise as the reading becomes institutionalized (party machinery, union bureaucracy) and faces empirical challenges it must narratively contain; suppression rises as rival readings (vanguardism, council communism) gain historical credibility through failed gradualist experiments.
 *
 * PERSPECTIVAL GAP:
 *   The social-democratic-party seat sees the reading as a genuine mobilization and reform engine; revolutionary-militant seats see it as a trap that uses the 'democratic' framing to justify suppression. The analytical observer computes a tangled rope: real coordination mixed with asymmetric extraction. The reading's institutional beneficiaries have incentive to claim it is a pure rope; the victims have incentive to claim it is a pure snare. The structural data (beneficiaries, victims, requires_active_enforcement, suppression metrics) generate a computation that diverges from both beneficiary and victim framings — the engine measures the hybrid.
 *
 * DIRECTIONALITY LOGIC:
 *   Social-democratic parties sit at the beneficiary end (d ≈ 0.1–0.2): they set the frame, collect institutional authority, and face minimal suppression directed at themselves. Trade unions sit near the symmetric end or slightly toward targets (d ≈ 0.45–0.55): they benefit from labor protections and electoral voice but are identity-locked and constrained by the party-political calendar. Revolutionary militants sit at the target end (d ≈ 0.80–0.90): they are systematically delegitimized, often arrested or driven underground, and their exit is identity-locked — they cannot leave the commitment without apostasy. Capital owners sit paradoxically: they face extractive pressure (regulation, taxation, gradual nationalization) but benefit from the slowness of the process and the structural defense the capitalist state apparatus provides, placing them near symmetric (d ≈ 0.45–0.50). The vanguardist and council-communist excluded seats cannot be assigned a d directly because they are not embedded in the constraint's operation — they are the alternatives the reading suppresses.
 *
 * MANDATROPHY ANALYSIS:
 *   The reading avoids being mislabeled as pure extraction (snare) because it genuinely coordinates working-class political mobilization at scale — millions of workers have voted, struck, and organized within this frame and achieved real gains. It avoids being mislabeled as pure coordination (rope) because the suppression of rival readings and the identity-locking of their advocates against their will is structural, not accidental. The reading's mandate — to achieve socialism through democracy — remains live in many communities, especially in social democracies. But the reading faces mandatrophy pressure: every major historicalinstance where gradualism was attempted (Germany, Chile, Portugal, France post-1981) has ended with either communist suppression (Germany 1933) or capital recapture (post-1970s restructuring). The reading's founding problem (can working-class power be exercised within democracy?) is contested, and the corroboration is thin outside the benefiting parties. Theater rising over time suggests the reading increasingly functions as narrative cover (defending past gradualist failures as 'progress' while suppressing vanguardist and council-communist challenges) rather than as a live mobilization frame.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_falsifiability_of_gradualism,
    'What would constitute empirical evidence that democratic gradualism is IMPOSSIBLE (as opposed to merely difficult)? How many failed gradualist experiments (Germany 1919–1933, Chile 1970–1973, Portugal 1974–1975) suffice to falsify the reading''s core claim?',
    'Comparative historical analysis of gradualist attempts: track what happened to property, state apparatus, and class power in each case. Distinguish ''gradualism failed because we did not have enough electoral votes'' from ''gradualism failed because capital cannot be reformed away through votes.'' The first preserves the reading; the second falsifies it.',
    'If gradualism is empirically falsifiable by a historical pattern, the reading''s status shifts from contested to dead, and the vanguardist and council-communist readings gain institutional credibility they currently lack. The constraint''s classification would shift from tangled_rope toward snare, because the ''coordination'' framing would become pure cover for suppression of rivals.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_falsifiability_of_gradualism, empirical, 'Whether the reading''s core claim has empirical falsifiability conditions.').

omega_variable(
    axiom_overriding_through_structural_recapture,
    'If every gradualist electoral victory is followed by capital recapture (price inflation, asset flight, policy reversal), at what point does the structural pattern itself invalidate the reading''s axiom that democracy can constrain capital''s power?',
    'Institutional analysis of post-electoral outcomes: does capital use state apparatus (courts, police, central banks) to undo electoral reforms, or do reforms persist? The pattern answer determines whether the axiom ''democracy can achieve socialist transformation'' is holdable or has been empirically overridden.',
    'If the pattern shows consistent recapture, the axiom shifts from holdable to overridden-by-evidence, and the reading would be reclassified as historically defunct rather than contested. Revolutionary and council-communist readings would be vindicated as correct diagnoses of why gradualism fails.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(axiom_overriding_through_structural_recapture, empirical, 'Whether structural recapture patterns override the reading''s core axioms.').

omega_variable(
    identity_lock_mechanism_in_revolutionary_militants,
    'Is the suppression of revolutionary militants a structural result of the reading''s operation (the reading itself delegitimizes and thus enables suppression of rivals) or an incidental outcome of state violence? If the reading disappeared, would the suppression persist?',
    'Analyze suppression patterns: does state repression target vanguardists specifically BECAUSE the gradualist reading has delegitimized them as ''sectarian'' and made their suppression politically tolerable? Or does the state suppress them regardless of the reading''s status? Test by examining jurisdictions where gradualism failed and rival readings gained credibility — did suppression patterns change?',
    'If suppression is structural to the reading, the reading bears responsibility for the suffering of militants whose exit is identity-locked. The constraint''s extractiveness classification would rise (higher than 0.40) if this nexus is tightened. If suppression is incidental (would happen anyway), extractiveness stays at the authored level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_in_revolutionary_militants, empirical, 'Whether the reading''s delegitimizing frame structurally enables suppression of rivals.').

omega_variable(
    reading_forecloses_vs_coexists_with_vanguardism,
    'Does the democratic-gradualist reading logically foreclose the vanguardist reading (i.e., if you accept that gradualism is possible, can you still coherently hold that vanguardism is necessary)? Or can both readings coexist as live political positions despite their contradictory empirical claims?',
    'Examine whether the readings contradict at the level of axioms (if so, foreclosure is indicated) or merely compete empirically (if so, coexistence). The vanguardist claim is ''gradualism is IMPOSSIBLE''; the gradualist claim is ''gradualism is POSSIBLE.'' These are contradictory at the object level, but they could coexist as political positions held by different parties in the same movement (e.g., Italian Eurocommunism vs. Italian Communist Party traditionalists). The determination is whether a SINGLE PARTY can coherently hold both or whether they partition across factions.',
    'If forecloses: the two readings cannot coexist within a unified movement, and suppression of one by the other becomes philosophically justified as rationality (can''t believe in both). If coexists_with: the suppression is political (resource/power competition) rather than logical, and characterizes the constraint as more extractive (using the frame to suppress rivals is pure power play). Current evidence suggests coexistence across different parties; unification into a single party would test the question.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_forecloses_vs_coexists_with_vanguardism, conceptual, 'Whether the reading logically forecloses or politically coexists with vanguardism.').

omega_variable(
    constitutional_frame_as_constraint_vs_context,
    'Is the ''existing constitutional structures'' framing a CONTEXT (the reading operates within existing democracy) or a CONSTRAINT (the reading prohibits transcending existing democracy)? The difference determines whether gradualism genuinely leaves open the possibility of rupture or structurally forecloses it.',
    'Examine whether gradualist parties and movements have ever admitted the possibility that democracy might be superseded, or whether the reading''s operation requires defending democracy itself as the final form of human governance. Track discourse: do gradualists say ''democracy now, socialism later'' or ''democracy is how socialism is achieved and socialism will remain democratic''?',
    'If the constitutional frame is a hard constraint (democracy is final), the reading forecloses vanguardism and council communism and bears responsibility for suppressing the intellectual development of alternatives. If it is a soft context (we work within democracy for now, but do not foreclose future transcendence), the reading is less extractive and leaves space for rival readings to develop.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_frame_as_constraint_vs_context, conceptual, 'Whether the reading''s commitment to constitutional democracy is absolute or conditional.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(manifesto_revolutionary_method__democratic_gradualism_reading, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mani_tr_t0, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(mani_tr_t15, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement(mani_tr_t30, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 30, 0.21).
narrative_ontology:measurement(mani_tr_t45, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 45, 0.25).
narrative_ontology:measurement(mani_tr_t60, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 60, 0.28).
narrative_ontology:measurement(mani_tr_t90, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 90, 0.32).
narrative_ontology:measurement(mani_tr_t120, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 120, 0.35).

% Extraction over time
narrative_ontology:measurement(mani_be_t0, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(mani_be_t15, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 15, 0.33).
narrative_ontology:measurement(mani_be_t30, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 30, 0.36).
narrative_ontology:measurement(mani_be_t45, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 45, 0.39).
narrative_ontology:measurement(mani_be_t60, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 60, 0.4).
narrative_ontology:measurement(mani_be_t90, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 90, 0.42).
narrative_ontology:measurement(mani_be_t120, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 120, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(mani_su_t0, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(mani_su_t15, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 15, 0.26).
narrative_ontology:measurement(mani_su_t30, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 30, 0.29).
narrative_ontology:measurement(mani_su_t45, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 45, 0.32).
narrative_ontology:measurement(mani_su_t60, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 60, 0.35).
narrative_ontology:measurement(mani_su_t90, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 90, 0.37).
narrative_ontology:measurement(mani_su_t120, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 120, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(manifesto_revolutionary_method__democratic_gradualism_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(manifesto_revolutionary_method__democratic_gradualism_reading, 0.12).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__democratic_gradualism_reading, manifesto_revolutionary_method__vanguard_rupture_reading).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__democratic_gradualism_reading, manifesto_revolutionary_method__council_communist_reading).

% DUAL FORMULATION NOTE:
% This constraint is ONE READING of the kernel 'manifesto_revolutionary_method'. The democratic-gradualist reading asserts that socialism is achievable through electoral democracy and gradual institutional reform. Sibling readings (vanguard_rupture_reading, council_communist_reading) assert that gradualism is impossible or illegitimate. These are NOT three angles on one constraint — they are three structurally distinct constraints with different ε, beneficiary/victim sets, and empirical falsifiability. The epsilon values differ substantially: democratic_gradualism_reading ε=0.40 (mixed coordination and extraction), vanguard_rupture_reading ε≈0.60+ (higher extraction through party discipline), council_communist_reading ε≈0.35 (pure coordination through direct democracy). Each reading has different victims: gradualism suppresses revolutionaries; vanguardism suppresses council-communists; council communism suppresses party apparatus. The network links record that these constraints affect each other: if gradualism fails empirically, vanguardism gains credibility; if council communism succeeds, both electoral and vanguardist readings lose institutional justification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
