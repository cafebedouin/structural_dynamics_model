% ============================================================================
% CONSTRAINT STORY: manifesto_revolutionary_method__democratic_gradualism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   constraint_id: manifesto_revolutionary_method__democratic_gradualism_reading
 *   human_readable: Democratic Electoral Road to Socialism (Gradualist Reading)
 *   domain: political/historical_materialism
 *
 * SUMMARY:
 *   This constraint is the democratic_gradualism_reading of the contested
 *   manifesto_revolutionary_method kernel. It asserts that socialism is
 *   achievable through democratic electoral majorities and gradual
 *   institutional reform within existing liberal democratic structures, with
 *   working-class power exercised through parliament and recognized unions.
 *   The reading coexists with sibling readings (vanguard rupture, council
 *   communism) as rival factions in the historical workers' movement, but its
 *   institutionalization in social democratic parties produced active
 *   suppression of revolutionary militants as 'adventurists'. The authored
 *   metrics and claimed type are independent: the reading is claimed as
 *   tangled_rope because it combines genuine coordination (electoral
 *   integration, welfare reforms) with asymmetric extraction (suppression of
 *   extra-parliamentary left alternatives).
 *
 * KEY AGENTS:
 *   - social_democratic_parties (agenda_setter/institutional/identity_locked) â administer the parliamentary strategy and enforce boundary against rupture
 *   - trade_unions (beneficiary/organized/constrained) â collect institutionalized bargaining gains within the electoral rhythm
 *   - working_class_electorate (beneficiary/organized/constrained) â receives coordination but is channeled into ballot-box politics
 *   - revolutionary_militants (payer/moderate/constrained) â bear suppression costs for advocating extra-parliamentary methods
 *   - council_communist_currents (excluded/moderate/trapped) â structurally excluded from the coalition
 *   - historical_materialist_analysts (observer/analytical/analytical) â track the divergence between promise and outcome
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(manifesto_revolutionary_method__democratic_gradualism_reading, 0.4).
domain_priors:suppression_score(manifesto_revolutionary_method__democratic_gradualism_reading, 0.58).
domain_priors:theater_ratio(manifesto_revolutionary_method__democratic_gradualism_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(manifesto_revolutionary_method__democratic_gradualism_reading, tangled_rope).
narrative_ontology:human_readable(manifesto_revolutionary_method__democratic_gradualism_reading, "Democratic Electoral Road to Socialism (Gradualist Reading)").
narrative_ontology:topic_domain(manifesto_revolutionary_method__democratic_gradualism_reading, "political/historical_materialism").

domain_priors:requires_active_enforcement(manifesto_revolutionary_method__democratic_gradualism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(manifesto_revolutionary_method__democratic_gradualism_reading, 'ccfc1224-99fb-4bcc-8656-c180d30decc1').
narrative_ontology:cs_kernel_codification('ccfc1224-99fb-4bcc-8656-c180d30decc1', fixed_text).
narrative_ontology:cs_authority_grounding('ccfc1224-99fb-4bcc-8656-c180d30decc1', lineage).
narrative_ontology:cs_interpretation_layer_present('ccfc1224-99fb-4bcc-8656-c180d30decc1').
narrative_ontology:cs_reading_relation('ccfc1224-99fb-4bcc-8656-c180d30decc1', manifesto_revolutionary_method__vanguard_rupture_reading, forecloses).
narrative_ontology:cs_reading_relation('ccfc1224-99fb-4bcc-8656-c180d30decc1', manifesto_revolutionary_method__council_communist_reading, influences).
narrative_ontology:cs_axiom('ccfc1224-99fb-4bcc-8656-c180d30decc1', foundational, socialism_via_electoral_majority).
narrative_ontology:cs_axiom_status(socialism_via_electoral_majority, holdable).
narrative_ontology:cs_axiom_grounding('ccfc1224-99fb-4bcc-8656-c180d30decc1', socialism_via_electoral_majority, empirically_contingent).
narrative_ontology:cs_axiom('ccfc1224-99fb-4bcc-8656-c180d30decc1', foundational, institutional_continuity_as_virtue).
narrative_ontology:cs_axiom_status(institutional_continuity_as_virtue, holdable).
narrative_ontology:cs_axiom_grounding('ccfc1224-99fb-4bcc-8656-c180d30decc1', institutional_continuity_as_virtue, conventional).
narrative_ontology:cs_reference_frame('ccfc1224-99fb-4bcc-8656-c180d30decc1', parliamentary_socialist_transition).
narrative_ontology:cs_drift_state('ccfc1224-99fb-4bcc-8656-c180d30decc1', contemporary_neoliberal_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ccfc1224-99fb-4bcc-8656-c180d30decc1', '').
narrative_ontology:cs_kernel_id(manifesto_revolutionary_method__democratic_gradualism_reading, manifesto_revolutionary_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, social_democratic_parties).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, trade_unions).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, working_class_electorate).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__democratic_gradualism_reading, revolutionary_militants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the strategy of achieving socialism through parliamentary majorities and incremental reform. Their organizational survival, funding, and ideological identity are fused with electoral competition and legislative negotiation within liberal democratic frameworks. They set the boundaries of acceptable left tactics and enforce discipline against extra-parliamentary challenges.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, social_democratic_parties, agenda_setter,
    institutional, generational, identity_locked, national).

% Benefit from legal recognition, collective bargaining frameworks, and institutionalized consultation with the state. Their access to bargaining tables depends on accepting the electoral rhythm and demobilizing direct action that would threaten the parliamentary coalition. Exit means abandoning legal protections for wildcat or revolutionary unionism.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, trade_unions, beneficiary,
    organized, biographical, constrained, national).

% Receive coordinated political expression through ballot boxes and union representation, along with incremental welfare gains. Their transformative energy is channeled into electoral cycles and legislative waiting periods. Alternatives such as council democracy or insurrection are rendered organizationally inaccessible by the same structures that provide representation.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, working_class_electorate, beneficiary,
    organized, biographical, constrained, national).

% Bear the costs of suppression when they advocate rupture outside parliamentary timelines. They are policed, expelled from coalitions, denounced as adventurists, and occasionally subjected to state repression tolerated or encouraged by the institutional left. Their exit options are constrained to underground organizing or demobilization.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, revolutionary_militants, payer,
    moderate, immediate, constrained, national).

% Advocate workers' councils as direct democratic organs replacing parliamentary representation. They are structurally excluded from social democratic coalition politics and from the theoretical framing of legitimate working-class power, though they remain a live theoretical current.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, council_communist_currents, excluded,
    moderate, biographical, trapped, national).

% Observe the divergence between the strategic promise of parliamentary socialism and its historical outcomes, tracking how the method channels class conflict into institutional forms and how the suppression of revolutionary alternatives stabilizes the arrangement.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, historical_materialist_analysts, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Channels working-class political energy into parliamentary elections, legislative bargaining, and incremental reform, providing a unified non-revolutionary path to socialist legislation while avoiding civil war and bourgeois counter-revolution.
% TRANSFER_FUNCTION: Moves political initiative and organizational resources from revolutionary extra-parliamentary movements into party and union institutions; transfers the cost of systemic stability onto revolutionary militants who are marginalized, policed, or expelled as adventurists.
% ABSENT_VOICES: Council communists and vanguard revolutionaries who hold that parliamentary institutions are structurally captive to capital and that gradualism neutralizes transformative potential. They are excluded from coalition politics and subjected to disciplinary suppression by the very organizations claiming to represent the working class.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, social democratic parties would lose their strategic rationale and face pressure for extra-parliamentary action; unions would need to reconsider direct-action repertoires; and revolutionary movements would no longer be suppressed by the institutional left. The landscape of left politics would reorganize around councils, vanguard parties, or new extra-parliamentary formations.
% FOUNDING_PROBLEM: How to achieve socialist transformation without triggering catastrophic bourgeois counter-revolution, and how to consolidate working-class power into a durable governing majority within existing constitutional frameworks.
% FOUNDING_PROBLEM_CORROBORATION: Revolutionary Marxist historians outside the social democratic tradition corroborate that the danger of counter-revolution was real, but dispute that gradualism solved it; liberal institutional historians corroborate the democratic continuity but reject the socialist end-goal; no independent corroboration from outside the left confirms that parliamentary gradualism successfully resolves the founding problem.
narrative_ontology:disappearance_verdict(manifesto_revolutionary_method__democratic_gradualism_reading, world_rearranges).
narrative_ontology:founding_problem_status(manifesto_revolutionary_method__democratic_gradualism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(manifesto_revolutionary_method__democratic_gradualism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(manifesto_revolutionary_method__democratic_gradualism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(manifesto_revolutionary_method__democratic_gradualism_reading, 0.4, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is authored at 0.40 (moderate) because the constraint channels working-class energy into institutional forms that limit transformative scope without fully liquidating it. Suppression is 0.58 because the constraint's persistence requires active disciplinary exclusion of revolutionary alternatives by both state and movement actors. Theater_ratio at 0.45 reflects the growing gap between socialist rhetoric and reformist practice, especially after the mid-20th century. Accessibility_collapse at 0.50 captures the organizational marginalization of council and vanguard alternatives without their complete theoretical disappearance. Resistance at 0.40 accounts for persistent revolutionary opposition and recurring intra-left splits. The temporal series show extraction accumulating through the consolidation of welfare-state social democracy, peaking as the strategy entered crisis, and moderating as the neoliberal era eroded the coordination function itself.
 *
 * PERSPECTIVAL GAP:
 *   The social democratic seat experiences the constraint as a rope: it solves the coordination problem of unifying the working class for elections and delivering incremental gains without catastrophic violence. The revolutionary militant seat experiences it as a snare: the parliamentary promise is cover for the neutralization of transformative politics, and the suppression of militants is the enforcement mechanism that proves it. The working-class electorate sits between, receiving genuine coordination benefits and bearing the diffuse cost of deferred systemic change. The engine resolves these divergent per-seat computations from the shared structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Social democratic parties and unions are declared beneficiaries (low d): they collect institutional continuity, organizational stability, and bargaining access. The working-class electorate is also a declared beneficiary, though its exit is constrained to the ballot box, damping its d slightly above the pure beneficiary end. Revolutionary militants are declared victims (high d): they are the targets of suppression, marginalization, and disciplinary violence. Council communists are excluded, sitting outside the directionality derivation entirely. The engine will compute low effective extraction for the institutional beneficiaries and high effective extraction for the militants, producing the tangled rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â achieving socialism while avoiding counter-revolution â was live in the late 19th and early 20th centuries. By the late 20th century, social democratic parties had largely abandoned the socialist horizon while retaining the institutional machinery, suggesting mandatrophy. However, the constraint is not a pure piton because active suppression of revolutionary alternatives continues whenever the electoral coalition is threatened, and the coordination function (welfare provision, union recognition) remained partially operative through the mid-20th century. The measurement series captures the accumulation of extraction and theater, providing data for the engine to evaluate whether the constraint has crossed into pure performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    working_class_beneficiary_ambiguity,
    'Is the working-class electorate a genuine beneficiary of coordination, or a diffuse payer whose transformative capacity is neutralized by the electoral channel?',
    'Comparative historical analysis: compare socioeconomic outcomes for working-class constituencies under social democratic governance versus periods of extra-parliamentary mobilization or council democracy.',
    'If the working class is primarily a diffuse payer, effective extraction is higher than the moderate base epsilon suggests, pushing the computed type toward snare for that seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(working_class_beneficiary_ambiguity, conceptual, 'Ambiguity about working-class position in the arrangement').

omega_variable(
    suppression_mechanism_origin,
    'Is the suppression of revolutionary militants executed by the liberal state alone, or jointly by social democratic party discipline and movement institutions?',
    'Archival and historiographic analysis of expulsions, factional discipline, and the role of social democratic leadership in requesting or legitimizing state repression against the left.',
    'If suppression is primarily intra-movement, the constraint''s directionality for militants is higher because the extraction is enforced by their own ostensible representatives rather than by an external enemy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_origin, empirical, 'Whether suppression is intra-movement or state-external').

omega_variable(
    electoral_majority_feasibility,
    'Can a parliamentary majority actually legislate socialism against the structural veto power of capital flight, judicial review, and international market discipline?',
    'Comparative case studies of social democratic governments attempting nationalization or radical redistribution, measuring the structural constraints they encountered.',
    'If parliamentary socialism is structurally infeasible, the coordination function is illusory and the constraint is primarily extractive, raising epsilon and shifting classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(electoral_majority_feasibility, empirical, 'Empirical feasibility of parliamentary socialist transformation').

omega_variable(
    kernel_reading_decomposition,
    'This constraint is one reading of the manifesto_revolutionary_method kernel. Would the vanguard_rupture_reading or council_communist_reading produce a structurally distinct classification?',
    'Generate sibling constraint stories and compare epsilon, beneficiary/victim sets, and computed per-seat types across the kernel family.',
    'Sibling readings likely produce higher epsilon and different victim/beneficiary structures, confirming that the kernel decomposes into multiple structurally distinct constraints rather than one observer-relative constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_decomposition, conceptual, 'Committer frame omega for kernel decomposition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(manifesto_revolutionary_method__democratic_gradualism_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(demgrad_tr_t0, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(demgrad_tr_t20, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(demgrad_tr_t40, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 40, 0.35).
narrative_ontology:measurement(demgrad_tr_t60, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 60, 0.5).
narrative_ontology:measurement(demgrad_tr_t80, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 80, 0.55).
narrative_ontology:measurement(demgrad_tr_t100, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 100, 0.45).

% Extraction over time
narrative_ontology:measurement(demgrad_be_t0, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(demgrad_be_t20, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement(demgrad_be_t40, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 40, 0.42).
narrative_ontology:measurement(demgrad_be_t60, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 60, 0.48).
narrative_ontology:measurement(demgrad_be_t80, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 80, 0.45).
narrative_ontology:measurement(demgrad_be_t100, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 100, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(demgrad_su_t0, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(demgrad_su_t20, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(demgrad_su_t40, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement(demgrad_su_t60, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 60, 0.5).
narrative_ontology:measurement(demgrad_su_t80, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 80, 0.45).
narrative_ontology:measurement(demgrad_su_t100, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 100, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(manifesto_revolutionary_method__democratic_gradualism_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
