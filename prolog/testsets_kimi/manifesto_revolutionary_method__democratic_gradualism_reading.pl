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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: manifesto_revolutionary_method__democratic_gradualism_reading
 *   human_readable: Democratic Gradualist Socialist Transition
 *   domain: political/philosophical/revolutionary_theory
 *
 * SUMMARY:
 *   This constraint story models the democratic gradualist reading of
 *   revolutionary method: the claim that socialism can be achieved through
 *   electoral majorities and incremental reform within existing liberal
 *   democratic institutions. As one reading of the
 *   manifesto_revolutionary_method kernel, it competes with vanguard rupture
 *   and council communist readings. The constraint operates as a tangled
 *   rope, genuinely coordinating working-class political energy into
 *   welfare-state achievements and democratic participation while
 *   asymmetrically extracting political initiative from revolutionary
 *   militants and channeling transformative demands into manageable
 *   institutional routines. Social democratic parties and union bureaucracies
 *   are the structural beneficiaries; revolutionary militants are the
 *   victims, suppressed as adventurist through party discipline, legal
 *   frameworks, and ideological boundary-policing. The authored metrics are
 *   independent of the claim: moderate extractiveness (0.40) reflects real
 *   reform achievements, while substantial suppression (0.60) reflects the
 *   active exclusion of revolutionary alternatives.
 *
 * KEY AGENTS:
 *   - social_democratic_parties: Primary agenda-setter (institutional/constrained) â administers the strategic framework through parliamentary and party machinery, sets the political horizon to electoral timelines, and captures state and institutional positions.
 *   - trade_union_bureaucracies: Primary beneficiary (organized/constrained) â recognized bargaining agents within industrial relations frameworks, channel workplace militancy into contractual processes, and police unauthorized strike activity.
 *   - revolutionary_militants: Primary payer (moderate/trapped) â organize for extra-parliamentary socialist transformation, bear the costs of expulsion and repression, and are ideologically marginalized as adventurists.
 *   - working_class_electorate: Dual-positioned beneficiary/payer (organized/constrained) â exercises formal power through suffrage and union membership, receives material reforms, but experiences strategic capture within electoral routines.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(manifesto_revolutionary_method__democratic_gradualism_reading, 0.4).
domain_priors:suppression_score(manifesto_revolutionary_method__democratic_gradualism_reading, 0.6).
domain_priors:theater_ratio(manifesto_revolutionary_method__democratic_gradualism_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(manifesto_revolutionary_method__democratic_gradualism_reading, tangled_rope).
narrative_ontology:human_readable(manifesto_revolutionary_method__democratic_gradualism_reading, "Democratic Gradualist Socialist Transition").
narrative_ontology:topic_domain(manifesto_revolutionary_method__democratic_gradualism_reading, "political/philosophical/revolutionary_theory").

domain_priors:requires_active_enforcement(manifesto_revolutionary_method__democratic_gradualism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(manifesto_revolutionary_method__democratic_gradualism_reading, '3a2616c0-aa7e-4ae4-bf5a-d49dd2be381e').
narrative_ontology:cs_kernel_codification('3a2616c0-aa7e-4ae4-bf5a-d49dd2be381e', formalized).
narrative_ontology:cs_authority_grounding('3a2616c0-aa7e-4ae4-bf5a-d49dd2be381e', lineage).
narrative_ontology:cs_interpretation_layer_present('3a2616c0-aa7e-4ae4-bf5a-d49dd2be381e').
narrative_ontology:cs_reading_relation('3a2616c0-aa7e-4ae4-bf5a-d49dd2be381e', manifesto_revolutionary_method__vanguard_rupture_reading, coexists_with).
narrative_ontology:cs_reading_relation('3a2616c0-aa7e-4ae4-bf5a-d49dd2be381e', manifesto_revolutionary_method__council_communist_reading, coexists_with).
narrative_ontology:cs_axiom('3a2616c0-aa7e-4ae4-bf5a-d49dd2be381e', foundational, socialist_transformation_via_electoral_majority).
narrative_ontology:cs_axiom_status(socialist_transformation_via_electoral_majority, holdable).
narrative_ontology:cs_axiom_grounding('3a2616c0-aa7e-4ae4-bf5a-d49dd2be381e', socialist_transformation_via_electoral_majority, instrumental).
narrative_ontology:cs_axiom('3a2616c0-aa7e-4ae4-bf5a-d49dd2be381e', foundational, institutional_continuity_of_liberal_democracy).
narrative_ontology:cs_axiom_status(institutional_continuity_of_liberal_democracy, holdable).
narrative_ontology:cs_axiom_grounding('3a2616c0-aa7e-4ae4-bf5a-d49dd2be381e', institutional_continuity_of_liberal_democracy, deontological).
narrative_ontology:cs_reference_frame('3a2616c0-aa7e-4ae4-bf5a-d49dd2be381e', parliamentary_socialist_transformation).
narrative_ontology:cs_drift_state('3a2616c0-aa7e-4ae4-bf5a-d49dd2be381e', neoliberal_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3a2616c0-aa7e-4ae4-bf5a-d49dd2be381e', '').
narrative_ontology:cs_kernel_id(manifesto_revolutionary_method__democratic_gradualism_reading, manifesto_revolutionary_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, social_democratic_parties).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, trade_union_bureaucracies).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__democratic_gradualism_reading, revolutionary_militants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, working_class_electorate).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__democratic_gradualism_reading, working_class_electorate).
narrative_ontology:constraint_vindicates(manifesto_revolutionary_method__democratic_gradualism_reading, parliamentary_sovereignty_thesis).
narrative_ontology:constraint_vindicates(manifesto_revolutionary_method__democratic_gradualism_reading, institutional_continuity_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control parliamentary strategy and party machinery; channel working-class demands into legislative agendas and ministerial portfolios; enforce party discipline against radical factions; occupy state positions whose authority depends on preserving liberal constitutional routines.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, social_democratic_parties, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(manifesto_revolutionary_method__democratic_gradualism_reading, social_democratic_parties, beneficiary).

% Legally recognized bargaining agents within industrial relations frameworks; negotiate contracts and grievance procedures through established channels; police unauthorized strikes and radical workplace actions to maintain institutional standing and legal protections.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, trade_union_bureaucracies, beneficiary,
    organized, biographical, constrained, national).

% Organize for extra-parliamentary or insurrectionary socialist transformation; subject to expulsion from unified left institutions, police repression, and ideological marginalization as adventurists or ultra-leftists; denied access to the dominant channels of working-class political expression.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, revolutionary_militants, payer,
    moderate, biographical, trapped, national).

% Exercises formal political power through universal suffrage and union membership; receives material improvements and social protections through reformist legislation; experiences the narrowing of strategic imagination to electoral timelines and the demobilization of direct-action capacity.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, working_class_electorate, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(manifesto_revolutionary_method__democratic_gradualism_reading, working_class_electorate, payer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Channels working-class political energy into parliamentary procedure and incremental reform, managing class conflict without civil war and achieving material improvements through institutional continuity.
% TRANSFER_FUNCTION: Moves political authority and strategic initiative from extra-parliamentary movements and revolutionary militants to parliamentary party leadership and recognized union bureaucracies; transfers the costs of social peace onto the excluded radical left.
% ABSENT_VOICES: Council communists, anarcho-syndicalists, and insurrectionary movements are structurally excluded from social democratic and union institutions; their advocacy for workers councils and direct action is delegitimized as incompatible with democratic norms.
% DISAPPEARANCE_RATIONALE: If democratic gradualism as a hegemonic method vanished, social democratic parties would lose their strategic monopoly over the left, revolutionary organizations would gain mass political space, and the boundary between reform and revolution would reopen â the institutional architecture of the parliamentary left would reorganize.
% FOUNDING_PROBLEM: How to achieve socialist transformation without the violence and dictatorship of a revolutionary rupture, leveraging existing democratic institutions, universal suffrage, and organized labor within liberal constitutional frameworks.
% FOUNDING_PROBLEM_CORROBORATION: Social democratic party historians and official labor movement archives attest the founding problem from within the beneficiary set. Revolutionary socialist and council communist historians from outside the beneficiary set attest that the problem was either insoluble within this framework or a misframing that served to stabilize capitalism. No independent corroboration exists; the problem's very status is the axis of political contestation.
narrative_ontology:disappearance_verdict(manifesto_revolutionary_method__democratic_gradualism_reading, world_rearranges).
narrative_ontology:founding_problem_status(manifesto_revolutionary_method__democratic_gradualism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(manifesto_revolutionary_method__democratic_gradualism_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate (0.40) because the constraint delivers genuine coordination goods: welfare states, labor protections, and democratic participation. It is not pure extraction. Suppression is substantial (0.60) because the persistence of gradualist hegemony requires active exclusion of revolutionary alternatives through party discipline, legal frameworks, and ideological boundary-policing. Theater ratio (0.45) reflects the growing gap between socialist rhetoric and the actual management of capitalist economies, especially from mid-century onward. Accessibility collapse (0.65) captures how electoral and union channels absorb working-class agency such that revolutionary alternatives become nearly unthinkable within the mainstream left. Resistance (0.55) reflects persistent contestation from revolutionary traditions and occasional rank-and-file insurgency. Measurements track the lifecycle on a single shared time grid: extraction peaks mid-century as social democracy achieves institutional maturity, theater rises as the gap between rhetoric and practice widens, and suppression fluctuates with the intensity of revolutionary challenge.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (social democratic parties) experiences the arrangement as genuine coordination â a necessary method for achieving socialist advances without catastrophic violence. The payer seat (revolutionary militants) experiences the same structure as suppression and capture. The working-class electorate sits near symmetric: they receive real benefits but pay through the foreclosure of extra-institutional power. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Social democratic parties and trade union bureaucracies are declared beneficiaries with constrained exit; the engine derives low directionality (near the beneficiary end). Revolutionary militants are declared victims with trapped exit; the engine derives high directionality (near the target end). The working-class electorate is not declared in base_properties beneficiaries or victims, but its dual stakeholder role signals near-symmetric directionality. The national spatial scope amplifies extraction modestly for the trapped target.
 *
 * MANDATROPHY ANALYSIS:
 *   Without the tangled rope classification, this constraint would likely be misread either as a Rope (ignoring the suppression of revolutionary alternatives and the capture of working-class strategic imagination) or as a Snare (ignoring the genuine welfare-state coordination and material improvements delivered to the working class). The Tangled Rope gate requires both a coordination function and asymmetric extraction with active enforcement â all of which are structurally present. The founding problem status is contested, preventing automatic mandatrophy resolution: proponents claim the problem is still live, while critics claim the method has outlived its function or was never adequate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This constraint is the democratic_gradualism_reading of the manifesto_revolutionary_method kernel. How would its classification shift if the vanguard_rupture_reading or council_communist_reading were operative instead?',
    'Cross-reading comparison within the kernel family; evaluate which structural elements (beneficiary set, victim set, enforcement requirements) are reading-specific versus kernel-invariant.',
    'Under vanguard rupture, beneficiaries and victims invert and coordination type shifts toward enforcement_mechanism; under council communism, the agenda-setter dissolves into distributed workplace assemblies and the extraction profile changes fundamentally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Sibling reading structural variance within the revolutionary method kernel.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of revolutionary militants under democratic gradualism primarily structural (party expulsion, legal barriers, police repression) or internalized (common-sense acceptance of electoral horizons, fear of chaos)?',
    'Post-crisis trajectory analysis: observe whether revolutionary organizing revives when structural barriers are temporarily suspended.',
    'If internalized, effective suppression exceeds the structural measure and the constraint operates partly as cognitive capture; if purely structural, it remains a conventional tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression of revolutionary alternatives.').

omega_variable(
    founding_problem_solvability,
    'Was the founding problem â achieving socialism through gradual reform â ever structurally soluble, or was the method inherently stabilizing for capitalism?',
    'Comparative historical analysis of social democratic majority governments and their structural relationship to capital.',
    'If inherently insoluble, the coordination function serves as cover and the constraint leans toward snare; if soluble but blocked, it remains tangled rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_solvability, conceptual, 'Whether parliamentary socialism was ever achievable or a stabilizing construct.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(manifesto_revolutionary_method__democratic_gradualism_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mani_tr_t0, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(mani_tr_t20, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(mani_tr_t40, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(mani_tr_t60, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 60, 0.5).
narrative_ontology:measurement(mani_tr_t80, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 80, 0.55).
narrative_ontology:measurement(mani_tr_t100, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 100, 0.5).

% Extraction over time
narrative_ontology:measurement(mani_be_t0, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(mani_be_t20, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement(mani_be_t40, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 40, 0.4).
narrative_ontology:measurement(mani_be_t60, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 60, 0.42).
narrative_ontology:measurement(mani_be_t80, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 80, 0.4).
narrative_ontology:measurement(mani_be_t100, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 100, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(mani_su_t0, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(mani_su_t20, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(mani_su_t40, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement(mani_su_t60, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 60, 0.65).
narrative_ontology:measurement(mani_su_t80, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 80, 0.55).
narrative_ontology:measurement(mani_su_t100, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 100, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(manifesto_revolutionary_method__democratic_gradualism_reading, identity_coordination).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__democratic_gradualism_reading, vanguard_rupture_reading).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__democratic_gradualism_reading, council_communist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the manifesto_revolutionary_method kernel, which decomposes into three structurally distinct constraints (democratic_gradualism, vanguard_rupture, council_communist). Each reading has a distinct epsilon, beneficiary/victim structure, and coordination type. They share the kernel but instantiate different constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
