% ============================================================================
% CONSTRAINT STORY: biblical_authority__sola_scriptura_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_authority__sola_scriptura_reading, []).

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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: biblical_authority__sola_scriptura_reading
 *   human_readable: Sola Scriptura: Scripture Alone as Self-Interpreting Authority
 *   domain: theological/religious_studies
 *
 * SUMMARY:
 *   The sola scriptura reading of biblical authority holds that Scripture
 *   alone is sufficient and self-interpreting for Christian doctrine and
 *   practice, without need for magisterial tradition or conciliar
 *   adjudication. It emerged as a contested Reformation claim against
 *   medieval hierarchical authority, and it functions as an
 *   identity-coordination mechanism for Protestant communities. Structurally,
 *   it distributes interpretive authority to individual believers and local
 *   congregations, eliminating clerical extraction at the cost of doctrinal
 *   fragmentation. This constraint is one reading of the biblical_authority
 *   kernel; sibling readings (tradition-scripture, conciliar) instantiate
 *   structurally distinct constraints with different epsilon values and
 *   beneficiary-victim profiles.
 *
 * KEY AGENTS:
 *   - lay_believers: Primary beneficiary (moderate/mobile) â gain autonomous interpretive access but face fragmentation costs
 *   - congregational_communities: Agenda setter (organized/constrained) â self-governing local bodies that enforce the sola scriptura boundary
 *   - ecumenical_movements: Primary payer (organized/constrained) â bear the structural cost of fragmentation and blocked institutional convergence
 *   - catholic_orthodox_hierarchies: Excluded observer (institutional/analytical) â represent the magisterial alternative structurally outside this framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_authority__sola_scriptura_reading, 0.32).
domain_priors:suppression_score(biblical_authority__sola_scriptura_reading, 0.45).
domain_priors:theater_ratio(biblical_authority__sola_scriptura_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_authority__sola_scriptura_reading, tangled_rope).
narrative_ontology:human_readable(biblical_authority__sola_scriptura_reading, "Sola Scriptura: Scripture Alone as Self-Interpreting Authority").
narrative_ontology:topic_domain(biblical_authority__sola_scriptura_reading, "theological/religious_studies").

domain_priors:requires_active_enforcement(biblical_authority__sola_scriptura_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_authority__sola_scriptura_reading, '77961de2-3d2b-46bf-b52a-4972cb085ef9').
narrative_ontology:cs_kernel_codification('77961de2-3d2b-46bf-b52a-4972cb085ef9', fixed_text).
narrative_ontology:cs_authority_grounding('77961de2-3d2b-46bf-b52a-4972cb085ef9', self_enforcing).
narrative_ontology:cs_reading_relation('77961de2-3d2b-46bf-b52a-4972cb085ef9', biblical_authority__tradition_scripture_reading, forecloses).
narrative_ontology:cs_reading_relation('77961de2-3d2b-46bf-b52a-4972cb085ef9', biblical_authority__conciliar_reading, coexists_with).
narrative_ontology:cs_axiom('77961de2-3d2b-46bf-b52a-4972cb085ef9', foundational, scripture_alone_sufficient).
narrative_ontology:cs_axiom_status(scripture_alone_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('77961de2-3d2b-46bf-b52a-4972cb085ef9', scripture_alone_sufficient, theological).
narrative_ontology:cs_axiom('77961de2-3d2b-46bf-b52a-4972cb085ef9', foundational, scripture_self_interpreting).
narrative_ontology:cs_axiom_status(scripture_self_interpreting, holdable).
narrative_ontology:cs_axiom_grounding('77961de2-3d2b-46bf-b52a-4972cb085ef9', scripture_self_interpreting, theological).
narrative_ontology:cs_reference_frame('77961de2-3d2b-46bf-b52a-4972cb085ef9', scriptural_self_sufficiency_framework).
narrative_ontology:cs_drift_state('77961de2-3d2b-46bf-b52a-4972cb085ef9', post_denominational_fragmentation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('77961de2-3d2b-46bf-b52a-4972cb085ef9', '').
narrative_ontology:cs_kernel_id(biblical_authority__sola_scriptura_reading, biblical_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_authority__sola_scriptura_reading, lay_believers).
narrative_ontology:constraint_beneficiary(biblical_authority__sola_scriptura_reading, congregational_communities).
narrative_ontology:constraint_victim(biblical_authority__sola_scriptura_reading, ecumenical_movements).
narrative_ontology:constraint_vindicates(biblical_authority__sola_scriptura_reading, priesthood_of_all_believers).
narrative_ontology:constraint_vindicates(biblical_authority__sola_scriptura_reading, congregational_autonomy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercise autonomous interpretation of Scripture without clerical mediation; gain direct access to divine authority but face competing interpretations across denominations without adjudicative recourse.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, lay_believers, beneficiary,
    moderate, biographical, mobile, global).

% Self-governing local bodies that determine doctrine and practice through their own reading of Scripture; they enforce the sola scriptura boundary against magisterial authority claims while lacking external accountability for resolving interpretive disputes.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, congregational_communities, agenda_setter,
    organized, generational, constrained, global).

% Seek visible unity across Christian communities but bear the structural cost of doctrinal fragmentation; the absence of shared adjudicative authority under sola scriptura multiplies incompatible interpretations that block institutional convergence.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, ecumenical_movements, payer,
    organized, generational, constrained, global).

% Represent magisterial and conciliar authority structures explicitly rejected by the sola scriptura framework; they contest the reading but stand structurally outside its authority logic, not directly governed by it.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, catholic_orthodox_hierarchies, excluded,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared authority source for Christian doctrine and practice that operates without centralized magisterial hierarchy, allowing dispersed communities to coordinate belief and worship around a common text.
% TRANSFER_FUNCTION: Moves interpretive authority from centralized magisterial institutions to individual believers and local congregations; transfers the cost of doctrinal arbitration from a universal hierarchy to fragmented local communities and ecumenical movements.
% ABSENT_VOICES: Catholic and Orthodox hierarchies who regard magisterial and conciliar tradition as necessary for authoritative interpretation are structurally excluded from the sola scriptura framework; their objections are treated as foreign to the authority structure rather than as internal dissent.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, Protestant communities would lose their primary boundary marker against magisterial Christianity; interpretive authority would recentralize toward tradition-bearing institutions or collapse into pure individualism, and the current landscape of denominational fragmentation would reorganize around alternative authority claims.
% FOUNDING_PROBLEM: The medieval Western church concentrated interpretive authority in a clerical hierarchy that was perceived as corrupt, opaque, and extractive; believers lacked direct access to scriptural authority and were subject to doctrinal innovations not grounded in the biblical text.
% FOUNDING_PROBLEM_CORROBORATION: Protestant historians and theologians attest the problem was real and persists in magisterial traditions. Catholic and Orthodox scholars attest the founding problem was a misreading of tradition's role; they corroborate that the problem as framed is contested, not that the hierarchy was extractive. Independent historians note both the genuine abuses and the political and economic motives of the Reformers.
narrative_ontology:disappearance_verdict(biblical_authority__sola_scriptura_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_authority__sola_scriptura_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_authority__sola_scriptura_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-19',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(biblical_authority__sola_scriptura_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_authority__sola_scriptura_reading, 0.32, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_authority__sola_scriptura_reading_tests).
:- end_tests(biblical_authority__sola_scriptura_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-to-moderate (0.32 at interval end) because the expected structural delta explicitly notes low clerical extraction; the constraint coordinates authority without creating a concentrated extraction seat. Suppression is moderate (0.45) because the 'alone' in sola scriptura requires active boundary maintenance against magisterial and traditional authority claims within Protestant spheres. Theater ratio is low (0.10â0.25) because enforcement is largely substantive boundary maintenance rather than performative compliance. Accessibility collapse is high (0.75) because once sola scriptura is accepted, magisterial alternatives collapse as live options within that community. Resistance is moderate (0.55) because Catholic, Orthodox, and some Anglican traditions actively contest the reading. The temporal series show slow extraction accumulation as denominational multiplication gradually thickened institutional barriers.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seat (lay believers, congregational communities) experiences the constraint as liberating coordination â direct access to divine authority without hierarchical mediation. The payer seat (ecumenical movements) experiences the same structure as a fragmentation engine that multiplies incompatible interpretations and prevents institutional convergence. The excluded seat (magisterial hierarchies) experiences it as an illegitimate usurpation of interpretive authority. The engine computes these divergences from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Lay believers and congregations are declared beneficiaries because the constraint's primary structural effect is to transfer interpretive authority downward from hierarchies to local communities and individuals. Ecumenical movements are declared victims because the same distributed-authority structure that empowers congregations prevents the centralized adjudication necessary for institutional unity. Catholic and Orthodox hierarchies are excluded rather than victims because they stand structurally outside the constraint â they reject its premise rather than being extracted through it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â perceived clerical corruption and opacity in the medieval Western church â is contested in status. If the founding problem is dead (contemporary Catholic and Orthodox scholars argue the abuses were historically specific or exaggerated), but the constraint persists as a boundary marker of Protestant identity, then the mandatrophy mechanism flags the constraint as potentially piton-like. However, the low theater ratio and the ongoing genuine coordination function (providing authority without hierarchy) prevent piton classification. The classification as tangled_rope captures both the real coordination and the asymmetric fragmentation cost, preventing misreading as pure extraction (snare) or pure coordination (rope).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    self_interpreting_ambiguity,
    'Is Scripture genuinely self-interpreting, or does the appearance of self-interpretation mask the operation of hidden interpretive traditions and community assumptions?',
    'Comparative analysis of interpretive outcomes across isolated sola scriptura communities versus tradition-guided communities; detection of convergent interpretive patterns that correlate with denominational heritage rather than text alone.',
    'If hidden traditions are doing the interpretive work, the constraint''s claimed coordination mechanism is actually identity_coordination through unacknowledged tradition, raising base extractiveness and shifting the classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_interpreting_ambiguity, conceptual, 'Whether self-interpretation is genuine or masks hidden traditional mediation').

omega_variable(
    fragmentation_cost_allocation,
    'Is doctrinal fragmentation an inherent cost of distributed authority, or does it primarily harm agents outside the benefiting set?',
    'Measure ecumenical negotiation failure rates and lay believer doctrinal confusion indices across authority structures; compare outcomes in magisterial versus sola scriptura contexts.',
    'If fragmentation costs fall primarily on lay believers, they are victims as well as beneficiaries, shifting classification toward snare; if costs are borne mainly by ecumenical institutions external to the benefiting set, the tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fragmentation_cost_allocation, empirical, 'Who bears the structural cost of doctrinal fragmentation').

omega_variable(
    kernel_reading_contest,
    'Is this constraint one coherent reading of the biblical authority kernel, or does its classification depend on treating sola scriptura as exclusive rather than as a practical priority?',
    'Analysis of prima scriptura and neo-traditional Protestant formulations that affirm sola without rejecting tradition entirely; assessment of whether soft forms are structurally viable within this reading.',
    'If softer forms are valid within this reading, the extracted victim set shrinks and the constraint may reclassify as rope; if exclusivity is structurally required, the tangled_rope classification is robust.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether the reading requires absolute exclusion of tradition or allows prioritization').

omega_variable(
    divine_ordinance_vs_construct,
    'Is sola scriptura a discovered divine pattern governing authoritative revelation, or a constructed theological response to specific historical abuses?',
    'Historical analysis of pre-Reformation Christian authority structures and the novelty of the alone-formulation; examination of whether the constraint persists by naturality or by institutional enforcement.',
    'If constructed, the constraint''s natural-law framing collapses and its coordination function is revealed as historically contingent identity maintenance; this would raise theater ratio and shift drift classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_ordinance_vs_construct, empirical, 'Whether the constraint is natural or historically constructed').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_authority__sola_scriptura_reading, 0, 500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sola_scriptura_tr_t0, biblical_authority__sola_scriptura_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sola_scriptura_tr_t100, biblical_authority__sola_scriptura_reading, theater_ratio, 100, 0.15).
narrative_ontology:measurement(sola_scriptura_tr_t200, biblical_authority__sola_scriptura_reading, theater_ratio, 200, 0.18).
narrative_ontology:measurement(sola_scriptura_tr_t300, biblical_authority__sola_scriptura_reading, theater_ratio, 300, 0.2).
narrative_ontology:measurement(sola_scriptura_tr_t400, biblical_authority__sola_scriptura_reading, theater_ratio, 400, 0.22).
narrative_ontology:measurement(sola_scriptura_tr_t500, biblical_authority__sola_scriptura_reading, theater_ratio, 500, 0.25).

% Extraction over time
narrative_ontology:measurement(sola_scriptura_be_t0, biblical_authority__sola_scriptura_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(sola_scriptura_be_t100, biblical_authority__sola_scriptura_reading, base_extractiveness, 100, 0.25).
narrative_ontology:measurement(sola_scriptura_be_t200, biblical_authority__sola_scriptura_reading, base_extractiveness, 200, 0.28).
narrative_ontology:measurement(sola_scriptura_be_t300, biblical_authority__sola_scriptura_reading, base_extractiveness, 300, 0.3).
narrative_ontology:measurement(sola_scriptura_be_t400, biblical_authority__sola_scriptura_reading, base_extractiveness, 400, 0.31).
narrative_ontology:measurement(sola_scriptura_be_t500, biblical_authority__sola_scriptura_reading, base_extractiveness, 500, 0.32).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(biblical_authority__sola_scriptura_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(biblical_authority__sola_scriptura_reading, tradition_scripture_reading).
narrative_ontology:affects_constraint(biblical_authority__sola_scriptura_reading, conciliar_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the biblical_authority kernel. The kernel decomposes into at least three structurally distinct claims: sola scriptura (Scripture alone, self-interpreting), tradition-scripture (Scripture requires tradition/magisterium), and conciliar (Scripture through councils). Each reading has different beneficiary/victim structures and epsilon values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
