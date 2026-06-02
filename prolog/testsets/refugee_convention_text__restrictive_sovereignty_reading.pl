% ============================================================================
% CONSTRAINT STORY: refugee_convention_text__restrictive_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_refugee_convention_text__restrictive_sovereignty_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: refugee_convention_text__restrictive_sovereignty_reading
 *   human_readable: 1951 Refugee Convention: Restrictive Sovereignty Reading
 *   domain: international_law/migration_governance/human_rights
 *
 * SUMMARY:
 *   The 1951 Convention Relating to the Status of Refugees establishes a
 *   floor of protection — a minimum standard that signatory states must meet.
 *   The restrictive sovereignty reading interprets this floor as permitting
 *   maximum state discretion: the Convention guarantees certain narrow
 *   protections but does not constrain states from excluding large categories
 *   of people experiencing genuine persecution. Under this reading,
 *   'persecution' requires action by the state or state-affiliated agents
 *   (excluding gang violence, domestic abuse, generalized criminal violence);
 *   'well-founded fear' requires individualized proof of targeting rather
 *   than inference from group patterns; and 'particular social group' is
 *   limited to immutable characteristics that the state explicitly
 *   recognizes. This reading enables offshore asylum processing, narrow
 *   eligibility screening, and externalization of gatekeeping functions. It
 *   presents itself as protecting state sovereignty while preserving the
 *   Convention's core protections for a limited class of applicants. The
 *   constraint exhibits mixed coordination and extraction: states genuinely
 *   benefit from clear eligibility criteria (coordination function), but the
 *   criteria exclude applicants with genuine protection needs (extraction
 *   function). The theater ratio reflects increasing performativity of asylum
 *   adjudication — formal processes that produce narrow outcomes despite
 *   humanitarian framing. Suppression requirement has increased over the
 *   interval as states have invested in offshore processing infrastructure,
 *   identity documentation demands, and nationality verification barriers
 *   that make applicants' burden of proof steeper.
 *
 * KEY AGENTS:
 *   - Non-state persecution victims (powerless/trapped) — bears extraction; excluded by definition of 'persecution'
 *   - Generalized violence victims (powerless/trapped) — bears extraction; requires individualized targeting proof they cannot provide
 *   - Applicants without immutable markers (powerless/trapped) — bears extraction; denied by PSG immutability requirement
 *   - Asylum processing bureaucracy (organized/constrained) — mixed beneficiary and victim; benefits from clear rules but constrained by resource limits and supervision
 *   - High-capacity receiving states (institutional/arbitrage) — primary beneficiary; interprets convention narrowly to manage political tolerance for asylum
 *   - Low-capacity origin/transit states (institutional/constrained) — secondary victim; faces pressure to host refugees excluded from high-capacity states; constrained by own resource limits
 *   - International human rights bodies (institutional/arbitrage) — piton perspective; formally committed to broad protection but materially aligned with state sovereignty logic
 *   - Analytical observer (analytical/analytical) — risks naturalization of sovereignty as immutable legal constraint rather than institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(refugee_convention_text__restrictive_sovereignty_reading, 0.58).
domain_priors:suppression_score(refugee_convention_text__restrictive_sovereignty_reading, 0.68).
domain_priors:theater_ratio(refugee_convention_text__restrictive_sovereignty_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(refugee_convention_text__restrictive_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(refugee_convention_text__restrictive_sovereignty_reading, "1951 Refugee Convention: Restrictive Sovereignty Reading").
narrative_ontology:topic_domain(refugee_convention_text__restrictive_sovereignty_reading, "international_law/migration_governance/human_rights").

domain_priors:requires_active_enforcement(refugee_convention_text__restrictive_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(refugee_convention_text__restrictive_sovereignty_reading, 'b229930f-907f-4488-ad99-901b1a2ba17e').
narrative_ontology:cs_kernel_codification('b229930f-907f-4488-ad99-901b1a2ba17e', fixed_text).
narrative_ontology:cs_authority_grounding('b229930f-907f-4488-ad99-901b1a2ba17e', lineage).
narrative_ontology:cs_interpretation_layer_present('b229930f-907f-4488-ad99-901b1a2ba17e').
narrative_ontology:cs_reading_relation('b229930f-907f-4488-ad99-901b1a2ba17e', refugee_convention_text__expansive_humanitarian_reading, coexists_with).
narrative_ontology:cs_reading_relation('b229930f-907f-4488-ad99-901b1a2ba17e', refugee_convention_text__procedural_integrity_reading, coexists_with).
narrative_ontology:cs_axiom('b229930f-907f-4488-ad99-901b1a2ba17e', foundational, persecution_requires_state_action).
narrative_ontology:cs_axiom_status(persecution_requires_state_action, holdable).
narrative_ontology:cs_axiom_grounding('b229930f-907f-4488-ad99-901b1a2ba17e', persecution_requires_state_action, conventional).
narrative_ontology:cs_axiom('b229930f-907f-4488-ad99-901b1a2ba17e', foundational, individualized_targeting_requirement).
narrative_ontology:cs_axiom_status(individualized_targeting_requirement, holdable).
narrative_ontology:cs_axiom_grounding('b229930f-907f-4488-ad99-901b1a2ba17e', individualized_targeting_requirement, conventional).
narrative_ontology:cs_axiom('b229930f-907f-4488-ad99-901b1a2ba17e', secondary, psg_immutability_criterion).
narrative_ontology:cs_axiom_status(psg_immutability_criterion, holdable).
narrative_ontology:cs_axiom_grounding('b229930f-907f-4488-ad99-901b1a2ba17e', psg_immutability_criterion, conventional).
narrative_ontology:cs_reference_frame('b229930f-907f-4488-ad99-901b1a2ba17e', state_sovereignty_and_controlled_borders).
narrative_ontology:cs_drift_state('b229930f-907f-4488-ad99-901b1a2ba17e', contemporary_humanitarian_pressure_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b229930f-907f-4488-ad99-901b1a2ba17e', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(refugee_convention_text__restrictive_sovereignty_reading, refugee_convention_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(refugee_convention_text__restrictive_sovereignty_reading, nation_states_high_asylum_capacity).
narrative_ontology:constraint_beneficiary(refugee_convention_text__restrictive_sovereignty_reading, border_control_regimes).
narrative_ontology:constraint_victim(refugee_convention_text__restrictive_sovereignty_reading, applicants_non_state_persecution).
narrative_ontology:constraint_victim(refugee_convention_text__restrictive_sovereignty_reading, applicants_generalized_violence).
narrative_ontology:constraint_victim(refugee_convention_text__restrictive_sovereignty_reading, applicants_lack_immutable_markers).
narrative_ontology:constraint_victim(refugee_convention_text__restrictive_sovereignty_reading, asylum_seeker_epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-STATE PERSECUTION VICTIM (SNARE) — Trapped. Faces severe persecution (gang violence, trafficking, domestic abuse) in origin country but the restrictive reading excludes non-state actors from the persecution definition. The applicant experiences maximum extraction: bears the full cost of persecution while the Convention's text refuses recognition. No exit option exists within the asylum system's parameters.
constraint_indexing:constraint_classification(refugee_convention_text__restrictive_sovereignty_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: GENERALIZED VIOLENCE VICTIM (SNARE) — Trapped. Lives in territory with endemic violence (active conflict, criminal war, state collapse) but the restrictive reading requires 'individualized persecution proof' — targeted by the state or its agents for a Convention reason. Generalized violence does not meet this threshold. The applicant bears extraction: faces genuine life-threatening danger yet excluded from protection on textual grounds.
constraint_indexing:constraint_classification(refugee_convention_text__restrictive_sovereignty_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: APPLICANT WITHOUT IMMUTABLE MARKERS (SNARE) — Trapped. Persecuted on grounds of ideology, political opinion, or mutable group affiliation (union membership, NGO work, clan affiliation acquired not inherited). The restrictive reading limits 'particular social group' to immutable characteristics with state awareness. The applicant experiences maximum extraction: genuine persecution but no textual shelter.
constraint_indexing:constraint_classification(refugee_convention_text__restrictive_sovereignty_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 4: ASYLUM PROCESSING BUREAUCRACY (TANGLED ROPE) — Constrained by resource limitations, training requirements, and international scrutiny; also benefits from the restrictive reading's clarity (reduces discretion, simplifies adjudication). The bureaucracy experiences mixed extraction and coordination: gatekeeping function is coordinated (clear rules reduce chaos) but also extractive (narrow rules exclude valid claims). Constrained exit — could adopt broader criteria but faces resource and sovereignty pressure.
constraint_indexing:constraint_classification(refugee_convention_text__restrictive_sovereignty_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: HIGH-CAPACITY RECEIVING STATE (ROPE) — Institutional beneficiary with arbitrage exit. Experiences the restrictive reading as coordination mechanism: narrow criteria reduce political pressure to accept applicants, offshore processing enables externalized gatekeeping, immutability requirement keeps victim numbers predictable. The state has discretion to adopt broader readings but maintains the restrictive interpretation to coordinate its own political tolerance for asylum. Net beneficiary.
constraint_indexing:constraint_classification(refugee_convention_text__restrictive_sovereignty_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: INTERNATIONAL HUMAN RIGHTS COMMENTARY BODY (PITON) — Formally committed to broad refugee protection via UNHCR General Comments and jurisprudence of treaty bodies; materially operates within restrictive state sovereignty logic. The interpretation machinery persists in a degraded state: produces expansive readings that states ignore, or adopts restrictive readings aligned with state practice, creating internal contradiction. Theater ratio high (formal inclusivity, actual exclusivity). Piton classification: the interpretive body is maintained through institutional inertia despite functional misalignment with stated mission.
constraint_indexing:constraint_classification(refugee_convention_text__restrictive_sovereignty_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / SOVEREIGNTY NATURALIZATION (MOUNTAIN) — From a civilizational perspective, the restrictive reading appears as an inevitable consequence of state sovereignty: no state can be bound to accept unlimited asylum claimants; therefore the Convention must be read narrowly to preserve state discretion; therefore individualized persecution and immutability are natural legal boundaries. This perspective risks naturalizing what is actually a contested institutional choice. The engine's false summit detector will identify this as a reading-dependent interpretation rather than a discovery of legal necessity.
constraint_indexing:constraint_classification(refugee_convention_text__restrictive_sovereignty_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(refugee_convention_text__restrictive_sovereignty_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(refugee_convention_text__restrictive_sovereignty_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(refugee_convention_text__restrictive_sovereignty_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(refugee_convention_text__restrictive_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(refugee_convention_text__restrictive_sovereignty_reading, TR),
    TR >= 0.70.

:- end_tests(refugee_convention_text__restrictive_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The restrictive reading creates a narrow victim set excluded from Convention protection despite genuine persecution. The extraction is not maximal because the reading does provide real protection for persecuted persons meeting its criteria (state-inflicted, individualized, based on immutable/recognized groups). Some asylum claimants do receive protection under this framework. But a substantial portion of people fleeing death or torture are systematically excluded — non-state actors account for an estimated 40-50% of persecution globally; generalized violence affects millions in conflict zones; and PSG immutability excludes political dissidents, activists, and people persecuted for acquired characteristics. Suppression (0.68): High. Multiple structural barriers suppress applicants' capacity to exit persecution through asylum: individualization requirement favors affluent applicants with documentation and advocacy; offshore processing creates physical barriers; immutability standard requires evidence of group status that applicants cannot always provide; burden of proof shifts to applicants in adversarial proceedings with limited legal representation. The suppression mechanism is enforcement infrastructure: documentation requirements, identity verification, group-status investigation, and evidentiary standards that operate cumulatively. Theater ratio (0.62): Moderate-high. Asylum adjudication processes are formally structured with due process rights, appeals procedures, and international monitoring, but materially operate within narrow gates that produce predetermined outcomes. The theater reflects the gap between the procedure's formal openness and the substantive closure of the restrictive reading — the process appears generous while the criteria ensure few approvals.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence. Applicants experiencing non-state persecution see a Snare: they are trapped and excluded by definition. The bureaucracy sees partial coordination (clear rules reduce chaos) mixed with extraction (must exclude valid humanitarian claims). The high-capacity receiving state sees coordination (manages political tolerance, enables offshore processing) with minimal extraction experience. The international human rights body sees contradiction: formally committed to broad refugee protection, materially constrained by state sovereignty interpretations. The analytical observer risks seeing sovereignty as natural law (mountain) — 'states necessarily have discretion over asylum' — when the restrictive reading is actually an institutional choice that other readings reject. The perspectival gap is sharp because the same text permits multiple coherent readings, each producing different classifications.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values vary sharply across perspectives. Non-state persecution victims: d ≈ 0.95 (full targets, trapped, powerless) → f(d) ≈ 1.42 → high experienced extraction. Applicants with immutable persecuted status: d ≈ 0.70 (mixed victim/beneficiary status — receive some protection while restricted) → f(d) ≈ 1.05 → moderate-high extraction. Asylum bureaucracy: d ≈ 0.55 (symmetric: benefits from clear rules, constrained by resource limits and supervision) → f(d) ≈ 0.75 → moderate extraction. High-capacity states: d ≈ 0.15 (beneficiaries with arbitrage exit) → f(d) ≈ -0.01 → negative/minimal extraction. The pipeline derives these values from beneficiary/victim declarations and exit options; the wide spread across perspectives reflects the structural divergence in how this constraint is experienced.
 *
 * MANDATROPHY ANALYSIS:
 *   The restrictive sovereignty reading resolves mandatrophy by claiming that coordination and extraction are cleanly separable: the Convention coordinates a shared minimum floor (state agreement on persecution definition, process due diligence), while states retain discretion to exclude applicants below that floor. This framing holds if you accept that (a) coordination genuinely requires narrow criteria and (b) states' benefit from predictable, limited liability is a legitimate policy aim. The alternative framings (expansive humanitarian reading, procedural integrity reading) argue that the coordination function does NOT require this degree of exclusion — that broader definitions of persecution, group-based evidence standards, and acquired-characteristic PSG would still coordinate state behavior while providing genuine protection. The mandatrophy is not resolved by facts; it is resolved by which reading you adopt. Each reading generates internally consistent metrics, beneficiary/victim structures, and classifications. The structural claim that 'you must choose a reading' is what mandatrophy resolves into.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    persecution_definition_boundary,
    'Are non-state actors capable of ''persecution'' within the Convention''s meaning, or is persecution inherently a state function?',
    'Textual analysis of ''persecution'' and ''by the State'' in 1951 Convention Article 1(A)(2); comparison to subsequent state practice and treaty body interpretation; examination of whether effective state failure to protect constitutes state-facilitated persecution',
    'If non-state persecution is included: victim set expands by estimated 40-60% of current global asylum seekers; constraint reclassifies toward Snare from many perspectives. If excluded: restrictive reading confirmed; extraction flow remains as modeled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(persecution_definition_boundary, conceptual, 'Whether persecution requires state action or includes state failure-to-protect').

omega_variable(
    individualization_requirement_empirical_basis,
    'Is the requirement for ''individualized persecution proof'' a necessary legal boundary or a procedural choice amenable to group-based evidence and inference?',
    'Comparative analysis of adjudication standards across jurisdictions; examination of whether statistical likelihood, group targeting patterns, and context-based inference are legally sufficient; empirical study of how individualization standard affects applicants from mass persecution contexts (genocides, ethnic cleansing, criminal networks with systematic targeting)',
    'If group-based inference and statistical patterns are legally sufficient: individualization becomes a procedural tool, not a substantive gate; constraint reclassifies toward Tangled Rope from powerless perspectives. If individualization is non-waivable: current modeling confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(individualization_requirement_empirical_basis, empirical, 'Empirical feasibility and legal sufficiency of group-based vs. individual-level persecution proof').

omega_variable(
    particular_social_group_immutability_reading,
    'Does ''particular social group'' (PSG) require immutability, or does the Convention permit recognition of groups defined by acquired characteristics (profession, ideology, organizational membership, acquired clan status)?',
    'Textual comparison of ''particular social group'' to the four express grounds (race, religion, nationality, political opinion); examination of UNHCR Handbook guidance and subsequent treaty body jurisprudence; analysis of whether immutability was the textual intent or a post-hoc interpretive restriction',
    'If immutability is not required: PSG expands to include teachers, human rights activists, defectors, union members, and clan-affiliated persons without birthright connection; victim set grows significantly; constraint reclassifies toward mixed coordination/extraction from many perspectives. If immutability is required: restrictive reading confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(particular_social_group_immutability_reading, conceptual, 'Whether immutability is a necessary feature of ''particular social group'' or an interpretive overlay').

omega_variable(
    state_awareness_requirement_scope,
    'Does ''state awareness'' of group membership function as a coherent legal test, or does it collapse under pressure from contexts where the state deliberately obscures its persecution targeting?',
    'Examination of cases where persecution is deliberately framed as ''ordinary crime'' or ''ordinary violence'' to obscure state agency; analysis of whether requiring explicit state acknowledgment of group-status persecution creates perverse incentives for state concealment; comparison to jurisprudence in other human rights domains (genocide, crimes against humanity) that do not require state awareness',
    'If state awareness is non-dispensable: current reading confirmed; offshore processing and narrow victim identification continue. If state awareness requirement produces false negatives (excludes applicants genuinely persecuted on group grounds but with state denial/concealment): constraint reclassifies toward higher extraction, more perspectives see Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_awareness_requirement_scope, empirical, 'Coherence of state-awareness requirement under cases of deliberate state concealment').

omega_variable(
    kernel_reading_contest,
    'Which reading of the 1951 Convention text correctly interprets the drafters'' intent and the Convention''s normative purpose?',
    'Comparative framing analysis: restrictive reading (convention as floor permitting maximum sovereignty) vs. expansive reading (convention as binding commitment to broad protection) vs. procedural reading (convention as framework for good-faith determination). Historical analysis of drafting records, subsequent state behavior, treaty body jurisprudence evolution, and real-world humanitarian consequences under each reading.',
    'If restrictive reading is ''correct'': current modeling confirmed; sovereignty is the binding constraint. If expansive reading is ''correct'': constraint reclassifies to higher extraction (Snare from more perspectives); false summit detection fires on sovereignty-as-natural-law framing. If procedural reading is ''correct'': both are legitimate framings of a shared text; the contest itself becomes the structural constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer-frame: which reading of the Convention kernel is the legitimate interpretation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(refugee_convention_text__restrictive_sovereignty_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refcon_tr_t0, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 0, 0.5).
narrative_ontology:measurement(refcon_tr_t10, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 10, 0.6).
narrative_ontology:measurement(refcon_tr_t20, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 20, 0.62).

% Extraction over time
narrative_ontology:measurement(refcon_be_t0, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(refcon_be_t10, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(refcon_be_t20, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(refcon_su_t0, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(refcon_su_t10, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(refcon_su_t20, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(refugee_convention_text__restrictive_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(refugee_convention_text__restrictive_sovereignty_reading, refugee_convention_text__expansive_humanitarian_reading).
narrative_ontology:affects_constraint(refugee_convention_text__restrictive_sovereignty_reading, refugee_convention_text__procedural_integrity_reading).
narrative_ontology:affects_constraint(refugee_convention_text__restrictive_sovereignty_reading, asylum_offshore_processing_regime).
narrative_ontology:affects_constraint(refugee_convention_text__restrictive_sovereignty_reading, statelessness_and_protection_gap).

% DUAL FORMULATION NOTE:
% The 1951 Refugee Convention kernel admits three structurally distinct constraint readings: restrictive sovereignty (this file), expansive humanitarian, and procedural integrity. Each reading instantiates a different constraint with a different ε, different victim sets, different classifications. The readings do not coexist as a single constraint with observer-dependent outcomes — they are genuinely different constraint structures that different states adopt. Link all three stories via network.affects_constraints to show the constraint family structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(refugee_convention_text__restrictive_sovereignty_reading, institutional, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
