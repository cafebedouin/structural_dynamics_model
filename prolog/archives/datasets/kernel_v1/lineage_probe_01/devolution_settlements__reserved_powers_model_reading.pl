% ============================================================================
% CONSTRAINT STORY: devolution_settlements__reserved_powers_model_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_devolution_settlements__reserved_powers_model_reading, []).

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
 *   constraint_id: devolution_settlements__reserved_powers_model_reading
 *   human_readable: Scotland's Reserved Powers Model — Expansive Home Rule by Statutory Silence
 *   domain: constitutional_law/devolution
 *
 * SUMMARY:
 *   Scotland's devolution settlement (Scotland Act 1998) instantiates a
 *   constitutional choice about default power allocation: instead of listing
 *   all powers granted to Holyrood (the additive model used in Northern
 *   Ireland and pre-1998 Wales), the statute lists only what is RESERVED to
 *   Westminster, meaning everything unlisted defaults to Edinburgh. This
 *   inversion — silence as grant rather than retention — embedded expansive
 *   home rule into the statute's structure. The constraint examines how this
 *   drafting choice operates as a coordination mechanism (it clarifies
 *   boundaries via the reserved list) and as an extraction mechanism
 *   (Westminster's residual claims are silently extinguished by the logic of
 *   the model). The reserved powers model is one reading of a contested
 *   kernel: the devolution settlement itself. Other readings interpret the
 *   same settlement as an independence pathway (it built the institutional
 *   foundations for exit), or as a structure now strained by Brexit (the
 *   Sewel Convention revealed as performative). This reading focuses on the
 *   structural logic of power allocation by omission.
 *
 * KEY AGENTS:
 *   - Holyrood (Scottish Parliament): Primary beneficiary (institutional/arbitrage) — gains competence through the inverted default; can claim new powers by noting their absence from the reserved list
 *   - Westminster (UK Parliament): Primary victim (institutional/constrained) — loses residual claim over unlisted powers by the statute's logic; must defend its competence boundaries through litigation
 *   - Centralist Constitutional Doctrine: Secondary victim (organized/constrained) — the theory that Parliament is sovereign and regions are subordinate is structurally undermined by the inversion
 *   - Courts (UK Supreme Court, Scottish Court of Session): Enforcement mechanism (institutional/constrained) — must interpret the boundaries of reserved vs devolved; continuously clarify what the silence means
 *   - Sewel Convention: Performative layer (institutional/constrained) — gestures respect for the boundary but offers no structural barrier to Westminster overriding it
 *   - Analytical Observer: Civilizational reading (analytical/analytical) — sees the reserved powers model as the kernel commitment that other disputes (independence, Sewel strain) presuppose
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(devolution_settlements__reserved_powers_model_reading, 0.38).
domain_priors:suppression_score(devolution_settlements__reserved_powers_model_reading, 0.52).
domain_priors:theater_ratio(devolution_settlements__reserved_powers_model_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(devolution_settlements__reserved_powers_model_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(devolution_settlements__reserved_powers_model_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(devolution_settlements__reserved_powers_model_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(devolution_settlements__reserved_powers_model_reading, tangled_rope).
narrative_ontology:human_readable(devolution_settlements__reserved_powers_model_reading, "Scotland's Reserved Powers Model — Expansive Home Rule by Statutory Silence").
narrative_ontology:topic_domain(devolution_settlements__reserved_powers_model_reading, "constitutional_law/devolution").

domain_priors:requires_active_enforcement(devolution_settlements__reserved_powers_model_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(devolution_settlements__reserved_powers_model_reading, 'ac551505-e35d-4cca-88c4-0e956cf16560').
narrative_ontology:cs_kernel_codification('ac551505-e35d-4cca-88c4-0e956cf16560', formalized).
narrative_ontology:cs_authority_grounding('ac551505-e35d-4cca-88c4-0e956cf16560', extraction).
narrative_ontology:cs_interpretation_layer_present('ac551505-e35d-4cca-88c4-0e956cf16560').
narrative_ontology:cs_reading_relation('ac551505-e35d-4cca-88c4-0e956cf16560', devolution_settlements__independence_pathway_reading, influences).
narrative_ontology:cs_reading_relation('ac551505-e35d-4cca-88c4-0e956cf16560', devolution_settlements__sewel_strain_reading, coexists_with).
narrative_ontology:cs_axiom('ac551505-e35d-4cca-88c4-0e956cf16560', foundational, silence_grants_competence).
narrative_ontology:cs_axiom_status(silence_grants_competence, holdable).
narrative_ontology:cs_axiom_grounding('ac551505-e35d-4cca-88c4-0e956cf16560', silence_grants_competence, conventional).
narrative_ontology:cs_axiom('ac551505-e35d-4cca-88c4-0e956cf16560', secondary, residual_claim_surrender_by_default).
narrative_ontology:cs_axiom_status(residual_claim_surrender_by_default, holdable).
narrative_ontology:cs_axiom_grounding('ac551505-e35d-4cca-88c4-0e956cf16560', residual_claim_surrender_by_default, conventional).
narrative_ontology:cs_reference_frame('ac551505-e35d-4cca-88c4-0e956cf16560', parliamentary_delegative_supremacy_inverted).
narrative_ontology:cs_drift_state('ac551505-e35d-4cca-88c4-0e956cf16560', contemporary_brexit_and_internal_market_disputes, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ac551505-e35d-4cca-88c4-0e956cf16560', '').
narrative_ontology:cs_kernel_id(devolution_settlements__reserved_powers_model_reading, devolution_settlements).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(devolution_settlements__reserved_powers_model_reading, holyrood_competence).
narrative_ontology:constraint_beneficiary(devolution_settlements__reserved_powers_model_reading, scottish_legislative_autonomy).
narrative_ontology:constraint_victim(devolution_settlements__reserved_powers_model_reading, westminster_residual_claims).
narrative_ontology:constraint_victim(devolution_settlements__reserved_powers_model_reading, centralist_constitutional_defaults).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WESTMINSTER RESIDUAL CLAIM (SNARE) — Cannot exit or reclaim powers through statute's silence; bears full cost of the drafting choice that inverted the default. The residual claim is trapped in the unlisted — every unlisted power is lost to Westminster by the settlement's logic. No suppression escape because the mechanism is textual: what is not reserved, is devolved. Westminster experiences maximal extraction: it loses sovereignty over undefined powers without active suppression, purely by drafting convention.
constraint_indexing:constraint_classification(devolution_settlements__reserved_powers_model_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: HOLYROOD'S COMPETENCE (ROPE) — Benefits from the inverted default; experiences the constraint as pure coordination function. The reserved powers list is a communication mechanism: it establishes what Edinburgh does NOT control, and by implication, what it does. Holyrood can exercise arbitrage (appeal to the reserved list, claim new competences by noting their absence from the list, trigger constitutional expansion through reinterpretation). Net beneficiary — extraction flows toward this agent.
constraint_indexing:constraint_classification(devolution_settlements__reserved_powers_model_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: CENTRALIST CONSTITUTIONAL DEFAULTS (TANGLED ROPE) — The constitutional doctrine that Parliament is sovereign and regional actors hold delegated powers faces extraction from the statute's inverted logic. Centralism coordinates Westminster's institutional identity and theoretical authority; it also extracts value by making regional actors subordinate. But the reserved powers model partially forecloses centralism: it makes Holyrood not subordinate but competent-by-default. Centralism is both coordinator (holds the theory together) and victim (the theory is undermined by the structural inversion). Constrained exit because changing the default requires amending the statute — legally possible but politically difficult.
constraint_indexing:constraint_classification(devolution_settlements__reserved_powers_model_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: SCOTTISH EXECUTIVE BRANCH (TANGLED ROPE) — Both benefits and is constrained. Benefits from the expansive competence the reserved list implies. Constrained because the executive cannot act outside its competence without triggering constitutional dispute; courts must interpret the boundaries continuously. Requires active enforcement: disputes arise at the margin (Scottish judges must clarify which powers are reserved, which are devolved). The executive experiences coordination (it has real powers) alongside extraction (it must continuously defend its competence against Westminster's attempted reclamation).
constraint_indexing:constraint_classification(devolution_settlements__reserved_powers_model_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSTITUTIONAL AMBIGUITY BUFFER (SCAFFOLD) — The silence in the statute — what is NOT on the reserved list — functions as a temporary scaffold that absorbs constitutional pressure between Westminster and Holyrood. The scaffold has implicit sunset: once the boundaries are litigated and clarified, the ambiguity buffer collapses. Courts (Supreme Court, EU Court before Brexit) have been progressively narrowing the silence through interpretation. Each ruling makes the structure less ambiguous, reducing the scaffold's function. As the boundaries crystallize, the scaffold's role diminishes — it will disappear when the reserved/devolved boundary is fully juridified.
constraint_indexing:constraint_classification(devolution_settlements__reserved_powers_model_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: SEWEL CONVENTION (PITON) — The Sewel Convention ('Westminster will not normally legislate on devolved matters') is a performative layer built on top of the reserved powers model. It is degraded machinery: 'not normally' was revealed as 'not until it matters' during Brexit (Sewel was withheld but overridden). The convention persists through theatrical consent and respect ritual, not through structural force. Theater_ratio ≈ 0.80: the convention is mostly a signal that Westminster respects the settlement, not an actual barrier to Westminster legislating on devolved matters.
constraint_indexing:constraint_classification(devolution_settlements__reserved_powers_model_reading, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / RESERVED POWERS READING (TANGLED ROPE) — This reading interprets the Scotland Act 1998 as instantiating a deliberate inversion: the default rule is competence, not subordination. The constraint is the legal mechanism that performs this inversion — by listing what is reserved, the statute grants everything else to Edinburgh. This is both coordination (establishes clear boundaries via the reserved list) and extraction (Westminster's residual claims are silently extinguished by the drafting choice). Analytically: ε ≈ 0.38 because the extraction is built into the statute itself, not actively enforced, but suppression is present (Westminster cannot easily reclaim powers without amending the statute).
constraint_indexing:constraint_classification(devolution_settlements__reserved_powers_model_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(devolution_settlements__reserved_powers_model_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(devolution_settlements__reserved_powers_model_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(devolution_settlements__reserved_powers_model_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(devolution_settlements__reserved_powers_model_reading, TR),
    TR >= 0.70.

:- end_tests(devolution_settlements__reserved_powers_model_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The extraction is structural and built into the statute — Westminster loses powers without active enforcement of suppression, purely by the logic of the inverted default. But the extraction is not maximal because it operates through interpretation: courts and legal doctrine mediate the claim. The initial value (t=0, post-1998) was lower (0.18) because the boundaries were unclear; as litigation clarified what was reserved, Westminster's losses became real, raising extractiveness to 0.28. Post-Brexit stress (t=10), with Sewel overridden and disputes over internal market competence, extractiveness rose to 0.38. Suppression (0.52): Moderate-high. Westminster is suppressed from reclaiming unlisted powers by the statutory text itself — it would require amending the Scotland Act, a high-threshold change. But suppression is not absolute: courts can reinterpret the boundaries, and Westminster can exercise power without amending if it claims the power is actually reserved. Theater ratio (0.35, rising to 0.35 by t=10): Low-moderate. The reserved powers mechanism is primarily functional (it actually allocates power) rather than performative. The modest theater reflects the Sewel Convention layer — the convention is mostly performative, but the reserved list itself has real bite. The rising theater trajectory reflects increasing reliance on Sewel as a legitimacy ritual after Sewel was first withheld (Brexit), suggesting Westminster is compensating for loss of structural claim with increased rhetorical respect.
 *
 * PERSPECTIVAL GAP:
 *   The reserved powers reading produces large perspectival gaps. Holyrood sees pure coordination (Rope): the statute clarifies what it controls. Westminster sees extraction (Snare): it loses powers without defense. Centralism sees both coordination and extraction (Tangled Rope): the doctrine is partially undermined. The executive sees coordination plus constraint (Tangled Rope): it has real powers but must defend them. The Sewel Convention is degraded machinery (Piton): it is mostly theatrical respect, not structural force. The analytical observer sees the constraint as tangled coordination-extraction (Tangled Rope) where the inversion is the mechanism. The gaps reveal that the same constitutional structure is experienced as benefit, loss, undermining, empowerment, theater, and structural ambiguity depending on position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies by agent. Holyrood (institutional/arbitrage) experiences low d ≈ 0.15 because it is a beneficiary with exit capacity — it can claim new competences by reinterpreting the silence. Westminster (institutional/constrained) experiences high d ≈ 0.70 because it is a victim without easy exit — it cannot reclaim powers without legislative action. Centralist doctrine (organized/constrained) experiences moderate-high d ≈ 0.65 because it is structurally undermined by the inversion but can still advance its interpretation of the reserved list. Courts (institutional/constrained) experience moderate d ≈ 0.50 because they are enforcement mechanisms: they benefit from clarity (low d) but are burdened by ambiguity (high d), depending on the case. The analytical perspective derives d from the reading's own structural commitment: treating silence as grant rather than retention places the observer in a position where Westminster appears as victim and Holyrood as beneficiary, so d ≈ 0.72 (analytical observer of an extraction mechanism).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    silence_as_grant_vs_retention,
    'Does statutory silence on a power constitute a grant of that power to the devolved legislature, or a retained power of Westminster?',
    'Jurisprudential analysis of common-law constitutional interpretation; comparison with federal systems (US, Canada, Australia) and their default rules; examination of legislative intent in the Scotland Act 1998',
    'If silence = grant (this reading''s premise): Holyrood''s competence expands as Westminster discovers unlisted powers. If silence = retained: Westminster retains residual sovereignty. This is the core interpretive dispute.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(silence_as_grant_vs_retention, conceptual, 'Whether statutory silence grants devolved power or retains Westminster power').

omega_variable(
    legislative_intent_clarity,
    'Was the Scotland Act 1998 deliberately drafted with inverted defaults (everything not reserved is devolved) or was this a doctrinal accident of English constitutional tradition?',
    'Parliamentary record of the devolution settlement (debates, white papers, Lords testimony); comparative analysis of earlier devolution schemes (Northern Ireland, Wales pre-1998) and their framings; interviews with the draftspeople',
    'If deliberate: reading confirms the settlement as expansive home rule by design. If accidental: reading misinterprets statutory silence as intentional silence. This affects whether the constraint is enforced doctrinally or challenged as unintended.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legislative_intent_clarity, empirical, 'Legislative intent regarding inverted default rule').

omega_variable(
    reserved_list_completeness,
    'Is the reserved powers list in the Scotland Act sufficiently detailed that new powers can be reliably identified as ''unlisted and therefore devolved,'' or does the list remain open to reinterpretation?',
    'Doctrinal analysis of Schedule 5 (reserved matters) and case law on powers claimed as newly devolved; audit of disputes over competence boundaries; assessment of whether courts can reliably determine ''reserved'' status',
    'If complete: silence as grant remains structurally clear. If ambiguous: silence becomes fog — both Westminster and Holyrood can claim undefined powers are within their competence, increasing disputes and weakening the constraint''s enforceability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reserved_list_completeness, empirical, 'Completeness and clarity of the reserved powers list').

omega_variable(
    interreading_foreclosure_test,
    'Does this reserved-powers reading logically foreclose the independence-pathway reading, or can both coexist within the same constitutional framework?',
    'Logical analysis: does expanded Holyrood competence (this reading) make independence more or less structurally plausible? Does the settled fact of expansive devolution foreclose the demand for independence, or does it enable it?',
    'If forecloses: independence-pathway reading is logically incompatible with this one (you cannot use the settlement to argue for independence if the settlement is stable). If coexists: both readings are live positions (the settlement is expansive yet insufficient).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(interreading_foreclosure_test, conceptual, 'Whether this reading forecloses the independence-pathway reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(devolution_settlements__reserved_powers_model_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_t0_initial, devolution_settlements__reserved_powers_model_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(theater_t5_mid, devolution_settlements__reserved_powers_model_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement(theater_t10_current, devolution_settlements__reserved_powers_model_reading, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(extrac_t0_initial_settlement, devolution_settlements__reserved_powers_model_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(extrac_t5_devolution_tensions, devolution_settlements__reserved_powers_model_reading, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(extrac_t10_post_brexit, devolution_settlements__reserved_powers_model_reading, base_extractiveness, 10, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(suppress_t0_initial, devolution_settlements__reserved_powers_model_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(suppress_t5_mid, devolution_settlements__reserved_powers_model_reading, suppression_requirement, 5, 0.45).
narrative_ontology:measurement(suppress_t10_current, devolution_settlements__reserved_powers_model_reading, suppression_requirement, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(devolution_settlements__reserved_powers_model_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(devolution_settlements__reserved_powers_model_reading, devolution_settlements__independence_pathway_reading).
narrative_ontology:affects_constraint(devolution_settlements__reserved_powers_model_reading, devolution_settlements__sewel_strain_reading).
narrative_ontology:affects_constraint(devolution_settlements__reserved_powers_model_reading, scottish_competence_boundaries).
narrative_ontology:affects_constraint(devolution_settlements__reserved_powers_model_reading, brexit_internal_market_disputes).

% DUAL FORMULATION NOTE:
% The reserved powers model is upstream of both the independence pathway and Sewel strain readings. Each sibling reading presupposes the structural logic of the reserved powers model: independence arguments use the model to claim Holyrood has expansive competence; Sewel strain observations rely on the model's boundary disputes. The three stories are decomposed by reading, not by ε-variance — each reading has approximately the same extractiveness (0.35–0.42) but different structural interpretations of what the constraint means.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(devolution_settlements__reserved_powers_model_reading, institutional, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
