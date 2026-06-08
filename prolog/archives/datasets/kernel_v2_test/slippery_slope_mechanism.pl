% ============================================================================
% CONSTRAINT STORY: slippery_slope_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_slippery_slope_mechanism, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: slippery_slope_mechanism
 *   human_readable: Slippery Slope Mechanism in Germline Gene Modification
 *   domain: bioethics/reproductive_medicine/genetic_engineering
 *
 * SUMMARY:
 *   The slippery slope mechanism in germline gene modification policy
 *   represents a constraint that has degraded from functional coordination
 *   into institutional theater over its 40+ year lifespan. Initially invoked
 *   in the 1970s-1980s during early IVF debates as a legitimate precautionary
 *   principle for managing uncertainty about novel reproductive technologies,
 *   the slope argument claimed that accepting treatment applications would
 *   create institutional and psychological momentum toward enhancement
 *   applications. However, empirical precedents have not validated this
 *   mechanism: IVF (1978), preimplantation genetic diagnosis (1990), and
 *   somatic gene therapy (2000s) were all accepted for treatment purposes
 *   without leading to the predicted enhancement cascade. Despite this
 *   empirical record, slope arguments persist in regulatory discourse,
 *   maintained through institutional inertia and professional gatekeeping
 *   rather than functional coordination. The constraint's theater ratio has
 *   risen from 0.35 (1978) to 0.68 (2018) as the gap between rhetorical
 *   invocation and empirical support has widened. The constraint exhibits
 *   piton characteristics: an atrophied coordination function maintained as
 *   performance, with identifiable beneficiaries (bioethics gatekeepers,
 *   regulatory bodies) who extract rents from their position as arbiters of
 *   the slope mechanism.
 *
 * KEY AGENTS:
 *   - Bioethics Gatekeepers: Primary beneficiary (institutional/arbitrage) — professional bioethicists and ethics committee members who benefit from slope argument persistence through consulting demand, committee positions, and publication opportunities
 *   - Regulatory Bodies: Primary beneficiary (institutional/arbitrage) — agencies that use slope rhetoric to defer difficult decisions while appearing prudent; benefit from the constraint as a coordination tool
 *   - Conservative Advocacy Groups: Secondary beneficiary (organized/mobile) — groups opposed to genetic modification who use slope arguments to justify prohibition; benefit from the constraint's persistence
 *   - Research Continuity: Primary victim (powerless/trapped) — the research community trapped by funding dependencies and regulatory approval requirements; experiences the constraint as performative gatekeeping
 *   - Families with Genetic Disease: Primary victim (powerless/trapped) — bear the cost of treatment delays justified by speculative future harms; cannot exit their genetic condition or bypass regulatory theater
 *   - Patient Advocacy Groups: Mixed position (moderate/constrained) — experience both coordination (forces careful boundary articulation) and extraction (delays in treatment approval)
 *   - Scientific Consensus Coalition: Organized agents (organized/mobile) — National Academies, WHO, professional societies building evidence-based alternative frameworks; see the constraint as transitional
 *   - Policy Clarity: Secondary victim (powerless/trapped) — the abstract collective good of clear regulatory boundaries; degraded by slope mechanism's vagueness and empirical unsupportability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(slippery_slope_mechanism, 0.28).
domain_priors:suppression_score(slippery_slope_mechanism, 0.35).
domain_priors:theater_ratio(slippery_slope_mechanism, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(slippery_slope_mechanism, extractiveness, 0.28).
narrative_ontology:constraint_metric(slippery_slope_mechanism, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(slippery_slope_mechanism, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(slippery_slope_mechanism, piton).
narrative_ontology:human_readable(slippery_slope_mechanism, "Slippery Slope Mechanism in Germline Gene Modification").
narrative_ontology:topic_domain(slippery_slope_mechanism, "bioethics/reproductive_medicine/genetic_engineering").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(slippery_slope_mechanism, bioethics_gatekeepers).
narrative_ontology:constraint_beneficiary(slippery_slope_mechanism, regulatory_bodies).
narrative_ontology:constraint_beneficiary(slippery_slope_mechanism, conservative_advocacy_groups).
narrative_ontology:constraint_victim(slippery_slope_mechanism, research_continuity).
narrative_ontology:constraint_victim(slippery_slope_mechanism, policy_clarity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RESEARCH CONTINUITY (PITON) — The constraint persists as institutional theater: slippery slope arguments are invoked reflexively in every regulatory debate regardless of empirical precedent. The research community cannot exit the constraint (trapped by funding dependencies and regulatory approval requirements) but experiences it primarily as performative gatekeeping rather than substantive coordination. The slope mechanism is maintained theatrically — committees cite it, ethicists invoke it, but the actual causal claim (that treatment acceptance causes enhancement momentum) remains empirically unverified across 40+ years of IVF, PGD, and somatic gene therapy precedents.
constraint_indexing:constraint_classification(slippery_slope_mechanism, piton,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REGULATORY BODIES (ROPE) — Experience the constraint as coordination: the slippery slope framing provides a shared vocabulary for managing public concern and justifying precautionary delays. Regulatory agencies benefit from the constraint by using slope rhetoric to defer difficult decisions while appearing prudent. They have arbitrage-level exit (can reframe the debate, adopt alternative regulatory logics) and experience the constraint as a useful coordination tool rather than extraction.
constraint_indexing:constraint_classification(slippery_slope_mechanism, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: BIOETHICS GATEKEEPERS (ROPE) — Professional bioethicists and ethics committee members benefit from the constraint's persistence: slippery slope arguments generate consulting demand, committee positions, and publication opportunities. They experience the constraint as coordination (managing societal anxiety about genetic technology) with minimal extraction. Arbitrage exit options include reframing toward alternative ethical frameworks (autonomy, justice, harm principle) when slope arguments lose credibility.
constraint_indexing:constraint_classification(slippery_slope_mechanism, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PATIENT ADVOCACY GROUPS (TANGLED ROPE) — Groups advocating for genetic disease treatment experience both coordination (the slope mechanism forces careful boundary articulation, which can strengthen treatment justifications) and extraction (delays in treatment approval, resource diversion to endless ethical debate). Constrained exit: can lobby and organize but cannot bypass the regulatory theater. The constraint coordinates public deliberation while extracting time and resources from families with genetic disease.
constraint_indexing:constraint_classification(slippery_slope_mechanism, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: SCIENTIFIC CONSENSUS COALITION (SCAFFOLD) — Organized scientific bodies (National Academies, WHO, professional societies) see the slope mechanism as a temporary coordination problem with an empirical sunset: as longitudinal data accumulates from IVF, PGD, and somatic gene therapy showing that treatment applications do NOT automatically lead to enhancement applications, the slope argument loses empirical support. The coalition has mobile exit options (can shift to evidence-based frameworks) and sees the constraint as transitional — maintained until sufficient precedent data forces regulatory logic to update.
constraint_indexing:constraint_classification(slippery_slope_mechanism, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: FAMILIES WITH GENETIC DISEASE (SNARE) — Experience pure extraction: the slope mechanism delays treatment access with no corresponding benefit to them. They are trapped (cannot access treatment outside regulatory approval, cannot exit their genetic condition) and bear the full cost of precautionary delays justified by speculative future harms. The coordination story (protecting society from enhancement) is cover for extraction from a powerless group with urgent medical need.
constraint_indexing:constraint_classification(slippery_slope_mechanism, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (PITON) — From a civilizational perspective, the slippery slope mechanism is a degraded coordination tool: it once served a legitimate function (managing uncertainty about novel reproductive technologies in the 1970s-1980s) but has atrophied into institutional theater. The empirical claim (treatment → enhancement momentum) has been tested across IVF, PGD, preimplantation genetic diagnosis, and somatic gene therapy — none of which led to the predicted enhancement cascade. Yet the slope argument persists in regulatory discourse, maintained through institutional inertia and professional gatekeeping rather than functional coordination. The constraint is a piton: what remains is mostly performance.
constraint_indexing:constraint_classification(slippery_slope_mechanism, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(slippery_slope_mechanism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(slippery_slope_mechanism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(slippery_slope_mechanism, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(slippery_slope_mechanism, TR),
    TR >= 0.70.

:- end_tests(slippery_slope_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-moderate. The constraint extracts primarily from research continuity and families with genetic disease through treatment delays and resource diversion to endless ethical debate. However, extraction is not severe — some research proceeds, some treatments are eventually approved, and the delays are measured in years rather than decades. The extraction is real but limited. Suppression (0.35): Low-moderate. The constraint suppresses alternative regulatory frameworks (consent-based, harm-based, justice-based) and creates barriers to treatment access, but suppression is not total — jurisdictions like the UK and Belgium have adopted more permissive frameworks, and scientific consensus is building toward evidence-based alternatives. Theater ratio (0.68): High. The constraint is substantially performative: slope arguments are invoked reflexively in regulatory debates regardless of empirical precedent. The actual causal claim (treatment acceptance → enhancement momentum) remains empirically unverified across 40+ years of IVF, PGD, and somatic gene therapy precedents. The theater ratio has risen steadily as the gap between rhetorical invocation and empirical support has widened. The constraint persists through institutional inertia and professional gatekeeping rather than functional coordination.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates how a coordination mechanism can degrade into theater while maintaining different experiential realities for different agents. Bioethics gatekeepers and regulatory bodies see coordination (Rope) — the slope mechanism provides a shared vocabulary for managing public concern and justifying precautionary delays. The scientific consensus coalition sees a temporary problem with an empirical sunset (Scaffold) — as longitudinal data accumulates showing that treatment applications do NOT lead to enhancement applications, the slope argument loses support. Patient advocacy groups see mixed coordination and extraction (Tangled Rope) — the constraint both enables careful boundary articulation and delays treatment access. Research continuity and the analytical observer see degraded theater (Piton) — the constraint persists through institutional inertia despite empirical unsupportability. Families with genetic disease see pure extraction (Snare) — the slope mechanism delays treatment access with no corresponding benefit. The perspectival gap reveals that the constraint's function has atrophied while its performance persists, maintained by beneficiaries who extract rents from their gatekeeping position.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (bioethics gatekeepers, regulatory bodies, conservative advocacy groups) experience low directionality values — they benefit from the constraint's persistence through consulting demand, regulatory authority, and rhetorical ammunition. Regulatory bodies and bioethics gatekeepers have arbitrage-level exit options (can reframe debates, adopt alternative frameworks) and experience the constraint as coordination rather than extraction. Victims (research continuity, families with genetic disease, policy clarity) experience high directionality values — they bear the costs of treatment delays and regulatory vagueness with no corresponding benefit. Research continuity and families with genetic disease are trapped (cannot bypass regulatory approval, cannot exit their genetic conditions) and experience maximum extraction. Patient advocacy groups occupy a middle position (moderate power, constrained exit) — they experience both coordination (the slope mechanism forces careful boundary articulation) and extraction (delays and resource diversion). The scientific consensus coalition has mobile exit options and sees the constraint as transitional — they are building evidence-based alternatives that will eventually replace slope logic.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint approaches mandatrophy: its original mandate (managing uncertainty about novel reproductive technologies) has been empirically tested and found unsupported across 40+ years of precedents, yet the constraint persists through institutional inertia and professional gatekeeping. The theater ratio trajectory (rising from 0.35 to 0.68) indicates that the gap between rhetorical invocation and functional coordination is widening. The constraint is maintained as performance by beneficiaries (bioethics gatekeepers, regulatory bodies) who extract rents from their position as arbiters, while victims (families with genetic disease, research continuity) bear the costs of delays justified by speculative harms that have not materialized in any prior case. The constraint's persistence despite empirical unsupportability is the signature of mandatrophy: the mandate has outlived its function, but the institutional structure remains.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_slope_existence,
    'Does accepting germline modification for disease treatment actually create institutional or psychological momentum toward enhancement applications, or is the slope mechanism empirically unsupported?',
    'Longitudinal analysis of regulatory trajectories in jurisdictions that permitted treatment applications (UK, Belgium) vs those that prohibited all germline modification. Comparison with historical precedents: IVF (1978), PGD (1990), somatic gene therapy (2000s) — did any of these treatment technologies lead to enhancement applications as slope arguments predicted?',
    'If slope is empirically real: the constraint is legitimate coordination (Scaffold or Rope from more perspectives). If slope is empirically unsupported: the constraint is extraction or theater (Piton or Snare from more perspectives). Current evidence after 40+ years of reproductive technology precedents suggests the slope mechanism is not empirically validated — treatment applications have NOT led to enhancement applications in any prior case.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(empirical_slope_existence, empirical, 'Whether the slippery slope mechanism is empirically real or rhetorical').

omega_variable(
    alternative_boundary_mechanisms,
    'Are there alternative regulatory mechanisms (consent frameworks, harm-based criteria, justice principles) that could manage treatment/enhancement boundaries without invoking slope logic?',
    'Comparative analysis of regulatory frameworks: jurisdictions using slope arguments vs those using alternative ethical frameworks. Assessment of boundary maintenance effectiveness and false positive/negative rates.',
    'If alternatives exist and work: slope mechanism is unnecessary theater (Piton confirmed). If alternatives fail: slope mechanism may be the least-bad coordination tool despite its theatrical elements (Rope or Scaffold from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_boundary_mechanisms, conceptual, 'Whether alternative regulatory frameworks can replace slope logic').

omega_variable(
    gatekeeping_vs_coordination,
    'Is the constraint''s primary function coordination (managing genuine public concern about genetic technology) or gatekeeping (professional bioethicists and regulatory bodies extracting rents from their position as arbiters)?',
    'Analysis of who benefits from slope argument persistence: do regulatory delays correlate with bioethics consulting demand, committee appointments, and publication opportunities? Do slope arguments appear more frequently when regulatory bodies face budget pressure or legitimacy challenges?',
    'If primarily coordination: Rope or Scaffold from more perspectives. If primarily gatekeeping: Piton or Snare from more perspectives. The beneficiary structure (bioethics gatekeepers, regulatory bodies) suggests significant gatekeeping function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gatekeeping_vs_coordination, empirical, 'Whether the constraint coordinates or extracts rents through gatekeeping').

omega_variable(
    theater_ratio_trajectory,
    'Is the theater ratio increasing (slope arguments becoming more performative as empirical evidence accumulates against them) or stable (slope arguments maintain consistent relationship to actual regulatory function)?',
    'Temporal analysis of slope argument invocation frequency vs empirical precedent accumulation. If theater ratio rises as counter-evidence accumulates, the constraint is degrading into pure performance.',
    'Rising theater ratio confirms Piton classification and suggests the constraint is approaching mandatrophy (function has atrophied but performance persists). Stable theater ratio suggests the constraint retains some coordination function despite theatrical elements.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theater_ratio_trajectory, empirical, 'Whether the constraint''s theater ratio is increasing over time').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(slippery_slope_mechanism, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(slope_theater_1978, slippery_slope_mechanism, theater_ratio, 0, 0.35).
narrative_ontology:measurement(slope_theater_1988, slippery_slope_mechanism, theater_ratio, 10, 0.42).
narrative_ontology:measurement(slope_theater_1998, slippery_slope_mechanism, theater_ratio, 20, 0.51).
narrative_ontology:measurement(slope_theater_2008, slippery_slope_mechanism, theater_ratio, 30, 0.6).
narrative_ontology:measurement(slope_theater_2018, slippery_slope_mechanism, theater_ratio, 40, 0.68).

% Extraction over time
narrative_ontology:measurement(slope_extract_1978, slippery_slope_mechanism, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(slope_extract_1988, slippery_slope_mechanism, base_extractiveness, 10, 0.18).
narrative_ontology:measurement(slope_extract_1998, slippery_slope_mechanism, base_extractiveness, 20, 0.22).
narrative_ontology:measurement(slope_extract_2008, slippery_slope_mechanism, base_extractiveness, 30, 0.25).
narrative_ontology:measurement(slope_extract_2018, slippery_slope_mechanism, base_extractiveness, 40, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(slippery_slope_mechanism, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% The slippery slope mechanism is downstream of the treatment_enhancement_boundary constraint. The boundary constraint has its own extractiveness reflecting the conceptual difficulty of distinguishing treatment from enhancement; the slope mechanism has its own extractiveness reflecting the institutional theater and gatekeeping that persists despite empirical unsupportability. The slope mechanism is the enforcement layer that claims to protect the boundary, but the enforcement has degraded into performance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
