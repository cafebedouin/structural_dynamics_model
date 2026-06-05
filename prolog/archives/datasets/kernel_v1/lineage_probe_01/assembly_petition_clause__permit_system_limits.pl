% ============================================================================
% CONSTRAINT STORY: assembly_petition_clause__permit_system_limits
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_assembly_petition_clause__permit_system_limits, []).

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
 *   constraint_id: assembly_petition_clause__permit_system_limits
 *   human_readable: Permit System Limits on Assembly: Content-Neutral Standards Constraint
 *   domain: legal/constitutional/first_amendment
 *
 * SUMMARY:
 *   The permit system for public assembly sits at the intersection of two
 *   constitutional interests: the state's legitimate need to coordinate use
 *   of shared public space (traffic, safety, resource allocation), and the
 *   disfavored speaker's fundamental right to petition and assemble without
 *   state gatekeeping. This constraint instantiates ONE READING of the
 *   contested assembly-petition kernel: the reading that requires permit
 *   systems be governed by definite, content-neutral, administratively
 *   objective standards, with no official discretion to deny based on
 *   anticipated hostile reaction and no fee scaling based on opposition
 *   intensity. This reading directly addresses the extraction mechanism —
 *   official discretion to deny unpopular marches or price them prohibitively
 *   — and aims to suppress it through formal doctrine. The constraint
 *   exhibits Tangled Rope structure: genuine coordination function (police
 *   resource planning, traffic management, public safety) coexists with
 *   systematic extraction of assembly rights from disfavored speakers. The
 *   theater ratio (0.55) reflects that formal compliance with
 *   'content-neutral standards' coexists with practical circumvention through
 *   safety-based denials, permit delays, and fee structures nominally
 *   content-neutral but effectively scaled to suppress. The suppression
 *   trajectory (0.62 → 0.68 over the interval) shows rising de facto
 *   gatekeeping despite formal doctrine requiring its elimination.
 *   Extractiveness (0.58) reflects the moderate but measurable suppression of
 *   assembly for unpopular speech through administrative mechanisms.
 *
 * KEY AGENTS:
 *   - Unpopular Marchers / Disfavored Speakers (powerless/trapped): Primary victims of the constraint — face denial risk, delay, and fee-scaling based on content
 *   - Permitting Officials / Police Department (institutional/arbitrage): Primary beneficiaries of coordination function — gain predictable route management and resource allocation
 *   - Marginalized Advocacy Groups (moderate/constrained): Secondary victims — require permits, face resource burden, limited exit capacity
 *   - Establishment Advocacy Groups (powerful/mobile): Secondary beneficiaries — experience coordination function with minimal extraction barrier
 *   - Constitutional Doctrine of Content Neutrality (institutional/constrained): Formal enforcer of the definite-standards requirement; effectiveness degraded by circumvention
 *   - Analytical Observer at Civilizational Scale (analytical/analytical): Risks naturalizing the permit requirement as inevitable coordination infrastructure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(assembly_petition_clause__permit_system_limits, 0.58).
domain_priors:suppression_score(assembly_petition_clause__permit_system_limits, 0.68).
domain_priors:theater_ratio(assembly_petition_clause__permit_system_limits, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(assembly_petition_clause__permit_system_limits, extractiveness, 0.58).
narrative_ontology:constraint_metric(assembly_petition_clause__permit_system_limits, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(assembly_petition_clause__permit_system_limits, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(assembly_petition_clause__permit_system_limits, tangled_rope).
narrative_ontology:human_readable(assembly_petition_clause__permit_system_limits, "Permit System Limits on Assembly: Content-Neutral Standards Constraint").
narrative_ontology:topic_domain(assembly_petition_clause__permit_system_limits, "legal/constitutional/first_amendment").

domain_priors:requires_active_enforcement(assembly_petition_clause__permit_system_limits).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(assembly_petition_clause__permit_system_limits, '3ad26518-807b-4476-b530-8a960283affd').
narrative_ontology:cs_kernel_codification('3ad26518-807b-4476-b530-8a960283affd', formalized).
narrative_ontology:cs_authority_grounding('3ad26518-807b-4476-b530-8a960283affd', extraction).
narrative_ontology:cs_interpretation_layer_present('3ad26518-807b-4476-b530-8a960283affd').
narrative_ontology:cs_reading_relation('3ad26518-807b-4476-b530-8a960283affd', assembly_petition_clause__expressive_association_doctrine, coexists_with).
narrative_ontology:cs_reading_relation('3ad26518-807b-4476-b530-8a960283affd', assembly_petition_clause__petition_clause_independence, coexists_with).
narrative_ontology:cs_axiom('3ad26518-807b-4476-b530-8a960283affd', foundational, official_discretion_forbidden_assembly_permit).
narrative_ontology:cs_axiom_status(official_discretion_forbidden_assembly_permit, holdable).
narrative_ontology:cs_axiom_grounding('3ad26518-807b-4476-b530-8a960283affd', official_discretion_forbidden_assembly_permit, deontological).
narrative_ontology:cs_axiom('3ad26518-807b-4476-b530-8a960283affd', foundational, hostile_reaction_fee_prohibition).
narrative_ontology:cs_axiom_status(hostile_reaction_fee_prohibition, holdable).
narrative_ontology:cs_axiom_grounding('3ad26518-807b-4476-b530-8a960283affd', hostile_reaction_fee_prohibition, deontological).
narrative_ontology:cs_reference_frame('3ad26518-807b-4476-b530-8a960283affd', content_neutral_objective_standards).
narrative_ontology:cs_drift_state('3ad26518-807b-4476-b530-8a960283affd', contemporary_circumvention_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3ad26518-807b-4476-b530-8a960283affd', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(assembly_petition_clause__permit_system_limits, assembly_petition_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(assembly_petition_clause__permit_system_limits, unpopular_marchers).
narrative_ontology:constraint_beneficiary(assembly_petition_clause__permit_system_limits, disfavored_speakers).
narrative_ontology:constraint_victim(assembly_petition_clause__permit_system_limits, street_public_forum_access).
narrative_ontology:constraint_victim(assembly_petition_clause__permit_system_limits, marginalized_advocacy_groups).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISFAVORED MARCHER (SNARE) — Trapped in the permit system. Without permit, assembly is illegal and subject to arrest. With permit, the official gate is discretionary — licensing officer can deny based on content (hostile reaction), delay until the moment passes, or scale fees to suppress unpopular speech. No exit: the marcher cannot assemble without crossing the street into state control. Maximum extraction from this perspective — the constraint exists precisely to gatekeep disfavored speakers.
constraint_indexing:constraint_classification(assembly_petition_clause__permit_system_limits, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: ADVOCACY ORGANIZATION (TANGLED ROPE) — Constrained by resource requirements for permits, legal compliance, timing uncertainty, and public-safety justification burden. But also benefits from the permit system through coordination: predictability of event logistics, liability clarity, police cooperation on safety planning. Asymmetric extraction — the organization must pay legal and administrative costs, but gains some legitimacy and coordination function. Not trapped, but constrained by regulatory overhead and discretionary denial risk.
constraint_indexing:constraint_classification(assembly_petition_clause__permit_system_limits, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: POLICE DEPARTMENT / PERMITTING AUTHORITY (ROPE) — Experiences the constraint as coordination: the permit system enables police to plan routes, allocate resources, and manage traffic flow. The definite-standards doctrine (content-neutral, no discretion, no hostile-reaction fees) actually reduces extraction burden — it removes the temptation to deny permits based on anticipated opposition and replaces it with rule-governed procedure. From this institutional view, the constraint is pure coordination with minimal coercion overhead. Net beneficiary of the coordination mechanism.
constraint_indexing:constraint_classification(assembly_petition_clause__permit_system_limits, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ESTABLISHMENT ADVOCACY GROUP (TANGLED ROPE) — Popular speech, mainstream message. The permit system is primarily a coordination tool for them — they get permits routinely, fee scales pose no burden (fees are modest for mainstream events), and officials lack discretion to deny them. They experience genuine coordination benefit (predictable logistics, police cooperation). But they also benefit from the extraction mechanism applied to disfavored speakers — the same permit system that smooths their path creates barriers for competitors or opponents. Asymmetric extraction benefits them indirectly.
constraint_indexing:constraint_classification(assembly_petition_clause__permit_system_limits, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: FALSE-SUMMIT ANALYTIC VIEW (MOUNTAIN) — A civilizational perspective might see the permit requirement itself as a natural law: crowds in shared public space require coordination; some official mechanism for scheduling and traffic management is inherent to dense urban life. This perspective risks naturalizing the permit system as inevitable infrastructure. However, the structural data — identifiable beneficiaries (police coordination, establishment speakers), identified victims (disfavored marchers, marginalized groups), discretionary gatekeeping mechanism — contradicts the mountain classification. This is a candidate false summit: the naturalizing framing hides a contingent institutional arrangement that advantages some speakers over others.
constraint_indexing:constraint_classification(assembly_petition_clause__permit_system_limits, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: FORMAL DOCTRINE (PITON) — The definite-standards requirement (content-neutral, no discretion, no hostile-reaction fees) is a formal doctrinal constraint enforced through judicial review. But the constraint's actual function has degraded: licensing officials continue to exercise de facto discretion through subjective 'traffic management' classifications, permit delay tactics, fee structures nominally content-neutral but scaled to event size (and larger events are often disfavored speech), and public-safety justifications that track content. The doctrine says 'no discretion,' but practice permits substantial official gatekeeping. Theater ratio is elevated (0.55) — formal compliance with content-neutral standards combined with practical circumvention of those standards.
constraint_indexing:constraint_classification(assembly_petition_clause__permit_system_limits, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(assembly_petition_clause__permit_system_limits_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(assembly_petition_clause__permit_system_limits, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(assembly_petition_clause__permit_system_limits, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(assembly_petition_clause__permit_system_limits, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(assembly_petition_clause__permit_system_limits, TR),
    TR >= 0.70.

:- end_tests(assembly_petition_clause__permit_system_limits_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts assembly rights from disfavored speakers through three mechanisms: (1) discretionary denial risk despite formal content-neutral standards (safety-based rationalization); (2) permit delays that allow timing to be weaponized; (3) fee scaling that, while nominally content-neutral, correlates with opposition intensity or event size proxy for unpopular speech. The constraint also provides genuine coordination benefit (police planning, traffic management), reducing pure extractiveness below 0.70. The measurement trajectory shows rising extractiveness (0.42 → 0.58) as de facto discretion persists despite formal doctrine. Suppression (0.68): High. Barriers to assembly without permit are severe — assembly becomes illegal, subject to arrest, disruption, dispersal. Permit denial is effectively final (judicial review is slow and post-event); delay is punitive (march moves occur on fixed dates). Fee scaling imposes resource burden that smaller advocacy groups cannot absorb. Theater ratio (0.55): Moderate-high. Formal doctrine requires 'content-neutral, no discretion, no hostile-reaction fees,' but practice demonstrates consistent circumvention through ostensibly content-neutral safety justifications. The gap between doctrine and practice is the theater — compliance performed while gatekeeping continues.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence across power and exit-option axes. The disfavored marcher sees pure extraction (Snare) — the permit system is the sole legal pathway to assembly, yet the path is gatekept and extractive. The police see pure coordination (Rope) — the permit system enables traffic management with minimal coercion overhead, especially under the content-neutral doctrine. The advocacy organization sees a mixed structure (Tangled Rope) — coordination benefits exist (predictable logistics, liability clarity) alongside extraction costs (permits denied, delayed, or priced punitively). The establishment speaker sees coordination with invisible extraction benefit (Tangled Rope) — they benefit from the same mechanism that suppresses competitors. The formal doctrine sees itself as piton (theater performing content-neutrality while practice circumvents it). The civilizational analytics risks a false summit (naturalizing the permit requirement). The perspectival gap reveals that the same rule structure — 'content-neutral, definite standards, no discretion' — has radically different force depending on the speaker's popularity and the official's incentive to gatekeep.
 *
 * DIRECTIONALITY LOGIC:
 *   Powerless disfavored speakers (trapped exit) experience maximum extraction: d = 0.95, high f(d). Institutional police (arbitrage exit, beneficiary) experience near-zero or negative extraction: d = 0.05, negative f(d). Moderate advocacy organizations (constrained exit, victim-beneficiary mix) experience mid-range extraction: d = 0.55-0.65, moderate f(d). Powerful establishment groups (mobile exit, beneficiary) experience minimal extraction: d = 0.20, low f(d). The perspectival gap is large: the same constraint structure produces Snare experience (powerless), Tangled Rope experience (moderate and establishment), and Rope experience (police). The doctrinal/piton perspective highlights the degradation: formal content-neutral standards (intended to suppress discretion) have become a theater masking continued practical discretion.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint's tangled-rope structure is genuine: it coordinAtes and extracts simultaneously. The doctrine (content-neutral standards, no discretion) is not a false constraint that should be ignored; it genuinely reduces extraction compared to frank discretionary denial (which would be Snare across all perspectives). But the doctrine's force has degraded — de facto discretion persists through safety rationalization, timing weaponization, and fee scaling. The classification is stable: Tangled Rope because coordination and extraction both operate. The trajectory toward higher extractiveness (0.42 → 0.58) reflects accumulating circumvention of the doctrinal limits, not collapse of the coordination function. The constraint remains in force (not piton yet) because the doctrine has judicial review backing and ongoing enforcement effort, but the trajectory suggests drift toward Snare if circumvention continues unchecked.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    discretion_disguised_as_safety,
    'Can content-neutral standards meaningfully constrain official discretion when public-safety and traffic-management justifications can rationalize any denial?',
    'Empirical analysis: comparing permit denial rates across content (unpopular vs mainstream), measuring variance in ostensibly content-neutral factors (traffic, noise, public safety) as proxies for content, longitudinal tracking of officials'' stated reasons vs outcome patterns',
    'If safety rationalization reliably disguises content discrimination: the constraint is a facade and extractiveness is higher (0.70+). If safety factors genuinely operate independently of content: constraint has real limiting force and extractiveness is lower (0.40).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(discretion_disguised_as_safety, empirical, 'Whether content-neutral standards constrain discretion or become pretexts for content-based gatekeeping').

omega_variable(
    fee_scaling_proportionality,
    'Are permit fees nominally ''content-neutral'' but actually scaled to suppress disfavored speech through administrative burden?',
    'Fee schedule analysis: comparing permit fees for mainstream vs disfavored speech events of similar size; measuring actual cost pass-through to organizers; tracking whether fee waivers are granted and to whom; regression analysis of fee amount against predicted opposition/hostile reaction',
    'If fees are genuinely proportional to administrative cost regardless of speech content: the constraint permits fees legitimately and extractiveness is lower. If fees correlate with anticipated hostile reaction or opposition intensity: the constraint allows fee-scaling extraction and extractiveness is higher (0.65+).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fee_scaling_proportionality, empirical, 'Whether permit fees remain truly content-neutral or become tools of suppression').

omega_variable(
    kernel_reading_distinctness,
    'Does the permit-system-limits reading genuinely foreclose or merely coexist with the expressive-association and petition-clause-independence readings?',
    'Doctrinal analysis: Can a court simultaneously hold (1) permit systems require definite standards with no discretion, (2) associations have constitutional right to exclude/organize for expressive ends, and (3) petition is a distinct constitutional guarantee? Or does one reading''s core premise logically rule out another''s?',
    'If readings are logically incompatible (one forecloses the other): the constraint family represents a live doctrinal fork where only one can prevail. If coexistent: they represent different aspects of assembly protection that can coexist in a single framework. If one influences the other: they have asymmetric structural pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_distinctness, conceptual, 'Whether the permit-limits reading forecloses or coexists with sibling assembly readings').

omega_variable(
    natural_law_vs_constructed,
    'Is the permit requirement an inherent feature of coordinating shared public space (natural law), or a contingent institutional arrangement that could be replaced with permit-free or lottery-based mechanisms?',
    'Historical and comparative analysis: jurisdictions without advance permits (EU open public space norms), lottery-based timing systems, post-hoc police accountability without gatekeeping. If functioning alternatives exist: natural law classification is defeated.',
    'If permit system is truly inevitable: mountain classification may be justified. If alternatives exist with lower suppression: the mountain is a false summit masking institutional extraction. Current classification candidate: false summit with beneficiaries declared.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed, empirical, 'Whether permit requirements are natural law or contingent institutional design').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(assembly_petition_clause__permit_system_limits, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(assembly_permit_tr_t0, assembly_petition_clause__permit_system_limits, theater_ratio, 0, 0.35).
narrative_ontology:measurement(assembly_permit_tr_t15, assembly_petition_clause__permit_system_limits, theater_ratio, 15, 0.45).
narrative_ontology:measurement(assembly_permit_tr_t30, assembly_petition_clause__permit_system_limits, theater_ratio, 30, 0.55).

% Extraction over time
narrative_ontology:measurement(assembly_permit_be_t0, assembly_petition_clause__permit_system_limits, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(assembly_permit_be_t15, assembly_petition_clause__permit_system_limits, base_extractiveness, 15, 0.52).
narrative_ontology:measurement(assembly_permit_be_t30, assembly_petition_clause__permit_system_limits, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(assembly_permit_su_t0, assembly_petition_clause__permit_system_limits, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(assembly_permit_su_t15, assembly_petition_clause__permit_system_limits, suppression_requirement, 15, 0.65).
narrative_ontology:measurement(assembly_permit_su_t30, assembly_petition_clause__permit_system_limits, suppression_requirement, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(assembly_petition_clause__permit_system_limits, enforcement_mechanism).
narrative_ontology:affects_constraint(assembly_petition_clause__permit_system_limits, assembly_petition_clause__expressive_association_doctrine).
narrative_ontology:affects_constraint(assembly_petition_clause__permit_system_limits, assembly_petition_clause__petition_clause_independence).

% DUAL FORMULATION NOTE:
% The assembly-petition kernel generates three distinct constraint stories with different extractiveness values and authority-grounding modes. This story (permit-system-limits) focuses on the administrative gatekeeping mechanism and its formalization. The expressive-association reading would focus on the organizational protection aspect (different epsilon, different authority grounding: lineage/expertise). The petition-clause-independence reading would focus on the petition right as separate from speech (different epsilon). All three are linked via network edges to show the kernel-reading structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
