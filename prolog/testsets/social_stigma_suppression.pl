% ============================================================================
% CONSTRAINT STORY: social_stigma_suppression
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_social_stigma_suppression, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: social_stigma_suppression
 *   human_readable: Social Stigma Suppression Mechanism
 *   domain: social/psychological/institutional
 *
 * SUMMARY:
 *   Social stigma suppression is a constraint that operates through the
 *   simultaneous application of multiple mechanisms: institutional
 *   discrimination (direct extraction), social exclusion (enforced
 *   isolation), identity fusion (cognitive capture), and knowledge
 *   suppression (epistemic damage). The constraint manifests differently
 *   across power positions, making it a crucial case for understanding how
 *   extraction mechanisms operate at the social level. Stigmatized
 *   individuals and communities experience the constraint as a snare with
 *   maximum suppression and minimal exit options. Non-stigmatized peers and
 *   institutions benefit through status allocation and reduced competition.
 *   Destigmatization institutions maintain performative efforts that preserve
 *   underlying hierarchies. The epistemic commons loses access to knowledge
 *   about the stigmatized population. The theater ratio has increased over
 *   the measurement interval (0.35 to 0.55) as formal anti-stigma efforts
 *   have become more visible while structural extraction persists — a
 *   signature pattern of institutional Goodhart drift where metric
 *   optimization (appearing progressive) decouples from outcome improvement
 *   (actual inclusion).
 *
 * KEY AGENTS:
 *   - Stigmatized Individuals: Primary victims (powerless/trapped) — face total suppression through discrimination, exclusion, and internalized shame; no meaningful exit options
 *   - Stigmatized Communities: Primary victims (moderate/identity_locked) — community identity constituted through shared stigma; collective extraction through economic/political marginalization
 *   - Non-Stigmatized Peers: Secondary beneficiaries (moderate/constrained) — benefit from status hierarchy while bearing coordination costs of maintaining social distance
 *   - Status-Benefiting Institutions: Primary beneficiaries (institutional/arbitrage) — extract status allocation and capacity sorting benefits; can switch target categories
 *   - Epistemic Commons: Victim (analytical/analytical) — suppressed knowledge production about stigmatized populations; no self-correction mechanism
 *   - Destigmatization Institutions: Theater maintainers (institutional/constrained) — perform commitment to inclusion while preserving status structures
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(social_stigma_suppression, 0.62).
domain_priors:suppression_score(social_stigma_suppression, 0.75).
domain_priors:theater_ratio(social_stigma_suppression, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(social_stigma_suppression, extractiveness, 0.62).
narrative_ontology:constraint_metric(social_stigma_suppression, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(social_stigma_suppression, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(social_stigma_suppression, snare).
narrative_ontology:human_readable(social_stigma_suppression, "Social Stigma Suppression Mechanism").
narrative_ontology:topic_domain(social_stigma_suppression, "social/psychological/institutional").

domain_priors:requires_active_enforcement(social_stigma_suppression).
% --- Structural relationships ---
narrative_ontology:constraint_victim(social_stigma_suppression, stigmatized_individuals).
narrative_ontology:constraint_victim(social_stigma_suppression, stigmatized_communities).
narrative_ontology:constraint_victim(social_stigma_suppression, epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STIGMATIZED INDIVIDUAL (SNARE) — Faces overwhelming social and institutional barriers to exit or challenge the stigma. Identity fusion with the stigmatized category makes exit conceptually unthinkable. Maximum suppression: economic discrimination, social isolation, internalized shame, institutional exclusion. No meaningful exit options; extraction is total.
constraint_indexing:constraint_classification(social_stigma_suppression, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: STIGMATIZED COMMUNITY (SNARE) — Community identity is constituted through the shared stigma. Collective exit would require abandoning group identity and historical/cultural continuity. High suppression: limited economic opportunity, educational barriers, health disparities, political marginalization. Identity lock creates structural entrapment despite some organizational capacity.
constraint_indexing:constraint_classification(social_stigma_suppression, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 3: PROXIMATE NON-STIGMATIZED PEER (TANGLED ROPE) — Faces coordination problem: maintaining social distance while cooperating in shared institutions (workplace, school, healthcare). Also extracts status and opportunities through the stigma hierarchy. Benefits from stigma's suppression of competition; constrained by effort required to maintain the boundary.
constraint_indexing:constraint_classification(social_stigma_suppression, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: STATUS-BENEFITING INSTITUTION (ROPE) — Benefits from stigma's social ordering function. Hospitals benefit from capacity to exclude certain patients; employers benefit from ability to underpay or exclude workers; schools benefit from stratification. Coordination function: stigma enables rapid sorting without explicit negotiation. High arbitrage — can switch status allocation to new target categories.
constraint_indexing:constraint_classification(social_stigma_suppression, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: EPISTEMIC COMMONS (SNARE) — Stigma suppresses knowledge production about the stigmatized category: research is blocked by access barriers, institutional review boards restrict studies, stigmatized voices are excluded from knowledge production. The commons loses empirical understanding of health, wellbeing, and social dynamics in stigmatized populations. No exit mechanism; the suppression is structural.
constraint_indexing:constraint_classification(social_stigma_suppression, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 6: DESTIGMATIZATION INSTITUTION (PITON) — Formal anti-stigma efforts (advocacy organizations, educational programs, legal protections) have low functional impact relative to performative effort. Theater ratio high: awareness campaigns, diversity statements, DEI programs perform commitment while structural extraction persists. Institutions maintain destigmatization theater to appear progressive while preserving underlying status hierarchies.
constraint_indexing:constraint_classification(social_stigma_suppression, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(social_stigma_suppression_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(social_stigma_suppression, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(social_stigma_suppression, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(social_stigma_suppression, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(social_stigma_suppression, TR),
    TR >= 0.70.

:- end_tests(social_stigma_suppression_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High. The constraint extracts significantly from stigmatized individuals and communities through multiple channels: economic discrimination (reduced income/employment), health disparities (reduced access to quality care), social exclusion (reduced social capital), and epistemic suppression (excluded from knowledge production). The extraction is not maximal (0.72+) because some institutional pathways provide partial exit (relocation, identity change, organizational protection) and some destigmatization efforts have modest effects. The upward trajectory (0.48 to 0.62 over 30 years) reflects increasing extraction despite formal anti-stigma efforts — suggesting institutional capture of destigmatization language. Suppression (0.75): Very high. The binding is enforced through institutional discrimination, social shunning, economic penalty, and internalized shame. Barriers to exit are severe: relocating requires capital and carries identity rupture costs; challenging the stigma requires institutional power and social capital the stigmatized lack; legal protections exist but carry enforcement costs. Suppression operates both structurally (external barriers) and through identity lock (internal binding). Theater ratio (0.55): Moderate-high. The constraint increasingly operates through performative anti-stigma work (diversity initiatives, awareness campaigns, protected-category language) while structural extraction persists. The theater has grown as a proportion of apparent anti-stigma effort over the interval, indicating Goodhart drift — institutions optimize for appearing progressive while preserving underlying status hierarchies.
 *
 * PERSPECTIVAL GAP:
 *   The fundamental gap is between those who benefit from the constraint (non-stigmatized peers, status-benefiting institutions) who perceive the constraint as a coordination mechanism (Rope or Tangled Rope) — necessary sorting for social organization — and those harmed by it (stigmatized individuals, epistemic commons) who perceive it as pure extraction (Snare) with no coordination benefit. The institutional perspective shows a third gap: status-benefiting institutions genuinely believe they coordinate necessary functions while extracting status; destigmatization institutions genuinely believe they're reducing extraction while actually performing a theater that enables its persistence. The epistemic commons perspective reveals knowledge suppression that neither the beneficiaries nor the victims may fully recognize — stigma operates partly through preventing the stigmatized from producing knowledge about themselves, and preventing non-stigmatized researchers from accessing data. The analytical observer risks naturalizing the stigma as inevitable social ordering when it is actually a contingent institutional arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values flow from beneficiary/victim declarations and exit options. Stigmatized individuals classified as powerless/trapped experience maximum directionality toward victimhood (d ≈ 0.95), producing high f(d) ≈ 1.42. Non-stigmatized peers with moderate power and constrained exit experience mixed directionality (d ≈ 0.45), producing f(d) ≈ 0.45. Status-benefiting institutions with institutional power and arbitrage options experience low directionality toward victimhood (d ≈ 0.10), producing f(d) ≈ -0.08 (beneficiary sign). Identity-locked communities experience different directionality than trapped individuals despite similar power levels — the identity lock keeps d ≈ 0.89 (victim of identity constraint) rather than 0.95 (purely structural trap), reflecting that identity reframing could in principle create exit, even though it is cognitively blocked. This differentiation enables the engine to show that identity-locked agents have hidden degrees of freedom that trapped agents do not.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating how the same structural phenomenon (social ordering through stigma) appears as pure extraction to the victims, mixed extraction/coordination to the beneficiaries, and performative coordination to institutions claiming to reduce it. The mandatrophy is not 'is stigma extraction or coordination?' but 'who experiences it as what, and why?' The perspective from powerless/trapped agents reveals that the coordination narrative is a cover story — the constraint primarily functions to extract status and capacity without solving a genuine collective action problem. The institutional perspective reveals that the beneficiaries sincerely believe they are coordinating necessary functions (hospitals sorting patients, schools stratifying learners, employers allocating roles) while extracting asymmetric value. The analytical perspective must resist naturalizing this as inevitable — the 'natural sorting' narrative is itself a key mechanism that sustains extraction by making the constraint appear unchangeable. The piton perspective (destigmatization institutions) reveals the critical mechanism: the constraint persists not through force but through performative theater that creates appearance of change while preserving underlying extraction. Theater ratio increase (0.35 to 0.55) is the diagnostic signal that mandatrophy resolution is failing — institutions have learned to package extraction as inclusion efforts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_vs_constrained_boundary,
    'For stigmatized individuals, is the binding mechanism internalized identity fusion (identity_locked) or external structural barriers (constrained/trapped)?',
    'Longitudinal analysis of post-exit trajectories: individuals who exit stigmatized contexts and sustain psychological distress show identity lock; those whose distress resolves show structural trapping. Survey data on perceived barriers (internal shame vs external discrimination).',
    'If primarily identity_locked: intervention requires identity reframing and cognitive work. If primarily trapped: intervention requires removing external barriers. Classification differs materially — same individual may appear trapped initially, identity_locked after partial barrier removal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_constrained_boundary, empirical, 'Whether stigma binding is identity fusion or structural barriers').

omega_variable(
    suppression_internalization_residue,
    'Does suppression persist after stigmatized individuals exit the social context (internalized suppression), or is it entirely context-dependent (structural suppression)?',
    'Comparative study of post-exit populations: measured internalized shame, self-efficacy, and coping in individuals who have relocated/changed identity vs those remaining in stigma context. Persistence of suppression effects post-exit indicates internalization.',
    'If internalized: effective suppression is higher than structural measure suggests — the constraint travels with the agent. If context-dependent: suppression declines sharply after exit, indicating fully structural binding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_residue, empirical, 'Whether suppression is internalized or context-dependent').

omega_variable(
    status_extraction_vs_coordination_necessity,
    'Does institutional reliance on stigma-based sorting serve genuine coordination functions or is the status extraction primary with coordination as cover story?',
    'Intervention analysis: institutions required to implement identity-blind sorting mechanisms show operational efficiency losses (if coordination is real) or maintain efficiency (if status extraction was primary). Comparative case studies: labor markets, healthcare systems, educational institutions.',
    'If coordination is primary: stigma is tangled rope (mixed coordination/extraction). If extraction is primary: stigma is snare (pure extraction with coordination narrative as theater). Classification differs across multiple institutional perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(status_extraction_vs_coordination_necessity, empirical, 'Whether stigma-based sorting is coordination or status extraction').

omega_variable(
    epistemic_suppression_reversibility,
    'If stigma were removed, could the epistemic commons recover knowledge about the formerly stigmatized population, or is knowledge loss permanent?',
    'Historical analysis of destigmatization outcomes (post-Jim Crow medicine, post-homosexuality-pathologization psychiatry, post-disability institutionalization): comparison of knowledge recovery rates and remaining epistemic gaps. Timeline for knowledge production to normalize.',
    'If recoverable: epistemic damage is temporary; snare classification holds but with sunset potential. If permanent: knowledge loss compounds extraction; snare classification intensifies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_suppression_reversibility, empirical, 'Whether epistemic suppression is reversible post-destigmatization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(social_stigma_suppression, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stigma_tr_t0, social_stigma_suppression, theater_ratio, 0, 0.35).
narrative_ontology:measurement(stigma_tr_t10, social_stigma_suppression, theater_ratio, 10, 0.48).
narrative_ontology:measurement(stigma_tr_t20, social_stigma_suppression, theater_ratio, 20, 0.55).
narrative_ontology:measurement(stigma_tr_t30, social_stigma_suppression, theater_ratio, 30, 0.62).

% Extraction over time
narrative_ontology:measurement(stigma_be_t0, social_stigma_suppression, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(stigma_be_t10, social_stigma_suppression, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(stigma_be_t20, social_stigma_suppression, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(stigma_be_t30, social_stigma_suppression, base_extractiveness, 30, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(social_stigma_suppression, identity_coordination).
narrative_ontology:affects_constraint(social_stigma_suppression, institutional_discrimination).
narrative_ontology:affects_constraint(social_stigma_suppression, health_access_stratification).
narrative_ontology:affects_constraint(social_stigma_suppression, epistemic_exclusion).
narrative_ontology:affects_constraint(social_stigma_suppression, status_hierarchy_persistence).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(social_stigma_suppression, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
