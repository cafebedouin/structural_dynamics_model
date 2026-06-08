% ============================================================================
% CONSTRAINT STORY: stakeholder_representation_gap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_stakeholder_representation_gap, []).

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
 *   constraint_id: stakeholder_representation_gap
 *   human_readable: Stakeholder Representation Gap in Germline Genetic Modification Discourse
 *   domain: bioethics/reproductive_medicine/genetic_engineering
 *
 * SUMMARY:
 *   The stakeholder representation gap in germline genetic modification (GGM)
 *   ethical discourse reflects a structural tension between expert-driven
 *   bioethics and democratic legitimacy. While bioethicists and research
 *   scientists dominate the literature (98% of reviewed articles), patient
 *   communities, disability advocacy groups, and the general public are
 *   systematically underrepresented despite being the primary stakeholders
 *   affected by GGM policy. This constraint exhibits scaffold characteristics
 *   from the analytical and organized perspectives: participatory governance
 *   mechanisms (citizen juries, consensus conferences, community-based
 *   participatory research) are being institutionalized with an implicit
 *   sunset logic — the gap is transitional, meant to be resolved as norms
 *   mature. However, from the disability community perspective, the gap
 *   functions as a snare: their identity-constituted lived experience is
 *   precisely what the medical model framing of GGM discourse treats as a
 *   problem to be eliminated, creating an epistemic lock where participation
 *   requires accepting premises that delegitimize their perspective. The
 *   theater_ratio (0.42) reflects that some participatory mechanisms are
 *   performative (stakeholder input acknowledged but not substantively
 *   incorporated), though not as theatrical as degraded peer review in other
 *   domains. Suppression is declining over the interval (0.60 → 0.48) as
 *   participatory norms gain traction, but extraction is rising (0.20 → 0.35)
 *   as the gap between rhetoric of inclusion and reality of expert dominance
 *   becomes more visible.
 *
 * KEY AGENTS:
 *   - Disability Community Members: Primary victim (powerless/identity_locked) — lived experience delegitimized by medical model framing; cannot exit discourse that affects their social legitimacy
 *   - Patient Advocacy Organizations: Secondary victim (moderate/constrained) — resource barriers to participation but also benefit when included; mixed extraction experience
 *   - Bioethics Professionals: Primary beneficiary (institutional/arbitrage) — professional expertise is currency of participation; capture epistemic authority and platform
 *   - Research Scientists: Primary beneficiary (institutional/arbitrage) — technical expertise grants access; benefit from expert-dominated discourse that privileges scientific framing
 *   - Participatory Governance Coalition: Organized agents (organized/mobile) — building alternative pathways through citizen juries, consensus conferences, deliberative polling; see gap as temporary with sunset
 *   - National Bioethics Commissions: Institutional actor (institutional/constrained) — benefit from expert authority but bear legitimacy costs of exclusion; mixed directionality
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees gap as transitional phase in bioethics governance maturation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(stakeholder_representation_gap, 0.35).
domain_priors:suppression_score(stakeholder_representation_gap, 0.48).
domain_priors:theater_ratio(stakeholder_representation_gap, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(stakeholder_representation_gap, extractiveness, 0.35).
narrative_ontology:constraint_metric(stakeholder_representation_gap, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(stakeholder_representation_gap, theater_ratio, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(stakeholder_representation_gap, scaffold).
narrative_ontology:human_readable(stakeholder_representation_gap, "Stakeholder Representation Gap in Germline Genetic Modification Discourse").
narrative_ontology:topic_domain(stakeholder_representation_gap, "bioethics/reproductive_medicine/genetic_engineering").

domain_priors:requires_active_enforcement(stakeholder_representation_gap).
narrative_ontology:has_sunset_clause(stakeholder_representation_gap).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(stakeholder_representation_gap, bioethics_professionals).
narrative_ontology:constraint_beneficiary(stakeholder_representation_gap, research_scientists).
narrative_ontology:constraint_beneficiary(stakeholder_representation_gap, academic_institutions).
narrative_ontology:constraint_victim(stakeholder_representation_gap, patient_communities).
narrative_ontology:constraint_victim(stakeholder_representation_gap, disability_advocacy_groups).
narrative_ontology:constraint_victim(stakeholder_representation_gap, general_public_epistemic_access).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISABILITY COMMUNITY MEMBER (SNARE) — Identity-locked because the community's lived experience is constituted through disability identity, yet that identity is precisely what GGM discourse frames as a problem to be eliminated. Cannot exit the discourse (it affects their material conditions and social legitimacy) but also cannot enter it on equal terms. The representation gap extracts epistemic authority while suppressing alternative framings of disability as difference rather than defect. Maximum experienced extraction from an identity-locked position.
constraint_indexing:constraint_classification(stakeholder_representation_gap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: PATIENT ADVOCACY ORGANIZATION (TANGLED ROPE) — Constrained by resource barriers (lacks academic credentials, funding for participation in international conferences, time to engage with technical literature) but also benefits from the discourse ecosystem when included: gains platform for concerns, influences policy recommendations, builds coalitions. Mixed experience: the gap both excludes and creates demand for their participation when institutions seek legitimacy. Moderate extraction with genuine coordination function.
constraint_indexing:constraint_classification(stakeholder_representation_gap, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: BIOETHICS PROFESSIONAL (ROPE) — Benefits from the current structure: professional expertise is the currency of participation, academic credentials grant access, institutional affiliation provides platform. Experiences the constraint as coordination: the discourse needs boundary maintenance to remain rigorous, and professional gatekeeping serves that function. Net beneficiary with arbitrage-level exit (can move between institutions, advisory boards, journals). Low effective extraction because extraction runs toward this agent.
constraint_indexing:constraint_classification(stakeholder_representation_gap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PARTICIPATORY GOVERNANCE COALITION (SCAFFOLD) — Organized agents (citizen juries, consensus conferences, deliberative polling initiatives, community-based participatory research networks) see the representation gap as a temporary coordination failure with a sunset: participatory governance mechanisms are being institutionalized in bioethics (e.g., UK Nuffield Council citizen panels, NIH community engagement requirements, WHO stakeholder consultation frameworks). The gap is transitional — its justification is building toward inclusive governance, not maintaining expert monopoly. Estimated sunset: 15-25 years for participatory norms to mature in GGM policy.
constraint_indexing:constraint_classification(stakeholder_representation_gap, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: NATIONAL BIOETHICS COMMISSION (TANGLED ROPE) — Institutional actor that both benefits from and is constrained by the gap. Benefits: expert composition grants authority and efficiency in deliberation. Constrained: legitimacy depends on public trust, which erodes when stakeholders are excluded; faces political pressure to demonstrate inclusivity. Mixed directionality: the commission extracts epistemic authority but also bears legitimacy costs. Constrained exit because institutional mandate requires balancing expert rigor with democratic accountability.
constraint_indexing:constraint_classification(stakeholder_representation_gap, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SCAFFOLD) — From a civilizational perspective, the representation gap is a transitional phase in the maturation of bioethics governance. Early-stage expert-dominated discourse is giving way to participatory models as the field recognizes that technical expertise alone cannot resolve value-laden questions about human enhancement, disability, and reproductive autonomy. The sunset is structural: as GGM moves from theoretical possibility to clinical reality, the legitimacy costs of exclusion become unsustainable, forcing institutional adaptation. The constraint is temporary support for a transition, not a steady-state extraction mechanism.
constraint_indexing:constraint_classification(stakeholder_representation_gap, scaffold,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(stakeholder_representation_gap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(stakeholder_representation_gap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(stakeholder_representation_gap, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(stakeholder_representation_gap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. Bioethics professionals and research scientists capture epistemic authority, platform access, and policy influence during the expert-dominated phase. However, extraction is not as severe as pure gatekeeping because some genuine coordination function exists (technical expertise does matter for evaluating genetic modification risks) and participatory mechanisms are being built. The rising trajectory (0.20 → 0.35) reflects that as GGM moves closer to clinical reality, the stakes increase and the gap's extraction becomes more visible. Suppression (0.48): Moderate. Significant barriers include academic credential requirements, technical jargon, resource constraints (time, funding for conference participation), publication bias toward expert authorship, and epistemic framing that privileges medical model over social model of disability. But suppression is declining (0.60 → 0.48) as participatory norms gain institutional traction and funding agencies require community engagement. Theater ratio (0.42): Moderate. Some participatory mechanisms are performative — stakeholder input is solicited for legitimacy but not substantively incorporated into policy recommendations. However, theater is not as high as in other domains because some participatory processes do produce genuine policy shifts (e.g., UK citizen juries influencing Nuffield Council recommendations). Rising trajectory (0.25 → 0.42) reflects that as participatory rhetoric increases, the gap between rhetoric and reality becomes more visible.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon — underrepresentation of non-expert stakeholders — appears differently depending on the observer's position. Bioethics professionals see coordination (Rope) — expert gatekeeping maintains discourse rigor. The participatory governance coalition sees a temporary problem with a sunset (Scaffold) — participatory mechanisms are being institutionalized and will resolve the gap. National bioethics commissions see mixed coordination and extraction (Tangled Rope) — they benefit from expert authority but bear legitimacy costs. Patient advocacy organizations see mixed coordination and extraction (Tangled Rope) — the gap both excludes and creates demand for their participation. Disability community members see pure extraction (Snare) — the medical model framing forecloses their social model perspective, creating an identity lock where participation requires accepting premises that delegitimize their lived experience. The analytical observer sees scaffold — the gap is transitional, justified by the need to build participatory capacity, not by maintaining expert monopoly. The perspectival gap reveals that 'stakeholder representation' is not a neutral coordination problem but a contested site where epistemic authority, professional interests, and democratic legitimacy collide.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position. Disability community members are victims with identity_locked exit — high d toward full target (1.0) because their identity is constituted through the very characteristic GGM discourse frames as defect. Patient advocacy organizations are victims with constrained exit — moderate-high d because they face resource barriers but also gain some benefit when included. Bioethics professionals and research scientists are beneficiaries with arbitrage exit — low d toward full beneficiary (0.0) because they capture epistemic authority and can move freely between institutions. Participatory governance coalition is organized with mobile exit — low-moderate d because they have agency to build alternatives and see a sunset path. National bioethics commissions are institutional with constrained exit — moderate d because they benefit from expert authority but bear legitimacy costs, creating mixed directionality. The analytical observer at civilizational time sees scaffold — the gap is temporary support for a transition toward participatory governance, not a steady-state extraction mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold classification resolves the mandatrophy by showing that the representation gap's mandate is transitional: building participatory governance capacity in bioethics, not maintaining expert monopoly indefinitely. The sunset clause is structural — as GGM moves from theoretical possibility to clinical reality, the legitimacy costs of excluding affected communities become unsustainable, forcing institutional adaptation. However, the omega variables document irreducible uncertainties: Is the sunset timeline credible (15-25 years) or does the gap serve persistent institutional interests? Do participatory mechanisms produce substantive policy change or merely legitimacy theater? Does the medical model framing structurally foreclose disability community perspectives? These uncertainties mean the scaffold classification is provisional — if the gap persists beyond generational timeframe or participatory mechanisms remain theatrical, reclassification to tangled_rope or snare is warranted. The disability community's snare perspective is not resolved by the scaffold framing — their identity lock is a real structural feature that participatory mechanisms may not address if the medical model framing persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    expertise_legitimacy_threshold,
    'What level of technical expertise is genuinely necessary for meaningful participation in GGM ethical discourse versus what level serves as gatekeeping?',
    'Comparative analysis of policy outcomes from expert-only vs. participatory deliberation; assessment of whether lay participants'' contributions are substantively incorporated or performatively acknowledged',
    'If threshold is low: current gap is pure extraction (Snare from more perspectives). If threshold is high: gap serves legitimate coordination function (Rope from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expertise_legitimacy_threshold, conceptual, 'Threshold distinguishing necessary expertise from gatekeeping').

omega_variable(
    participatory_mechanism_effectiveness,
    'Do institutionalized participatory mechanisms (citizen juries, consensus conferences) actually shift policy outcomes or merely provide legitimacy theater for pre-determined expert positions?',
    'Longitudinal tracking of policy recommendations before and after participatory input; comparison of recommendations from expert-only vs. participatory processes; analysis of which stakeholder concerns are incorporated vs. acknowledged-but-dismissed',
    'If effective: scaffold perspective confirmed — sunset is real and participatory governance is functional. If theatrical: the gap persists as extraction mechanism disguised as inclusion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(participatory_mechanism_effectiveness, empirical, 'Whether participatory mechanisms produce substantive policy change').

omega_variable(
    disability_framing_foreclosure,
    'Does the medical model framing embedded in GGM discourse structurally foreclose disability community perspectives that frame disability as difference rather than defect?',
    'Discourse analysis of how disability perspectives are incorporated when present; identification of whether social model framings are engaged substantively or reframed into medical model terms; assessment of whether participation requires accepting medical model premises',
    'If foreclosed: the identity_locked classification is structural — participation requires abandoning the community''s core framing. If not foreclosed: the gap is a resource barrier (constrained) rather than an epistemic lock.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disability_framing_foreclosure, conceptual, 'Whether medical model framing forecloses social model participation').

omega_variable(
    sunset_timeline_credibility,
    'Is the 15-25 year sunset estimate for participatory norm maturation realistic, or does the gap serve persistent institutional interests that will resist closure?',
    'Historical analysis of bioethics governance evolution (e.g., informed consent, research ethics boards); identification of institutional incentives for maintaining expert monopoly; tracking of participatory mechanism adoption rates and depth of implementation',
    'If sunset is credible: scaffold classification holds. If gap persists beyond generational timeframe: reclassify as tangled_rope or snare — the coordination story is cover for extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sunset_timeline_credibility, empirical, 'Credibility of participatory governance sunset timeline').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(stakeholder_representation_gap, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stake_rep_theater_t0, stakeholder_representation_gap, theater_ratio, 0, 0.25).
narrative_ontology:measurement(stake_rep_theater_t3, stakeholder_representation_gap, theater_ratio, 3, 0.32).
narrative_ontology:measurement(stake_rep_theater_t6, stakeholder_representation_gap, theater_ratio, 6, 0.38).
narrative_ontology:measurement(stake_rep_theater_t9, stakeholder_representation_gap, theater_ratio, 9, 0.42).

% Extraction over time
narrative_ontology:measurement(stake_rep_extract_t0, stakeholder_representation_gap, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(stake_rep_extract_t3, stakeholder_representation_gap, base_extractiveness, 3, 0.26).
narrative_ontology:measurement(stake_rep_extract_t6, stakeholder_representation_gap, base_extractiveness, 6, 0.31).
narrative_ontology:measurement(stake_rep_extract_t9, stakeholder_representation_gap, base_extractiveness, 9, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(stake_rep_suppress_t0, stakeholder_representation_gap, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(stake_rep_suppress_t3, stakeholder_representation_gap, suppression_requirement, 3, 0.55).
narrative_ontology:measurement(stake_rep_suppress_t6, stakeholder_representation_gap, suppression_requirement, 6, 0.51).
narrative_ontology:measurement(stake_rep_suppress_t9, stakeholder_representation_gap, suppression_requirement, 9, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(stakeholder_representation_gap, identity_coordination).
narrative_ontology:affects_constraint(stakeholder_representation_gap, informed_consent_adequacy).
narrative_ontology:affects_constraint(stakeholder_representation_gap, genetic_counseling_access).
narrative_ontology:affects_constraint(stakeholder_representation_gap, disability_rights_framework).

% DUAL FORMULATION NOTE:
% The stakeholder representation gap is upstream of specific GGM policy constraints (informed consent, genetic counseling, disability rights) but represents a distinct structural constraint. The downstream constraints have their own extractiveness values reflecting the adequacy of specific policies; the representation gap has its own extractiveness reflecting the epistemic authority asymmetry and barriers to participation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(stakeholder_representation_gap, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
