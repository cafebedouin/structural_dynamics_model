% ============================================================================
% CONSTRAINT STORY: legitimate_knowledge_boundary__experiential_pluralism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_knowledge_boundary__experiential_pluralism_reading, []).

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
 *   constraint_id: legitimate_knowledge_boundary__experiential_pluralism_reading
 *   human_readable: Legitimate Knowledge Boundary (Experiential Pluralism Reading)
 *   domain: epistemology/science_studies/political_theory
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of a contested kernel about what
 *   constitutes legitimate knowledge. The experiential pluralism reading
 *   asserts that lived experience and community validation are fundamentally
 *   legitimate sources of knowledge, with methodological standards treated as
 *   tools that may or may not apply depending on context. This reading arises
 *   from decolonial theory, feminist epistemology, environmental justice
 *   scholarship, and science and technology studies (STS) critiques of
 *   universalist claims. The constraint exhibits genuine coordination
 *   function (democratizing epistemic authority, validating knowledge
 *   produced outside credentialed institutions) alongside significant
 *   extraction (appropriation of marginalized knowledge, dilution of
 *   cumulative scientific standards that protect against harm, fragmentation
 *   of shared reality). The trajectory shows rising extractiveness and rising
 *   theater ratio over the 10-year interval, indicating institutionalization
 *   of pluralist rhetoric without proportional change in underlying
 *   validation mechanisms. Theater increases as institutions adopt 'diversity
 *   in epistemology' language while methodological gatekeeping persists below
 *   procedural surfaces.
 *
 * KEY AGENTS:
 *   - Community Knowledge Holders: Primary beneficiaries (moderate/mobile) — see legitimacy of lived experience recognized; local scope gives them agency
 *   - Indigenous Knowledge Systems: Primary victims (powerless/trapped) — pressured to validate against external frameworks or remain invisible; appropriation risk without material protection
 *   - Institutional Pluralists: Secondary actors (powerful/constrained) — benefit from appearing inclusive but constrained by downstream accountability to methodological gatekeepers and funders
 *   - Pluralist Movement Coalition: Organized beneficiaries (organized/constrained) — gain academic legitimacy and funding but constrained by disciplinary gatekeeping and co-optation risk
 *   - Methodological Standards System: Secondary victim (institutional/arbitrage) — experiences erosion of gatekeeping function but persists through inertia and circumlocution (theater ratio rising)
 *   - Analytical Observer: Risk of false-summit naturalization (analytical/analytical) — risks treating social constructedness as epistemically disqualifying and thereby dissolving the analytical problem
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.58).
domain_priors:suppression_score(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.62).
domain_priors:theater_ratio(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_knowledge_boundary__experiential_pluralism_reading, tangled_rope).
narrative_ontology:human_readable(legitimate_knowledge_boundary__experiential_pluralism_reading, "Legitimate Knowledge Boundary (Experiential Pluralism Reading)").
narrative_ontology:topic_domain(legitimate_knowledge_boundary__experiential_pluralism_reading, "epistemology/science_studies/political_theory").

domain_priors:requires_active_enforcement(legitimate_knowledge_boundary__experiential_pluralism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_knowledge_boundary__experiential_pluralism_reading, '09376860-982f-4c39-b1a4-c2112c7e9b24').
narrative_ontology:cs_kernel_codification('09376860-982f-4c39-b1a4-c2112c7e9b24', distributed).
narrative_ontology:cs_authority_grounding('09376860-982f-4c39-b1a4-c2112c7e9b24', distributed).
narrative_ontology:cs_reading_relation('09376860-982f-4c39-b1a4-c2112c7e9b24', legitimate_knowledge_boundary__credentialed_expertise_reading, coexists_with).
narrative_ontology:cs_reading_relation('09376860-982f-4c39-b1a4-c2112c7e9b24', legitimate_knowledge_boundary__hybrid_coproduction_reading, coexists_with).
narrative_ontology:cs_axiom('09376860-982f-4c39-b1a4-c2112c7e9b24', foundational, experiential_validity_unconditional).
narrative_ontology:cs_axiom_status(experiential_validity_unconditional, holdable).
narrative_ontology:cs_axiom_grounding('09376860-982f-4c39-b1a4-c2112c7e9b24', experiential_validity_unconditional, deontological).
narrative_ontology:cs_axiom('09376860-982f-4c39-b1a4-c2112c7e9b24', foundational, methodological_standards_contextual_not_universal).
narrative_ontology:cs_axiom_status(methodological_standards_contextual_not_universal, holdable).
narrative_ontology:cs_axiom_grounding('09376860-982f-4c39-b1a4-c2112c7e9b24', methodological_standards_contextual_not_universal, instrumental).
narrative_ontology:cs_reference_frame('09376860-982f-4c39-b1a4-c2112c7e9b24', epistemic_pluralism_foundation).
narrative_ontology:cs_drift_state('09376860-982f-4c39-b1a4-c2112c7e9b24', contemporary_institutional_absorption, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('09376860-982f-4c39-b1a4-c2112c7e9b24', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(legitimate_knowledge_boundary__experiential_pluralism_reading, legitimate_knowledge_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__experiential_pluralism_reading, experiential_knowledge_holders).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__experiential_pluralism_reading, marginalized_communities).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__experiential_pluralism_reading, pluralist_epistemic_movements).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__experiential_pluralism_reading, methodological_precision).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__experiential_pluralism_reading, cumulative_scientific_knowledge).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__experiential_pluralism_reading, cross_context_transferability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COMMUNITY KNOWLEDGE HOLDER (ROPE) — Benefits from recognition of lived experience as legitimate; experiences the constraint as genuine coordination among equals. Sees methodological standards as tools they can accept or adapt, not gatekeepers. Mobile exit options at local scope — can validate knowledge within community without external credential. Low effective extraction because this agent has agency and clear benefit.
constraint_indexing:constraint_classification(legitimate_knowledge_boundary__experiential_pluralism_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 2: INSTITUTIONAL PLURALIST (TANGLED ROPE) — University or research institution adopting 'knowledge pluralism' frameworks; benefits from avoiding credibility loss and institutional capture accusations, but constrained by disciplinary review processes and funding bodies that still privilege methodological standards. Experiences mixed coordination (genuine democratization of knowledge validation) and extraction (dilution of standards creates liability). Constrained exit — cannot fully decouple from methodological accountability.
constraint_indexing:constraint_classification(legitimate_knowledge_boundary__experiential_pluralism_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INDIGENOUS KNOWLEDGE SYSTEM (SNARE) — Embedded in communities with limited institutional power. Trapped between pressure to validate against methodological standards (which may not apply to traditional ways of knowing) and pressure to reject all external frameworks. No exit: either surrender to external validation criteria or remain invisible to the broader epistemic landscape. Experiences maximum extraction — knowledge is appropriated, repackaged through methodological frameworks, and recirculated without attribution or benefit.
constraint_indexing:constraint_classification(legitimate_knowledge_boundary__experiential_pluralism_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 4: PLURALIST MOVEMENT COALITION (TANGLED ROPE) — Organized networks of STS scholars, decolonial theorists, and environmental justice advocates pushing for epistemic democratization. Benefits from institutional legitimacy, funding, and academic positioning; constrained by dominant epistemological gatekeeping and the risk of co-optation (their radical critique of methodological standards gets absorbed into 'diversity' language while standards persist). Active enforcement of pluralist norms required to prevent reversion.
constraint_indexing:constraint_classification(legitimate_knowledge_boundary__experiential_pluralism_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: DISCIPLINARY METHODOLOGICAL SYSTEM (PITON) — Peer review, publication standards, and credentialing infrastructure persist through institutional inertia despite nominally embracing pluralism. Theater ratio (0.68) reflects that 'pluralist' review processes often become performative — committees add language about diverse ways of knowing while methodological gatekeeping operates below the surface. Institutional actors benefit from maintaining the appearance of inclusivity while preserving actual hierarchies. The system is degraded (sees its own standards as contested) but persists.
constraint_indexing:constraint_classification(legitimate_knowledge_boundary__experiential_pluralism_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From a universal/civilizational standpoint, all knowledge is socially constructed and contextually embedded; no privileged epistemological position exists. This perspective treats the demarcation of 'legitimate' knowledge as fundamentally undecidable — any boundary is a naturalized contingency. However, this view dissolves the analytical problem: if all positions are equally constructed, what makes the pluralist reading's core claim true? The perspective risks performative self-contradiction.
constraint_indexing:constraint_classification(legitimate_knowledge_boundary__experiential_pluralism_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: POLICY MANDATE HOLDER (SCAFFOLD) — Government or institutional mandate requiring 'meaningful community engagement' in research validation. Experiences the constraint as a temporary coordination tool: bring stakeholders into the process, collect their validation, then proceed. Theater ratio supports this reading — the mandated 'inclusion' often becomes a procedural box-check. Sunset clause implicit: as community engagement becomes institutionalized (participatory budgeting, community benefit agreements mature into routine), the constraint's temporary function shifts to permanent infrastructure or dissolves.
constraint_indexing:constraint_classification(legitimate_knowledge_boundary__experiential_pluralism_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_knowledge_boundary__experiential_pluralism_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(legitimate_knowledge_boundary__experiential_pluralism_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(legitimate_knowledge_boundary__experiential_pluralism_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimate_knowledge_boundary__experiential_pluralism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(legitimate_knowledge_boundary__experiential_pluralism_reading, TR),
    TR >= 0.70.

:- end_tests(legitimate_knowledge_boundary__experiential_pluralism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising. At t=0 (early adoption), extractiveness is lower (0.35) because the reading is genuinely new and less co-optable. By t=10 (institutionalization), extractiveness rises to 0.58 as institutions absorb pluralist language while preserving methodological hierarchies. The rise reflects that the reading's radical potential (genuine epistemic democratization) is being transformed into performative inclusion that masks continued gatekeeping. Suppression (0.62): Moderate-high. Barriers include: institutional requirements to justify knowledge claims through methodological terms, funding systems that still privilege credentialed research, publication barriers for non-academic knowledge, and the cognitive barrier of working across incommensurable frameworks. These barriers suppress challenges to methodological authority even while pluralism is nominally adopted. Theater ratio (0.68): High and rising. Institutions adopt pluralist language, mandate community engagement processes, and create diversity committees, but the performative content exceeds the functional content. Validation through community engagement often becomes procedural box-checking; methodological standards continue to operate as the real gate, narrated as 'rigorous engagement with diverse ways of knowing.' Theater rises as the gap between pluralist rhetoric and actual practice widens.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap runs between community knowledge holders (who see rope — genuine recognition and coordination) and methodological gatekeepers (who see piton — a degraded but persistent ritual). Institutional pluralists are caught in the middle (tangled rope) — they coordinate across perspectives but also extract through appropriation. Indigenous knowledge systems experience snare: trapped between devaluation under methodological standards and appropriation under pluralist inclusion. The analytical observer risks the mountain perspective: if all knowledge is constructed, the pluralist reading's core claim (experiential knowledge is legitimate) becomes just another construction without privileged epistemic status. The movement coalition sees scaffold: they are building new institutional pathways, but the sunset clause depends on whether those pathways actually displace methodological gatekeeping or merely supplement it.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from each agent's structural relationship to the constraint. Community knowledge holders benefit from legitimacy recognition (low d); methodological systems experience gatekeeping erosion (higher d); indigenous systems experience maximum extraction (highest d — appropriation masked by pluralist inclusion). Institutional pluralists are ambiguous (beneficiary status but constrained exit, leading to mid-range d). The sigmoid f(d) translates these positions into experienced extractiveness chi. For agents with arbitrage options (institutional methodological system), d is low despite victim status, because they can exit to credentialed alternatives. For agents with trapped options (indigenous systems), d approaches 1.0, producing maximum f(d) and maximum experienced extraction. Overrides are not needed — the structural derivation captures the relative positions accurately.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    experiential_knowledge_incommensurability,
    'Are lived experience and methodological validation genuinely incommensurable, or are they different descriptions of overlapping epistemic practices?',
    'Comparative case analysis: instances where experiential claims and methodological validation converge vs. diverge; evidence from domains with longest co-evolution of method and experience (e.g., agriculture, medicine, ecology)',
    'If incommensurable: pluralism requires genuine institutional separation (different validation pathways, different legitimacy criteria). If overlapping: separation becomes extraction mechanism (gatekeeping narrated as pluralism). Classification shifts from tangled_rope (mixed coordination/extraction) to snare (extraction masked by pluralist rhetoric).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(experiential_knowledge_incommensurability, conceptual, 'Degree of incommensurability between experiential and methodological validation').

omega_variable(
    appropriation_risk_under_pluralism,
    'Does recognition of experiential knowledge as legitimate increase or decrease appropriation risk for marginalized communities?',
    'Longitudinal tracking of attribution practices, benefit distribution, and control over knowledge use in institutions that adopted pluralist epistemologies; comparison with institutions maintaining methodological gatekeeping',
    'If increases: pluralism without material protection becomes extraction mechanism (legitimizes resource flow from marginalized to dominant institutions). Perspective 3 (indigenous system) shifts from snare to severe snare with false-summit pluralist framing. If decreases: pluralism provides genuine protection; tangled_rope classification confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(appropriation_risk_under_pluralism, empirical, 'Whether pluralist legitimacy increases or decreases appropriation of experiential knowledge').

omega_variable(
    distributed_validation_scalability,
    'Does community validation scale beyond local/regional scope, or does context-specificity make distributed validation incoherent at continental/global scales?',
    'Analysis of knowledge claims originating in one community but circulating in another; tracking of validation mechanisms across boundaries (urban vs rural, North vs South, linguistic communities)',
    'If scales: pluralism can coordinate global knowledge networks (more rope-like). If not: pluralism fragments into local validation silos, and claims that do travel face identity erasure or forced translation into methodological terms (extraction mechanism re-emerges).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributed_validation_scalability, empirical, 'Scalability of distributed community validation across spatial scopes').

omega_variable(
    kernel_reading_disambiguation,
    'Is this reading a genuine claim that experiential validation is epistemically sufficient, or a procedural claim that it should be included in decisions without being sufficient?',
    'Close reading of doctrinal statements from pluralist movements, policy mandates, and institutional frameworks; coding for sufficiency language vs inclusion language',
    'If sufficiency claim: tangled_rope classification may underestimate the reading''s epistemic radicalism. If inclusion claim: this reading is less about epistemology and more about power distribution (distinct from credentialed_expertise_reading mostly in scope, not in mechanism). Affects axiomatic structure and relative positioning of sibling readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disambiguation, conceptual, 'Whether this reading claims epistemic sufficiency or procedural inclusion for experiential knowledge').

omega_variable(
    identity_lock_through_pluralism,
    'Does adoption of pluralist epistemology create identity lock for institutional actors, preventing exit even when pluralism becomes counterproductive?',
    'Case studies of institutions that adopted pluralism as identity marker and later attempted to revert; tracking of organizational resistance and reputational cost of backing away from pluralist commitments',
    'If yes: perspective 2 (institutional pluralist) should shift from constrained to identity_locked exit option, changing classification and f(d) derivation. The pluralist reading creates binding through institutional identity, not just external barriers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_through_pluralism, empirical, 'Whether pluralist epistemology creates identity lock for institutional adoption').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_knowledge_boundary__experiential_pluralism_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legit_exp_theater_t0, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(legit_exp_theater_t5, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 5, 0.58).
narrative_ontology:measurement(legit_exp_theater_t10, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(legit_exp_extractiveness_t0, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(legit_exp_extractiveness_t5, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(legit_exp_extractiveness_t10, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(legit_exp_suppression_t0, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(legit_exp_suppression_t5, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(legit_exp_suppression_t10, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_knowledge_boundary__experiential_pluralism_reading, identity_coordination).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__experiential_pluralism_reading, legitimate_knowledge_boundary__credentialed_expertise_reading).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__experiential_pluralism_reading, legitimate_knowledge_boundary__hybrid_coproduction_reading).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__experiential_pluralism_reading, methodological_standard_gatekeeping).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__experiential_pluralism_reading, indigenous_knowledge_appropriation).

% DUAL FORMULATION NOTE:
% The legitimate_knowledge_boundary kernel generates three distinct constraint stories with different ε values and different beneficiary/victim structures. This reading (experiential_pluralism_reading, ε=0.58, tangled_rope) treats the boundary as partially dissolved — experiential knowledge is legitimate independent of methodological validation. The credentialed_expertise_reading (ε≈0.15, rope) treats methodological standards as the primary gate. The hybrid_coproduction_reading (ε≈0.42, tangled_rope) treats the boundary as a negotiated synthesis requiring both. Each reading is a distinct constraint because the ε values differ substantially (0.58 vs 0.15 vs 0.42) and because they have different victim sets and enforcement mechanisms. They are linked by network.affects_constraints to show the kernel structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
