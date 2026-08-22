% ============================================================================
% CONSTRAINT STORY: provincial_sovereignty_boundary__compact_federalism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_provincial_sovereignty_boundary__compact_federalism, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: provincial_sovereignty_boundary__compact_federalism
 *   human_readable: Provincial Sovereignty Boundary (Compact Federalism Reading)
 *   domain: political_economy/federalism
 *
 * SUMMARY:
 *   Canada's confederation was negotiated as a compact among four colonial
 *   provinces (Ontario, Quebec, Nova Scotia, New Brunswick) in 1867, each
 *   retaining substantial autonomy over natural resources, social policy, and
 *   internal governance. The compact-federalism reading asserts that
 *   provinces retained residual sovereignty—sovereignty that was never
 *   transferred to the federal government—and that the federation's terms are
 *   thus continuously renegotiable. When federal authority overreaches (as in
 *   unilateral climate policy or equalization redistribution that
 *   resource-rich provinces reject), provinces have the structural right to
 *   threaten exit and renegotiate the compact's terms. This reading coexists
 *   with two competing interpretations: constitutional-subordination (which
 *   reads the same 1867-1982 texts as establishing federal supremacy and
 *   treating provinces as creatures of federal law, with exit requiring
 *   federal consent) and resource-sovereignty-primacy (which grounds
 *   provincial sovereignty in s.92A of the Constitution Act 1982, treating
 *   resource ownership as the basis of provincial territorial authority). The
 *   constraint story here instantiates ONLY the compact-federalism reading,
 *   not the contest itself.
 *
 * KEY AGENTS:
 *   - Provincial governments: set resource policy, regulate extraction, negotiate equalization terms; maintain confederation by threatening exit renegotiation.
 *   - Federal government: manages transfer mechanisms, sets national standards (climate, environment), faces the constraint that unilateral authority is contested.
 *   - Resource-dependent provinces (Alberta, Saskatchewan, Newfoundland): pay through equalization obligations and federal climate constraints; benefit from confederation's market access.
 *   - Economically peripheral provinces (Atlantic provinces, parts of Ontario): pay through small populations and limited leverage; benefit from equalization transfers.
 *   - Federal courts: interpret the Constitution Acts to enforce (or deny) the conditional nature of federal authority; provide interpretive buffer moderating federal-provincial conflict.
 *   - Indigenous nations: structurally excluded from the sovereignty compact; would claim territorial sovereignty independent of both provincial and federal authority.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(provincial_sovereignty_boundary__compact_federalism, 0.58).
domain_priors:suppression_score(provincial_sovereignty_boundary__compact_federalism, 0.62).
domain_priors:theater_ratio(provincial_sovereignty_boundary__compact_federalism, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, extractiveness, 0.58).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(provincial_sovereignty_boundary__compact_federalism, tangled_rope).
narrative_ontology:human_readable(provincial_sovereignty_boundary__compact_federalism, "Provincial Sovereignty Boundary (Compact Federalism Reading)").
narrative_ontology:topic_domain(provincial_sovereignty_boundary__compact_federalism, "political_economy/federalism").

domain_priors:requires_active_enforcement(provincial_sovereignty_boundary__compact_federalism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(provincial_sovereignty_boundary__compact_federalism, '9abc284f-f58d-4905-b496-de6df16c5959').
narrative_ontology:cs_kernel_codification('9abc284f-f58d-4905-b496-de6df16c5959', fixed_text).
narrative_ontology:cs_authority_grounding('9abc284f-f58d-4905-b496-de6df16c5959', lineage).
narrative_ontology:cs_interpretation_layer_present('9abc284f-f58d-4905-b496-de6df16c5959').
narrative_ontology:cs_reading_relation('9abc284f-f58d-4905-b496-de6df16c5959', provincial_sovereignty_boundary__constitutional_subordination, coexists_with).
narrative_ontology:cs_reading_relation('9abc284f-f58d-4905-b496-de6df16c5959', provincial_sovereignty_boundary__resource_sovereignty_primacy, influences).
narrative_ontology:cs_axiom('9abc284f-f58d-4905-b496-de6df16c5959', foundational, confederation_consensual_compact).
narrative_ontology:cs_axiom_status(confederation_consensual_compact, holdable).
narrative_ontology:cs_axiom_grounding('9abc284f-f58d-4905-b496-de6df16c5959', confederation_consensual_compact, conventional).
narrative_ontology:cs_axiom('9abc284f-f58d-4905-b496-de6df16c5959', foundational, provincial_residual_sovereignty_real).
narrative_ontology:cs_axiom_status(provincial_residual_sovereignty_real, holdable).
narrative_ontology:cs_axiom_grounding('9abc284f-f58d-4905-b496-de6df16c5959', provincial_residual_sovereignty_real, conventional).
narrative_ontology:cs_axiom('9abc284f-f58d-4905-b496-de6df16c5959', secondary, exit_negotiable_under_duress).
narrative_ontology:cs_axiom_status(exit_negotiable_under_duress, holdable).
narrative_ontology:cs_axiom_grounding('9abc284f-f58d-4905-b496-de6df16c5959', exit_negotiable_under_duress, instrumental).
narrative_ontology:cs_reference_frame('9abc284f-f58d-4905-b496-de6df16c5959', confederation_as_negotiated_compact_1867).
narrative_ontology:cs_drift_state('9abc284f-f58d-4905-b496-de6df16c5959', contemporary_climate_equity_reckoning, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9abc284f-f58d-4905-b496-de6df16c5959', '').
narrative_ontology:cs_kernel_id(provincial_sovereignty_boundary__compact_federalism, provincial_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__compact_federalism, provinces_coordinating_role).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__compact_federalism, provincial_governments).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__compact_federalism, economically_peripheral_provinces).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__compact_federalism, resource_dependent_provinces).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__compact_federalism, resource_dependent_provinces).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and enforce resource policy, regulate natural resources within provincial territory, negotiate equalization transfers from federal revenue pool. They maintain the compact by asserting residual sovereignty and renegotiating the confederation's terms whenever federal overreach is perceived. This reading vests them with authority to threaten exit (separation) as a negotiating mechanism, treating exit as costlier-than-compliance but negotiable under conditions of substantial federal overreach.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, provincial_governments, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(provincial_sovereignty_boundary__compact_federalism, provincial_governments, beneficiary).

% Manages interprovincial transfer mechanisms (equalization), sets conditions on federal funding, attempts to impose national standards (climate policy, environmental regulation). Under this reading, the federal government's authority is conditional on provincial consent; it cannot unilaterally revise the confederation's terms. It maintains the arrangement by accepting that provinces have the structural right to renegotiate or exit, though at significant cost.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, federal_government, agenda_setter,
    institutional, generational, analytical, national).

% Depend on equalization transfers from federal revenue (which comes disproportionately from resource-rich provinces and urban centers). They pay through constrained bargaining power: smaller populations, less resource leverage, fewer exit alternatives. Their agreement to the confederation persists because exit isolation would be costlier than the transfer asymmetry they accept. They have structural voice in the compact but limited ability to reshape its terms.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, economically_peripheral_provinces, payer,
    moderate, biographical, trapped, national).

% Control valuable natural resource endowments (oil, minerals, forestry) and claim sovereignty over them via s.92A of the Constitution Act 1982. They pay through equalization obligations (transferring resource revenue to the federal pool for redistribution), federal climate regulations that constrain resource extraction, and federal environmental standards. They benefit from the compact's coordination (access to national markets, federal infrastructure investment) but experience it as coercive because their resource sovereignty is structurally subordinated to the federal redistribution mechanism.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, resource_dependent_provinces, payer,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(provincial_sovereignty_boundary__compact_federalism, resource_dependent_provinces, beneficiary).

% Interpret the Constitution Act 1867 and 1982 to adjudicate disputes over jurisdiction and the scope of provincial vs. federal authority. Under the compact-federalism reading, they are tasked with enforcing the conditional nature of federal authority and recognizing provincial residual sovereignty. They provide the interpretive buffer that moderates the conflict between federation and provincial autonomy.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, federal_courts, observer,
    institutional, generational, analytical, national).

% Indigenous nations within Canadian territory would claim their own sovereignty and territorial rights, independent of both provincial and federal authority. They are systematically excluded from the federalism compact and from negotiation over the confederation's terms. Their absence is structural to the constraint and maintained by both federal and provincial governments.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, regional_coalitions, excluded,
    organized, biographical, constrained, national).

% The historiographical claim that Confederation was a negotiated compact among sovereign provinces (vs. a unilateral federal constitution) vindicates this reading's legitimacy. Scholars who assert compact-federalism premises benefit from the constraint's persistence because it validates their interpretive frame as the operational federalism of Canada.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, historical_compact_scholarship, beneficiary,
    analytical, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(provincial_sovereignty_boundary__compact_federalism, historical_compact_scholarship).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(provincial_sovereignty_boundary__compact_federalism, federal_government).
narrative_ontology:fixing_cost_class(provincial_sovereignty_boundary__compact_federalism, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables economic union and interprovincial trade while preserving provincial control over natural resources and social policy; creates a redistributive mechanism (equalization) that allows peripheral provinces to remain in the federation; solves the collective-action problem of a multinational state with unequal resource distribution.
% TRANSFER_FUNCTION: Resource revenue flows from resource-rich provinces into the federal pool; federal redistribution flows from the pool to economically peripheral provinces in the form of equalization transfers. Federal climate and environmental regulations also flow from federal authority toward resource-dependent provinces, constraining extraction in the name of national interest.
% ABSENT_VOICES: Indigenous nations whose territories span provincial boundaries are excluded from both provincial sovereignty claims and federal jurisdiction; they would assert sovereignty independent of the confederation compact and demand representation in its renegotiation. Their structural exclusion is maintained by both levels of government. Ecological commons (watersheds, atmosphere, migratory animal populations) that cross provincial and federal boundaries are also effectively excluded—treated as property subject to provincial resource extraction rather than as subjects with standing in the confederal dispute.
% DISAPPEARANCE_RATIONALE: If the compact-federalism constraint disappeared—meaning the principle that provincial consent is required for federal authority were abandoned—provinces would immediately face federal unilateral control over resource policy, equalization terms, and climate regulation. Resource-rich provinces would attempt secession or radical renegotiation; peripheral provinces would lose their equalization guarantees and would be left choosing between economic isolation or subordination to federal redistribution. The Canadian federation would reorganize around either federal subordination (constitutional_subordination reading operationally dominant) or provincial fragmentation into resource-controlled territories (resource_sovereignty_primacy reading dominant).
% FOUNDING_PROBLEM: Confederation required negotiating a union among colonies with distinct interests, unequal resource endowments, and no prior history of centralized governance. The founding problem was: how to create a federal system that preserves provincial autonomy in resource and social policy while enabling national economic coordination and ensuring that resource-rich provinces subsidize peripheral provinces through equalization, without triggering either federal subordination (which would make resource provinces exit) or resource hoarding (which would leave peripheral provinces isolated).
% FOUNDING_PROBLEM_CORROBORATION: Compact-federalism historians, Quebec constitutional scholars, and provincial governments assert the founding problem remains live: ongoing disputes over federal climate policy (carbon tax litigation, interprovincial environmental standards), equalization renegotiation (Atlantic provinces demanding increased transfers, resource provinces demanding equalization reform), and resource sovereignty (Alberta and Saskatchewan's disputes with federal climate policy) demonstrate that the balance struck in 1867 and reaffirmed in 1982 is continuously renegotiated. Constitutional-subordination scholars argue the founding problem was solved by establishing federal supremacy in the patriation of the Constitution and the Charter of Rights and Freedoms, which transcended provincial jurisdiction. Resource-sovereignty advocates argue the 1982 Constitution Act solved it by entrenching provincial resource control and making provincial property rights absolute. Attestations come from within each reading's own tradition; no neutral external corroboration from parties outside the Canadian federalism dispute exists. The constraint is self-justifying within Canadian constitutional law and political science.
narrative_ontology:disappearance_verdict(provincial_sovereignty_boundary__compact_federalism, world_rearranges).
narrative_ontology:founding_problem_status(provincial_sovereignty_boundary__compact_federalism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(provincial_sovereignty_boundary__compact_federalism, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(provincial_sovereignty_boundary__compact_federalism, 'none', 1).
narrative_ontology:epsilon_provenance(provincial_sovereignty_boundary__compact_federalism, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(provincial_sovereignty_boundary__compact_federalism_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(provincial_sovereignty_boundary__compact_federalism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(provincial_sovereignty_boundary__compact_federalism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.58) because the constraint operates a unidirectional transfer mechanism (equalization) and imposes federal standards (climate, environment) that constrain resource provinces' policy autonomy. However, under the compact-federalism reading, resource provinces retain negotiation power—they can threaten exit or renegotiation—which dampens extractiveness below what pure federal subordination would produce. Suppression is similarly high (0.62) because the constraint's operation depends on both federal enforcement of national standards AND provincial acceptance of equalization obligations; federal coercion (threat of withholding federal infrastructure funding, federal regulation) and provincial coercion (threat of secession) operate continuously. Theater ratio is moderate (0.42) because the constraint includes genuine coordination functions (equalization ensures peripheral provinces remain in the federation, federal standards internalize cross-provincial externalities like climate emissions) but an increasing share of enforcement activity defends extraction itself—defending equalization transfers, defending federal climate authority against provincial override—rather than the coordination these mechanisms nominally serve. The measurement series show extractiveness rising from 1867 to the early 2000s (as federal authority expanded and equalization became more redistributive) and plateauing after 2015 (as provincial resistance to unilateral federal climate authority constrained further federal expansion). Theater ratio rises gradually over the same period (increasing performative justification for equalization and federal standards as extraction becomes more salient). Suppression jumps at 1982 with the patriation of the Constitution and the entrenchment of s.92A (resource sovereignty), as the legal framework for contested authority was formalized. All metrics are authored on a single shared time grid (1867, 1926, 1982, 2000, 2015, 2025) so temporal analysis has consistent measurement points.
 *
 * PERSPECTIVAL GAP:
 *   The federal-government seat and the provincial-government seats compute dramatically different types from identical structural data. From the federal perspective, the arrangement appears as rope-with-coordination: redistribution ensures national union, federal standards address interprovincial externalities, and the federation holds because both levels benefit. From the provincial perspective—particularly resource provinces—the arrangement appears as tangled rope bordering on snare: federal authority is experienced as coercive (climate standards, equalization demands), exits are constrained (economic integration, sunk-cost institutional relationships), and renegotiation power is the only leverage available. The engine's per-seat computation captures this divergence: federal actors compute d near beneficiary (extract from the transfer mechanism), provincial actors compute d near target (constrained payers). This is NOT a claim that one perspective is 'right'—it is structural: the same constraint produces asymmetric benefits and costs for different seats, which is exactly what tangled-rope classification identifies.
 *
 * DIRECTIONALITY LOGIC:
 *   Provincial governments (institutional power) claim residual sovereignty and maintain the compact through continuous renegotiation; they are the agenda-setters in this reading. Federal government (institutional power) manages equalization and standards but faces the structural constraint that unilateral authority is contested and exit threats are credible; this reading treats federal authority as conditional on provincial consent. Resource-dependent provinces (powerful power atom) pay through equalization obligations and federal climate constraints; they have leverage to renegotiate because they control valuable resources and exit would be costly to the federation, but they are constrained by economic integration and the federal government's control of interprovincial trade and infrastructure. Economically peripheral provinces (moderate power atom) are trapped: they depend on equalization transfers for basic service delivery, their exit would be economically isolating, and they have minimal leverage to reshape federation terms. They benefit from coordination (get equalization) but pay through subordination to the federation's consolidated authority. Federal courts (institutional, analytical) interpret whether federal authority is conditional (supporting the compact reading) or supreme (supporting subordination). The exit-options axis is critical: resource provinces can plausibly threaten secession (constrained, not trapped), while peripheral provinces would face immediate fiscal and economic collapse upon exit (trapped). This structural asymmetry is the core of the tangled-rope classification: the arrangement coordinates (ensures peripheral provinces stay in the federation) and extracts (redistributes from resource to peripheral provinces) through the same mechanism, but the exit options that generate the asymmetry are REAL (not nominal), which differentiates tangled rope from snare.
 *
 * MANDATROPHY ANALYSIS:
 *   The compact-federalism reading asserts the founding problem remains LIVE: ongoing disputes over federal climate authority, equalization mechanics, and resource sovereignty demonstrate continuous renegotiation. The constraint persists not because the problem is solved but because the compromise—federal authority conditional on provincial renegotiation—still holds at the level of operative legality (courts recognize provincial residual sovereignty, provincial exit threats are credible). If the founding problem were DEAD (if resource distribution and federal-provincial authority were settled and uncontested), the constraint should reclassify to piton (theatrical performance, inertial maintenance). If the constraint were a SNARE (victims trapped without renegotiation power), the founding problem would be superseded by the reality of federal subordination and provincial impotence. The classification tangled-rope holds because: (1) the founding problem remains live (provinces continuously renegotiate terms in response to federal overreach), (2) there is genuine coordination (equalization keeps peripheral provinces in the federation, federal standards internalize emissions externalities), and (3) the asymmetry is sustained through active enforcement of equalization obligations and federal standards, not through nominal consent. If courts were to overturn the compact reading and establish pure federal supremacy (constitutional-subordination), the mandate would be dead (federal authority would be no longer contested) and the constraint would reclassify toward snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_operational_dominance,
    'Which of the three readings of the provincial_sovereignty_boundary kernel is operationally dominant in the Canadian legal system and political practice at present?',
    'Track Supreme Court decisions over a 5-10 year window, federal-provincial dispute resolutions, and the rhetoric used by government actors (especially when defending or contesting federal authority). If courts consistently enforce provincial residual sovereignty and treat federal authority as conditional, compact-federalism dominates. If courts establish unilateral federal authority, subordination dominates. If courts ground provincial authority in resource ownership (s.92A), resource-sovereignty-primacy dominates.',
    'The operationally dominant reading determines the actual constraint classification. If compact-federalism is dominant, this constraint persists as tangled rope. If subordination is dominant, it reclassifies to snare (federal extraction from provinces without renegotiation power). If resource-sovereignty-primacy is dominant, this constraint''s foundations are undercut and a new constraint (resource-control-based sovereignty) becomes the operative one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_operational_dominance, empirical, 'Which reading of the kernel is operationally dominant in Canadian law and practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(provincial_sovereignty_boundary__compact_federalism, 1867, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prov_tr_t1867, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 1867, 0.25).
narrative_ontology:measurement_basis(prov_tr_t1867, projected).
narrative_ontology:measurement(prov_tr_t1926, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 1926, 0.28).
narrative_ontology:measurement_basis(prov_tr_t1926, observed).
narrative_ontology:measurement(prov_tr_t1982, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 1982, 0.38).
narrative_ontology:measurement_basis(prov_tr_t1982, observed).
narrative_ontology:measurement(prov_tr_t2000, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 2000, 0.41).
narrative_ontology:measurement_basis(prov_tr_t2000, observed).
narrative_ontology:measurement(prov_tr_t2015, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 2015, 0.43).
narrative_ontology:measurement_basis(prov_tr_t2015, observed).
narrative_ontology:measurement(prov_tr_t2025, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 2025, 0.42).
narrative_ontology:measurement_basis(prov_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(prov_be_t1867, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 1867, 0.35).
narrative_ontology:measurement_basis(prov_be_t1867, projected).
narrative_ontology:measurement(prov_be_t1926, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 1926, 0.42).
narrative_ontology:measurement_basis(prov_be_t1926, observed).
narrative_ontology:measurement(prov_be_t1982, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 1982, 0.54).
narrative_ontology:measurement_basis(prov_be_t1982, observed).
narrative_ontology:measurement(prov_be_t2000, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 2000, 0.57).
narrative_ontology:measurement_basis(prov_be_t2000, observed).
narrative_ontology:measurement(prov_be_t2015, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 2015, 0.59).
narrative_ontology:measurement_basis(prov_be_t2015, observed).
narrative_ontology:measurement(prov_be_t2025, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 2025, 0.58).
narrative_ontology:measurement_basis(prov_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(prov_su_t1867, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 1867, 0.4).
narrative_ontology:measurement_basis(prov_su_t1867, projected).
narrative_ontology:measurement(prov_su_t1926, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 1926, 0.48).
narrative_ontology:measurement_basis(prov_su_t1926, observed).
narrative_ontology:measurement(prov_su_t1982, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 1982, 0.62).
narrative_ontology:measurement_basis(prov_su_t1982, observed).
narrative_ontology:measurement(prov_su_t2000, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 2000, 0.61).
narrative_ontology:measurement_basis(prov_su_t2000, observed).
narrative_ontology:measurement(prov_su_t2015, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 2015, 0.62).
narrative_ontology:measurement_basis(prov_su_t2015, observed).
narrative_ontology:measurement(prov_su_t2025, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 2025, 0.62).
narrative_ontology:measurement_basis(prov_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(provincial_sovereignty_boundary__compact_federalism, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(provincial_sovereignty_boundary__compact_federalism, 0.18).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__compact_federalism, constitutional_subordination).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__compact_federalism, resource_sovereignty_primacy).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__compact_federalism, equalization_transfer_mechanism).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__compact_federalism, federal_climate_authority).

% DUAL FORMULATION NOTE:
% The provincial_sovereignty_boundary kernel decomposes into three structurally distinct constraints corresponding to three readings of the same constitutional texts (Constitution Act 1867, Constitution Act 1982). This story (compact_federalism) treats the confederation as a negotiated compact with operationally real provincial exit rights and federal authority conditional on consent. The constitutional_subordination reading treats the same texts as establishing federal supremacy and provinces as derived entities. The resource_sovereignty_primacy reading treats s.92A as grounding absolute provincial territorial control. Each reading produces a different constraint_id, a different ε value, and a different victim set. Sibling constraints are linked via network.affects_constraints to indicate that a shift in how the kernel is read (e.g., a Supreme Court decision reinterpreting Confederation) would cause simultaneous reclassification across all three constraint stories. The readings coexist because different Canadian political constituencies operate within different readings operationally.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(provincial_sovereignty_boundary__compact_federalism, powerful, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
