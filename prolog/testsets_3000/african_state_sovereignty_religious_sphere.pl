% ============================================================================
% CONSTRAINT STORY: african_state_sovereignty_religious_sphere
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_african_state_sovereignty_religious_sphere, []).

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
 *   constraint_id: african_state_sovereignty_religious_sphere
 *   human_readable: African State Sovereignty and Religious Sphere Boundaries
 *   domain: political/religious/institutional
 *
 * SUMMARY:
 *   The constraint governing African state sovereignty and religious sphere
 *   boundaries represents a structural tension between post-colonial state
 *   consolidation and religious institutional autonomy. Since independence
 *   (circa 1960, with expanded timescale to present), African states have
 *   maintained or strengthened regulatory control over religious institutions
 *   through constitutional constraints, security legislation, property law,
 *   and administrative practice. This constraint exhibits hybrid
 *   characteristics: states claim religious sphere regulation serves security
 *   coordination and plural coexistence management; religious minorities
 *   experience it as systematic extraction of autonomy and resources;
 *   independent faith institutions operate within subordinated space that
 *   provides real service delivery but with persistent political
 *   interference. The extractiveness value has increased from ~0.35 (early
 *   post-colonial period with weaker state capacity) to ~0.58 (contemporary
 *   era with consolidated surveillance and regulatory infrastructure), while
 *   theater_ratio has risen from 0.45 to 0.62 as international religious
 *   freedom frameworks have created performative compliance without
 *   structural change. The constraint is maintained through active state
 *   enforcement (security screening, regulatory licensing, property control)
 *   combined with international theater (constitutional guarantees, treaty
 *   signature) that lacks enforcement mechanisms.
 *
 * KEY AGENTS:
 *   - Religious Minority Communities: Primary victims (powerless/trapped) — subjected to discriminatory laws, surveillance, resource denial, and exclusion from public institutions without exit mechanism
 *   - Independent Faith Institutions: Secondary victims (moderate/constrained) — provide genuine social coordination but operate under state regulatory subordination with constrained autonomy
 *   - State Security & Political Elites: Primary beneficiaries (institutional/arbitrage) — leverage religious sphere management for political legitimacy, security intelligence, and opposition control
 *   - Pan-African Civil Society Networks: Organized intermediaries (organized/constrained) — create regional coordination norms on religious freedom but face state sovereignty resistance at enforcement level
 *   - International Religious Freedom Apparatus: Performative layer (institutional/arbitrage) — UN/donor mechanisms maintain treaty frameworks without enforcement; generate theater of international monitoring
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks treating Westphalian sovereignty as immutable natural law when specific religious boundary regimes are contingent institutional choices
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(african_state_sovereignty_religious_sphere, 0.58).
domain_priors:suppression_score(african_state_sovereignty_religious_sphere, 0.65).
domain_priors:theater_ratio(african_state_sovereignty_religious_sphere, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(african_state_sovereignty_religious_sphere, extractiveness, 0.58).
narrative_ontology:constraint_metric(african_state_sovereignty_religious_sphere, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(african_state_sovereignty_religious_sphere, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(african_state_sovereignty_religious_sphere, tangled_rope).
narrative_ontology:human_readable(african_state_sovereignty_religious_sphere, "African State Sovereignty and Religious Sphere Boundaries").
narrative_ontology:topic_domain(african_state_sovereignty_religious_sphere, "political/religious/institutional").

domain_priors:requires_active_enforcement(african_state_sovereignty_religious_sphere).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(african_state_sovereignty_religious_sphere, state_security_apparatus).
narrative_ontology:constraint_beneficiary(african_state_sovereignty_religious_sphere, incumbent_political_elites).
narrative_ontology:constraint_victim(african_state_sovereignty_religious_sphere, religious_minorities).
narrative_ontology:constraint_victim(african_state_sovereignty_religious_sphere, independent_faith_communities).
narrative_ontology:constraint_victim(african_state_sovereignty_religious_sphere, pluralistic_civil_society).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RELIGIOUS MINORITIES (SNARE) — Trapped within national borders with no exit mechanism. Face state surveillance, resource denial, legal harassment, and institutionalized discrimination in service delivery and public participation. Cannot organize collective exit or independent institutional structures without state permission. Maximum experienced extraction.
constraint_indexing:constraint_classification(african_state_sovereignty_religious_sphere, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INDEPENDENT FAITH INSTITUTIONS (TANGLED ROPE) — Provide genuine social coordination (healthcare, education, community welfare) that reduces state burden and creates social cohesion. Simultaneously subordinated to state regulatory oversight, political interference in leadership selection, and resource control. Significant mobility constraints (relocation, legal registration, cross-border funding) but some functional autonomy. Mixed extraction.
constraint_indexing:constraint_classification(african_state_sovereignty_religious_sphere, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE SECURITY APPARATUS & POLITICAL ELITES (ROPE) — Benefit from religious sphere subordination through political legitimacy leverage, security intelligence, and capability to co-opt religious institutions for political mobilization. Experience the constraint as functional coordination: the ability to manage religious boundaries enables state consolidation. Low experienced extraction — asymmetry runs toward these agents.
constraint_indexing:constraint_classification(african_state_sovereignty_religious_sphere, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PAN-AFRICAN CIVIL SOCIETY (TANGLED ROPE) — Organized regional actors (African Union, continental NGOs, diaspora networks) provide coordination on human rights standards and religious freedom norms, reducing unilateral state coercion. Simultaneously constrained by state sovereignty doctrine and lack of enforcement mechanisms. Create genuine coordination value but face persistent extraction through regulatory capture at state level.
constraint_indexing:constraint_classification(african_state_sovereignty_religious_sphere, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: INTERNATIONAL RELIGIOUS FREEDOM APPARATUS (PITON) — UN mechanisms, international NGOs, and donor conditionality create performative religious freedom frameworks (constitutions guarantee freedoms, governments sign treaties) with minimal enforcement power. The theater of international monitoring persists despite routine violations and state non-compliance. Maintenance through institutional inertia rather than functional effect.
constraint_indexing:constraint_classification(african_state_sovereignty_religious_sphere, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / SOVEREIGNTY PRINCIPLE (MOUNTAIN) — From a universal/civilizational view, state monopoly on legitimate coercion necessarily includes religious sphere management — sovereignty is definitionally the state's exclusive right to define the boundaries between state and civil society. This perspective sees the constraint as an immutable consequence of Westphalian statehood. However, structural data reveals this as a false summit: specific religious boundary regimes are contingent institutional arrangements, not logical necessities of sovereignty itself.
constraint_indexing:constraint_classification(african_state_sovereignty_religious_sphere, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(african_state_sovereignty_religious_sphere_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(african_state_sovereignty_religious_sphere, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(african_state_sovereignty_religious_sphere, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(african_state_sovereignty_religious_sphere, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(african_state_sovereignty_religious_sphere, TR),
    TR >= 0.70.

:- end_tests(african_state_sovereignty_religious_sphere_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The state captures significant benefits from religious sphere subordination through political legitimacy leverage (co-opting religious leadership for state validation), security intelligence (surveillance of religious gathering sites and networks), and opposition neutralization (suppressing independent platforms). However, extractiveness is not maximal (0.72+) because: (1) independent faith institutions genuinely provide service coordination that reduces state burden, creating real beneficiary relationship; (2) some religious minorities have developed internal organizational capacity despite constraints; (3) international norms create pressure for regulatory restraint. The increase from 0.35 to 0.58 reflects institutional consolidation — early post-colonial states had weaker capacity for systematic religious sphere control, while contemporary states deploy sophisticated surveillance and administrative mechanisms. Suppression (0.65): High. Multiple structural barriers constrain religious minority and independent institution exit: legal framework (constitutions reserve state role in religious sphere), security apparatus (surveillance of religious spaces), property law (state control of religious site registration and use), resource barriers (dependency on state-controlled service delivery), and geographic constraints (limited cross-border religious institution autonomy). Suppression approaches mountain-level for religious minorities (trapped exit) but remains below mountain for organized faith institutions (constrained exit with some functional autonomy). Theater ratio (0.62): Moderate-high and rising. International religious freedom frameworks (African Charter protections, AU mechanisms, UN condemnation) create performative compliance without enforcement: states sign treaties, constitutionalize religious guarantees, and cooperate with international monitoring, yet systematically violate agreed protections. The theater has increased as international pressure has mounted — states have adapted by creating regulatory facade (formal freedoms + informal suppression) rather than substantive change. The rise from 0.45 to 0.62 indicates theater substitution: states are spending more effort on international performance (compliance committees, regulatory documentation) while maintaining or intensifying actual suppression.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows how the same structural phenomenon — state regulatory control of religious institutions — produces radically different perceived constraint types depending on observer power and exit capacity. The victim (powerless/trapped) sees pure extraction (snare). The beneficiary (institutional/arbitrage) sees functional coordination (rope). The organized mediator (organized/constrained) sees mixed function with sunset potential (scaffold or tangled-rope). The performative international layer (institutional/arbitrage) sees degraded theater (piton). The civilizational observer (analytical/analytical) risks seeing immutable law (mountain, false summit). The perspectival gap reveals that the constraint is not naturally monolithic — its type depends on structural position. Reconciling perspectives requires acknowledging: (1) state security functions are real coordination value (legitimate rope component); (2) religious minority extraction is also real (legitimate snare component); (3) these are not opposites but layered aspects of the same constraint structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: State security apparatus and political elites derive legitimacy leverage (religious leaders validate state authority), security intelligence (surveillance capability over organized civil society), and opposition neutralization (religious minorities cannot organize independent political platforms). These benefits are asymmetrically distributed — the state captures coordination value from religious institution provision (healthcare, education, social welfare) while constraining religious institutions' ability to convert this value into independent power. Victims: Religious minorities bear resource costs (exclusion from public services, property barriers, funding restrictions), institutional autonomy costs (inability to govern without state permission, surveillance), and political voice costs (exclusion from public discourse platforms). Independent faith institutions bear regulatory costs (licensing, inspection, leadership approval) and autonomy constraints but simultaneously benefit from the coordination value they provide (reduced state burden, social cohesion role). The directionality reversal between beneficiary and victim perspectives is the crux: state sees religious sphere management as functional coordination (positive value creation); minorities see it as extraction (value capture). The tangled-rope classification captures both simultaneously — the constraint has genuine coordination function AND asymmetric extraction layered on top.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The mandatrophy is resolved by recognizing that religious sphere regulation operates simultaneously as coordination (genuine state-faith institution cooperation on service delivery, communal conflict prevention) and extraction (asymmetric autonomy suppression, resource capture, political opposition neutralization). The constraint is not 'pure extraction' (snare) nor 'pure coordination' (rope) but explicitly tangled because: (1) independent faith institutions provide real value (healthcare, education, community welfare) that reduces state burden — true coordination function; (2) this coordination is achieved through regulatory subordination, property control, and leadership co-option — genuine asymmetric extraction. The mandatrophy would manifest as mislabeling: treating the constraint as 'just coordination' (rope) erases the suppression of religious minorities and independent institution autonomy; treating it as 'just extraction' (snare) erases the real service coordination role. The tangled-rope classification preserves both dimensions. The increasing theater_ratio (0.45→0.62) reflects mandatrophy manifestation risk: as international norms pressure states, they create performative compliance (constitutional guarantees, treaty signature, compliance reporting) that masks continued extraction. The theater substitution preserves extraction while reducing visibility — a classic mandatrophy pattern where the constraint type appears to improve (moves toward rope) while functional extraction remains constant or increases. Mandatrophy resolution requires measuring: (1) actual regulatory behavior vs formal commitments; (2) religious minority autonomy vs international framework promises; (3) faith institution service provision vs state capacity displacement. The gap between these measurements is mandatrophy depth.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    state_capacity_vs_ideological_control,
    'Does the state''s regulation of the religious sphere primarily serve state capacity/security objectives or ideological/religious supremacy objectives?',
    'Comparative analysis of regulatory patterns: do restrictions target operational autonomy (financial transparency, security screening) or doctrinal content (sermon approval, theological positioning)? Correlation between leader religious affiliation and minority targeting patterns.',
    'If capacity-driven: constraint is lower extractiveness (legitimate security/coordination function). If ideology-driven: constraint is higher extractiveness (religion becomes state propaganda mechanism). Different theta trajectories follow from the resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_capacity_vs_ideological_control, empirical, 'Whether state religious sphere control serves capacity or ideological objectives').

omega_variable(
    colonial_institutional_inheritance,
    'To what degree is the African state''s religious sphere subordination inherited from colonial administrative structures versus post-colonial choices by incumbent elites?',
    'Historical reconstruction of state capacity and elite ideology at independence; comparison of religious sphere autonomy under different post-colonial governments in same country; analysis of constitutional changes and regulatory expansion patterns.',
    'If largely inherited: the constraint is institutional inertia (piton-drift scenario) with potential for reform through institutional redesign. If actively maintained by elites: the constraint is actively chosen extraction (snare-reinforcement scenario) requiring stronger external pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colonial_institutional_inheritance, conceptual, 'Degree of colonial vs post-colonial institutional choice in religious sphere control').

omega_variable(
    religious_diversity_suppression_mechanism,
    'Is suppression of religious minorities primarily structural (legal, economic, geographic barriers) or internalized (identity-based acceptance of religious hierarchy)?',
    'Exit trajectory analysis: when minorities emigrate or gain resources for relocation, does suppression persist? Survey data on internalized religious hierarchy acceptance; comparison of suppression levels in high-literacy vs low-literacy communities; analysis of identity_locked vs trapped exit patterns.',
    'If structural: suppression metric accurately reflects barrier magnitude; policy intervention can reduce barriers directly. If internalized: suppression persists after barrier removal; intervention requires cognitive re-framing and identity reconstruction work.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(religious_diversity_suppression_mechanism, empirical, 'Whether suppression of religious minorities is structural or internalized').

omega_variable(
    pan_african_norm_convergence,
    'Are regional/continental religious freedom norms (African Charter, AU mechanisms) actually converging state behavior or remaining performative without enforcement?',
    'Time-series analysis of state religious sphere policies before/after regional treaty ratification; correlation between treaty signature and actual regulatory change; mechanisms of non-compliance (treaty violation without consequence).',
    'If converging: scaffold perspective is accurate — regional mechanisms have sunset logic. If performative: piton classification dominates; regional theater masks continued state autonomy. Theater_ratio will remain high or increase.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(pan_african_norm_convergence, empirical, 'Whether pan-African religious freedom norms produce actual convergence').

omega_variable(
    functional_coordination_vs_power_maintenance,
    'What proportion of the state''s religious sphere regulation serves genuine coordination functions (preventing communal violence, managing plural coexistence) versus pure power maintenance (elite legitimacy, opposition neutralization)?',
    'Regression analysis of regulatory intensity against security incidents, community religious diversity levels, and political opposition strength; case studies of regulatory changes correlating with political transitions; comparison of coordination outcomes in states with vs without religious sphere subordination.',
    'If primarily coordination: constraint is legitimately tangled_rope (mixed function). If primarily power maintenance: constraint is snare with tangled_rope framing. Beneficiary/victim asymmetry becomes clearer with resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functional_coordination_vs_power_maintenance, empirical, 'Balance between functional coordination and power maintenance in religious regulation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(african_state_sovereignty_religious_sphere, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(afri_tr_t0, african_state_sovereignty_religious_sphere, theater_ratio, 0, 0.45).
narrative_ontology:measurement(afri_tr_t25, african_state_sovereignty_religious_sphere, theater_ratio, 25, 0.58).
narrative_ontology:measurement(afri_tr_t50, african_state_sovereignty_religious_sphere, theater_ratio, 50, 0.62).

% Extraction over time
narrative_ontology:measurement(afri_be_t0, african_state_sovereignty_religious_sphere, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(afri_be_t25, african_state_sovereignty_religious_sphere, base_extractiveness, 25, 0.52).
narrative_ontology:measurement(afri_be_t50, african_state_sovereignty_religious_sphere, base_extractiveness, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(african_state_sovereignty_religious_sphere, resource_allocation).
narrative_ontology:affects_constraint(african_state_sovereignty_religious_sphere, african_state_capacity_consolidation).
narrative_ontology:affects_constraint(african_state_sovereignty_religious_sphere, colonial_administrative_inheritance).
narrative_ontology:affects_constraint(african_state_sovereignty_religious_sphere, international_human_rights_theater).

% DUAL FORMULATION NOTE:
% This constraint is downstream of post-colonial state consolidation choices (institutional inheritance and elite ideology) and upstream of contemporary religious freedom violations and international enforcement gaps. The coordination type is resource_allocation because the constraint operates primarily through state control of service delivery infrastructure (schools, hospitals, welfare systems) that religious institutions provide — the regulatory mechanism is property/resource control rather than pure coercion. Decomposition would separate: (1) state-faith institution service coordination (lower ε, genuine rope component); (2) religious minority suppression and opposition neutralization (higher ε, snare component); (3) international compliance theater (piton component with high theater_ratio). These form a constraint family linked by affects_constraints edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(african_state_sovereignty_religious_sphere, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
