% ============================================================================
% CONSTRAINT STORY: sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sovereignty_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sovereignty_reading
 *   human_readable: Federation Membership Sovereignty Reading: Welfare & Labor Market Preservation
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   Federation membership creates a structural tension between member state
 *   authority to preserve welfare systems and labor market regulations
 *   (portrayed as core sovereignty) and the rights of mobile citizens within
 *   the federation to equal access to employment and social benefits. The
 *   sovereignty reading instantiates one interpretation of the contested
 *   federation kernel: that member states must retain decisive authority over
 *   welfare eligibility and labor market design to sustain fiscal legitimacy,
 *   democratic accountability, and social cohesion. This reading generates
 *   substantial extractiveness (0.58) because it restricts mobile workers'
 *   access to labor markets and welfare while generating net benefits for
 *   unskilled native workers and welfare system sustainability. The
 *   constraint is tangled rope — not pure extraction (snare) because genuine
 *   coordination functions exist (preventing regulatory races to the bottom,
 *   maintaining fiscal boundaries, preserving democratic control), and not
 *   pure coordination (rope) because the distribution of costs and benefits
 *   is sharply asymmetric. The sovereignty reading coexists with the
 *   integration reading (which subordinates welfare to federation-wide
 *   standards) and the hybrid reading (which attempts coordinated welfare
 *   standards with member state variation). This analysis treats the
 *   sovereignty reading as one defensible commitment within a contested
 *   political kernel, not as a natural law or structural necessity. The
 *   measured theater_ratio (0.48) remains moderate because the sovereignty
 *   justification appeals to real governance principles (democratic
 *   accountability, fiscal sustainability) even as it extracts from mobile
 *   workers' opportunity set.
 *
 * KEY AGENTS:
 *   - Mobile Workers (Restricted Access): Primary victims (powerless/trapped) — structurally mobile across federation borders but functionally trapped by welfare and labor market eligibility restrictions. No exit option within the constraint; must accept restricted access or abandon mobility.
 *   - Unskilled Native Workers: Primary beneficiaries (moderate/constrained) — benefit from labor market protection (reduced competition) and welfare system preservation, but face costs of reduced economic efficiency and higher taxes for welfare boundary maintenance.
 *   - Member State Governments: Institutional beneficiaries (institutional/arbitrage) — retain policy autonomy to design welfare and labor regulation; experience the constraint as pure coordination mechanism enabling democratic control.
 *   - Federation Authority (Supranational Level): Organized institutional actor (organized/constrained) — coordinates member states' sovereignty preservation while managing intra-federation mobility rights; benefits from coordination function but also extracts through centralized approval authority.
 *   - Harmonization Coalition: Organized advocates (organized/constrained) — see sovereignty-based restrictions as temporary regulatory failures being solved by judicial harmonization and reciprocal recognition; perceive sunset clause as implicit in court-driven welfare portability expansion.
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing a political reading (sovereignty preservation as essential federalism) into a natural law; benefits analysis reveals this reading serves identifiable beneficiaries.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereignty_reading, 0.58).
domain_priors:suppression_score(sovereignty_reading, 0.65).
domain_priors:theater_ratio(sovereignty_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereignty_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(sovereignty_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(sovereignty_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(sovereignty_reading, "Federation Membership Sovereignty Reading: Welfare & Labor Market Preservation").
narrative_ontology:topic_domain(sovereignty_reading, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(sovereignty_reading, formalized).
narrative_ontology:cs_authority_grounding(sovereignty_reading, lineage).
narrative_ontology:cs_interpretation_layer_present(sovereignty_reading).
narrative_ontology:cs_kernel_id(sovereignty_reading, federation_membership_kernel).
narrative_ontology:cs_reading_relation(sovereignty_reading, integration_reading, forecloses).
narrative_ontology:cs_reading_relation(sovereignty_reading, hybrid_reading, coexists_with).
narrative_ontology:cs_axiom(sovereignty_reading, foundational, member_state_welfare_control_essential).
narrative_ontology:cs_axiom_status(member_state_welfare_control_essential, holdable).
narrative_ontology:cs_axiom_grounding(sovereignty_reading, member_state_welfare_control_essential, deontological).
narrative_ontology:cs_axiom(sovereignty_reading, foundational, fiscal_boundary_integrity_preservation).
narrative_ontology:cs_axiom_status(fiscal_boundary_integrity_preservation, holdable).
narrative_ontology:cs_axiom_grounding(sovereignty_reading, fiscal_boundary_integrity_preservation, empirically_contingent).
narrative_ontology:cs_reference_frame(sovereignty_reading, member_state_welfare_preservation).
narrative_ontology:cs_drift_state(sovereignty_reading, contemporary_eu_cjeu_harmonization_era, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereignty_reading, unskilled_native_workers).
narrative_ontology:constraint_beneficiary(sovereignty_reading, welfare_system_sustainability).
narrative_ontology:constraint_victim(sovereignty_reading, mobile_workers_restricted_access).
narrative_ontology:constraint_victim(sovereignty_reading, economic_efficiency_foregone_gains).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MOBILE WORKER RESTRICTED ACCESS (SNARE) — Structurally mobile across federation borders but functionally trapped by welfare/labor restrictions. Cannot exit the constraint without abandoning citizenship rights or migration opportunity. Bears full extraction cost through reduced access to employment and welfare benefits. No coordination benefit perceived — the constraint appears as pure extraction from the powerless.
constraint_indexing:constraint_classification(sovereignty_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: UNSKILLED NATIVE WORKER (TANGLED ROPE) — Benefits from labor market protection (reduced competition from mobile workers) and welfare system preservation (public goods available to established residents). But also bears costs: foregone economic efficiency gains, reduced consumer choice, higher taxes to maintain protected welfare boundaries. Genuine coordination function (protecting social solidarity within national communities) alongside asymmetric extraction (protection concentrated on native low-skill workers, costs dispersed onto immigrants and efficiency).
constraint_indexing:constraint_classification(sovereignty_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MEMBER STATE GOVERNMENT (ROPE) — Experiences the constraint as pure coordination: preserving member state authority to design welfare eligibility and labor market regulation enables democratic accountability and fiscal sustainability. Net beneficiary through retained policy autonomy. Low experienced extraction because government has full agency over implementation. Exit option is arbitrage — can adjust welfare generosity and labor policy within federation bounds to attract or deter migration.
constraint_indexing:constraint_classification(sovereignty_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: FEDERATION AUTHORITY (TANGLED ROPE) — Organized actor that coordinates member states' sovereignty preservation while managing intra-federation mobility rights. Genuinely benefits from coordination function (prevents regulatory arbitrage races to the bottom, coordinates labor mobility standards) but also extracts from that coordination (centralizes approval authority, can enforce restrictions member states individually could not sustain). Constrained exit because federation legitimacy depends on honoring sovereignty commitments — cannot simply override member state welfare design without institutional collapse.
constraint_indexing:constraint_classification(sovereignty_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: HARMONIZATION COALITION (SCAFFOLD) — Coalition of progressive institutions (supranational courts, labor rights organizations, mobility advocates) sees sovereignty-based welfare restrictions as a temporary regulatory failure being solved by judicial harmonization, reciprocal recognition agreements, and portability provisions. The sunset clause is implicit: as coordinated welfare standards and earned-rights portability spread, the need for member states to preserve absolute control declines. Low theater because the coalition has agency through litigation and standard-setting.
constraint_indexing:constraint_classification(sovereignty_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational scale, federations must preserve member state control over welfare and labor markets to sustain fiscal legitimacy and democratic accountability — this appears as an immutable structural requirement of federalism itself. But the engine's false-summit detection reveals that beneficiary declarations (unskilled native workers, welfare system sustainability) identify this as a reading that naturalizes a political choice. The naturalization serves actors with power to enforce the restriction.
constraint_indexing:constraint_classification(sovereignty_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereignty_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sovereignty_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sovereignty_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting asymmetric cost distribution on mobile workers alongside genuine coordination functions. The constraint extracts through restricted employment and welfare access (measured on mobile worker perspective: d ≈ 0.88, f(d) ≈ 1.2, chi amplified by continental scope σ=1.1 → effective chi ≈ 0.76, experienced as snare). Unskilled native workers perceive lower extractiveness because they are beneficiaries (d ≈ 0.35, f(d) ≈ 0.40), and the coordination function is real (labor market protection). Suppression (0.65): High, reflecting substantial barriers to mobile worker access (regulatory exclusion of non-citizen welfare access, labor market licensing restrictions, family reunification limits). These barriers are enforced through law and institutional practice. Theater ratio (0.48): Moderate. The sovereignty justification appeals to real governance principles (democratic accountability, fiscal sustainability, social cohesion maintenance) — not purely performative. However, the theater component reflects that the same coordination goals could be achieved through alternative architectures (reciprocal recognition, portable benefits, federation-wide standards) that distribute costs more evenly. The sovereignty framing emphasizes member state control as essential when alternative designs exist.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between the mobile worker's snare and the unskilled native worker's tangled rope reveals the constraint's core asymmetry. Both see the same structural mechanism (welfare/labor eligibility restrictions), but one is trapped and bears extraction costs while the other is protected and receives coordination benefits. The gap is the redistribution: member state authority transfers economic rents from mobile workers to native workers with welfare dependence. This is the constraint's functional purpose (from the sovereignty reading's perspective), but it is also the mechanism of extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality values (d) derive from structural position. Mobile workers are full targets of the extraction (d ≈ 0.88): restricted access, no exit, no beneficiary status. Unskilled native workers are mixed (d ≈ 0.35): beneficiaries of protection, but also pay efficiency costs; constrained by the restriction even as it protects them. Member state governments are net beneficiaries (d ≈ 0.10): retain policy autonomy, can arbitrage within federation bounds. The federation authority is split between beneficiary function (coordinating mobility standards) and extractor role (centralizing approval): institutional power with constrained exit (d ≈ 0.45). These derive automatically from the beneficiary/victim declarations and exit options; the chi computation then applies the sigmoid f(d) and scope modifier σ(S) to produce experienced extractiveness.
 *
 * MANDATROPHY ANALYSIS:
 *   The sovereignty reading resolves mandatrophy by grounding the tangled rope classification in genuine coordination functions (democratic accountability, fiscal sustainability, welfare boundary maintenance) alongside asymmetric extraction. The constraint is not pure extraction (snare) because the coordination functions are real and measurable — member states do preserve fiscal control and democratic design authority. But it is not pure coordination (rope) because the benefits concentrate on native workers and welfare systems while costs concentrate on mobile workers and foregone efficiency. The tangled rope classification holds this tension: both the rope and snare elements are structural. The sibling readings (integration, hybrid) would resolve mandatrophy differently by either subordinating sovereignty to mobility rights (integration → snare or piton for sovereignty reading) or by attempting to balance sovereignty with harmonized welfare standards (hybrid → rope with partial scaffold elements). The committer frame acknowledges that mandatrophy resolution depends on which reading of the federation kernel one commits to.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    welfare_harmonization_feasibility,
    'Can EU-style coordinated welfare standards and reciprocal benefit recognition actually replace member state welfare design without triggering fiscal cascades or legitimacy collapse in high-immigration receiving states?',
    'Longitudinal analysis of welfare spending and migration flows in EU member states with strongest harmonization (Schengen, CJEU case law); comparison to federation designs that preserve strict state-level control (US federal/state, Canada federal/provincial). Track whether welfare costs actually equilibrate or continue concentrating in high-immigration regions despite harmonization.',
    'If feasible: scaffold perspective is structural — harmonization is a genuine sunset mechanism reducing the need for absolute welfare control. If not feasible: sovereignty constraint persists as tangled rope indefinitely, and the coordination gains are overstated. The constraint becomes more clearly extractive (snare from welfare-receiving state perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_harmonization_feasibility, empirical, 'Whether welfare harmonization can replace sovereign member state design').

omega_variable(
    sovereignty_vs_integration_kernel_ambiguity,
    'Is this constraint a reading of a contested political kernel (federation membership itself), or is it a structural description of how federations necessarily function?',
    'Historical analysis of federation designs: do all stable federations preserve member state welfare control, or only some? Are there federations that subordinate welfare policy to union-level design and remain stable? What does stability actually track (fiscal sustainability, democratic legitimacy, migration equilibrium, or something else)?',
    'If kernel (political choice): the sovereignty reading is one of multiple defensible readings; the integration and hybrid readings are equally valid from different commitments. Sibling readings coexist. If structural necessity: the sovereignty reading approaches mountain status; sibling readings are aspirational but not structural. The constraint''s classification depends on whether we treat federation design as a political choice or an emergent structural requirement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereignty_vs_integration_kernel_ambiguity, conceptual, 'Whether sovereignty constraint is a kernel reading or structural necessity').

omega_variable(
    economic_extraction_vs_coordination_sorting,
    'The measured extractiveness (0.58) from mobile workers'' perspective reflects pure extraction, but the moderate unskilled worker''s perspective sees tangled rope (genuine protection + coordination). Is the asymmetry a sign that the constraint is genuinely mixed coordination-extraction, or that the power to define ''coordination benefit'' is itself part of the extraction?',
    'Welfare-outcome analysis: do unskilled native workers actually experience measurable welfare gains from the restriction (wage protection, employment, welfare access), or is the perceived benefit primarily psychological/identity (belonging, protection, solidarity)? If welfare gains are real and measurable, the tangled rope classification is justified. If benefits are primarily identity-based, the constraint is snare masquerading as rope through framing.',
    'If genuine welfare gains: tangled rope is accurate; the constraint coordinates real economic protection. If identity-based: the constraint is extractive (snare from multiple perspectives), and the ''coordination'' is ideological cover. This determines whether the sibling integration reading ''forecloses'' the sovereignty reading or merely ''coexists with'' it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_extraction_vs_coordination_sorting, empirical, 'Whether welfare protection generates real economic gains or ideological cover').

omega_variable(
    federation_type_structural_variation,
    'Does the sovereignty reading apply uniformly across different federation types (federal republics with residual state powers vs confederations with delegated union powers vs supra-national unions like EU), or does extractiveness vary substantially by federation architecture?',
    'Comparative analysis: US (federal state model), Canada (federal state model), Switzerland (cantonal confederal model), EU (delegated supranational union), Australia (federal state model). For each: what authority structure preserves welfare/labor control, what mobility restrictions flow from that structure, what is the measured extraction on mobile workers and foregone efficiency?',
    'If extractiveness is uniform: the sovereignty constraint is federation-generic. If it varies: the reading is architecture-contingent, and different federation types may support different sibling readings with equal structural validity. The constraint family may decompose further into architecture-specific stories.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(federation_type_structural_variation, empirical, 'Whether sovereignty constraint extractiveness varies by federation architecture').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereignty_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sove_tr_t0, sovereignty_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(sove_tr_t20, sovereignty_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(sove_tr_t40, sovereignty_reading, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(sove_be_t0, sovereignty_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(sove_be_t20, sovereignty_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(sove_be_t40, sovereignty_reading, base_extractiveness, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereignty_reading, resource_allocation).
narrative_ontology:affects_constraint(sovereignty_reading, integration_reading).
narrative_ontology:affects_constraint(sovereignty_reading, hybrid_reading).

% DUAL FORMULATION NOTE:
% The federation membership kernel decomposes into three readings with distinct extractiveness profiles and beneficiary/victim structures. The sovereignty reading (this story) preserves member state welfare control at the cost of mobile worker access. The integration reading (separate story) subordinates welfare to union-wide standards, reducing extractiveness on mobile workers but increasing it on unskilled native workers. The hybrid reading (separate story) attempts coordinated standards with member state variation, producing a compromise tangled rope. All three link to the same kernel and address the same structural domain; they differ in their foundational commitments about what federation membership requires. Each story has its own ε, beneficiary/victim set, and measurement trajectory.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sovereignty_reading, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
