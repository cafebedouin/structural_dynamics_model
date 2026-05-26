% ============================================================================
% CONSTRAINT STORY: integration_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_integration_primary, []).

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
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: integration_primary
 *   human_readable: Free Movement as Constitutive of EU Integration (Integration-Primary Reading)
 *   domain: political_economy/federalism/migration_policy/welfare_state
 *
 * SUMMARY:
 *   The integration-primary reading of federation membership obligations
 *   establishes free movement of workers as constitutive of EU citizenship
 *   and single market functioning, with the consequence that member state
 *   welfare boundaries must yield to mobility rights. This reading represents
 *   the dominant institutional position of the European Commission and the
 *   European Court of Justice (ECJ) since the 1980s. Structurally, this
 *   constraint extracts from displaced local workers in receiving states and
 *   from member state governments seeking to maintain welfare sovereignty,
 *   while benefiting mobile workers and EU-level institutional actors. The
 *   constraint is enforced through ECJ jurisprudence, which has progressively
 *   expanded the scope of mobility rights beyond the original Treaty text,
 *   and through infringement procedures against member states that attempt to
 *   restrict welfare access to non-residents. The extractiveness has
 *   increased over time (from 0.28 to 0.52) as ECJ case law has broadened the
 *   definition of 'worker,' reduced the social welfare restrictions, and
 *   limited member state discretion. Theater ratio remains moderate (0.48)
 *   because ECJ rulings are formally grounded in Treaty interpretation and
 *   proportionality review, even though the functional outcome (mobility >
 *   sovereignty) is largely predetermined. This reading coexists with two
 *   sibling readings: member-sovereignty-primary (which privileges member
 *   state control over welfare boundaries and labor market policy) and
 *   selective-solidarity (which accepts free movement but couples it with
 *   genuine EU-level fiscal redistribution to compensate affected workers and
 *   regions). The kernel itself—federation-membership-obligations—is the
 *   foundational commitment underlying all three readings; they differ on
 *   which obligation is primary (integration vs. member autonomy vs.
 *   intra-European solidarity).
 *
 * KEY AGENTS:
 *   - Mobile workers (particularly from lower-wage member states): Primary beneficiaries (powerful/mobile) — gain unrestricted access to higher-wage labor markets and receiving-state welfare systems
 *   - Displaced local workers in receiving states: Primary victims (powerless/trapped) — bear wage depression and job displacement costs without EU-level compensation or voice in ECJ proceedings
 *   - Member state welfare administrations: Secondary victims (moderate/constrained) — obligated to provide full welfare access to mobile workers while bearing fiscal costs and lacking political leverage in EU proceedings
 *   - European Commission: Primary institutional beneficiary (institutional/arbitrage) — expands supranational authority and harmonization power through mobility rights as enforcement lever
 *   - European Court of Justice: Institutional arbiter and beneficiary (institutional/arbitrage) — maintains and expands institutional authority through case law that systematically privileges mobility, relies on performative jurisprudential framing (theater of proportionality) to maintain legitimacy while predetermined functional outcomes
 *   - Member state governments: Institutional victims (institutional/trapped) — cannot exit EU without catastrophic economic consequences; cannot redesign welfare policy without ECJ override; cannot restrict mobility without infringement proceedings
 *   - Solidarity-seeking coalition (labor unions, progressive parties, redistributive welfare advocates): Organized agents (organized/constrained) — see alternative institutional design (coupled mobility + fiscal redistribution) as viable but faces implementation barriers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(integration_primary, 0.52).
domain_priors:suppression_score(integration_primary, 0.65).
domain_priors:theater_ratio(integration_primary, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(integration_primary, extractiveness, 0.52).
narrative_ontology:constraint_metric(integration_primary, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(integration_primary, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(integration_primary, tangled_rope).
narrative_ontology:human_readable(integration_primary, "Free Movement as Constitutive of EU Integration (Integration-Primary Reading)").
narrative_ontology:topic_domain(integration_primary, "political_economy/federalism/migration_policy/welfare_state").

domain_priors:requires_active_enforcement(integration_primary).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(integration_primary, formalized).
narrative_ontology:cs_authority_grounding(integration_primary, lineage).
narrative_ontology:cs_interpretation_layer_present(integration_primary).
narrative_ontology:cs_kernel_id(integration_primary, federation_membership_obligations).
narrative_ontology:cs_reading_relation(integration_primary, member_sovereignty_primary, coexists_with).
narrative_ontology:cs_reading_relation(integration_primary, selective_solidarity, influences).
narrative_ontology:cs_axiom(integration_primary, foundational, economic_integration_primacy).
narrative_ontology:cs_axiom_status(economic_integration_primacy, holdable).
narrative_ontology:cs_axiom(integration_primary, secondary, state_welfare_boundaries_subordinate).
narrative_ontology:cs_axiom_status(state_welfare_boundaries_subordinate, holdable).
narrative_ontology:cs_reference_frame(integration_primary, single_market_with_mobility).
narrative_ontology:cs_drift_state(integration_primary, post_2015_migration_crisis, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(integration_primary, mobile_workers).
narrative_ontology:constraint_beneficiary(integration_primary, european_commission).
narrative_ontology:constraint_beneficiary(integration_primary, ecj_institutional_authority).
narrative_ontology:constraint_victim(integration_primary, displaced_local_labor).
narrative_ontology:constraint_victim(integration_primary, receiving_state_welfare_systems).
narrative_ontology:constraint_victim(integration_primary, member_state_fiscal_sovereignty).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPLACED LOCAL WORKER (SNARE) — Faces labor market competition without exit: cannot leave the nation-state framework or its labor market, bears full adjustment costs (wage depression, job displacement, retraining burden), receives no compensatory welfare access. Suppression is structural and complete: no organized exit, no EU-level redress, no direct voice in ECJ proceedings. Maximum extraction experienced.
constraint_indexing:constraint_classification(integration_primary, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RECEIVING STATE WELFARE ADMINISTRATION (TANGLED ROPE) — Experiences mixed coordination and extraction. Genuine coordination function: mobile workers enable labor market flexibility, fiscal contributions, and demographic sustainability in aging populations. Asymmetric extraction: obligated to provide full welfare access to non-resident workers despite having limited fiscal control and political accountability only to residents. Constrained exit: cannot opt out of mobility rights without violating EU law; can adjust welfare design but faces ECJ review.
constraint_indexing:constraint_classification(integration_primary, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: MOBILE WORKER (ROPE) — Net beneficiary experiencing the constraint as pure coordination. Free movement enables access to optimal labor markets and welfare systems; receives full welfare rights in receiving states without corresponding fiscal contribution history. Suppression is minimal: voluntary mobility, substantial exit options (can choose destination state or return), and EU legal guarantees. Experiences the constraint as enabling, not restrictive.
constraint_indexing:constraint_classification(integration_primary, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 4: EUROPEAN COMMISSION (TANGLED ROPE) — Institutional beneficiary with arbitrage options. Genuine coordination function: mobilizing labor markets increases EU-wide economic efficiency and reduces regional unemployment asymmetries. Asymmetric extraction: uses mobility rights as a lever to expand supranational authority; can override member state welfare boundaries through harmonization pressure. Can exit (return autonomy to member states) but gains institutional power by maintaining the constraint.
constraint_indexing:constraint_classification(integration_primary, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: MEMBER STATE GOVERNMENT (SNARE) — Experiences constraint as extractive lock-in. Trapped by EU law: cannot design welfare eligibility, labor market protection, or fiscal boundaries without ECJ override. Suppression is legal and institutional: violation triggers infringement proceedings and financial penalties. For member states seeking to protect local labor or fiscal boundaries, exit requires full EU withdrawal (economic/political catastrophe). No internal exit or renegotiation lever.
constraint_indexing:constraint_classification(integration_primary, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 6: SOLIDARITY-SEEKING COALITION (SCAFFOLD) — Organized actors (labor unions, left-wing parties, progressive welfare advocates) see free movement as temporary under current welfare fragmentation. Genuine coordination: mobility rights can coexist with worker protection IF coupled with genuine fiscal redistribution (EU-level unemployment insurance, targeted adjustment assistance, harmonized minimum standards). Low extraction perceived because exit path exists: redesign the welfare substrate to absorb mobility rather than pit workers against each other. Sunset logic: current pain is transitional; institutional reform can dissolve the constraint within one generation.
constraint_indexing:constraint_classification(integration_primary, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 7: ECJ AS INSTITUTIONAL REFEREE (PITON) — The European Court of Justice is the formal arbiter of mobility rights vs. member state discretion, but its own jurisprudence has become substantially performative. The ECJ maintains authority through case-by-case rulings (theater of proportionality review) while systematically privileging mobility rights over state boundaries; the functional decision (mobility > sovereignty) precedes the formal reasoning. Theater ratio reflects the gap between the Court's stated principle (balancing free movement against legitimate state interests) and its actual jurisprudence (finding state interests secondary). Piton classification: the Court's role persists through institutional inertia and authority lineage, not through perceived judicial effectiveness in mediating the genuine tension.
constraint_indexing:constraint_classification(integration_primary, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, economic integration inherently requires free movement of factors of production (labor, capital); this is a structural law of how markets function, as immutable as the speed of light is for physics. State welfare boundaries are seen as parochial holdovers incompatible with market integration itself. However, this perspective instantiates a false summit: the 'naturalness' of this integration trajectory is a contingent political choice (post-WWII European federalism strategy), not a law of economics or human nature. The engine's false summit detector will identify beneficiaries (mobile workers, Commission, ECJ) and flag this as naturalization of a constructed arrangement.
constraint_indexing:constraint_classification(integration_primary, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(integration_primary_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(integration_primary, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(integration_primary, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(integration_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(integration_primary, TR),
    TR >= 0.70.

:- end_tests(integration_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, reflecting the real asymmetry between beneficiaries (mobile workers, institutional actors) and victims (displaced workers, member states), but not severe because the mechanism is not coercive deception—it is transparent law. The constraint operates through legal rules that are formally legitimate, not through hidden manipulation. However, the rules were chosen by EU institutions with substantial power asymmetry; member states bound by treaty cannot easily renegotiate. Suppression (0.65): High. Structural barriers prevent exit: member states cannot unilaterally opt out of mobility rights without violating EU law and facing economic sanctions; displaced workers cannot exit the labor market or nation-state framework; the constraint is enforced through legal/institutional coercion, not merely through incentives. Theater ratio (0.48): Moderate. ECJ jurisprudence maintains a formal structure of proportionality review and Treaty interpretation, but the substantive outcome (mobility rights > state boundaries) is largely predetermined; the reasoning is rationalization of an institutional commitment rather than genuine balancing. The theater is not high (>0.7) because the functional mechanism is transparent: ECJ rulings clearly state the mobility-rights priority, even if the formal reasoning masks the intensity of that priority.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival range is extreme—from Rope (mobile workers, immediate/mobile perspective) to Snare (displaced local workers, biographical/trapped perspective) to Piton (ECJ, civilizational/arbitrage perspective viewing its own authority as inertial) to Mountain (analytical/analytical perspective risking naturalization). The gap reflects genuine structural differences: (1) Mobile workers experience only benefits and exit freedoms, so they see coordination. (2) Displaced workers experience only costs and exit barriers, so they see pure extraction. (3) Member states experience legal lock-in and fiscal obligations without commensurate benefits, so they see extraction. (4) The ECJ experiences authority expansion and institutional persistence, so it sees its own role as performative and inertial (piton). (5) The civilizational observer risks seeing all of this as inevitable economic integration, naturalizing what is actually a contested institutional choice. The scaffold perspective (solidarity coalition) represents an organized attempt to bridge the gap by coupling mobility with genuine fiscal redistribution, producing a different constraint geometry where extraction is offset by compensation.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation: Mobile workers appear as beneficiaries with mobile exit options (can choose destination states, not trapped), so d ≈ 0.15 (low extraction toward them). Member states appear as victims with trapped exit options (cannot exit EU without catastrophe, cannot redesign welfare without ECJ override), so d ≈ 0.95 (high extraction from them). Displaced local workers appear as victims with trapped exit options (no exit from national labor market, no EU-level redress), so d ≈ 0.95. European Commission and ECJ appear as institutional beneficiaries with arbitrage options (can expand or contract authority depending on political context, but retain institutional prestige and budgetary flows), so d ≈ 0.20-0.30. The effective extractiveness χ scales these base d values by f(d) and scope modifier σ(S). For continental scope, σ(S) = 1.1, amplifying the extraction visible across large territories. The sigmoidal mapping converts d into experienced power: low d for beneficiaries produces negative f(d) (they see the constraint as unburdensome); high d for victims produces positive f(d) > 1.0 (they see extraction acutely magnified). This produces the perspectival gap: beneficiaries classify as Rope (pure coordination), victims classify as Snare or Tangled Rope (extraction or mixed).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy (tension between the constraint's claimed coordination function and actual extraction mechanism) is partially resolved but latent. The integration-primary reading claims that free movement is functionally necessary for market integration (coordination function). This is partially true: labor market flexibility does increase EU-wide economic efficiency. However, the extraction component is substantial and non-negotiable in this reading: member states must absorb welfare costs for non-resident mobile workers, displaced workers must absorb adjustment costs without compensation, and alternative institutional designs (coupling mobility with fiscal redistribution) are rejected as incompatible with integration-primary framing. The mandatrophy is resolved by acknowledging that the constraint is genuinely tangled (both coordination and extraction), but the reading prioritizes coordination benefits at the cost of imposing extraction on powerless and institutional actors with no voice in the design. The false summit perspective (mountain/analytical) represents a mandatrophy hazard: if the constraint is naturalized as a law of markets rather than a policy choice, the extraction becomes invisible, the alternative readings become unthinkable, and the constraint becomes self-reinforcing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    welfare_boundary_vs_market_integration,
    'Is welfare state sovereignty structurally incompatible with single market free movement, or is incompatibility a policy choice that could be resolved by fiscal redistribution and harmonization?',
    'Comparative analysis of federalism models (Canada, Australia, US) where free internal mobility coexists with substantial regional welfare variation via fiscal transfers; modeling of EU-level unemployment insurance and adjustment funds as alternative institutional designs',
    'If structurally incompatible: integration-primary reading is an expression of inevitable economic law (mountain view justified). If policy choice: the constraint''s extractiveness reflects political decisions about who bears adjustment costs, and alternative readings (member-sovereignty-primary, selective-solidarity) are fully coherent institutional designs.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(welfare_boundary_vs_market_integration, preference, 'Whether welfare boundaries are structurally incompatible with free movement or a policy choice').

omega_variable(
    ecj_authority_expansion_mechanism,
    'Does ECJ jurisprudential expansion of mobility rights constitute genuine legal reasoning from the Treaty, or does it reflect institutional capture where the Court has become an advocate for integration rather than an impartial arbiter?',
    'Textual analysis of Treaty provisions on free movement vs. social policy; historical comparison of ECJ mobility jurisprudence vs. directives that were explicitly rejected or narrowed by Council; measurement of divergence between ECJ rulings and member state legislative intent at time of accession',
    'If genuine legal reasoning: ECJ authority is legitimate. If captured advocacy: the constraint''s suppression mechanism (legal enforcement) reflects the Court''s institutional position rather than democratically-grounded law; the piton perspective becomes primary; alternative institutional designs (member state referendums on welfare eligibility, subsidiarity reforms) become viable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecj_authority_expansion_mechanism, empirical, 'Whether ECJ authority expansion reflects legal interpretation or institutional capture').

omega_variable(
    displaced_worker_compensation_feasibility,
    'Could EU-level fiscal transfers and job retraining programs sufficiently compensate displaced local workers such that the constraint would reclassify from snare to tangled-rope or scaffold from the powerless worker perspective?',
    'Quantitative analysis of wage depression and job displacement caused by intra-EU mobility; cost modeling of adequate compensation and retraining programs; political feasibility assessment of EU-level revenue mechanisms to fund these transfers',
    'If feasible at scale: the integration-primary reading remains coherent but incomplete; coupling with genuine solidarity mechanisms (selective-solidarity reading) becomes viable institutional design. If infeasible: the snare classification is durable; the constraint structurally depends on bearing costs on displaced workers without compensation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(displaced_worker_compensation_feasibility, empirical, 'Whether adequate compensation for displaced workers is fiscally and politically feasible').

omega_variable(
    reading_kernel_identity,
    'Does the federation-membership-obligations kernel admit multiple readings (integration-primary, member-sovereignty-primary, selective-solidarity), or does the ''constitutive'' framing of free movement logically foreclose alternative readings?',
    'Textual analysis of Treaty preamble, founding principles, and core institution design; historical analysis of founding actors'' intentions; examination of whether alternative readings can maintain internal coherence while accepting core EU commitments',
    'If kernel truly admits multiple readings: all three readings remain live policy options, and the sibling relations (coexists_with, influences) are appropriate. If integration-primary reading forecloses alternatives: the constraint exhibits foreclosure relation, and member-sovereignty-primary reading is incoherent within EU framework (member states chose integration and cannot logically un-choose it while remaining members).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_identity, conceptual, 'Whether the federation kernel admits multiple coherent readings or foreclosures bind them').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(integration_primary, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(integ_tr_t0, integration_primary, theater_ratio, 0, 0.32).
narrative_ontology:measurement(integ_tr_t10, integration_primary, theater_ratio, 10, 0.42).
narrative_ontology:measurement(integ_tr_t20, integration_primary, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(integ_be_t0, integration_primary, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(integ_be_t10, integration_primary, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(integ_be_t20, integration_primary, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(integration_primary, resource_allocation).
narrative_ontology:affects_constraint(integration_primary, member_sovereignty_primary).
narrative_ontology:affects_constraint(integration_primary, selective_solidarity).
narrative_ontology:affects_constraint(integration_primary, posted_workers_wage_undercutting).
narrative_ontology:affects_constraint(integration_primary, fiscal_burden_shifting_welfare_states).

% DUAL FORMULATION NOTE:
% The integration-primary reading is structurally coupled with its sibling readings (member-sovereignty-primary, selective-solidarity) through a contested kernel. All three readings operate on the same empirical foundation (EU treaty structure, ECJ jurisprudence, labor migration patterns) but prioritize different obligations. Decomposition into separate constraint stories is appropriate per the ε-invariance principle: each reading has a distinct beneficiary/victim structure and produces different classifications from the same perspectives. The shared kernel is recorded in cs_structure; the network affects_constraints edges link downstream effects (wage undercutting in posted-worker scenarios, fiscal burden shifting in welfare states) that emerge differently depending on which reading is operant.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(integration_primary, institutional, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
