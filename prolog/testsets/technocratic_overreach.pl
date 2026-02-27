% ============================================================================
% CONSTRAINT STORY: technocratic_overreach
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technocratic_overreach, []).

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
 *   constraint_id: technocratic_overreach
 *   human_readable: The Rule of the Expert (Technocratic Overreach)
 *   domain: political/technological
 *
 * SUMMARY:
 *   Technocratic overreach describes the structural condition where
 *   administrative or technical experts impose constraints under legitimating
 *   narratives of optimization, safety, or efficiency, while systematically
 *   removing ordinary people's capacity to participate in decisions that
 *   affect them. This constraint exhibits multiple structural faces depending
 *   on the observer's position. For the expert administrator, it is a
 *   coordination mechanism solving collective action
 *   problems—standardization, consistency, safety optimization. For the
 *   disenfranchised public, it is a snare: the constraints are enforceable,
 *   inescapable, and imposed without meaningful participation. For organized
 *   movements advocating participatory governance, it is a temporary problem
 *   with a structural exit—mechanisms like citizen assemblies and community
 *   oversight are building alternative pathways. For the credentialing system
 *   itself, it is a piton: the gatekeeping function that reserves expertise
 *   to the credentialed is maintained partly through institutional inertia
 *   and partly through performative credentialing rituals rather than through
 *   genuine verification of superior competence. The analytical observer
 *   risks naturalizing expert rule as an immutable law of industrial
 *   society—a mountain—but the structural data reveals this as a false
 *   summit: institutional design choices could maintain technical integrity
 *   while restoring meaningful participation.
 *
 * KEY AGENTS:
 *   - Expert Administrators: Primary beneficiary (institutional/arbitrage) — capture policy authority, prestige, budget allocation; have exit options across agencies, consulting, international positions
 *   - Disenfranchised Publics: Primary victim (powerless/trapped) — subject to constraints without voice; cannot exit or influence technical decisions that govern their lives
 *   - Local Community Organizations: Secondary victim (moderate/constrained) — benefit from coordination functions but lose local control and adaptive capacity; have some agency through advocacy but face resource asymmetry
 *   - Participatory Democracy Movement: Organized agent (organized/constrained) — building exit pathways through citizen assemblies, public review boards, participatory budgeting; constrained by institutional resistance but seeing genuine structural change
 *   - Credentialing System: Institutional gatekeeper (institutional/arbitrage) — maintains exclusive authority through degree requirements and professional licensing; preserves scarcity that justifies expert monopoly
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks conflating genuine expertise need with excessive gatekeeping
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technocratic_overreach, 0.58).
domain_priors:suppression_score(technocratic_overreach, 0.68).
domain_priors:theater_ratio(technocratic_overreach, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technocratic_overreach, extractiveness, 0.58).
narrative_ontology:constraint_metric(technocratic_overreach, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(technocratic_overreach, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technocratic_overreach, tangled_rope).
narrative_ontology:human_readable(technocratic_overreach, "The Rule of the Expert (Technocratic Overreach)").
narrative_ontology:topic_domain(technocratic_overreach, "political/technological").

domain_priors:requires_active_enforcement(technocratic_overreach).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technocratic_overreach, expert_administrators).
narrative_ontology:constraint_beneficiary(technocratic_overreach, credentialed_technocrats).
narrative_ontology:constraint_victim(technocratic_overreach, disenfranchised_publics).
narrative_ontology:constraint_victim(technocratic_overreach, local_decision_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISENFRANCHISED PUBLIC (SNARE) — Citizens subject to expert-mandated constraints (zoning, infrastructure design, algorithmic resource allocation) have no meaningful exit and no voice in the technical decisions that govern their lives. The constraint is enforced through administrative mechanisms that bypass democratic deliberation. Cannot exit, cannot influence, cannot organize effectively against distributed technical authority.
constraint_indexing:constraint_classification(technocratic_overreach, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LOCAL COMMUNITY ORGANIZATION (TANGLED ROPE) — Local groups benefit from infrastructure coordination (safety standards, resource distribution) but also bear extraction costs through loss of local control and inability to adapt expertise to context. Some agency through protest and advocacy, but constrained by technical asymmetry and resource disparity. Mixed experience: genuine coordination function plus asymmetric power exertion.
constraint_indexing:constraint_classification(technocratic_overreach, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: EXPERT ADMINISTRATOR (ROPE) — Technocrats experience the constraint as a coordination mechanism that solves collective action problems: standardization, safety optimization, and technical consistency. They have arbitrage options (move between agencies, consult, advise governments). The constraint appears as a beneficial coordination tool, not as extraction. Extraction runs toward this agent through prestige, budget authority, and policy influence.
constraint_indexing:constraint_classification(technocratic_overreach, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PARTICIPATORY DEMOCRACY MOVEMENT (SCAFFOLD) — Organized civic movements (citizen assemblies, community review boards, public comment mechanisms) are building exit pathways from pure technocratic rule. These mechanisms introduce structured participation that gives voice to the disenfranchised without abandoning technical expertise. The constraint has a sunset: as participatory methods mature and prove functional, the exclusive rule of unaccountable experts loses legitimacy and force. Theater ratio is high in participatory processes themselves, but they represent genuine institutional change, not mere performance.
constraint_indexing:constraint_classification(technocratic_overreach, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: CREDENTIALING SYSTEM (PITON) — University degrees, professional licenses, and certification boards maintain the gatekeeping function that reserves decision-making authority for the credentialed. The original function was to ensure quality; the current function is substantially performative—maintaining credential value through scarcity and excluding alternative knowledge forms. The credentialing system is maintained through institutional inertia despite evidence that expertise is context-dependent and that non-credentialed practitioners often perform comparably on domain tasks. Theater ratio is very high; actual verification of claimed expertise is low.
constraint_indexing:constraint_classification(technocratic_overreach, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURALIZATION VIEW (MOUNTAIN) — From a civilizational perspective, technical expertise does create genuine coordination problems that require delegation to specialists. Complex systems (power grids, water treatment, pharmaceutical safety) cannot be governed by pure majoritarian democracy without losing their protective functions. From this view, some degree of expert authority is an immutable feature of modern industrial society—a structural property that cannot be eliminated without abandoning the goods (public health, infrastructure reliability) that technical expertise provides. However, this perspective risks naturalizing the degree of disenfranchisement as necessary, when institutional design choices (transparency, accountability mechanisms, participatory oversight) could mitigate extraction without sacrificing technical integrity.
constraint_indexing:constraint_classification(technocratic_overreach, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technocratic_overreach_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(technocratic_overreach, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(technocratic_overreach, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(technocratic_overreach, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(technocratic_overreach, TR),
    TR >= 0.70.

:- end_tests(technocratic_overreach_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Expert administrators and credentialed professionals extract career authority, prestige, and policy influence from the constraint. The extraction is significant—ordinary people's disenfranchisement is the direct mechanism that creates space for expert authority. However, extractiveness is not as extreme as pure predation (0.70+) because legitimate coordination functions exist: technical standards do solve real problems, safety protocols do prevent harm, and some delegation to specialists is genuinely necessary in complex systems. The constraint conflates justified expertise with unjustified gatekeeping. Suppression (0.68): High. Multiple barriers prevent citizen participation in technical decision-making: technical jargon creates informational asymmetries, credentialing requirements restrict who can speak authoritatively, administrative procedures are opaque and slow, and the default assumption is that ordinary people lack capacity for technical judgment. The primary victims (the disenfranchised public) cannot easily exit, form alternative governance structures, or challenge expert determinations. Theater ratio (0.64): Moderate-high and rising. Public comment periods, expert advisory boards, and consultation processes often function as performative windows: the appearance of participation without meaningful power to modify expert conclusions. The theater ratio has increased over the measurement interval as participatory mechanisms have proliferated while actual citizen influence has remained constrained. Credentialing processes themselves have significant theater—degree requirements serve gatekeeping functions as much as competence verification.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the maximal perspectival divergence in the corpus. The expert administrator (institutional/arbitrage) sees Rope: coordination that solves genuine collective action problems. The disenfranchised public (powerless/trapped) sees Snare: constraints that are unescapable, uninfluenceable, and extractive. The participatory democracy movement (organized/constrained) sees Scaffold: a temporary structural arrangement being replaced by participatory mechanisms with genuine power. The credentialing system (institutional/arbitrage) manifests as Piton: gatekeeping functions maintained through inertia despite weak verification of claimed expertise. The analytical observer (analytical/analytical) risks seeing Mountain: the naturalization of expert rule as inevitable in complex systems. No single perspective is false—they are all structurally accurate from their positions. The constraint's extractiveness depends on whether accountability mechanisms exist (moving toward Rope) or whether experts operate without external verification (moving toward Snare).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from the structural position of each agent relative to the extraction flow. Expert administrators are beneficiaries with arbitrage exit options—they derive low or negative d values from the constraint derivation, experiencing it as beneficial coordination. The disenfranchised public are victims with trapped exit options—they derive high d values (0.85-0.95), experiencing maximum effective extraction. Local organizations are victims with constrained exit options—they derive moderate-high d values (0.55-0.65). The organized participatory movement has constrained exit but is building alternatives—moderate d values (0.45-0.55) that reflect partial agency. The credentialing system as an institutional structure has arbitrage exit (can move across sectors)—low d values similar to the expert administrator. The analytical observer uses canonical d values (0.73 for analytical power atoms) that reflect detachment from the direct extraction mechanism. The measurement interval shows extractiveness rising while theater ratio also rises, indicating that the constraint is becoming more performative (more public participation theater) while remaining more extractive in practice (disenfranchisement persisting despite new participatory mechanisms).
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC CASE: Technocratic overreach resolves the mandatrophy by demonstrating that expert-imposed coordination is not automatically benign. The classification pathway distinguishes between (a) genuine coordination functions (where expertise genuinely solves collective action problems with broad net benefit), and (b) extraction disguised as coordination (where expertise is used to exclude participation and centralize authority beyond what coordination requires). The Tangled Rope classification at the primary beneficiary level indicates both a real coordination function (safety standards, technical consistency) and asymmetric extraction (disenfranchisement, removal of decision-making authority). The Snare classification from the disenfranchised public's perspective indicates that the costs are unevenly distributed and justified primarily through expert authority rather than through demonstrable net benefit to all parties. The Scaffold perspective indicates that participatory mechanisms represent a genuine structural exit, not mere theater, though this is an omega variable rather than established fact. The Piton classification for the credentialing system reveals that gatekeeping is maintained partly through performative credential requirement rather than through rigorous verification of expertise. The false Mountain (analytical observer at civilizational scope) reveals the mandatrophy most clearly: the risk is naturalizing as inevitable what is actually a contingent institutional choice. Complex technical systems do require expertise delegation, but the degree of disenfranchisement is not determined by technical necessity—it is a policy choice. Institutional design could maintain technical expertise while restoring meaningful participation. The mandatrophy is resolved by showing that all six types are legitimate readings of the same structural phenomenon, and the question 'which type is correct?' is actually 'what design choices do we make about accountability, transparency, and participation?' The constraint itself (the structural feature of delegation to experts) is invariant; the extraction level and the type classification depend on institutional choices about how experts are selected, how they are held accountable, and whether their decisions can be meaningfully influenced.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    expertise_context_dependency,
    'To what degree is expert decision-making genuinely superior to informed citizen participation on domain-specific problems, versus to what degree does expert superiority depend on excluding citizen input to preserve professional authority?',
    'Comparative outcome analysis: randomized trials of expert-only vs participatory governance on identical problems; longitudinal tracking of implementation success and public satisfaction; meta-analysis of cases where non-credentialed practitioners achieved equivalent or superior outcomes.',
    'If expertise is contextually dependent: some decisions should devolve to local actors with domain knowledge. If expertise is universally superior: centralized expert authority is justified. The current constraint assumes universal superiority; evidence suggests conditional superiority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expertise_context_dependency, empirical, 'Whether expert authority is context-dependent or universal').

omega_variable(
    participation_mechanism_sufficiency,
    'Do structured participation mechanisms (citizen assemblies, public comment periods, community review boards) provide meaningful veto or modification power, or are they performative windows that preserve expert decision-making authority while simulating democratic input?',
    'Analysis of formal power: frequency and scope of citizen recommendations that reverse expert proposals; budget allocation to participation vs implementation; comparison of community-identified problems versus expert-prioritized problems in actual policy outcomes.',
    'If participatory mechanisms have genuine power: scaffold classification is structural and the sunset is real. If they are performative: the constraint remains snare or tangled rope for the disenfranchised; scaffold is aspirational theater.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(participation_mechanism_sufficiency, empirical, 'Whether participation mechanisms have genuine decision-making power').

omega_variable(
    knowledge_form_legitimacy,
    'Which forms of knowledge (credentialed expertise, experiential knowledge, traditional ecological knowledge, community-based evidence) should hold authority in technical decision-making, and on what grounds?',
    'Documentation of cases where diverse knowledge forms produced superior outcomes; analysis of legitimation criteria (reproducibility, scale, consistency); examination of how different governance systems weight different knowledge forms.',
    'If only credentialed expertise is legitimate: the constraint is justified as preventing unqualified decision-making. If multiple knowledge forms are legitimately authoritative: the constraint is excessive gatekeeping that disenfranchises non-credentialed knowledge holders. If knowledge legitimacy is domain-specific: rules must differentiate between domains where credentialing is necessary (pharmaceuticals) and domains where it is not (community design, resource allocation).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(knowledge_form_legitimacy, conceptual, 'Which knowledge forms should hold authority in technical decision-making').

omega_variable(
    accountability_mechanism_enforcement,
    'When expert decisions cause harm or fail to achieve stated objectives, what accountability mechanisms exist, and are they sufficient to deter negligence or capture?',
    'Audit of expert decision reversals due to harm; analysis of consequences for experts whose decisions fail; comparison of expert liability across jurisdictions; case studies of major expert-driven policy failures.',
    'If accountability is strong: expert authority is constrained by external verification. If accountability is weak: experts can impose costs with minimal consequence, enabling extraction. Accountability mechanism strength determines whether expert rule degrades to snare or remains tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accountability_mechanism_enforcement, empirical, 'Sufficiency of accountability mechanisms for expert decisions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technocratic_overreach, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t0, technocratic_overreach, theater_ratio, 0, 0.45).
narrative_ontology:measurement(tech_tr_t10, technocratic_overreach, theater_ratio, 10, 0.58).
narrative_ontology:measurement(tech_tr_t20, technocratic_overreach, theater_ratio, 20, 0.64).

% Extraction over time
narrative_ontology:measurement(tech_be_t0, technocratic_overreach, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(tech_be_t10, technocratic_overreach, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(tech_be_t20, technocratic_overreach, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technocratic_overreach, enforcement_mechanism).
narrative_ontology:affects_constraint(technocratic_overreach, regulatory_capture).
narrative_ontology:affects_constraint(technocratic_overreach, credentialing_gatekeeping).
narrative_ontology:affects_constraint(technocratic_overreach, administrative_opacity).

% DUAL FORMULATION NOTE:
% Technocratic overreach is upstream of regulatory capture (regulatory experts capture agencies to serve industry) and credentialing gatekeeping (professions use credentials to exclude competitors). The extractiveness of technocratic overreach itself derives from the degree to which expert authority is unaccountable and from the breadth of domains where expertise is claimed to be necessary.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(technocratic_overreach, organized, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
