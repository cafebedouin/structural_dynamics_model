% ============================================================================
% CONSTRAINT STORY: biotechnology_designer_liability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biotechnology_designer_liability, []).

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
 *   constraint_id: biotechnology_designer_liability
 *   human_readable: Biotechnology Designer Liability and Dual-Use Risk Asymmetry
 *   domain: biotechnology/governance/legal_liability
 *
 * SUMMARY:
 *   Biotechnology designer liability structures create a fundamental
 *   asymmetry: researchers and institutions gain access to powerful genetic
 *   design capabilities while distributing risk exposure to public health
 *   commons and biosafety oversight capacity. The constraint exhibits
 *   characteristics of both coordination (legitimate research coordination
 *   requires shared safety norms) and extraction (researchers benefit from
 *   innovation access while institutional liability remains ambiguous, and
 *   oversight capacity lacks resources to enforce meaningful vetting). The
 *   theater ratio has increased over the measurement interval as synthetic
 *   biology review processes have become more elaborate in ritual while their
 *   functional capacity to prevent dual-use misuse has not grown
 *   commensurately. The constraint can be understood as pure extraction
 *   (snare) from the public health perspective, mixed coordination-extraction
 *   (tangled rope) from the regulatory perspective, or coordination (rope)
 *   from the research institution perspective. Distributed DNA synthesis
 *   screening and international biosafety standards represent an emerging
 *   scaffold structure that could provide a sunset mechanism by creating
 *   alternative accountability pathways.
 *
 * KEY AGENTS:
 *   - Research Institutions and Biotech Corporations: Primary beneficiaries (institutional/arbitrage) — capture innovation benefits with distributed liability; access to cutting-edge genetic design without bearing full downstream responsibility
 *   - Public Health Commons: Primary victim (powerless/trapped) — exposed to dual-use risk with no exit mechanism; bears cost of accidents and malicious misuse without corresponding benefit
 *   - National Biosafety Regulators: Secondary victim and moderate agent (moderate/constrained) — face resource constraints and technical capacity gaps but also benefit from research collaboration; caught between enabling innovation and preventing misuse
 *   - International Biosafety Standard Bodies: Organized coalition (organized/constrained) — WHO, synthetic biology standards committees, DNA synthesis screening networks building alternative verification pathways
 *   - Academic Freedom Institutional Narrative: Theater maintainer (institutional/arbitrage) — perpetuates unrestricted access framing through inertia despite degraded functional capacity
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent dual-use risk allocation as inherent to biological knowledge
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biotechnology_designer_liability, 0.58).
domain_priors:suppression_score(biotechnology_designer_liability, 0.65).
domain_priors:theater_ratio(biotechnology_designer_liability, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biotechnology_designer_liability, extractiveness, 0.58).
narrative_ontology:constraint_metric(biotechnology_designer_liability, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(biotechnology_designer_liability, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biotechnology_designer_liability, tangled_rope).
narrative_ontology:human_readable(biotechnology_designer_liability, "Biotechnology Designer Liability and Dual-Use Risk Asymmetry").
narrative_ontology:topic_domain(biotechnology_designer_liability, "biotechnology/governance/legal_liability").

domain_priors:requires_active_enforcement(biotechnology_designer_liability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biotechnology_designer_liability, research_institutions).
narrative_ontology:constraint_beneficiary(biotechnology_designer_liability, biotech_corporations).
narrative_ontology:constraint_victim(biotechnology_designer_liability, public_health_commons).
narrative_ontology:constraint_victim(biotechnology_designer_liability, biosafety_oversight_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PUBLIC HEALTH COMMONS (SNARE) — Cannot exit the dual-use risk exposure; bears full cost of accidents and malicious misuse. Biosafety oversight capacity lacks enforcement mechanisms, funding, or organizational coherence to challenge the constraint. Maximum extraction with no alternative pathways.
constraint_indexing:constraint_classification(biotechnology_designer_liability, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NATIONAL BIOSAFETY REGULATORS (TANGLED ROPE) — Constrained by resource limitations, jurisdictional fragmentation, and lack of technical capacity to review complex designs. Also benefit from research institution collaboration on threat assessment and legitimate research access. Face asymmetric extraction but retain some coordination function.
constraint_indexing:constraint_classification(biotechnology_designer_liability, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RESEARCH INSTITUTIONS AND BIOTECH CORPORATIONS (ROPE) — Primary beneficiaries. Experience the constraint as coordination: liability structures that permit rapid innovation while maintaining plausible deniability for harmful applications. Access to genetic design tools and publishing networks without bearing full responsibility for downstream misuse. Net extraction flows toward these agents.
constraint_indexing:constraint_classification(biotechnology_designer_liability, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTERNATIONAL BIOSAFETY STANDARD BUILDERS (SCAFFOLD) — Organized agents (WHO, synthetic biology societies, open-source monitoring initiatives) are building alternative accountability pathways: distributed screening of DNA synthesis orders, community-maintained pathogen databases, and federated biosafety assessment. These represent a sunset mechanism — as distributed oversight matures, centralized designer liability becomes less necessary.
constraint_indexing:constraint_classification(biotechnology_designer_liability, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ACADEMIC FREEDOM NARRATIVE (PITON) — The framing that genetic design must remain unrestricted for scientific progress persists through institutional inertia despite degrading verification. Synthetic biology reviews cite academic freedom as justification for minimal liability, but the functional coordination (vetting dangerous designs) has atrophied. The narrative maintains the structure even as its real function decays.
constraint_indexing:constraint_classification(biotechnology_designer_liability, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal perspective, biological knowledge asymmetry appears as an inherent feature of science itself: the same information that enables medical research enables pathogen engineering. This perspective risks naturalizing a contingent institutional arrangement (designer liability regimes that accept dual-use risk) as an immutable law of knowledge.
constraint_indexing:constraint_classification(biotechnology_designer_liability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biotechnology_designer_liability_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(biotechnology_designer_liability, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(biotechnology_designer_liability, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(biotechnology_designer_liability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(biotechnology_designer_liability, TR),
    TR >= 0.70.

:- end_tests(biotechnology_designer_liability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Research institutions and corporations capture significant benefits (publication priority, market access, innovation prestige) during design and deployment phases while distributed liability through designer frameworks creates plausible deniability for harmful applications. The extraction is not maximal because some genuine coordination occurs (biosafety review processes, institutional safety committees) and researchers do internalize some responsibility. However, the asymmetry is substantial: institutions retain benefits regardless of downstream misuse, while oversight systems bear all structural costs. Suppression (0.65): High. Suppression mechanisms are multifaceted: lack of funding and technical capacity for meaningful biosafety review, jurisdictional fragmentation across national systems, information asymmetry (designers have superior knowledge of sequence function), academic freedom narrative that frames safety review as obstruction, and international coordination failures due to competitive advantage seeking. However, suppression is not total — some vetted research does occur, distributed screening systems are emerging, and institutional safety committees represent alternative oversight. Theater ratio (0.58): Moderate-high. Designer liability review processes have become increasingly elaborate in ritual: institutional biosafety committees, ethics boards, publication review — but functional capacity to identify and prevent dual-use misuse has not kept pace with synthetic biology capability advancement. Theater has increased over the measurement interval as the gap between review complexity and actual detection/prevention capacity has widened.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap is between research institutions and public health commons. From the institutional perspective, designer liability is coordination: legitimate research requires shared safety norms, liability frameworks enable innovation by clarifying responsibilities, and distributed screening represents consensus-building. From the public health perspective, designer liability is extraction: researchers benefit from access to tools and publications while the commons bears dual-use risk exposure with no veto or alternative pathway. The regulatory perspective shows tangled rope — moderate agents experience both coordination (biosafety review does prevent some dangerous sequences) and extraction (review resources are inadequate, competitive pressures undermine enforcement, and responsibility ultimately defaults to regulators when institutions claim research continuity exemptions). The scaffold perspective reveals an emerging sunset mechanism: distributed synthesis screening, community pathogen databases, and international standards are building alternative verification pathways that could reduce dependence on designer liability once mature.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from beneficiary/victim status and exit options. Research institutions as beneficiaries with arbitrage-level exit (can publish globally, access design tools across jurisdictions, move research programs internationally) experience low directionality values, producing negative effective extraction from their perspective. Public health commons as victims with trapped-level exit (cannot exit exposure to dual-use risk; no alternative biosafety infrastructure to switch to) experience high directionality values, producing maximum effective extraction from their perspective. National regulators as moderate agents with constrained exit (can slow approval processes but face pressure from competitive advantage concerns, cannot fully exit international research network) experience medium directionality values. The institutional beneficiaries with arbitrage options are incentivized toward rope (coordination) classification; the trapped public health commons toward snare (extraction) classification. The gap between these perspectives reveals the extraction structure.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE CLASSIFICATION RESOLVES MANDATROPHY: The constraint exhibits genuine coordination function (biosafety review processes do prevent some dangerous designs, shared safety norms do constrain some researchers, international standards do exist) AND asymmetric extraction (researchers benefit disproportionately, oversight capacity is underfunded relative to risk, institutions avoid accountability for downstream misuse through liability diffusion). The mandatrophy is resolved by recognizing that both elements are structurally present. The false choice is between 'designer liability is pure safety coordination' (rope) and 'designer liability is pure institutional extraction' (snare). The tangled rope classification captures the real hybrid: genuine safety coordination that is simultaneously asymmetrically exploited. The theater ratio increase signals Goodhart drift — designer liability review has become more elaborate in ritual as its functional capacity to prevent misuse has degraded relative to advancing synthetic biology capability. The constraint remains snare-like from the public health perspective precisely because the tangled rope's coordination function is insufficiently strong to constrain institutional extraction behavior.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    designer_intent_verification,
    'Can the intent and expected use of a genetic design be verified from sequence information alone, or is intent verification inherently incomplete?',
    'Analysis of known dual-use cases: how many misuse incidents involved sequences that passed initial review? Correlation between design intent statements and actual downstream deployment.',
    'If intent is verifiable: liability regimes can function through design-phase review. If intent is opaque: designer liability is largely theater, and the constraint is fundamentally snare-like.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(designer_intent_verification, empirical, 'Whether designer intent can be verified from sequence data').

omega_variable(
    distributed_screening_sufficiency,
    'Do distributed DNA synthesis screening systems (order-level pathogen detection) actually prevent dual-use sequences from reaching users, or do they create false security through complexity?',
    'Evaluation of synthesis screening efficacy: evasion attempt success rates, false negative rates on known pathogenic sequences, time lag between new threat identification and screening update.',
    'If screening is effective: scaffold perspective is justified, sunset mechanism is real. If screening is theater: alternative safety model must address fundamental information asymmetry, and constraint remains snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributed_screening_sufficiency, empirical, 'Whether synthesis order screening prevents dual-use misuse').

omega_variable(
    liability_allocation_incentive_alignment,
    'Does designer liability create incentive alignment for safety, or does it primarily insulate institutions from accountability while concentrating responsibility on individual researchers?',
    'Comparative analysis: do institutions with strong designer liability regimes show lower dual-use incident rates? Do liability frameworks increase or decrease researcher-level safety attention vs shifting focus to plausible deniability?',
    'If liability aligns incentives: tangled rope classification is appropriate. If liability is primarily cover for institutional extraction: constraint becomes snare with institutional beneficiaries and public health victims.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(liability_allocation_incentive_alignment, conceptual, 'Whether designer liability creates or obscures safety incentives').

omega_variable(
    foundational_versus_applied_research_boundary,
    'Is the distinction between foundational genetic research and applied pathogen engineering defensible at the design phase, or is it fundamentally blurred?',
    'Historical analysis of dual-use incidents: what fraction involved sequences that were legitimately defensible as foundational research? Where was the boundary actually crossed?',
    'If boundary is defensible: designer liability can function as coordination. If boundary is inherently blurred: constraint structure is predicated on a false distinction, and liability becomes a cover story rather than a functional mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foundational_versus_applied_research_boundary, empirical, 'Whether foundational-applied research boundary is defensible').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biotechnology_designer_liability, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(biotech_tr_t0, biotechnology_designer_liability, theater_ratio, 0, 0.35).
narrative_ontology:measurement(biotech_tr_t5, biotechnology_designer_liability, theater_ratio, 5, 0.45).
narrative_ontology:measurement(biotech_tr_t10, biotechnology_designer_liability, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(biotech_be_t0, biotechnology_designer_liability, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(biotech_be_t5, biotechnology_designer_liability, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(biotech_be_t10, biotechnology_designer_liability, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biotechnology_designer_liability, enforcement_mechanism).
narrative_ontology:affects_constraint(biotechnology_designer_liability, synthetic_biology_publication_norms).
narrative_ontology:affects_constraint(biotechnology_designer_liability, international_biosafety_coordination).
narrative_ontology:affects_constraint(biotechnology_designer_liability, gain_of_function_research_governance).

% DUAL FORMULATION NOTE:
% Designer liability is downstream of dual-use research governance and upstream of specific institutional safety policies. The liability framework has its own extraction asymmetry (institution vs commons) distinct from but coupled with specific research governance constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(biotechnology_designer_liability, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
