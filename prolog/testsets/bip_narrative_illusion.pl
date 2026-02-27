% ============================================================================
% CONSTRAINT STORY: bip_narrative_illusion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bip_narrative_illusion, []).

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
 *   constraint_id: bip_narrative_illusion
 *   human_readable: The Black Iron Prison (BIP) and Sensory Optimization
 *   domain: philosophical/social/technological
 *
 * SUMMARY:
 *   The Black Iron Prison (BIP) narrative frames the modern world as a
 *   totalizing system created by corporate and institutional power that
 *   maintains human subordination through sensory optimization and narrative
 *   framing. The constraint operates across technological, social, and
 *   philosophical domains: advertising systems, algorithmic content curation,
 *   institutional mythology, and the structure of human perception itself all
 *   function as components of a unified extraction mechanism. From the
 *   perspective of an individual consciousness embedded in this system, exit
 *   appears impossible — the 'reality tunnel' through which subjective
 *   experience is filtered is constructed and maintained by Empire. From the
 *   perspective of the institutional apparatus, the BIP functions as a
 *   coordination mechanism that stabilizes attention flows, consumer
 *   behavior, and social legitimacy. The constraint exhibits high theater
 *   (0.68) — the BIP narrative performs explanatory and motivational work
 *   while remaining epistemically underdetermined. The core ambiguity is
 *   whether the BIP is primarily a narrative illusion (false consciousness
 *   that dissolves upon proper interpretation) or a structural reality
 *   (institutional extraction that requires material reorganization to
 *   escape).
 *
 * KEY AGENTS:
 *   - Individual Consciousness: Primary victim (powerless/trapped) — embedded in sensory optimization systems, perceives world through institutional narrative frameworks with no exit
 *   - Corporate Narrative Apparatus: Primary beneficiary (institutional/arbitrage) — advertising networks, media systems, recommendation algorithms coordinate attention and sustain extraction
 *   - Institutional Power Structures: Secondary beneficiary (institutional/arbitrage) — government, bureaucracy, educational systems maintain legitimacy through BIP mythology
 *   - Countercultural Subcultures: Victims and partial beneficiaries (moderate/constrained) — recognize BIP constraint but remain materially embedded; benefit from distributed niche networks
 *   - Philosophical/Mystical Traditions: Secondary victims (moderate/mobile) — offer alternative frameworks but operate in margins of institutional system
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees systemic constraint but risks naturalizing contingent institutional arrangements as inevitable
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bip_narrative_illusion, 0.58).
domain_priors:suppression_score(bip_narrative_illusion, 0.72).
domain_priors:theater_ratio(bip_narrative_illusion, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bip_narrative_illusion, extractiveness, 0.58).
narrative_ontology:constraint_metric(bip_narrative_illusion, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(bip_narrative_illusion, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bip_narrative_illusion, snare).
narrative_ontology:human_readable(bip_narrative_illusion, "The Black Iron Prison (BIP) and Sensory Optimization").
narrative_ontology:topic_domain(bip_narrative_illusion, "philosophical/social/technological").

domain_priors:requires_active_enforcement(bip_narrative_illusion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bip_narrative_illusion, institutional_power_structures).
narrative_ontology:constraint_beneficiary(bip_narrative_illusion, corporate_narrative_apparatus).
narrative_ontology:constraint_victim(bip_narrative_illusion, individual_epistemic_autonomy).
narrative_ontology:constraint_victim(bip_narrative_illusion, genuine_subjective_experience).
narrative_ontology:constraint_victim(bip_narrative_illusion, countercultural_alternatives).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IMPRISONED CONSCIOUSNESS (SNARE) — Individual agents perceive the world through sensory optimization systems they cannot exit or modify. Corporate and institutional narrative apparatus constructs the 'reality tunnel' within which subjective experience occurs. Maximum extraction: the trapped consciousness funds, sustains, and validates the system that constrains it.
constraint_indexing:constraint_classification(bip_narrative_illusion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SKEPTICAL SUBCULTURE (TANGLED ROPE) — Countercultural agents (mystics, artists, dissidents) recognize the BIP narrative but remain materially embedded in it. They benefit from distributed communication networks and niche communities while bearing costs of marginalization and resource deprivation. Neither full extraction nor clean escape — hybrid constraint.
constraint_indexing:constraint_classification(bip_narrative_illusion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: NARRATIVE APPARATUS (ROPE) — Institutional and corporate actors experience the BIP constraint as coordination infrastructure. Media systems, advertising networks, recommendation algorithms, and narrative frameworks coordinate attention flows and stabilize consumer behavior. Low experience of extraction because these institutions designed and benefit from the system.
constraint_indexing:constraint_classification(bip_narrative_illusion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PHILOSOPHICAL OBSERVER (PITON) — From a civilizational perspective, the BIP narrative exhibits high theater (0.68): mystical language about 'Black Iron' and 'Empire' performs explanatory work while remaining empirically underdetermined. The framing naturalizes institutional power as cosmic inevitability. Theater persists through institutional inertia and narrative appeal, not through demonstrated functional necessity.
constraint_indexing:constraint_classification(bip_narrative_illusion, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: STRUCTURAL LIMITS VIEW (MOUNTAIN) — From the analytical/universal perspective, any system of distributed consciousness faces fundamental verification bottlenecks: subjective experience cannot be directly observed by external agents; narrative frameworks are unavoidable mediation layers; sensory optimization is intrinsic to how information systems work. This perspective risks falsely naturalizing contingent institutional arrangements as invariant structural features of consciousness itself.
constraint_indexing:constraint_classification(bip_narrative_illusion, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bip_narrative_illusion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bip_narrative_illusion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bip_narrative_illusion, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(bip_narrative_illusion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(bip_narrative_illusion, TR),
    TR >= 0.70.

:- end_tests(bip_narrative_illusion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The BIP constraint extracts from individual consciousness through continuous attention capture, behavioral conditioning, and narrative colonization of subjective experience. The extraction is not absolute (some consciousness remains, some resistance emerges) but substantial — individuals fund and validate systems that constrain them. Suppression (0.72): High. Multiple suppression mechanisms operate: technological (algorithmic filtering, notification systems), institutional (media gatekeeping, credential systems), psychological (narrative framing that naturalizes subordination), and epistemological (difficulty accessing alternative worldviews). The suppression is not total but formidable. Theater ratio (0.68): High. The BIP narrative performs significant explanatory work — it accounts for perceived unfairness, powerlessness, and alienation in modern life. But it also obscures the mechanisms through which it operates and risks naturalizing institutional power as cosmic inevitability. The gap between the narrative's explanatory appeal and its empirical precision is substantial.
 *
 * PERSPECTIVAL GAP:
 *   The imprisoned consciousness experiences the BIP as an inescapable Snare — sensory optimization shapes perception, narrative frameworks limit possibilities, exit appears impossible. The institutional apparatus experiences the same constraint as a Rope — coordination of attention and behavior produces stable systems that serve institutional interests. The skeptical subculture experiences a Tangled Rope — they perceive the constraint but remain dependent on institutional infrastructure; resistance and benefit are mixed. The philosophical observer recognizes high theater and risks naturalizing the BIP as a Mountain — an inevitable feature of consciousness itself. The structural limits view (analytical/universal) must wrestle with whether verification bottlenecks in consciousness research justify the mountain classification or whether they simply reflect current methodological limitations.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is determined by each agent's structural position relative to the extraction flow. Individual consciousness: high d (0.90+) due to trapped exit status and victim position — full target of extraction. Institutional apparatus: low d (0.10-0.20) due to arbitrage exit and beneficiary position — benefits from the system. Countercultural subcultures: moderate d (0.55-0.65) due to constrained exit and mixed victim/beneficiary status — neither fully target nor fully beneficiary. The analytical observer at civilizational scope: moderate-high d (0.72) as analytical observer — sees systemic structure but doesn't occupy structural position within it. The derived f(d) values feed into the chi calculation: high-d agents experience larger effective extraction; low-d agents experience smaller (or negative) effective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION STRATEGY: The BIP constraint avoids the mandatrophy (false labeling of coordination as extraction or vice versa) through its perspectival structure. From the trapped individual's view, it is genuinely a Snare (extraction dominates). From the institutional view, it is genuinely a Rope (coordination function is primary). These are not contradictory — they are incompatible measurements from incompatible structural positions. The mandatrophy is resolved by recognizing that no single type caption is correct; the presheaf of perspectives IS the answer. The false summit risk (mountain classification) is addressed by the analytical observer's perspective: the claim that BIP is inevitable/natural is deconstructed as a naturalization of contingent institutional arrangements. The high theater ratio (0.68) detects the performative excess in BIP narrativization — the explanatory apparatus exceeds what empirical evidence supports.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    narrative_vs_structural_extraction,
    'Is the BIP constraint primarily a narrative/epistemological illusion (false consciousness) or a structural/institutional extraction mechanism (real material asymmetry)?',
    'Comparative analysis of material outcomes for agents who reject vs accept BIP framing; measurement of resource flows and decision-making power between institutional actors and individual agents; empirical testing of whether alternative narrative frameworks produce different outcomes',
    'If primarily narrative: constraint is partially self-sustaining through belief — reclassification possible via epistemological shift. If primarily structural: constraint is real regardless of narrative framing — exit requires material reorganization.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(narrative_vs_structural_extraction, conceptual, 'Whether BIP is a narrative illusion or structural extraction mechanism').

omega_variable(
    sensory_optimization_necessity,
    'Can distributed consciousness operate without some form of sensory optimization and narrative mediation, or is the optimization layer an artifact of specific institutional choices?',
    'Historical analysis of pre-industrial narrative systems and attention flows; experimental design of alternative coordination mechanisms with different optimization priorities; comparison of consciousness states across cultures with radically different sensory optimization frameworks',
    'If optimization is structurally necessary: BIP is closer to Mountain — inevitable constraint on consciousness. If contingent: BIP is structurally a Snare of specific institutional choices.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sensory_optimization_necessity, empirical, 'Whether sensory optimization is intrinsic to consciousness or institutionally contingent').

omega_variable(
    empire_vs_polycentric_power,
    'Is institutional power organized as a unified ''Empire'' (singular control) or as polycentric overlapping hierarchies without central controller?',
    'Network analysis of institutional decision-making; identification of unified command structures vs distributed coordination failures; testing whether ''Empire'' is explanatory or metaphorical',
    'If unified: BIP framing is structurally accurate; single extraction mechanism. If polycentric: BIP narrative oversimplifies; multiple intersecting Snares and Tangled Ropes, not singular Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empire_vs_polycentric_power, empirical, 'Whether institutional power is unified Empire or polycentric structure').

omega_variable(
    exit_mechanism_viability,
    'Do genuine exit mechanisms exist from the BIP constraint (alternative consciousness states, countercultural networks, mystical practices), or are all apparent exits merely repositioning within the same constraint?',
    'Longitudinal tracking of agents claiming successful BIP exit; measurement of whether alternative frameworks produce materially different outcomes; analysis of whether countercultural movement is structurally independent or parasitic on mainstream systems',
    'If viable exits exist: constraint is closer to Tangled Rope from subculture perspective — real alternatives available. If exits are illusions: constraint is closer to pure Snare — all apparent alternatives are narrative repositioning.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exit_mechanism_viability, empirical, 'Whether genuine exits from BIP narrative exist').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bip_narrative_illusion, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bip_tr_t0, bip_narrative_illusion, theater_ratio, 0, 0.45).
narrative_ontology:measurement(bip_tr_t5, bip_narrative_illusion, theater_ratio, 5, 0.58).
narrative_ontology:measurement(bip_tr_t10, bip_narrative_illusion, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(bip_be_t0, bip_narrative_illusion, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(bip_be_t5, bip_narrative_illusion, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(bip_be_t10, bip_narrative_illusion, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bip_narrative_illusion, information_standard).
narrative_ontology:affects_constraint(bip_narrative_illusion, algorithmic_attention_capture).
narrative_ontology:affects_constraint(bip_narrative_illusion, narrative_mythology_enforcement).
narrative_ontology:affects_constraint(bip_narrative_illusion, sensory_substitution_dependency).

% DUAL FORMULATION NOTE:
% BIP constraint decomposes into three structurally distinct sub-constraints: (1) algorithmic attention capture (technical/economic extractiveness), (2) institutional narrative mythology (social/epistemological extractiveness), (3) sensory optimization dependency (cognitive/existential extractiveness). Each has different ε values and different materialization pathways. The unified 'Black Iron Prison' narrative conflates these three constraints into a single explanatory frame — this conflation is itself part of the theater mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bip_narrative_illusion, moderate, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
