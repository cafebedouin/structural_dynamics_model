% ============================================================================
% CONSTRAINT STORY: cultural_memory_decay
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cultural_memory_decay, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: cultural_memory_decay
 *   human_readable: The Digital Dark Age Siphon
 *   domain: social/technological
 *
 * SUMMARY:
 *   The transition from durable paper archives to ephemeral, proprietary
 *   digital formats has created a structural constraint where the systematic
 *   loss of historical context flows upward to beneficiary institutions
 *   (platform operators, data extractors) and downward to victims (future
 *   historians, archival institutions, knowledge commons). The constraint
 *   exhibits a snare structure: archives cannot exit their preservation
 *   mandate, platform operators benefit from format obsolescence and lock-in,
 *   legal frameworks prevent digital preservation even as cultural materials
 *   decay. The theater ratio (0.64) reflects that archival work includes
 *   performative compliance with copyright law, DMCA anti-circumvention
 *   provisions, and licensing agreements that legally prevent preservation.
 *   Meanwhile, the actual preservation mechanism — open formats, distributed
 *   copies, community curation — operates outside the theater. The
 *   extractiveness (0.58) increased over the 20-year interval as proprietary
 *   platforms accumulated dominant share of cultural output, vendors created
 *   deeper lock-in, and legal barriers hardened.
 *
 * KEY AGENTS:
 *   - Platform Operators: Primary beneficiary (institutional/arbitrage) — benefit from proprietary formats, data lock-in, planned obsolescence; can exit preservation requirement anytime by deleting old servers
 *   - Future Historians: Primary victim (powerless/trapped) — cannot access degraded digital records; no exit from temporal arrow; inherit impoverished cultural record
 *   - Archival Institutions: Secondary victim (moderate/constrained) — bear preservation costs; legally constrained by DMCA/copyright; cannot fully exit preservation role without abandoning mission
 *   - Open Knowledge Communities: Organized actor (organized/constrained) — provide alternative coordination (Internet Archive, open standards) but face legal and resource constraints
 *   - Copyright/IP Regime: Institutional framework (institutional/arbitrage) — maintains legal enclosure that prevents preservation; operates largely performatively (DMCA enforcement is selective)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent digital fragility as thermodynamic inevitability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cultural_memory_decay, 0.58).
domain_priors:suppression_score(cultural_memory_decay, 0.68).
domain_priors:theater_ratio(cultural_memory_decay, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cultural_memory_decay, extractiveness, 0.58).
narrative_ontology:constraint_metric(cultural_memory_decay, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(cultural_memory_decay, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cultural_memory_decay, snare).
narrative_ontology:human_readable(cultural_memory_decay, "The Digital Dark Age Siphon").
narrative_ontology:topic_domain(cultural_memory_decay, "social/technological").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cultural_memory_decay, platform_operators).
narrative_ontology:constraint_beneficiary(cultural_memory_decay, proprietary_format_vendors).
narrative_ontology:constraint_victim(cultural_memory_decay, future_historians).
narrative_ontology:constraint_victim(cultural_memory_decay, cultural_commons).
narrative_ontology:constraint_victim(cultural_memory_decay, archival_institutions).
narrative_ontology:constraint_victim(cultural_memory_decay, open_knowledge_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CULTURAL COMMONS (SNARE) — Future historians and knowledge inheritors have no exit from the decay of digital records. Unable to access proprietary formats, abandoned platforms, or server-dependent content. The constraint extracts the possibility of historical understanding itself. Zero degrees of freedom: trapped by temporal arrow and technical dependency.
constraint_indexing:constraint_classification(cultural_memory_decay, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ARCHIVAL INSTITUTION (SNARE) — Librarians and archivists face resource barriers and legal constraints (DMCA, copyright) preventing preservation of digital materials. High costs for format migration, server maintenance, legal uncertainty. Cannot exit preservation role without abandoning cultural stewardship mission. Extraction flows from resource scarcity and legal enclosure.
constraint_indexing:constraint_classification(cultural_memory_decay, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM OPERATOR (ROPE) — Tech companies experience the constraint as coordination: proprietary formats enable business models (data extraction, lock-in, planned obsolescence). The constraint solves the platform's profit optimization problem. High exit options (can pivot, change formats, sunset legacy systems). Extraction runs toward this agent — they are net beneficiaries.
constraint_indexing:constraint_classification(cultural_memory_decay, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN KNOWLEDGE MOVEMENT (TANGLED ROPE) — Decentralized preservation efforts (Internet Archive, open-source archivists, standards bodies) have genuine coordination function (building preservation infrastructure, advocating for open formats) but face enforcement barriers and resource constraints. Benefits from developing alternative technologies but bears extraction through legal pressure, platform hostility, and resource limitation. Hybrid: both coordination and extraction.
constraint_indexing:constraint_classification(cultural_memory_decay, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY REGULATORY FRAMEWORK (PITON) — Copyright and IP law were designed for physical goods and now create theater around digital preservation. DMCA anti-circumvention provisions perform protection but prevent archival access. The legal framework persists through institutional inertia (updating copyright law is hard) even as its function degrades. Archivists cannot preserve legally without violating the law. Theater ratio high because enforcement is selective and performative.
constraint_indexing:constraint_classification(cultural_memory_decay, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a thermodynamic and informatic perspective, entropy always increases: digital formats degrade, server hardware fails, magnetic media oxidizes, technical knowledge required to run old software is lost. Seen from deep time, all digital records are temporary — bitrot is universal law. This perspective risks naturalizing the contingent choices (proprietary formats, vendor lock-in, platform dependence) as inherent to information technology. The engine's false summit detector will reveal this as naturalization.
constraint_indexing:constraint_classification(cultural_memory_decay, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cultural_memory_decay_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cultural_memory_decay, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cultural_memory_decay, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cultural_memory_decay, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cultural_memory_decay, TR),
    TR >= 0.70.

:- end_tests(cultural_memory_decay_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Platform operators extract cultural memory through format proprietariness and planned obsolescence. The extraction is not maximal (0.70+) because some communities are building open alternatives and some digital materials persist robustly. But the trend is clearly upward: as proprietary platforms become the primary repository of cultural material, and as legacy formats become harder to access, extraction mechanisms strengthen. Suppression (0.68): High. Multiple barriers prevent exit or alternatives: legal barriers (DMCA, copyright), technical barriers (closed formats, vendor lock-in), resource barriers (preservation is expensive), and collective action barriers (no individual archive can preserve the entire internet). These are not natural or inevitable but are enforced through law and business model. Theater ratio (0.64): Moderate-high. Copyright compliance and DMCA observance are largely theater — they don't prevent decay, they prevent preservation. Archival work that occurs includes performative compliance with legal frameworks that don't serve preservation function. Open-source archival efforts (Internet Archive, Community Collections) operate with lower theater by avoiding proprietary formats and legal enclosures.
 *
 * PERSPECTIVAL GAP:
 *   The platform operator sees coordination (Rope) — proprietary formats solve the business problem of data lock-in and customer retention. The analyst sees natural law (Mountain) — digital decay is thermodynamic entropy. The open knowledge movement sees a solvable coordination problem with legal barriers (Tangled Rope / Scaffold) — they are building alternative technology (lower extraction) but face legal constraint (higher suppression). The archival institution sees pure extraction (Snare) — they are legally prevented from doing their job. The legacy legal framework sees itself as a coordination mechanism (Rope) for protecting authors but operates as theater (Piton) — DMCA prevents access without preventing decay. The cultural commons sees existential loss (Snare) — historical understanding itself is being extracted from possibility space.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform operators (beneficiary) have arbitrage exit options — they can change business models, delete legacy systems, or migrate to new formats. They experience extraction flowing toward them (low d, negative chi). Future historians (victim) have no exit options — they are trapped recipients of whatever cultural record survives. They experience maximum extraction (high d, high f(d), high chi). Archival institutions (victim) are constrained — they can theoretically exit their preservation mandate but doing so violates their institutional mission and social contract. They experience high extraction but not maximal extraction. Open knowledge communities (organized victim/partial beneficiary) have some agency through alternative technical solutions but face legal and resource constraints preventing full escape. Their d value is moderate — they have some power but significant suppression.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that the 'natural law' framing (bitrot is inevitable, digital decay is thermodynamic law) actually naturalizes a socially constructed extraction mechanism. The analytical observer at the civilizational scope risks seeing Mountain where Snare actually operates. The key resolution: format proprietariness is a policy choice, not a law of physics. Open formats, community archiving, and sustained institutional commitment can drastically reduce decay rates. The snare classification is correct because the extraction mechanism (platform control, legal enclosure, resource scarcity) is socially enforced, not physically inevitable. The false summit in the mountain perspective reveals the naturalizing move: treating contingent institutional choices (proprietary systems, weak archival funding, aggressive copyright enforcement) as immutable constraints of information technology itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    format_obsolescence_mechanism,
    'Is digital format decay primarily a technical inevitability (bitrot, hardware failure) or a socially contingent choice (proprietary formats, lack of investment in preservation)?',
    'Comparison of preservation success rates between open vs proprietary formats; analysis of formats with sustained organizational commitment vs abandoned formats; retroactive study of causes of format loss in historical record',
    'If technical inevitability: mountain classification confirmed, decay rate applies universally. If socially contingent: snare classification confirmed, decay rate is policy-dependent and reversible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(format_obsolescence_mechanism, empirical, 'Whether digital decay is technical or social').

omega_variable(
    archival_capacity_ceiling,
    'What is the true resource cost to preserve digital cultural artifacts at scale, and does current institutional capacity match it?',
    'Cost-benefit analysis of preservation infrastructure; comparison of preservation budgets to volume of digital material created annually; longitudinal study of format migration success rates at library scale',
    'If capacity exists: suppression (0.68) is overstated, constraints are binding law (Mountain). If capacity is absent: extraction mechanism is viable, snare classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(archival_capacity_ceiling, empirical, 'Whether archival capacity can sustain digital preservation').

omega_variable(
    platform_longevity_incentive,
    'Do platform operators have any structural incentive to maintain historical archives, or does business model pressure systematically drive format abandonment and content deletion?',
    'Analysis of platform lifecycle patterns (Instagram Stories, Snapchat, social media platform deaths); comparison of deletion vs preservation rates across platforms; study of archive policies relative to business cycles',
    'If incentive exists: rope perspective is accurate, platforms see preservation as coordination problem. If incentive absent: snare/extraction mechanism is structural, platform operators benefit from decay.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(platform_longevity_incentive, empirical, 'Whether platforms have incentive to preserve history').

omega_variable(
    legal_framework_remediation,
    'Are copyright and DMCA modifications (fair use for archivists, preservation exemptions) legally and politically viable, or are they systemically blocked by institutional interests?',
    'Legislative history of preservation amendments; analysis of lobbying against archival exemptions; study of jurisdictions with different legal frameworks and their preservation outcomes',
    'If viable: piton constraint can transition to scaffold (sunset clause enforceable). If blocked: legal framework remains extractive barrier, snare deepens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_framework_remediation, empirical, 'Whether legal framework can be reformed for preservation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cultural_memory_decay, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cmdecay_tr_t0, cultural_memory_decay, theater_ratio, 0, 0.35).
narrative_ontology:measurement(cmdecay_tr_t10, cultural_memory_decay, theater_ratio, 10, 0.52).
narrative_ontology:measurement(cmdecay_tr_t20, cultural_memory_decay, theater_ratio, 20, 0.64).

% Extraction over time
narrative_ontology:measurement(cmdecay_be_t0, cultural_memory_decay, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(cmdecay_be_t10, cultural_memory_decay, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(cmdecay_be_t20, cultural_memory_decay, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cultural_memory_decay, information_standard).
narrative_ontology:affects_constraint(cultural_memory_decay, platform_vendor_lock_in).
narrative_ontology:affects_constraint(cultural_memory_decay, copyright_enclosure_regime).
narrative_ontology:affects_constraint(cultural_memory_decay, open_format_standardization).

% DUAL FORMULATION NOTE:
% The digital dark age decomposition: (1) format_proprietariness (ε≈0.45, how much business model drives closed formats) affects (2) preservation_resource_scarcity (ε≈0.52, how much preservation costs exceed budget) which affects (3) legal_archival_barrier (ε≈0.38, how much copyright law prevents preservation). All three feed the cultural_memory_decay snare. The stories are distinct: format choice is vendor-driven; resource scarcity is institutional-budget-driven; legal barriers are legislative/lobbying-driven. Decomposition prevents conflating mechanism with symptom.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
