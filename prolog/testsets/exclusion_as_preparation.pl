% ============================================================================
% CONSTRAINT STORY: exclusion_as_preparation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_exclusion_as_preparation, []).

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
 *   constraint_id: exclusion_as_preparation
 *   human_readable: Social Exclusion as Preparation for Material Reception in Eusocial Colonies
 *   domain: political_economy/labor_systems/epistemic_infrastructure
 *
 * SUMMARY:
 *   In eusocial insect colonies, particularly termites and ants, a structural
 *   constraint operates at the intersection of individual senescence, colony
 *   resource allocation, and nest integrity maintenance. When an individual's
 *   participation in nest-touch and communal coordination activities declines
 *   below a threshold (typically 40-60% of baseline), the colony orchestrates
 *   social ejection: workers increase exclusionary behavior, the individual
 *   is physically removed from communal spaces, and alternative resource
 *   flows are redirected toward remaining productive members. This constraint
 *   is a diagnostic exemplar for how the same structural phenomenon can
 *   appear as natural law, pure coordination, mixed coordination-extraction,
 *   or pure extraction depending on the observer's epistemic position. The
 *   constraint coordinates a genuine problem—preparing the colony for
 *   individual loss before senescent dysfunction compromises structural
 *   integrity—but achieves this through asymmetric labor (workers perform
 *   exclusion) and irreversible extraction (the ejected individual's
 *   evolutionary lineage terminates). The claim that exclusion is
 *   'preparation for material reception' refers to the hypothesis that the
 *   social ritual is coupled to material changes in the nest (resource
 *   redistribution, architectural modification, care provision) that enable
 *   smooth transition to post-individual conditions. Testing this hypothesis
 *   distinguishes between the scaffold interpretation (exclusion is genuine
 *   adaptive transition support) and the snare interpretation (exclusion is
 *   pure termination with no functional coupling to preparation).
 *
 * KEY AGENTS:
 *   - Excluded Individual: Primary victim (powerless/trapped) — ejected from communal spaces, denied resource access, forced into senescence or death. No alternatives; no exit capacity.
 *   - Colony Worker Collective: Secondary victim and enforcement agent (moderate/constrained) — perform exclusion labor and bear structural risk; also benefit from resource protection. Mixed position creates tangled_rope classification.
 *   - Colony Queen/Reproductive Authority: Primary beneficiary (institutional/arbitrage) — exclusion protects her resource access and breeding monopoly. Can modulate signaling to trigger or suppress mechanism. Experiences rope (pure coordination from her standpoint).
 *   - Eusocial Lineage (Multi-Generational): Evolved system (organized/constrained) — benefits from efficient senescence management but extracts from excluded individuals at scale of evolutionary fitness. Tangled rope from generational perspective.
 *   - Senescence Detection System: Transient coordination structure (organized/constrained) — functions as biological scaffold enabling graduated transition from productive to senescent states. Scaffold classification reflects its natural sunset (individual death).
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangement as immutable feature of eusocial organization. False summit candidate.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exclusion_as_preparation, 0.52).
domain_priors:suppression_score(exclusion_as_preparation, 0.48).
domain_priors:theater_ratio(exclusion_as_preparation, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exclusion_as_preparation, extractiveness, 0.52).
narrative_ontology:constraint_metric(exclusion_as_preparation, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(exclusion_as_preparation, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exclusion_as_preparation, tangled_rope).
narrative_ontology:human_readable(exclusion_as_preparation, "Social Exclusion as Preparation for Material Reception in Eusocial Colonies").
narrative_ontology:topic_domain(exclusion_as_preparation, "political_economy/labor_systems/epistemic_infrastructure").

domain_priors:requires_active_enforcement(exclusion_as_preparation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exclusion_as_preparation, colony_survival_coordination).
narrative_ontology:constraint_beneficiary(exclusion_as_preparation, nest_structural_integrity).
narrative_ontology:constraint_victim(exclusion_as_preparation, excluded_individual).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED INDIVIDUAL (SNARE) — Once nest-touch participation falls below threshold (~40-60% baseline), ejection is irreversible within the colony's decision-making system. The individual cannot re-enter; their social position is terminally degraded. No alternatives exist; death or dispersal are the only outcomes. Maximum suppression and extractiveness from this agent's standpoint: stripped of social recognition, removed from resource access, forced into terminal senescence or predation exposure. The constraint exists precisely to achieve irreversible isolation.
constraint_indexing:constraint_classification(exclusion_as_preparation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: COLONY WORKER COLLECTIVE (TANGLED ROPE) — Workers both benefit from and bear costs of the exclusion mechanism. Genuine coordination function: the ritual reliably signals which individuals are approaching senescence or infection-driven dysfunction, enabling the colony to redirect resources before nest-structural integrity fails. Asymmetric extraction: workers perform exclusion labor (physical ejection, nest-sealing) and bear the risk if senescent individuals attempt retaliatory damage. Constrained exit: workers cannot opt out of participation without violating the chemical signaling system that sustains colony cohesion. The constraint serves coordination (protect structural integrity) but requires asymmetric labor from workers (enforcement cost).
constraint_indexing:constraint_classification(exclusion_as_preparation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: COLONY QUEEN / REPRODUCTIVE AUTHORITY (ROPE) — The queen experiences the exclusion mechanism as pure coordination: it removes threats to colony survival and maintains stable breeding conditions. No extraction directed at the queen; instead, the mechanism protects her reproductive monopoly and resource access. The queen can arbitrage: she can modulate pheromonal signals that trigger or suppress exclusion behavior, and can retarget excluded individuals' resources toward her brood. The constraint coordinates colony homeostasis; the queen is the primary beneficiary of this coordination.
constraint_indexing:constraint_classification(exclusion_as_preparation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: EUSOCIAL LINEAGE / EVOLUTIONARY VIEW (TANGLED ROPE) — From a multi-generational evolutionary perspective, the exclusion ritual solves a genuine coordination problem: termite and ant colonies face existential pressure to shed non-productive members before they compromise nest integrity. The constraint has low theater (it works reliably) and genuine coordination function (resource allocation efficiency). But it also extracts from excluded individuals: their evolutionary lineage terminates; their genetic contribution ceases. This is asymmetric extraction at the scale of generations. The mechanism is active and enforced (pheromonal signaling, worker behavior); it persists because it solves the coordination problem reliably.
constraint_indexing:constraint_classification(exclusion_as_preparation, tangled_rope,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: SENESCENCE DETECTION SYSTEM / TRANSIENT STRUCTURE VIEW (SCAFFOLD) — The exclusion ritual is a temporary coordination structure that operates only during the individual's transition from productive to senescent state. Once ejection occurs, the mechanism is complete — no ongoing suppression is required. The constraint has a natural sunset: individual death. The low theater ratio (0.35) reflects that this mechanism is functional, not performative — it achieves its coordination goal (signal senescence, prepare nest for loss of individual) efficiently. From this view, the exclusion is a graduated social scaffold supporting the colony's adaptation to individual mortality, not an extractive trap.
constraint_indexing:constraint_classification(exclusion_as_preparation, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal evolutionary perspective, social exclusion of senescent individuals appears as an immutable feature of eusocial organization: any colony that failed to shed non-productive members would accumulate resource drains and structural compromise, losing evolutionary fitness. The mechanism appears unchangeable because the alternative (no exclusion) is strongly selected against. However, this perspective risks naturalizing what is actually a contingent constraint engineered by the colony's chemical signaling system. The exclusion mechanism is not a law of nature; it is a specific institutional arrangement (pheromone-based decision rule, worker behavior norms) that could theoretically be altered through changes to signaling or behavioral thresholds. The mountain classification marks this perspective as a false summit candidate.
constraint_indexing:constraint_classification(exclusion_as_preparation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(exclusion_as_preparation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(exclusion_as_preparation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(exclusion_as_preparation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(exclusion_as_preparation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(exclusion_as_preparation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts from excluded individuals (irreversible removal, resource termination, fitness loss) while providing coordination benefits to the colony. The value reflects that the mechanism is not pure extraction (it genuinely solves a coordination problem) but also not pure coordination (it asymmetrically targets one individual for termination). The 0.52 value sits at the tangled_rope/snare boundary, reflecting the ambiguity about whether the 'preparation' function is genuinely coupled to material changes or is post-hoc rationalization. Suppression (0.48): Moderate. The exclusion mechanism operates through chemical signaling and behavioral enforcement, not through overwhelming physical force. Excluded individuals could theoretically attempt resistance (aggressive defense, nest damage), but the coordination-based framework (pheromonal commitment, worker consensus) makes sustained resistance evolutionarily maladaptive. The suppression is real but not absolute; it relies on the individual's inability to benefit from defection. Theater ratio (0.35): Low-moderate. The exclusion ritual involves genuine functional coordination (detecting senescence, reallocating resources) alongside performative elements (choreographed ejection, nest-sealing ceremony). The low theater reflects that the mechanism works reliably; it is not sustained through purely symbolic performance. This distinguishes it from piton-type constraints where the ritual persists through inertia despite degraded function.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap centers on whether the exclusion mechanism is genuinely coupled to material preparation (supporting scaffold/tangled_rope interpretations) or operates as pure termination (supporting snare interpretation). The excluded individual sees pure snare: irreversible extraction with no coordination benefit from their standpoint. The worker collective sees tangled rope: genuine coordination (resource protection) requiring asymmetric labor (exclusion cost). The queen sees rope: pure coordination serving her strategic interests. The eusocial lineage sees tangled rope at generational scale: the mechanism benefits the collective while extracting from excluded individuals—a classic hybrid. The scaffold perspective sees the exclusion as a transient structure with natural sunset (individual mortality), supporting graduated transition. The analytical observer risks seeing a mountain (immutable feature of eusocial organization) but faces falsification from the directed evidence that the mechanism is chemically mediated and theoretically tunable. The gap reveals that 'preparation' is the crux: if material preparation is genuinely coupled (nest changes, resource flows, care provision), the scaffold and tangled_rope interpretations hold; if coupling is absent, the snare interpretation is validated.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality computation derives from the agent's structural relationship to the constraint. The excluded individual occupies the full-target position (d ≈ 0.95): trapped, powerless, bearing extraction with no alternatives. The worker collective occupies a mixed position (d ≈ 0.55): organized, constrained, both benefiting (resource protection) and bearing costs (exclusion labor). The queen occupies the full-beneficiary position (d ≈ 0.10): institutional, arbitrage-capable, shielded from extraction, able to modulate the mechanism. The eusocial lineage at the generational scale occupies a paradoxical position: the mechanism benefits the colony's fitness but extracts from excluded individuals, creating a collective-action framing where the group benefits while subsets are sacrificed. The analytical observer's position (d ≈ 0.72) is elevated because the observer cannot easily exit the analytical framing (identity_locked in institutional discourse about natural law), yet sees structure that challenges the natural-law interpretation. The directionality overrides are not needed; the standard derivation captures the structural relationships adequately.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint solves a genuine collective-action problem (senescence management) while operating through asymmetric extraction (targeting the senescent individual for termination). This is the canonical tangled_rope structure: both real coordination function (colony survival) and real extraction (irreversible individual loss) exist in the same mechanism. The constraint is not mislabeled as pure coordination (rope) because the extraction cost is material and directed. It is not mislabeled as pure extraction (snare) because the coordination function is genuine—the alternative (no exclusion) would lead to colony-level fitness loss. The snare perspective from the excluded individual's standpoint is their genuine experience, but not the system-level classification. The key to mandatrophy resolution is that tangled_rope correctly captures the hybrid nature: active enforcement (required for the tangled_rope gate) is present; beneficiaries (the colony) and victims (the excluded individual) are clearly identified; the coordination function (senescence detection and resource reallocation) is real. The mechanism's extractiveness (0.52) and suppression (0.48) both sit in the tangled_rope region of metric space, confirming the classification. The mandatrophy does not hide here; it is transparently resolved by the perspectival structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    senescence_threshold_determination,
    'What observable parameters (nest-touch frequency, pheromone concentration, metabolic rate) determine the 40-60% participation threshold at which exclusion is triggered?',
    'Experimental manipulation of individual participation rates while measuring colony pheromone profiles and behavioral response timing. Cross-species comparison of threshold values across termite and ant species to identify whether threshold is adaptive-specific or universal structural constant.',
    'If threshold is hardwired (universal constant): strengthens mountain classification. If threshold is tunable (responsive to colony condition, resource scarcity, threat level): weakens mountain, supports tangled_rope or scaffold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(senescence_threshold_determination, empirical, 'Observable parameters determining exclusion threshold').

omega_variable(
    individual_agency_in_participation_decline,
    'Is the decline in nest-touch participation a result of the individual''s intrinsic senescence (biological drift) or is it actively suppressed by colony workers (social mechanism)?',
    'Behavioral tracking of participation rates before and after experimental isolation of individuals from colony workers. Measurement of whether voluntary participation decline precedes or follows aggressive exclusion behavior.',
    'If intrinsic senescence drives participation decline: constraint is a detection mechanism (lower extraction interpretation). If colony workers actively suppress participation to trigger threshold: constraint is a constructed mechanism for controlled ejection (higher extraction interpretation, snare classification strengthens).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(individual_agency_in_participation_decline, empirical, 'Whether participation decline is intrinsic senescence or active suppression').

omega_variable(
    material_preparation_coupling,
    'Is the social exclusion ritual actually coupled to material preparation for the individual''s transition (nest architecture changes, resource redistribution, care provision for senescent individuals), or is exclusion simply a termination event?',
    'Temporal mapping of exclusion events against nest structural changes, resource flow changes, and any observed care provision for ejected individuals. Analysis of whether exclusion serves a transition function (scaffold) or a termination function (snare).',
    'If strongly coupled to material preparation: scaffold and rope interpretations are validated — exclusion is genuine coordination. If decoupled (ejection occurs without preparation): snare interpretation strengthens — exclusion is pure extraction without coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(material_preparation_coupling, empirical, 'Coupling between social exclusion and material preparation mechanisms').

omega_variable(
    evolutionary_optimality_vs_institutional_contingency,
    'Is the exclusion mechanism an evolved adaptive solution to senescence management, or is it a contingent institutional arrangement that persists because alternatives have not been selected against?',
    'Comparative analysis across eusocial species: species with exclusion vs species without exclusion, measuring fitness outcomes, colony longevity, and resource efficiency. Experimental evolution manipulating exclusion thresholds to test whether fitness correlates with mechanism parameters.',
    'If exclusive mechanism is optimal: mountain classification correct, constraint is inevitable given eusocial structure. If alternative arrangements (gradual role transition, extended elder care, integrated senescence) are viable: mountain is false summit, constraint is contingent institutional design.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(evolutionary_optimality_vs_institutional_contingency, empirical, 'Whether exclusion mechanism is adaptive optimum or contingent institutional arrangement').

omega_variable(
    chemical_signal_reversibility,
    'Are the pheromonal and behavioral signals that trigger exclusion permanently locked in (truly suppress alternative states) or could they theoretically be modulated or reversed by external intervention?',
    'Experimental manipulation of pheromone composition or worker behavior (e.g., blocking exclusion-signaling pheromones, introducing alternative behavioral cues) to test whether ejected individuals can be reintegrated or whether exclusion is computationally reversible.',
    'If signals are permanently locked (reversal impossible): suppression approaches immutability (mountain properties). If signals are theoretically reversible (but not reversed in natural conditions): suppression is contingent institutional design, constraint is tangled_rope, not mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chemical_signal_reversibility, empirical, 'Whether exclusion signals are reversible or permanently locked').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exclusion_as_preparation, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(excl_tr_t0, exclusion_as_preparation, theater_ratio, 0, 0.25).
narrative_ontology:measurement(excl_tr_t6, exclusion_as_preparation, theater_ratio, 6, 0.35).

% Extraction over time
narrative_ontology:measurement(excl_be_t0, exclusion_as_preparation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(excl_be_t3, exclusion_as_preparation, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(excl_be_t6, exclusion_as_preparation, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(excl_be_t8, exclusion_as_preparation, base_extractiveness, 8, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exclusion_as_preparation, resource_allocation).
narrative_ontology:affects_constraint(exclusion_as_preparation, colonial_senescence_threshold).
narrative_ontology:affects_constraint(exclusion_as_preparation, pheromonal_commitment_binding).

% DUAL FORMULATION NOTE:
% This constraint is part of a three-story family modeling eusocial coordination mechanisms: colonial_senescence_threshold (ε≈0.15, Rope) describes the detection mechanism for individual senescence; exclusion_as_preparation (ε≈0.52, Tangled Rope) describes the social enforcement of removal; pheromonal_commitment_binding (ε≈0.38, Tangled Rope) describes the chemical signaling system that makes ejection irreversible. The three stories decompose along the ε-invariance principle: each measures a structurally distinct claim with different empirical bases and different observable-dependent testing criteria.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
