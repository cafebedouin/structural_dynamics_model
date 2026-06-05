% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_decay
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_decay, []).

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
 *   constraint_id: catastrophe_memory_decay
 *   human_readable: Catastrophe Memory Decay and Land-Use Constraint Persistence
 *   domain: disaster_anthropology/institutional_memory
 *
 * SUMMARY:
 *   The Aneyoshi stone in Iwate Prefecture, Japan, represents a 78-year
 *   institutional memory constraint spanning the 1933 Showa tsunami through
 *   the 2011 Tōhoku tsunami. The stone's inscription warned 'do not build
 *   below this point,' encoding the knowledge of two catastrophic tsunamis
 *   (1896, 1933) into a spatially explicit constraint on future settlement.
 *   The 2011 Tōhoku earthquake and tsunami (magnitude 9.0) killed
 *   approximately 20,000 people across the Sendai coastal region, devastating
 *   communities up to 40 km inland. Yet all 11 households above the Aneyoshi
 *   stone line survived. The constraint operated across two non-catastrophe
 *   generations (approximately 35-45 years between 1933 and the early 1970s,
 *   and roughly 37 years from mid-1970s to 2011), during which the stone's
 *   behavioral force likely decayed from urgent warning to cultural monument.
 *   The kernel contest is whether the stone retained live institutional force
 *   throughout this period or decayed to commemorative symbol and was then
 *   vindicated by the 2011 event. This constraint exemplifies how catastrophe
 *   memory persists (or fails to persist) as a governance mechanism.
 *
 * KEY AGENTS:
 *   - Memory-bearing community: Primary beneficiary (institutional/arbitrage) — the Aneyoshi households and local knowledge holders who maintained the stone's significance and benefited from its protective force
 *   - Development pressure actors: Primary victims (powerless/trapped) — new residents, developers, and economic agents constrained by the building prohibition without understanding its rationale
 *   - Local governance institutions: Secondary actor (moderate/constrained) — caught between enforcing the memory constraint and responding to development pressure and economic incentives
 *   - Japanese national disaster governance: Beneficiary (institutional/arbitrage) — gains low-cost, durable coordination mechanism for intergenerational risk communication
 *   - Intergenerational knowledge transmission: Victim (moderate/constrained) — the abstract process of passing catastrophe knowledge across generations; face barriers of forgetting, skepticism, and narrative decay
 *   - Analytical observer: Civilizational perspective (analytical/analytical) — risks naturalizing the choice of where to draw the line as physics rather than governance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_decay, 0.35).
domain_priors:suppression_score(catastrophe_memory_decay, 0.62).
domain_priors:theater_ratio(catastrophe_memory_decay, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_decay, extractiveness, 0.35).
narrative_ontology:constraint_metric(catastrophe_memory_decay, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(catastrophe_memory_decay, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_decay, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_decay, "Catastrophe Memory Decay and Land-Use Constraint Persistence").
narrative_ontology:topic_domain(catastrophe_memory_decay, "disaster_anthropology/institutional_memory").

domain_priors:requires_active_enforcement(catastrophe_memory_decay).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_decay, memory_bearing_community).
narrative_ontology:constraint_beneficiary(catastrophe_memory_decay, coastal_governance_institutions).
narrative_ontology:constraint_victim(catastrophe_memory_decay, development_pressure_actors).
narrative_ontology:constraint_victim(catastrophe_memory_decay, intergenerational_knowledge_transmission).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEVELOPER OR MIGRANT BELOW THE STONE (SNARE) — Constrained by a prohibition they do not understand and cannot challenge. The stone's force persists as pure suppression: 'do not build here' without accessible rationale to new residents. No alternative sites, no exit option, no coordination benefit. The constraint appears as an arbitrary restriction backed by cultural authority they may not share.
constraint_indexing:constraint_classification(catastrophe_memory_decay, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: LOCAL GOVERNANCE INSTITUTIONS (TANGLED ROPE) — Must coordinate coastal safety (genuine coordination function) while facing development pressure and economic incentives. Enforcement requires active suppression of alternative land use. The constraint both protects communities and restricts development revenue. Moderately powerful but constrained by conflicting mandates.
constraint_indexing:constraint_classification(catastrophe_memory_decay, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: NATIONAL DISASTER GOVERNANCE (ROPE) — The stone and similar memory devices are tools for coordinating intergenerational risk communication. The state benefits from low-cost, durable constraint mechanisms that do not require continuous institutional maintenance. Pure coordination from this perspective: the state gains capacity to coordinate safety across generations without expensive bureaucratic overhead.
constraint_indexing:constraint_classification(catastrophe_memory_decay, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: THE STONE AS PERFORMATIVE MONUMENT (PITON) — Between catastrophes, the stone becomes primarily theatrical: a site of pilgrimage, documentary preservation, and identity markers rather than functional constraint on building behavior. The constraint persists through inertia and cultural reverence rather than active enforcement. Theater ratio rises during calm periods as the warning transforms into memory ritual.
constraint_indexing:constraint_classification(catastrophe_memory_decay, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 6: GEOPHYSICAL MOUNTAIN (FALSE SUMMIT CANDIDATE) — From a civilizational view, tsunami hazard on this coast is a physical inevitability. The constraint 'do not build below this line' could appear as encoding an immutable natural law about coastal vulnerability. However, the base properties reveal beneficiaries and victims: the state, the memory-bearing community, and the governance institutions all structure their interests around the constraint. This naturalizes a contingent social arrangement (where to draw the line, how to enforce it, how long to remember) as physics.
constraint_indexing:constraint_classification(catastrophe_memory_decay, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_decay_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(catastrophe_memory_decay, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(catastrophe_memory_decay, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(catastrophe_memory_decay, TR),
    TR >= 0.70.

:- end_tests(catastrophe_memory_decay_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate, declining over the interval. At t=0 (1933, immediately post-catastrophe), the constraint's extractiveness is high (0.48): building is prohibited, enforcement is strict, suppression is maximum because the trauma is fresh. As decades pass without catastrophe, the constraint's extractiveness declines (0.35 by 2011): development pressure mounts, younger residents lack direct catastrophe memory, the stone's force becomes cultural reverence rather than behavioral prohibition. The declining extractiveness reflects the decay of institutional memory. Suppression (0.62): High throughout. The constraint operates through prohibition ('do not build here') without transparent reasoning for new residents. Alternative sites exist but require relocation investment. Peer pressure and cultural authority maintain the ban even as the rationale recedes into history. Theater ratio (0.58): Moderate and rising. Immediately post-catastrophe (1933), the stone is primarily functional — a warning with urgent behavioral force. By mid-century, the stone becomes increasingly theatrical: it is visited, documented, incorporated into local identity narratives, and treated as a monument rather than an active prohibition. By 2011, the theater ratio has stabilized at 0.58 — the stone is simultaneously a warning (functional) and a symbol of the community's resilience and memory (performative). Claimed type (Tangled Rope): The constraint genuinely coordinates coastal safety (beneficiaries: memory-bearing community and governance institutions gain safety and institutional credibility) while imposing asymmetric extraction (victims: new residents and development actors face prohibition without full rationale). Active enforcement is required to maintain the constraint against economic incentives.
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces stark perspectival disagreement about its classification and legitimacy. The memory-bearing community (institutional/arbitrage) experiences Rope — the stone coordinates their collective safety and their identity as a community that heeds ancestral warnings. They benefit and perceive the constraint as fair coordination. New residents and developers (powerless/trapped) experience Snare — they face an incomprehensible prohibition with no viable alternative and no transparency about why this specific location is forbidden. Local governance (moderate/constrained) experiences Tangled Rope — they must enforce a constraint that coordinates safety but also suppresses development and alienates newcomers. The national disaster governance institution (institutional/arbitrage) sees pure Rope — an elegant, low-cost coordination mechanism for long-interval hazards. The stone itself, between catastrophes, appears as Piton to any observer who measures theater — the constraint's functional force decays as the stone becomes more symbol than warning. The mountain perspective risks naturalizing the constraint as inevitable geography rather than contingent social choice. The perspectival gap reveals that the constraint's legitimacy depends entirely on belief in the stone's rationale — which decays across generations without catastrophe to refresh the memory.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from each agent's structural position relative to the constraint. Memory-bearing households occupy a beneficiary position (they benefit from safety) with arbitrage-level exit (they can leave if they choose, but choose not to because the constraint aligns with their values). This produces low d, experienced as coordination (Rope). New residents occupy a victim position (constrained by prohibition) with trapped-level exit (limited alternative sites for development). This produces high d, experienced as suppression (Snare). Governance institutions occupy a mixed position: they benefit institutionally from a coordination mechanism that requires no expensive bureaucracy, but they bear the cost of enforcement against economic pressure. This produces moderate d, experienced as tangled coordination-extraction (Tangled Rope). The analytical observer cannot occupy either beneficiary or victim position but can recognize the structural contest between memory persistence and memory decay — this produces d ≈ 0.72, canonical for analytical positions. The piton perspective derives from time-horizon collapse: over the biographical timescale (where development decisions are made), the theater ratio rises as the constraint's functional force decays, making it appear as inertial performance rather than active coordination.
 *
 * MANDATROPHY ANALYSIS:
 *   The Aneyoshi stone resolves mandatrophy by distinguishing structural classification from outcome confirmation. The constraint was Tangled Rope before 2011 — it coordinated coastal safety (genuine function) while imposing extraction on developers (asymmetric costs). The 2011 outcome (households above the stone survived; those below were devastated) confirmed the constraint's efficacy but did not change its classification. This is crucial: the constraint was not secretly a Mountain (immutable natural law) all along, nor was it a Scaffold with unknown sunset. It was Tangled Rope throughout, and the outcome merely vindicated the beneficiaries' belief in its force. The mandatrophy is resolved by recognizing that memory constraints are Tangled Rope (not pure Rope) because they require active enforcement against development pressure and intergenerational skepticism. They are not Scaffolds because they lack a natural sunset clause (they persist until the next catastrophe or institutional failure). They are not Mountains because they encode contingent social choices (which elevation line, how strictly enforced) rather than universal geophysical truth. The false summit risk is real: observers might naturalize the constraint as immutable (Mountain) based on its 2011 vindication, but the constraint's structure was always hybrid coordination-extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    memory_decay_threshold,
    'At what number of generations without catastrophe does an institutional memory constraint decay from behavioral force to performative symbol?',
    'Comparative analysis of memory decay across Japanese, Indonesian, and Chilean coastal communities with different interstrike intervals (50-200 years). Correlation between generation count and violation rates, enforcement investment, and narrative persistence.',
    'If decay occurs < 3 generations: memory constraints are structurally unstable and cannot coordinate long-interval hazards. If decay > 5 generations: institutional memory mechanisms are robust enough for multi-century coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(memory_decay_threshold, empirical, 'Generation threshold at which memory constraints decay to symbol').

omega_variable(
    enforcement_mechanism_ambiguity,
    'Is the stone''s behavioral force in 2011 sustained by active enforcement (cultural authority, institutional sanction, peer pressure), passive geographic constraint (limited alternative sites), or identity fusion (the community''s self-concept as ''people who heed the stone'')?',
    'Post-2011 ethnographic study of households above vs below the line: interviews on decision rationales, cost-benefit analysis of relocation, knowledge of stone''s history, and counterfactual scenarios (would you build there if the stone didn''t exist?).',
    'If active enforcement dominant: constraint is Tangled Rope (requires continuous institutional investment). If passive geography: constraint approaches Mountain (exogenous barriers, not social enforcement). If identity fusion: constraint is Rope with identity_locked exit for non-members.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_ambiguity, empirical, 'Whether stone''s force derives from active enforcement, geography, or identity').

omega_variable(
    kernel_contest_institutional_status,
    'Is the Aneyoshi stone a reading of a broader kernel (coastal memory governance) with the 2011 event confirming one reading over alternatives, or is the stone itself the kernel being read differently across time?',
    'Analysis of how Japanese disaster governance institutions have since formalized stone-based risk communication (e.g., UNESCO Recognition of Aneyoshi in 2016, subsequent national guidelines for monument-based warnings). If formalization increases, the stone-as-kernel reading strengthens; if it remains local practice, the stone is one reading of coastal memory governance.',
    'If stone is kernel: subsequent tsunamis will be read as confirmation or refutation of the memory persistence thesis. If stone is reading of kernel: multiple coastal communities will develop competing memory devices, revealing the kernel''s underspecification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_contest_institutional_status, conceptual, 'Whether the stone is a kernel or one reading of coastal memory governance').

omega_variable(
    survivors_bias_in_perception,
    'Does the 2011 survival outcome (11 households above stone, devastating losses below) change the constraint''s classification retroactively, or does it reveal the classification that was structurally operative before the event?',
    'Pre-2011 ethnographic data on household locations, building decisions, stated reasons for settlement patterns. Counterfactual: would households have behaved differently in 1970 or 1990 if they knew the 2011 outcome? The constraint''s pre-event classification should not depend on the outcome it predicted.',
    'If classification changes retroactively: observers are confusing outcome confirmation with structural force. If classification holds: the constraint was operative all along, and 2011 merely revealed it. Methodologically: constraint stories must be authored from the agent''s perspective at time-of-decision, not from the analyst''s retrospective vantage.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(survivors_bias_in_perception, conceptual, 'Outcome confirmation vs structural classification').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_decay, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(catmem_tr_t0, catastrophe_memory_decay, theater_ratio, 0, 0.35).
narrative_ontology:measurement(catmem_tr_t20, catastrophe_memory_decay, theater_ratio, 20, 0.42).
narrative_ontology:measurement(catmem_tr_t40, catastrophe_memory_decay, theater_ratio, 40, 0.58).
narrative_ontology:measurement(catmem_tr_t78, catastrophe_memory_decay, theater_ratio, 78, 0.58).

% Extraction over time
narrative_ontology:measurement(catmem_be_t0, catastrophe_memory_decay, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(catmem_be_t20, catastrophe_memory_decay, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(catmem_be_t40, catastrophe_memory_decay, base_extractiveness, 40, 0.38).
narrative_ontology:measurement(catmem_be_t78, catastrophe_memory_decay, base_extractiveness, 78, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_decay, resource_allocation).
narrative_ontology:affects_constraint(catastrophe_memory_decay, intergenerational_hazard_communication).
narrative_ontology:affects_constraint(catastrophe_memory_decay, coastal_settlement_pattern_lock_in).
narrative_ontology:affects_constraint(catastrophe_memory_decay, disaster_warning_ritualization).

% DUAL FORMULATION NOTE:
% The catastrophe memory decay constraint is downstream of the fundamental kernel (institutional memory for long-interval events) but structurally distinct. Intergenerational hazard communication represents the coordination function; coastal settlement pattern lock-in represents the extraction mechanism; disaster warning ritualization represents the theater drift. Each has its own ε value and should be authored separately if the analysis requires detail on distinct mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_decay, moderate, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
