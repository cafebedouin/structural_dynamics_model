% ============================================================================
% CONSTRAINT STORY: unclos_2026
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: UNCLOS 1982; BBNJ Treaty (2026); South China Sea Award (2016-2026)
% ============================================================================

:- module(constraint_unclos_2026, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:extractiveness_for_agent/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: unclos_2026
 * human_readable: UN Convention on the Law of the Sea (2026 Context)
 * domain: legal/geopolitical/environmental
 * temporal_scope: 1982 - Present (2026 BBNJ integration)
 * spatial_scope: Global Oceans
 * * SUMMARY:
 * UNCLOS, often called the "Constitution of the Oceans," establishes a 
 * comprehensive legal framework for all maritime activities. In 2026, 
 * this constraint is revitalized by the entry into force of the BBNJ Treaty 
 * (High Seas Treaty) on January 17, 2026, which creates the first legally 
 * binding rules for biodiversity in international waters.
 * * KEY AGENTS:
 * - Coastal States (e.g., Philippines): Rely on UNCLOS to defend sovereign rights 
 * within 200nm Exclusive Economic Zones (EEZ).
 * - Hegemonic Dissents (e.g., China, USA): Challenge specific parts of the 
 * regime (e.g., seabed mining or compulsory arbitration) as coercive.
 * - International Seabed Authority (ISA): Institutional enforcer of mining rules.
 * * NARRATIVE ARC:
 * UNCLOS transitions from a "Rope" of functional cooperation to a "Mountain" of 
 * customary law for most. However, for powers claiming "historic rights" 
 * outside the Convention, it is treated as a "Snare" to be rejected.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION
   ========================================================================== */

% Structural Anchor for Audit Suite
narrative_ontology:interval(unclos_era, 1982, 2026).
narrative_ontology:constraint_claim(unclos_2026, rope).
narrative_ontology:human_readable(unclos_2026, "UN Convention on the Law of the Sea (2026 Context)").

% Base extractiveness: 0.3
% Rationale: While largely fair, it "extracts" absolute freedom of navigation 
% and resource exploitation from states to provide a stable order.
domain_priors:base_extractiveness(unclos_2026, 0.3).

% Suppression score: 0.2
% Rationale: It suppresses unilateral maritime claims (like the "cannon-shot rule" 
% or "nine-dash line") in favor of a zone-based framework.
domain_priors:suppression_score(unclos_2026, 0.2).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(unclos_2026, extractiveness, 0.3).
narrative_ontology:constraint_metric(unclos_2026, suppression_requirement, 0.2).

% Enforcement: Requires active maintenance (e.g., ISA meetings, arbitral tribunals).
domain_priors:requires_active_enforcement(unclos_2026).

% Beneficiaries & Victims
narrative_ontology:constraint_beneficiary(unclos_2026, land_locked_states). % Access to sea.
narrative_ontology:constraint_beneficiary(unclos_2026, coastal_middle_powers). % Stability of EEZ.
narrative_ontology:constraint_victim(unclos_2026, maritime_unilateralists). % Powers seeking "historic" titles.

% Metrics
/* ==========================================================================
   3. INDEXED CLASSIFICATIONS
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: SMALL COASTAL STATE (e.g., Philippines) - Mountain
   --------------------------------------------------------------------------
   WHO: powerless - Relatively weaker militarily against superpowers.
   WHEN: biographical - Sovereign survival depends on these "fixed" rules.
   WHERE: trapped - Bound by the geography of their coastline.
   SCOPE: national - Focused on protecting local fishing and resources.
   
   WHY THIS CLASSIFICATION:
   For states relying on the "rules-based order" for survival, UNCLOS and the 
   2016 Arbitral Award are treated as unchangeable natural laws (Mountain) 
   that bind all parties.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    unclos_2026,
    mountain,
    context(
        agent_power(powerless),
        time_horizon(biographical),
        exit_options(trapped),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE UN SECRETARIAT / EU - Rope
   --------------------------------------------------------------------------
   WHO: institutional - Rule-making and coordinating body.
   WHEN: civilizational - Looking at long-term ocean health (BBNJ).
   WHERE: mobile - Can negotiate and add treaties/annexes.
   SCOPE: global - Worldwide ocean governance.
   
   WHY THIS CLASSIFICATION:
   For the architects of the BBNJ treaty, UNCLOS is a functional "Constitution" 
   (Rope)—a tool to be refined to share benefits fairly and create protected 
   areas in the high seas.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    unclos_2026,
    rope,
    context(
        agent_power(institutional),
        time_horizon(civilizational),
        exit_options(mobile),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: MARITIME SUPERPOWER (e.g., China/USA) - Snare
   --------------------------------------------------------------------------
   WHO: powerful - Significant influence; can act unilaterally.
   WHEN: immediate - Focused on current tactical/resource advantages (mining/islands).
   WHERE: arbitrage - Claims "not bound" by specific rules while using others.
   SCOPE: global/regional - Ambitions spanning entire oceans.
   
   WHY THIS CLASSIFICATION:
   For powers like China, the 2016 Award is a "Snare" that attempts to 
   asymmetrically restrict their perceived "historic rights". For 
   the US, Part XI (seabed mining) is viewed as an extractive limit on 
   economic/security interests.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    unclos_2026,
    snare,
    context(
        agent_power(powerful),
        time_horizon(immediate),
        exit_options(arbitrage),
        spatial_scope(global)
    )
) :- !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(unclos_2026_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(unclos_2026, Type1, context(agent_power(powerless), time_horizon(biographical), exit_options(trapped), spatial_scope(national))),
    constraint_indexing:constraint_classification(unclos_2026, Type2, context(agent_power(institutional), time_horizon(civilizational), exit_options(mobile), spatial_scope(global))),
    Type1 = mountain,
    Type2 = rope.

test(hegemonic_snare_perception) :-
    % Powerful agents see it as a Snare when it restricts "historic" titles.
    constraint_indexing:constraint_classification(unclos_2026, snare, context(agent_power(powerful), time_horizon(immediate), exit_options(arbitrage), spatial_scope(global))).

test(bbnj_rope_dynamic) :-
    % BBNJ Treaty (High Seas) entry into force 2026 marks a Rope expansion.
    narrative_ontology:interval(unclos_era, _, 2026).

:- end_tests(unclos_2026_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. PERSPECTIVE GAPS: Highlighted the gap between the "Mountain" of law for 
 * smaller states and the "Snare" of restriction for dissenting superpowers.
 * 2. 2026 CONTEXT: Integrated the BBNJ treaty (Jan 2026) as proof of UNCLOS's 
 * nature as a "living" Rope for institutional actors.
 * 3. EXTRACTIVENESS: Set at 0.3. It is not purely extractive, but powerful 
 * states perceive it as such when it "locks" resources they could otherwise 
 * seize.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

omega_variable(
    us_seabed_unilateralism,
    "Will U.S. Executive Order 14285 create a permanent split in the seabed mining regime?",
    resolution_mechanism("Monitor the conflict between NOAA-issued licenses and ISA contracts in 2026-2027."),
    impact("If Yes: UNCLOS becomes a failing Rope. If No: It solidifies as a Mountain."),
    confidence_without_resolution(medium)
).

omega_variable(
    scs_finality,
    "Does international pressure suffice to enforce the 'final' status of the 2016 Award in 2026?",
    resolution_mechanism("Observe the shift in 'Arbitration Support' from neutral to fully supporting countries."),
    impact("If High Support: Mountain. If Low: It remains a contestable Snare for China."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Unilateral "Cannon-Shot" or "Historic Rights" Claims
 * Viability: Historically practiced; China's "nine-dash line" is a modern 
 * version.
 * Suppression: Explicitly invalidated by the 2016 Arbitral Tribunal.
 * * ALTERNATIVE 2: Regional Bilateral Agreements
 * Viability: Beijing's preferred method for South China Sea.
 * Suppression: Small states prefer the "Universal" Rope of UNCLOS to balance power.
 * * CONCLUSION:
 * The shift from "Historic Rights" (Mountain-like natural title) to UNCLOS 
 * zones (Institutional Rope) is the core of 2026 maritime tension.
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

% ============================================================================
% ENRICHMENT: Structural predicates for dynamic classification
% Generated: 2026-02-08
% Template: v5.2 namespace alignment
% Source: Derived from existing narrative and structural content in this file
% ============================================================================

% --- Multifile declarations for new predicates ---
:- multifile
    domain_priors:theater_ratio/2.

% --- Theater ratio (missing from base properties) ---
% Coordination mechanism in legal domain — moderate institutional framing
domain_priors:theater_ratio(unclos_2026, 0.14).
narrative_ontology:constraint_metric(unclos_2026, theater_ratio, 0.14).

% --- Analytical perspective classification (missing) ---
% chi = 0.3 * 1.15 (analytical) * 1.2 (global) = 0.414
% Classification: rope
constraint_indexing:constraint_classification(unclos_2026, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).
