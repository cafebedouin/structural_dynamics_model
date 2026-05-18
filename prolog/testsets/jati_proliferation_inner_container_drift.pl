% ============================================================================
% CONSTRAINT STORY: jati_proliferation_inner_container_drift
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jati_proliferation_inner_container_drift, []).

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
 *   constraint_id: jati_proliferation_inner_container_drift
 *   human_readable: Jati Proliferation as Inner-Container Operational Drift in Hindu Social Structure
 *   domain: religion/hindu_social_structure
 *
 * SUMMARY:
 *   Over two millennia, the Hindu varna system (four ritual classes: Brahmin,
 *   Kshatriya, Vaishya, Shudra) underwent massive inner-container operational
 *   drift. Below the outer kernel, thousands of jatis (endogamous
 *   occupational/kinship groups) formed, split, merged, and evolved.
 *   Merchants became separate jatis; craft guilds subdivided into regional
 *   jatis; new occupational groups generated new jatis. This drift was never
 *   formally acknowledged as drift. Instead, each new jati was retroactively
 *   classified within the existing varna framework through textual commentary
 *   (smritis, puranas, brahmanical elaboration). The constraint demonstrates
 *   how interpretive accretion can absorb structural change while maintaining
 *   the appearance of timeless order. New jatis gain legitimacy and social
 *   coherence by accepting classification within the hierarchy, but pay for
 *   it in permanent subordination. Low-status jatis locked into polluted
 *   occupations face a snare: no exit without losing identity. The
 *   Brahmanical hierarchy coordinates the entire system through continuous
 *   textual elaboration, absorbing new jatis without delegitimizing the
 *   original framework. The varna kernel persists as a ghost category —
 *   intellectually maintained through ritual and textual authority but
 *   detached from operational reality. The constraint tests the framework's
 *   prediction about composability: a system of pure fixity (like Spartan
 *   hoplite citizenship) would have rejected new jatis as illegitimate; the
 *   interpretive layer allowed absorption of massive drift without system
 *   collapse.
 *
 * KEY AGENTS:
 *   - Brahmanical Hierarchy: Primary beneficiary (institutional/arbitrage) — coordinates entire social structure; gains from jati proliferation by providing legitimacy layer; benefits from interpretation-authority monopoly
 *   - Emerging Occupational Groups: Primary target (moderate/identity_locked) — gain jati legitimacy and social coherence but lock into subordinate varna classification; identity fused with jati designation
 *   - Established Intermediate Jatis: Secondary beneficiary (organized/constrained) — benefit from hierarchical legitimacy and recognition; enforce boundaries against emerging groups through endogamy and occupational gatekeeping
 *   - Ritually Polluted Jatis: Primary victim (powerless/trapped) — locked into low-status occupations by jati designation; interpenetration of ritual status, occupational restriction, and marriage rules creates total suppression
 *   - Varna Kernel Doctrine: Institutional ghost category (institutional/arbitrage) — maintained through ritual and textual authority despite detachment from operational reality; high theater ratio
 *   - Analytical Observer: Distant view (analytical/analytical) — risks naturalizing contingent constructed system as immutable feature of Hindu structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jati_proliferation_inner_container_drift, 0.52).
domain_priors:suppression_score(jati_proliferation_inner_container_drift, 0.68).
domain_priors:theater_ratio(jati_proliferation_inner_container_drift, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jati_proliferation_inner_container_drift, extractiveness, 0.52).
narrative_ontology:constraint_metric(jati_proliferation_inner_container_drift, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(jati_proliferation_inner_container_drift, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jati_proliferation_inner_container_drift, tangled_rope).
narrative_ontology:human_readable(jati_proliferation_inner_container_drift, "Jati Proliferation as Inner-Container Operational Drift in Hindu Social Structure").
narrative_ontology:topic_domain(jati_proliferation_inner_container_drift, "religion/hindu_social_structure").

domain_priors:requires_active_enforcement(jati_proliferation_inner_container_drift).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jati_proliferation_inner_container_drift, brahmanical_hierarchy).
narrative_ontology:constraint_beneficiary(jati_proliferation_inner_container_drift, established_jatis).
narrative_ontology:constraint_victim(jati_proliferation_inner_container_drift, emerging_occupational_groups).
narrative_ontology:constraint_victim(jati_proliferation_inner_container_drift, ritually_polluted_jatis).
narrative_ontology:constraint_victim(jati_proliferation_inner_container_drift, inter_jati_mobility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RITUALLY POLLUTED JATI (SNARE) — Locked into low-status occupational categories (leather workers, sweepers, funeral specialists) through jati designation. The retroactive classification within varna provides theological legitimacy but no path for mobility. No material barriers alone would prevent exit, but the interpenetration of ritual status, occupational restriction, and marriage exogamy rules creates a total suppression system. Structural immobility perceived as cosmic necessity.
constraint_indexing:constraint_classification(jati_proliferation_inner_container_drift, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: EMERGING OCCUPATIONAL GROUP (TANGLED ROPE) — New occupational specialization (merchants, craftspeople, scribes) seeks jati status and varna classification to gain legitimacy and regulate internal marriage/occupation. Coordination benefit: the group gains social coherence, recognized status, and regulatory autonomy over membership and practice. But the cost is permanent subordination — once classified within the varna hierarchy (typically Shudra or below), the group accepts ritual and social inferiority as the price of legitimacy. Identity-locked: the group's identity becomes fused with its jati designation; exiting would mean losing the hard-won legitimacy.
constraint_indexing:constraint_classification(jati_proliferation_inner_container_drift, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 3: BRAHMANICAL HIERARCHY (ROPE) — The varna-jati system coordinates the entire social structure: it regulates marriage, occupation, ritual status, land rights, and political authority. The proliferation of jatis is absorbed through interpretive accretion — each new jati is retroactively fitted into the existing varna framework through commentary and textual elaboration (smritis, puranas). This is coordination work: absorbing drift without acknowledging drift, maintaining the appearance of timeless order while accommodating massive structural change. Brahmanical institutional position benefits from the system's flexibility (it can absorb new groups without delegitimating itself) and from the legitimacy it provides to the entire structure.
constraint_indexing:constraint_classification(jati_proliferation_inner_container_drift, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: ESTABLISHED INTERMEDIATE JATI (TANGLED ROPE) — A jati that has already achieved stable intermediate status (merchant guilds, scribal communities, agricultural castes) both benefits from and enforces the hierarchy. Benefits from hierarchical legitimacy and recognition. Enforces through endogamous marriage regulation and occupational gatekeeping against emerging groups. Constrained exit: leaving jati identity would forfeit all the legitimacy gains; maintaining status requires continuous boundary enforcement (marriage prohibition, ritual hierarchy maintenance).
constraint_indexing:constraint_classification(jati_proliferation_inner_container_drift, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: VARNA KERNEL DOCTRINE (PITON) — The original four-varna framework (Brahmin, Kshatriya, Vaishya, Shudra) is intellectually maintained as the foundational classification system despite the fact that actual social reality consists of thousands of jatis with complex, overlapping statuses. The varna doctrine persists through ritual and textual authority — Vedic references, Manusmriti citations, brahmanical prestige — long after it ceased to describe operational reality. Theater ratio is high: the performance of varna classification (ritual status markers, brahmanical authority, textual references) continues to legitimize the system even though coordination and enforcement happen at the jati level. The varna is a ghost category maintained by institutional inertia.
constraint_indexing:constraint_classification(jati_proliferation_inner_container_drift, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURALIZATION VIEW (MOUNTAIN) — From a distant civilizational view, the jati system can appear to be a natural, immutable feature of Hindu social structure — as fundamental and unchangeable as kinship itself. The system's longevity (2000+ years), its pervasiveness across regions and sects, and its deep theological grounding all support the naturalization narrative. However, the constraint story structure exposes this as a false summit: the system is contingent on active interpretation, textual elaboration, marriage enforcement, and institutional reproduction. Remove the interpretive layer (brahmanical commentary) and the enforcement mechanism (jati gatekeeping), and the system loses its binding power immediately.
constraint_indexing:constraint_classification(jati_proliferation_inner_container_drift, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jati_proliferation_inner_container_drift_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(jati_proliferation_inner_container_drift, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(jati_proliferation_inner_container_drift, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(jati_proliferation_inner_container_drift, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(jati_proliferation_inner_container_drift, TR),
    TR >= 0.70.

:- end_tests(jati_proliferation_inner_container_drift_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The system extracts from emerging groups and low-status jatis through occupational restriction, ritual subordination, and marriage prohibition. The extraction is not maximal (a true snare might reach 0.70+) because the system also provides genuine coordination benefits: new jatis gain social coherence, regulated membership, occupational stability, and ritual recognition. Emerging groups voluntarily enter the system to gain legitimacy. The tangled_rope classification captures this: genuine coordination (regulation of occupational specialization, marriage, ritual status) combined with asymmetric extraction (permanent subordination). Suppression (0.68): High. Multiple overlapping suppression mechanisms: occupational restriction (you must do your jati's hereditary occupation), marriage prohibition (you must marry within jati), ritual prohibition (contact with other jatis causes pollution), and ideological suppression (purity/pollution narrative naturalizes the hierarchy). But suppression is not total (would approach 0.85+ in a pure snare) because brahmanical authority provides a legitimation layer that makes participation partially voluntary. Theater ratio (0.65): Moderately high. The varna kernel persists as a Ghost category — the varna classification (Brahmin, Kshatriya, Vaishya, Shudra) is maintained through ritual, textual authority, and brahmanical prestige even though actual social coordination happens at the jati level. Theater rises over time (0.45 → 0.65) as the gap between varna doctrine and jati reality widens and requires increasingly elaborate textual elaboration to maintain coherence.
 *
 * PERSPECTIVAL GAP:
 *   The gap reveals how the same constraint system can appear as coordination (to those managing it), mixed benefit (to those climbing it), and extraction (to those locked in it). The varna-jati system coordinates social reality effectively from the Brahmanical perspective — it solves the problem of organizing occupational specialization across regions and centuries. But from the perspective of a ritually polluted jati, the coordination is a false front for extraction and immobilization. The identity_locked exit option for emerging groups is crucial: they experience the constraint as tangled_rope precisely because they have gained legitimacy and identity through jati designation and cannot imagine abandoning it — even though structurally they could organize resistance or negotiate alternative classifications. The theater-ratio rise (0.45 → 0.65) shows increasing reliance on textual elaboration and ritual performance to maintain coherence as the jati proliferation outpaces the varna framework's descriptive capacity. At time_point 0, the system is relatively transparent (theater 0.45) because new jatis are being absorbed without much interpretive strain. At time_point 10, the theater rises (0.65) as the gap widens and requires increasingly elaborate brahmanical commentary to fit thousands of jatis into a four-category framework.
 *
 * DIRECTIONALITY LOGIC:
 *   The automatic directionality derivation produces: Brahmanical hierarchy (institutional + arbitrage) → d ≈ 0.05 → low effective extraction. Emerging groups (moderate + identity_locked) → d ≈ 0.45 → moderate experienced extraction. Polluted jatis (powerless + trapped) → d ≈ 0.95 → maximum experienced extraction. These derivations are structurally accurate and require no overrides. The emerging group's moderate d reflects the genuine ambiguity of their position: they are being extracted from (subordinated) but also gaining something real (jati legitimacy, social coherence, occupational regulation). The identity_locked exit option is crucial here — they cannot exit even though they are experiencing extraction, because exiting would require abandoning the identity they have constructed through jati membership.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint is genuinely tangled_rope at the system level (coordinates occupational specialization + extracts subordination) while containing snare and rope sub-components from different positions. The system would be misclassified as pure rope if only the Brahmanical and established-jati perspectives were visible; it would be misclassified as pure snare if only the polluted-jati perspective was visible. The truth is compositional: the system coordinates (rope benefit) while extracting (snare cost). The coordination is genuine — the system does regulate occupational specialization in ways that provide stability and coherence. The extraction is also genuine — the system achieves coordination by creating permanent hierarchical subordination. The tangled_rope classification captures this hybrid: both coordination and asymmetric extraction are necessary structural features, not side effects or corruption of the system.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_hierarchy,
    'Is the jati system a natural outgrowth of occupational specialization and kinship (mountain), or a constructed hierarchy that naturalizes itself through theological and textual authority (snare/tangled_rope)?',
    'Comparative analysis of occupational specialization in societies without jati-like systems; historical tracing of moments when new jatis were accepted vs rejected; analysis of resistance movements that rejected jati classification',
    'If natural: the constraint is immutable without destroying occupational stability. If constructed: the constraint is contingent on reproducing interpretive authority and enforcement mechanisms; alternative structures are possible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_hierarchy, conceptual, 'Whether jati hierarchy is natural or constructed').

omega_variable(
    accretion_mechanism_limits,
    'How much structural change can the interpretive-accretion layer absorb before the varna kernel loses coherence? What triggers a jati schism vs a jati merger vs a new jati formation?',
    'Historical analysis of jati genealogies: what conditions led to splitting (geographic isolation, sect differentiation, occupational diversification)? What conditions led to merger (economic integration, marriage practice convergence)? How did brahmanical texts respond to each scenario?',
    'If accretion capacity is high: the system is extremely resilient and the inner-container drift is ongoing and stable. If capacity is low: the system is near a bifurcation point where interpretive authority will fragment (already visible in sectarian, regional, and caste-reform movements).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accretion_mechanism_limits, empirical, 'Limits of interpretive accretion capacity').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the suppression that locks low-status jatis into immobility primarily structural (occupational restriction, marriage prohibition, ritual prohibition enforced externally) or internalized (purity/pollution ideology, ritualized acceptance of subordination)?',
    'Post-reform analysis: when structural enforcement mechanisms are removed (legal prohibition of caste-based discrimination, occupational liberalization), do suppression-level behaviors persist? Do jati members maintain endogamous boundaries voluntarily? Do ritual purity narratives persist even when external enforcement stops?',
    'If structural: mobility becomes possible once enforcement is removed. If internalized: mobility remains suppressed even after structural barriers fall — the constraint is carried internally by the agent. If both: the internalized component is the more difficult to dismantle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural vs internalized suppression mechanism').

omega_variable(
    brahmanical_gain_from_proliferation,
    'Does the Brahmanical hierarchy gain extractiveness from jati proliferation (more groups paying legitimacy rent), or lose it (dilution of brahmanical authority as jatis develop independent status systems)?',
    'Historical analysis of brahmanical economic and political power relative to jati proliferation rate; comparison of regions where jati proliferation was high vs low; analysis of moments when brahmanical authority was challenged vs reinforced',
    'If gaining: the constraint is an extractive system that benefits from proliferation — the theater and coordination elements are secondary to rent-seeking. If losing: the system is primarily coordinating and has secondary extraction elements — the apparent cooperation is genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(brahmanical_gain_from_proliferation, empirical, 'Whether brahmanical extraction increases with jati proliferation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jati_proliferation_inner_container_drift, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jati_tr_t0, jati_proliferation_inner_container_drift, theater_ratio, 0, 0.45).
narrative_ontology:measurement(jati_tr_t5, jati_proliferation_inner_container_drift, theater_ratio, 5, 0.58).
narrative_ontology:measurement(jati_tr_t10, jati_proliferation_inner_container_drift, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(jati_be_t0, jati_proliferation_inner_container_drift, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(jati_be_t5, jati_proliferation_inner_container_drift, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(jati_be_t10, jati_proliferation_inner_container_drift, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jati_proliferation_inner_container_drift, identity_coordination).
narrative_ontology:affects_constraint(jati_proliferation_inner_container_drift, varna_endogamy_enforcement).
narrative_ontology:affects_constraint(jati_proliferation_inner_container_drift, brahmanical_interpretive_authority).
narrative_ontology:affects_constraint(jati_proliferation_inner_container_drift, occupational_heredity_transmission).

% DUAL FORMULATION NOTE:
% The jati proliferation constraint is downstream of the varna-jati duality itself. The varna framework (outer kernel, ghost category) has different extractiveness (higher theater, lower operational content) than the jati coordination mechanism (inner container, operational reality). Some analyses decompose these as separate constraints; this story treats them as aspects of a single system showing inner-container drift absorbed by outer-kernel interpretive accretion. The network links trace how jati proliferation affects marriage enforcement (endogamy), textual authority (brahmanical elaboration), and occupational transmission (heredity norms).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
