% ============================================================================
% CONSTRAINT STORY: federal_asymmetry__linguistic_reorganization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federal_asymmetry__linguistic_reorganization_reading, []).

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
    narrative_ontology:cs_story_uid/2,
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
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: federal_asymmetry__linguistic_reorganization_reading
 *   human_readable: Federal Asymmetry: Linguistic Reorganization Reading (1956)
 *   domain: legal/constitutional/political
 *
 * SUMMARY:
 *   The States Reorganization Act (1956) redrew the map of independent India
 *   along linguistic lines, creating federal units organized around dominant
 *   language groups. This decision conceded a principle that the independence
 *   leadership — particularly Nehru and Patel — had feared to concede: that
 *   linguistic identity was a legitimate basis for carving federal units from
 *   the Indian union. The reorganization was driven by sustained political
 *   movements (Tamil, Marathi, Gujarati, Telugu, Kannada, and Malayalam
 *   language movements) that mobilized linguistic consciousness as a basis
 *   for statehood demands. This constraint story instantiates ONE reading of
 *   a contested kernel about Indian federal asymmetry: the reading that
 *   linguistic reorganization was a legitimate constitutional choice that
 *   granted linguistic communities recognition and state form, even at the
 *   cost of the founders' multilingual-state design. This reading coexists
 *   with two sibling readings: (1) the Article 370 reading, which focuses on
 *   the special constitutional status granted to Jammu and Kashmir as an
 *   asymmetry that outlived its warrant; (2) the union-bias reading, which
 *   argues that Indian federalism was designed with structural centralization
 *   as a principle, making all state-level identity claims structurally
 *   vulnerable to center overrule. This reading emphasizes what was gained
 *   (linguistic communities achieved statehood); the sibling readings
 *   emphasize what was lost or never decentralized (Kashmir's autonomy,
 *   states' structural power). The constraint exhibits extractive dynamics
 *   (non-dominant linguistic minorities trapped within states organized by
 *   dominant language majorities) alongside genuine coordination (linguistic
 *   movements solved the collective action problem of boundary definition
 *   through mobilization). This Tangled Rope classification reflects the
 *   hybridization: the reorganization coordinated linguistic identity as a
 *   principle, but the coordination process created new victims (linguistic
 *   minorities who cannot form their own states).
 *
 * KEY AGENTS:
 *   - Linguistic communities with demographic weight to form states (Tamil, Marathi, Gujarati, Telugu, Kannada, Malayalam): Primary beneficiaries (organized/mobile) — achieved organized statehood and recognition for linguistic identity
 *   - Non-dominant linguistic minorities (speakers of languages without sufficient demographic concentration): Primary victims (powerless/trapped) — subordinated to majoritarian language policies within multilingual states; high exit costs
 *   - Multilingual-state design principle (Nehru-Patel constitutional vision): Victim (powerless/trapped) — abstract principle with no organized constituency; extracted from through fragmentation of planned multilingual units
 *   - Independence leadership / constitutional authority (Nehru, Patel, other framers): Secondary actor (institutional/constrained) — held firm against linguistic reorganization 1947-1956, then yielded to political pressure; experienced constraint as temporary (scaffold) compromise
 *   - State governments and language academies (institutional enforcers post-1956): Institutional actors (institutional/arbitrage) — benefit from language-based state organization (administrative coherence, electoral legitimacy); maintain performance through language policy enforcement
 *   - Analytical observer: Sees constraint as risk of naturalizing contingent choice as inevitable law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federal_asymmetry__linguistic_reorganization_reading, 0.38).
domain_priors:suppression_score(federal_asymmetry__linguistic_reorganization_reading, 0.42).
domain_priors:theater_ratio(federal_asymmetry__linguistic_reorganization_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federal_asymmetry__linguistic_reorganization_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(federal_asymmetry__linguistic_reorganization_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(federal_asymmetry__linguistic_reorganization_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federal_asymmetry__linguistic_reorganization_reading, tangled_rope).
narrative_ontology:human_readable(federal_asymmetry__linguistic_reorganization_reading, "Federal Asymmetry: Linguistic Reorganization Reading (1956)").
narrative_ontology:topic_domain(federal_asymmetry__linguistic_reorganization_reading, "legal/constitutional/political").

domain_priors:requires_active_enforcement(federal_asymmetry__linguistic_reorganization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federal_asymmetry__linguistic_reorganization_reading, 'ab728a74-1451-431c-a86c-987286f34681').
narrative_ontology:cs_kernel_codification('ab728a74-1451-431c-a86c-987286f34681', formalized).
narrative_ontology:cs_authority_grounding('ab728a74-1451-431c-a86c-987286f34681', lineage).
narrative_ontology:cs_interpretation_layer_present('ab728a74-1451-431c-a86c-987286f34681').
narrative_ontology:cs_reading_relation('ab728a74-1451-431c-a86c-987286f34681', federal_asymmetry__article_370_special_status, influences).
narrative_ontology:cs_reading_relation('ab728a74-1451-431c-a86c-987286f34681', federal_asymmetry__union_bias_design_reading, coexists_with).
narrative_ontology:cs_axiom('ab728a74-1451-431c-a86c-987286f34681', foundational, linguistic_identity_is_legitimate_federalism_basis).
narrative_ontology:cs_axiom_status(linguistic_identity_is_legitimate_federalism_basis, holdable).
narrative_ontology:cs_axiom_grounding('ab728a74-1451-431c-a86c-987286f34681', linguistic_identity_is_legitimate_federalism_basis, deontological).
narrative_ontology:cs_axiom('ab728a74-1451-431c-a86c-987286f34681', secondary, statehood_for_linguistic_communities_resolves_asymmetry).
narrative_ontology:cs_axiom_status(statehood_for_linguistic_communities_resolves_asymmetry, holdable).
narrative_ontology:cs_axiom_grounding('ab728a74-1451-431c-a86c-987286f34681', statehood_for_linguistic_communities_resolves_asymmetry, instrumental).
narrative_ontology:cs_reference_frame('ab728a74-1451-431c-a86c-987286f34681', founders_multilingual_federalism_principle).
narrative_ontology:cs_drift_state('ab728a74-1451-431c-a86c-987286f34681', post_1956_linguistic_state_consolidation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ab728a74-1451-431c-a86c-987286f34681', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(federal_asymmetry__linguistic_reorganization_reading, federal_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federal_asymmetry__linguistic_reorganization_reading, linguistic_communities_with_state_identity).
narrative_ontology:constraint_victim(federal_asymmetry__linguistic_reorganization_reading, multilingual_state_designs).
narrative_ontology:constraint_victim(federal_asymmetry__linguistic_reorganization_reading, non_dominant_linguistic_minorities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LINGUISTIC COMMUNITIES (ROPE) — Organized movements (Tamil, Marathi, Gujarati communities) mobilized for linguistic states and achieved coordination outcome: language became the legitimate basis for state formation. The constraint functioned as pure coordination — solving the collective action problem of how to draw federal boundaries in a multilingual society. Beneficiaries with genuine mobility (exit option: dissolve the movement or redirect energy) see the constraint as enabling, not extractive.
constraint_indexing:constraint_classification(federal_asymmetry__linguistic_reorganization_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 2: NON-DOMINANT LINGUISTIC MINORITIES (TANGLED ROPE) — Speakers of languages without sufficient demographic concentration to form their own state experience both coordination and extraction. The linguistic reorganization coordinated boundaries around dominant languages (Tamil, Marathi, Telugu, Kannada, Malayalam) but left smaller linguistic groups scattered across multiple states, subordinated to majoritarian language policies. High suppression (language policy enforcement, educational exclusion); constrained exit (costly migration to find linguistic community within state boundary; career and social costs of linguistic assimilation).
constraint_indexing:constraint_classification(federal_asymmetry__linguistic_reorganization_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MULTILINGUAL-STATE DESIGN (SNARE) — The principle that federal units should encompass multiple linguistic communities — a design choice made by independence leaders to prevent linguistic fragmentation and manage India's extraordinary diversity — becomes a victim of the reorganization. Once language becomes the legitimate organizing principle, states built on the design rationale (Bihar with Hindi and Urdu speakers; Bombay State with Marathi and Gujarati) fragment along linguistic lines. The multilingual state design has no voice, no exit, no organized constituency. Trapped, extracts resources and state capacity without benefit.
constraint_indexing:constraint_classification(federal_asymmetry__linguistic_reorganization_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 4: INDEPENDENCE LEADERSHIP (SCAFFOLD) — The Nehru-Patel axis and constitutional architects viewed linguistic reorganization as a dangerous precedent: conceding linguistic identity as a legitimate basis for state formation threatened the very federalism they had designed to hold India together. Linguistic reorganization represents a temporary constraint from this perspective — a tactical retreat on a principle the leadership held firm for 1947-1956 but eventually yielded to political pressure. The constraint has a sunset: once linguistic states are established and stabilized (by ~1970), the precedent becomes normal and the active suppression of linguistic state demands ceases. Extractiveness moderate because the leadership traded principle for political stability — a genuine bargain, not pure extraction.
constraint_indexing:constraint_classification(federal_asymmetry__linguistic_reorganization_reading, scaffold,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LINGUISTIC NATIONALISM AS IDEOLOGICAL PERFORMANCE (PITON) — The 1956 reorganization and subsequent linguistic state consolidation are substantially performative at the civilizational scale. The theater: language as authentic identity (theater_ratio ≈ 0.55) masks the reality that state boundaries reorganized along linguistic lines do not resolve multilingualism — they entrench it. Every Indian linguistic state contains significant linguistic minorities (up to 30-40% in some states). The constraint persists because the ideological claim (states organized by language preserve and protect linguistic identity) remains emotionally potent even as the actual function (creating homogeneous linguistic units) fails. Institutional actors (state governments, language academies, education departments) maintain the performance through language-based policies even when these policies are administratively degraded or ineffective.
constraint_indexing:constraint_classification(federal_asymmetry__linguistic_reorganization_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, the linguistic reorganization appears as an immutable feature of how large multilingual states organize themselves: language is an irreducible coordinate of human identity; federal systems must accommodate linguistic boundaries or face dissolution. This perspective risks naturalizing the 1956 reorganization as inevitable, even as the structural data reveals it as a contingent choice. The analytical observer must account for: (1) India's founders explicitly rejected linguistic federalism, (2) the reorganization was driven by political movements and pressure, not by structural inevitability, (3) other multilingual democracies (Canada, Switzerland, Belgium) use different organizational logics. The mountain classification is a false summit — contingent institutional choice masquerading as natural law.
constraint_indexing:constraint_classification(federal_asymmetry__linguistic_reorganization_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federal_asymmetry__linguistic_reorganization_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(federal_asymmetry__linguistic_reorganization_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(federal_asymmetry__linguistic_reorganization_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(federal_asymmetry__linguistic_reorganization_reading, TR),
    TR >= 0.70.

:- end_tests(federal_asymmetry__linguistic_reorganization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate, reflecting a genuine hybrid of coordination and extraction. The reorganization solved a real coordination problem — how to draw federal boundaries in a linguistically diverse polity without arbitrary colonial carve-outs — and linguistic communities achieved what they mobilized for (statehood). This is genuine coordination, not extraction. However, the coordination process created new asymmetries: non-dominant linguistic minorities became trapped within states organized by dominant language majorities. The trajectory from 0.22 → 0.38 captures how the initial benefit (achieving statehood) gradually reveals embedded extraction (language policies disadvantaging minorities within the new states). Suppression (0.42): Moderate-high, rising from 0.28. The constraint requires active enforcement through language-based state policies (medium of instruction, government employment, minority language marginalization). Suppression was lighter initially (colonial boundaries already suppressing linguistic consciousness) but intensified as linguistic state identities hardened and linguistic minorities within each state experienced policy-driven marginalization. Theater ratio (0.35): Moderate-low, the lowest among the three metrics. This reading's constraint is relatively functional (not primarily performative) because linguistic organization actually did create state units organized around language, and these units did coordinate some genuine functions. The theater that exists (the claim that language-based states 'naturally' preserve linguistic identity despite internal multilingualism) is substantial but not the primary mechanism of the constraint.
 *
 * PERSPECTIVAL GAP:
 *   The perspectives span from Rope (linguistic communities with state-building agency) through Tangled Rope (minorities trapped in dominant-language states) to Snare (multilingual-state design principle with no voice) to Scaffold (temporary leadership retreat under pressure) to Piton (ideological performance post-stabilization) to Mountain (false summit of naturalized contingency). This range demonstrates how a single constitutional choice appears through completely different lenses: (1) Rope to those who mobilized and achieved their demand (linguistic communities); (2) Tangled Rope/Snare to those who lost structure or gained subordination (minorities, multilingual-state design); (3) Scaffold to those who yielded ground (leadership, wanting to frame retreat as temporary); (4) Piton to those maintaining the arrangement after functional need passes (institutional actors); (5) false-summit Mountain to those who risk naturalizing the choice. The gap is not one of interpretation — it reflects real structural differences in how the constraint affects different agents. The same constitutional choice coordinates some agents and extracts from others.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality (d values) derives from each agent's structural relationship to linguistic reorganization. Linguistic communities with demographic weight achieve statehood (beneficiary status, mobile exit option: can dissolve movements or redirect energy → low d → negative/low χ, experienced as Rope). Non-dominant linguistic minorities face trapped status within states organized by dominant language majorities (victim status, trapped exit → high d → high χ, experienced as Snare). The multilingual-state design principle has no constituency and no exit (victim status, trapped → high d → high χ, experienced as pure victim). The independence leadership yields to organized pressure (constrained exit, transient beneficiary status → moderate d, experienced as temporary Scaffold). Institutional actors maintaining language policies benefit from state-organized linguistic coherence (beneficiary status, arbitrage exit → low d → low χ, experienced as Rope/Piton). The analytical observer sees the constraint from outside any particular beneficiary/victim position and must account for why the natural-law appearance (language naturally organizes federalism) differs from the structural data (founders rejected linguistic federalism, reorganization followed political mobilization, not structural inevitability).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not generate mandatrophy because it resolves the coordination/extraction distinction cleanly: linguistic reorganization genuinely coordinated (solved the boundary-drawing problem) AND genuinely extracted (created minority subordination). The Tangled Rope classification captures both functions simultaneously. The mandatrophy risk lies in the false-summit perspective: if the analytical observer naturalizes the reorganization as inevitable, they risk missing that the founders actively rejected it and that it required sustained political mobilization to overcome that rejection. The mandatrophy resolves through recognizing that legitimacy claims (language is an authentic basis for federalism) and structural outcomes (minorities within linguistic states are disadvantaged) are both real. The constraint's validity is not threatened by this dual reality — Tangled Ropes genuinely do both coordinate and extract.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    linguistic_identity_authenticity,
    'Is linguistic identity an authentic, pre-political fact that federal structures should recognize, or is it a politicized category constructed through nationalist movements and state enforcement?',
    'Historical analysis of linguistic consciousness pre-1900 vs post-1950; ethnographic documentation of how linguistic identity claims emerge and stabilize; comparison with cases where language policy failed to stabilize linguistic identity (Sri Lanka, Pakistan).',
    'If authentic: reorganization was coordination of pre-existing communities (Rope classification confirmed). If constructed: reorganization was channeling of political movements into state-building machinery (Tangled Rope/Snare classification confirmed). Classification oscillates based on the resolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(linguistic_identity_authenticity, conceptual, 'Whether linguistic identity is pre-political or constructed through nationalist movements').

omega_variable(
    multilingual_state_viability,
    'Could multilingual states (Bombay State, undivided Bihar) have functioned sustainably with genuine linguistic minority protections, or was reorganization inevitable once language became a mobilization axis?',
    'Counterfactual institutional analysis: what would constitutional protections for linguistic minorities within multilingual states have required? Comparison with multilingual states in other democracies (Belgium post-1970 reforms, Malaysia, Nigeria). Historical tracing of why minority language protections failed in Indian states (1950-1956).',
    'If viable: multilingual-state design was a victim of political pressure, not structural impossibility (Snare classification for the design principle confirmed). If structurally unviable: reorganization solved a genuine coordination failure (Rope classification from linguistic community perspective confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(multilingual_state_viability, empirical, 'Whether multilingual states could have sustainably protected linguistic minorities').

omega_variable(
    non_dominant_minority_extraction_magnitude,
    'What proportion of the constraint''s extractiveness flows to linguistic minorities who lack demographic weight to form their own states, versus accruing to dominant-language communities?',
    'Analysis of state-level language policy outcomes post-1956: school enrollment by minority language, government employment by language background, minority language media access. Measurement of exit costs (migration, assimilation pressure) by linguistic minority group size and geographic distribution.',
    'If >60% of extraction targets minorities: constraint is primarily a Snare on minorities (high extractiveness from powerless agents). If <40%: constraint functions more as Tangled Rope with distributed costs and benefits.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(non_dominant_minority_extraction_magnitude, empirical, 'Proportion of constraint''s extractiveness that flows to non-dominant linguistic minorities').

omega_variable(
    reading_ambiguity__linguistic_foundation,
    'This reading instantiates ONE claim within the federal_asymmetry kernel: that linguistic reorganization was a legitimate concession to identity-based federalism. But did the 1956 reorganization actually RESOLVE linguistic asymmetry, or did it CREATE NEW asymmetries by entrenching some languages as state-organizing principles while marginalizing others?',
    'Comparative analysis: measure asymmetry pre-1956 (arbitrary colonial boundaries, linguistic communities without state form) vs post-1956 (linguistic state form achieved, but linguistic minorities within states disadvantaged). Determine whether overall asymmetry increased or decreased. Compare with the alternative readings'' claims about asymmetry (Article 370 reading focuses on Jammu-Kashmir asymmetry; Union bias reading focuses on center-state asymmetry).',
    'If asymmetry decreased: this reading successfully describes a legitimate constitutional choice. If asymmetry shifted form (old form to new form): this reading captures one transformation within a larger asymmetry that persists. If asymmetry increased: this reading may be describing a false summit (naturalizing extraction as identity accommodation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_ambiguity__linguistic_foundation, conceptual, 'Whether linguistic reorganization resolved or relocated federal asymmetry').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federal_asymmetry__linguistic_reorganization_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lingorg_theater_t0, federal_asymmetry__linguistic_reorganization_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(lingorg_theater_t3, federal_asymmetry__linguistic_reorganization_reading, theater_ratio, 3, 0.3).
narrative_ontology:measurement(lingorg_theater_t6, federal_asymmetry__linguistic_reorganization_reading, theater_ratio, 6, 0.35).

% Extraction over time
narrative_ontology:measurement(lingorg_extractiveness_t0, federal_asymmetry__linguistic_reorganization_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(lingorg_extractiveness_t3, federal_asymmetry__linguistic_reorganization_reading, base_extractiveness, 3, 0.3).
narrative_ontology:measurement(lingorg_extractiveness_t6, federal_asymmetry__linguistic_reorganization_reading, base_extractiveness, 6, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(lingorg_suppression_t0, federal_asymmetry__linguistic_reorganization_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(lingorg_suppression_t3, federal_asymmetry__linguistic_reorganization_reading, suppression_requirement, 3, 0.35).
narrative_ontology:measurement(lingorg_suppression_t6, federal_asymmetry__linguistic_reorganization_reading, suppression_requirement, 6, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federal_asymmetry__linguistic_reorganization_reading, identity_coordination).
narrative_ontology:affects_constraint(federal_asymmetry__linguistic_reorganization_reading, federal_asymmetry__article_370_special_status).
narrative_ontology:affects_constraint(federal_asymmetry__linguistic_reorganization_reading, federal_asymmetry__union_bias_design_reading).

% DUAL FORMULATION NOTE:
% The federal_asymmetry kernel has three sibling readings, each focusing on a different aspect of how Indian federalism distributes power and identity recognition. This reading (linguistic_reorganization) emphasizes the legitimate grant of statehood to linguistic communities and the resulting extraction from linguistic minorities. The article_370 reading focuses on Kashmir's special constitutional status as a paradigmatic asymmetry. The union_bias reading emphasizes structural centralization as a design feature. All three are present in the actual Indian federal system; they represent different emphases on the same contested constitution. The readings do not foreclose each other — they coexist as different parties' claims about what Indian federalism is. However, this reading's claim (language is a legitimate organizing principle) influences the other two by establishing identity-based federalism as a constitutional principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federal_asymmetry__linguistic_reorganization_reading, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
