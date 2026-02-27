% ============================================================================
% CONSTRAINT STORY: microbiome_symbiosis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_microbiome_symbiosis, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: microbiome_symbiosis
 *   human_readable: The Microbial-Immune Symbiosis
 *   domain: biological/healthcare
 *
 * SUMMARY:
 *   The microbial-immune symbiosis represents a foundational biological
 *   constraint in which the human host and its resident gut microbiota are
 *   locked in obligate mutualistic coordination. The constraint exhibits the
 *   full range of DR classifications depending on observer position: from the
 *   civilizational analytical perspective, it appears as an immutable natural
 *   law (Mountain) — vertebrate immune systems cannot develop proper
 *   tolerance without microbial ligands, and humans cannot synthesize
 *   essential nutrients that microbiota produce. From the perspective of a
 *   healthy host, the symbiosis is pure coordination (Rope) — both partners
 *   benefit from stable coexistence. From the perspective of dysbiotic hosts
 *   with perturbed microbiota (following antibiotic use, dietary change, or
 *   chronic stress), the symbiosis degrades into a tangled rope where
 *   compromised microbiota provide insufficient immune education while the
 *   host remains dependent on microbial functions. The clinical and
 *   commercial probiotic intervention apparatus functions with substantial
 *   theater: evidence for specific strains is limited, most ingested microbes
 *   fail to colonize stably, and marketing claims far exceed demonstrated
 *   efficacy. This theater has increased over the measurement interval as
 *   commercial probiotic markets have grown while clinical efficacy remains
 *   modest. The constraint's base extractiveness (0.28) is moderate and
 *   primarily derives from the evolutionary lock-in between host immune
 *   genetics and microbial metabolic capacities rather than from deliberate
 *   suppression or coercion — the symbiosis is fundamentally coordinative,
 *   not extractive.
 *
 * KEY AGENTS:
 *   - Host Immune System: Co-beneficiary (powerful/mobile) — develops tolerance through microbial ligands; protected from pathogens through competitive exclusion by commensals
 *   - Gut Microbiota: Co-beneficiary (institutional/arbitrage) — gain stable growth environment, nutrients, and ecological protection from the host; no significant extraction from microbes
 *   - Dysbiotic Microbiota: Primary victim in dysbiotic state (moderate/constrained) — compromised community structure fails to provide immune education; host suffers inflamed barrier and aberrant immune responses
 *   - Nutrient Absorption System: Beneficiary (institutional/arbitrage) — dependent on microbial synthesis of vitamins B12, K2, and other factors
 *   - Pathogenic Microbes: Opportunistic beneficiary in dysbiotic states (moderate/mobile) — exploit dysbiotic niche unavailable in healthy microbiota
 *   - Probiotic Intervention Apparatus: Institutional actor (institutional/constrained) — commercial and clinical systems maintain probiotic recommendations despite modest evidence; theater_ratio indicates performative rather than functional content
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(microbiome_symbiosis, 0.28).
domain_priors:suppression_score(microbiome_symbiosis, 0.12).
domain_priors:theater_ratio(microbiome_symbiosis, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(microbiome_symbiosis, extractiveness, 0.28).
narrative_ontology:constraint_metric(microbiome_symbiosis, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(microbiome_symbiosis, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(microbiome_symbiosis, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(microbiome_symbiosis, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(microbiome_symbiosis, rope).
narrative_ontology:human_readable(microbiome_symbiosis, "The Microbial-Immune Symbiosis").
narrative_ontology:topic_domain(microbiome_symbiosis, "biological/healthcare").

domain_priors:emerges_naturally(microbiome_symbiosis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(microbiome_symbiosis, host_immune_system).
narrative_ontology:constraint_beneficiary(microbiome_symbiosis, gut_microbiota).
narrative_ontology:constraint_beneficiary(microbiome_symbiosis, nutrient_absorption).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NATURAL LAW (MOUNTAIN) — From a civilizational/universal view, the microbial-immune symbiosis is a fundamental constraint of vertebrate biology. Multicellular organisms cannot survive without microbial populations; immune tolerance requires microbial ligands to develop properly. This is not contingent on policy, culture, or institutional design — it emerges from the biochemistry of pathogen recognition receptors and the phylogeny of vertebrate development. d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.32 (mountain gate: ε≤0.25, suppression≤0.05 satisfied).
constraint_indexing:constraint_classification(microbiome_symbiosis, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: HOST ORGANISM (ROPE) — The individual human benefits from stable microbial colonization through nutrient synthesis (B12, K2), immune education, and pathogen exclusion. The constraint appears as coordination: the host and microbes solve the collective action problem of resource sharing and defense against pathogens. Exit options are limited (cannot truly exit the microbiota) but mobility exists through dietary and environmental microbiota modification. d≈0.50, f(d)≈0.65, σ=0.8 → χ≈0.15.
constraint_indexing:constraint_classification(microbiome_symbiosis, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 3: MICROBIAL COMMUNITY (ROPE) — Microbiota benefit from stable host environments, temperature regulation, and nutrient delivery; they solve the problem of competing with pathogens for ecological niches. The relationship is coordination without significant extraction from microbes — they are not suppressed or extracted from in a structural sense. d≈0.10, f(d)≈0.02, σ=1.2 → χ≈0.01.
constraint_indexing:constraint_classification(microbiome_symbiosis, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DYSBIOTIC HOST / IMMUNE-DYSREGULATED (TANGLED ROPE) — In dysbiotic states (antibiotic overuse, high-fat diet, stress), the symbiosis degrades. The host suffers from inflamed gut barrier and aberrant immune responses (allergies, autoimmune disease, IBS) while still dependent on microbial functions for survival. This is extraction with enforcement: the host is constrained by microbial deficiency yet cannot easily escape (cannot produce vitamins de novo, cannot function without some microbiota). Victims: immune homeostasis, barrier integrity. Beneficiaries: pathogenic microbes that exploit the dysbiotic niche. d≈0.85, f(d)≈1.15, σ=1.0 → χ≈0.32.
constraint_indexing:constraint_classification(microbiome_symbiosis, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INFANT GUT COLONIZATION (ROPE) — Newborns must establish microbiota from maternal and environmental sources; this is a coordination problem without alternatives. The infant has no choice but to cooperate with microbial establishment. However, this is not extraction (no suppression, no coercion beyond biological inevitability) — it is pure coordination. The symbiosis emerges through bacterial chemotaxis and epithelial selectivity, not through suppression. d≈0.90, f(d)≈1.38, σ=0.8 → χ≈0.32 (high d but low suppression keeps it Rope, not Snare).
constraint_indexing:constraint_classification(microbiome_symbiosis, rope,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 6: MEDICAL INTERVENTION / PROBIOTIC THEATER (PITON) — Clinical and commercial probiotic interventions (yogurt cultures, commercial probiotics, fecal microbiota transplantation) often function with high theater: probiotic marketing claims far exceed evidence; specific strains persist briefly and are often eliminated by the host immune system or excluded by resident microbiota; clinical efficacy is modest and strain-dependent. theater_ratio=0.50+ indicates that the intervention apparatus performs its function (restoring diversity) incompletely and often relies on placebo mechanisms. The piton persists through institutional inertia and consumer demand despite limited effectiveness in most conditions. d≈0.45, f(d)≈0.50, σ=1.0 → χ≈0.25.
constraint_indexing:constraint_classification(microbiome_symbiosis, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(microbiome_symbiosis_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(microbiome_symbiosis, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(microbiome_symbiosis, TypeOther, context(agent_power(analytical), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(microbiome_symbiosis, TR),
    TR >= 0.70.

:- end_tests(microbiome_symbiosis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-to-moderate. The symbiosis is fundamentally coordinative rather than extractive. Both host and microbiota benefit from the relationship. The measured extractiveness reflects (a) evolutionary lock-in: the host cannot easily modify immune genetics to reduce dependence on microbial ligands, and microbiota cannot exist without the host environment, and (b) dysbiotic degradation: in dysbiotic states, compromised microbiota extract costs from immune homeostasis while still being depended upon. The 0.28 value represents a healthy baseline with some dysbiotic variation. Suppression (0.12): Low. The symbiosis relies on biological coordination (chemotaxis, epithelial selectivity, metabolic complementarity), not on active suppression of alternatives. Hosts can modify their microbiota through diet and antibiotics, though this is suboptimal. Microbiota face no suppression — they thrive in the colonic environment. Theater ratio (0.35): Moderate, driven primarily by the clinical probiotic apparatus. Most commercial probiotics exhibit limited efficacy and high placebo content. Specific strains rarely establish stable colonization, and health claims often exceed evidence. This theater has increased over the interval (0.15 → 0.35) as probiotic markets have grown and as clinical recommendations have expanded beyond evidence-supported conditions. Accessibility collapse (0.92): Extreme. The symbiosis is inaccessible to modification at scale — hosts cannot redesign their immune systems or microbiota architecture without severe intervention (FMT, antibiotics), and those interventions have substantial side effects. Resistance (0.08): Very low. The symbiosis is resistant to disruption only through intentional agents (broad-spectrum antibiotics, extreme dietary change) or pathogenic challenge. In normal conditions, it is remarkably stable.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a perspectival gap primarily between healthy and dysbiotic states, not between coequal agents. From the healthy-state analytical perspective, the symbiosis appears as an immutable natural law (Mountain). From the dysbiotic perspective, it appears as a tangled rope where compromised microbiota extract costs while the host remains dependent. From the healthy host's perspective, it is pure coordination (Rope). From the dysbiotic host's perspective, it is mixed extraction and dependence. From the probiotic intervention system's perspective, it is a piton — performative recommendations persist despite limited efficacy. The gap is not perspectival disagreement among equally positioned agents but rather different structural states (health vs dysbiosis) producing different classifications. All perspectives converge that the baseline healthy symbiosis is Rope; the divergence occurs when dysbiosis or intervention occurs.
 *
 * DIRECTIONALITY LOGIC:
 *   Host immune system + nutrient absorption: Beneficiary + mobile → d≈0.50, f(d)≈0.65. Moderate directionality; benefits substantial but not asymmetric. Microbiota: Beneficiary + arbitrage → d≈0.10, f(d)≈0.02. Minimal directionality; microbiota are as much beneficiary as host. Dysbiotic microbiota: Victim in dysbiotic state but constrained exit → d≈0.85, f(d)≈1.15. High extraction in dysbiotic context. Probiotic apparatus: Institutional + constrained → d≈0.45, f(d)≈0.50. Moderate directionality; the apparatus is partially captured by market incentives to sell products with modest evidence. Analytical observer: d≈0.72, f(d)≈1.15. Mountain classification reflects naturalness, not high extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the healthy microbial-immune symbiosis is fundamentally Rope (pure coordination with low extraction), while dysbiotic and clinical intervention states introduce tangled rope and piton elements. The analytical observer's mountain classification reflects the fact that immune tolerance requires microbial ligands and that this is a universal, inaccessible constraint across all humans — not a contingent institutional arrangement. However, the mountain is a partial view: the symbiosis is a natural law of biology (immune tolerance irreducibly requires microbes), but specific dysbiotic or pathological states involve tangled ropes (compromised microbiota extraction) or pitons (performative intervention). The constraint system avoids the mandatrophy by decomposing observations: (1) baseline symbiosis = mountain (natural law of immune tolerance), (2) dysbiotic degradation = tangled rope (compromised coordination + extraction), (3) probiotic intervention = piton (theater-driven apparatus). All three are valid; none are contradictory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    symbiosis_vs_parasitism_boundary,
    'Where is the boundary between obligate mutualistic symbiosis and sustained parasitism in dysbiotic states? Are dysbiotic microbes extracting from the host, or are they simply maladapted remnants of the symbiotic community?',
    'Longitudinal genomic analysis of dysbiotic microbiota; comparison of pathogenic load, metabolic output, and inflammatory markers between dysbiotic and healthy states; assessment of whether dysbiotic microbes actively suppress immune recovery or passively fail to restore homeostasis',
    'If dysbiotic microbes are true parasites: dysbiotic perspective classifies as Snare (pure extraction). If maladapted remnants: classify as degraded Rope (Piton). Classification determines whether intervention should target pathogen elimination (Snare logic) or community restoration (Rope logic).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symbiosis_vs_parasitism_boundary, empirical, 'Boundary between symbiosis and parasitism in dysbiotic states').

omega_variable(
    immune_tolerance_necessity,
    'Is immune tolerance to commensal microbiota an irreducible requirement of vertebrate development, or is it a contingent product of evolutionary history that could theoretically be replaced by alternative mechanisms?',
    'Comparative immunology across species with different microbiota compositions; analysis of whether germ-free animals can develop normal immune function; investigation of whether tolerance can be induced by non-microbial ligands',
    'If irreducible: the mountain classification stands for all healthy-state perspectives. If contingent: the symbiosis is a tangled rope (coordination + some evolutionary lock-in) rather than a true natural law.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(immune_tolerance_necessity, empirical, 'Whether immune tolerance to microbiota is irreducible or contingent').

omega_variable(
    probiotic_strain_persistence,
    'Why do most ingested probiotic strains fail to stably colonize the human gut, and does this reflect biological limits or suboptimal formulation/dosing?',
    'High-resolution tracking studies of specific probiotic strains; comparison of strain persistence in different host populations; analysis of whether higher doses or different delivery mechanisms improve colonization',
    'If biological limit: probiotic theater is inherent to the system (Piton classification stable). If formulation issue: current probiotic theater is addressable, and the system could shift toward higher functional content (Rope from medical perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(probiotic_strain_persistence, empirical, 'Determinants of probiotic strain persistence in the gut').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(microbiome_symbiosis, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(microbiome_tr_t0, microbiome_symbiosis, theater_ratio, 0, 0.15).
narrative_ontology:measurement(microbiome_tr_t5, microbiome_symbiosis, theater_ratio, 5, 0.25).
narrative_ontology:measurement(microbiome_tr_t10, microbiome_symbiosis, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(microbiome_be_t0, microbiome_symbiosis, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(microbiome_be_t5, microbiome_symbiosis, base_extractiveness, 5, 0.2).
narrative_ontology:measurement(microbiome_be_t10, microbiome_symbiosis, base_extractiveness, 10, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(microbiome_symbiosis, global_infrastructure).
narrative_ontology:affects_constraint(microbiome_symbiosis, immune_tolerance_development).
narrative_ontology:affects_constraint(microbiome_symbiosis, antibiotic_resistance_selection).
narrative_ontology:affects_constraint(microbiome_symbiosis, inflammatory_bowel_disease_pathogenesis).

% DUAL FORMULATION NOTE:
% The microbial-immune symbiosis is a foundational constraint from which downstream constraints emerge: immune tolerance development (how the symbiosis establishes), antibiotic resistance (when symbiosis is perturbed), and IBD pathogenesis (when symbiosis degrades). Each downstream constraint has a higher ε reflecting contingent pathological or selection pressures, while the baseline symbiosis has low extractiveness.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
