% ============================================================================
% CONSTRAINT STORY: animal_status__welfare_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status__welfare_reading, []).

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
 *   constraint_id: animal_status__welfare_reading
 *   human_readable: Animal Sentience as Welfare Constraint (Welfare Reading)
 *   domain: applied_ethics/legal_philosophy/political_economy
 *
 * SUMMARY:
 *   The welfare reading of animal status recognizes that sentience constrains
 *   but does not prohibit human use of animals. This is one reading of a
 *   contested kernel — the animal_status kernel that different philosophical
 *   and legal traditions interpret differently. The welfare reading grounds
 *   its legitimacy in a foundational axiom: sentience generates welfare
 *   constraints (prohibition of gratuitous harm) but not use constraints
 *   (instrumental use remains permissible if necessary). This reading
 *   coexists with the abolitionist reading (instrumental use is categorically
 *   prohibited) and the property reading (sentience generates no moral
 *   constraint). The welfare constraint instantiates a tangled rope
 *   structure: it genuinely coordinates welfare improvements (reduction of
 *   unnecessary suffering, standardization of humane practices) while
 *   simultaneously extracting legitimacy from those improvements, using
 *   welfare frameworks to exempt continued instrumental use from escalating
 *   moral scrutiny. The extractiveness value (ε=0.45) reflects this hybrid:
 *   the constraint is neither pure coordination (Rope, ε<0.45) nor pure
 *   extraction (Snare, ε≥0.46). The constraint asymmetrically benefits
 *   animal-use industries (who gain moral cover and market access) while
 *   constraining them (who bear compliance costs). The theater ratio (0.64)
 *   documents the performative element: substantial regulatory apparatus
 *   (inspection regimes, certification standards, welfare labeling) manages
 *   visibility of harm while leaving harm levels largely unchanged across
 *   welfare-compliant systems.
 *
 * KEY AGENTS:
 *   - Animal subjects: powerless/trapped — bears full cost of welfare exemptions; experiences constraint as permitting extraction
 *   - Welfare advocates: moderate/constrained — constrained by cultural norms and institutional barriers; benefit from framework as vehicle for incremental protection
 *   - Animal agriculture and research industries: institutional/arbitrage — primary beneficiaries; experience constraint as solving moral coordination problem while legitimizing use
 *   - Rights-based reformers: organized/constrained — see welfare as temporary scaffold toward abolition; pushing boundary redefinitions of 'necessity'
 *   - Legal frameworks and enforcement bodies: institutional/arbitrage — maintain performative regulatory apparatus; theater-heavy compliance structures
 *   - Corporate reformers: powerful/mobile — experience constraint as coordination mechanism requiring visible welfare investment and reputational risk management
 *   - Analytical observer: analytical/analytical — risks naturalizing the welfare reading's constructed boundary (sentience permits use) as a natural fact
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status__welfare_reading, 0.45).
domain_priors:suppression_score(animal_status__welfare_reading, 0.58).
domain_priors:theater_ratio(animal_status__welfare_reading, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status__welfare_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(animal_status__welfare_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(animal_status__welfare_reading, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status__welfare_reading, tangled_rope).
narrative_ontology:human_readable(animal_status__welfare_reading, "Animal Sentience as Welfare Constraint (Welfare Reading)").
narrative_ontology:topic_domain(animal_status__welfare_reading, "applied_ethics/legal_philosophy/political_economy").

domain_priors:requires_active_enforcement(animal_status__welfare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__welfare_reading, 'e687daca-0e6b-4a44-9816-b20f6f75c0aa').
narrative_ontology:cs_kernel_codification('e687daca-0e6b-4a44-9816-b20f6f75c0aa', distributed).
narrative_ontology:cs_authority_grounding('e687daca-0e6b-4a44-9816-b20f6f75c0aa', distributed).
narrative_ontology:cs_reading_relation('e687daca-0e6b-4a44-9816-b20f6f75c0aa', animal_status__abolitionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('e687daca-0e6b-4a44-9816-b20f6f75c0aa', animal_status__property_reading, coexists_with).
narrative_ontology:cs_axiom('e687daca-0e6b-4a44-9816-b20f6f75c0aa', foundational, sentience_constrains_gratuitous_harm).
narrative_ontology:cs_axiom_status(sentience_constrains_gratuitous_harm, holdable).
narrative_ontology:cs_axiom_grounding('e687daca-0e6b-4a44-9816-b20f6f75c0aa', sentience_constrains_gratuitous_harm, deontological).
narrative_ontology:cs_axiom('e687daca-0e6b-4a44-9816-b20f6f75c0aa', foundational, necessity_exempts_sentience_constraint).
narrative_ontology:cs_axiom_status(necessity_exempts_sentience_constraint, holdable).
narrative_ontology:cs_axiom_grounding('e687daca-0e6b-4a44-9816-b20f6f75c0aa', necessity_exempts_sentience_constraint, instrumental).
narrative_ontology:cs_reference_frame('e687daca-0e6b-4a44-9816-b20f6f75c0aa', sentience_permits_welfare_constrained_use).
narrative_ontology:cs_drift_state('e687daca-0e6b-4a44-9816-b20f6f75c0aa', contemporary_reform_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('e687daca-0e6b-4a44-9816-b20f6f75c0aa', '').
narrative_ontology:cs_kernel_id(animal_status__welfare_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status__welfare_reading, animal_agriculture_industry).
narrative_ontology:constraint_beneficiary(animal_status__welfare_reading, biomedical_research_establishment).
narrative_ontology:constraint_beneficiary(animal_status__welfare_reading, human_consumers).
narrative_ontology:constraint_victim(animal_status__welfare_reading, animals_subject_to_use).
narrative_ontology:constraint_victim(animal_status__welfare_reading, ethics_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ANIMAL SUBJECT (SNARE) — Trapped without exit; bears full cost of welfare exemptions. Sentience is recognized but instrumentalized: the constraint permits suffering if deemed 'necessary' for human benefit. The animal experiences extraction as pure coercion — no legitimate benefit, only permitted harm justified by human interest hierarchy.
constraint_indexing:constraint_classification(animal_status__welfare_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ANIMAL WELFARE ADVOCATES (TANGLED ROPE) — Constrained by institutional barriers and cultural norms that privilege human interests, but also benefit from the welfare framework itself as a vehicle for incremental protections. The constraint coordinates genuine welfare improvements (reduction of gratuitous cruelty) with extraction (legitimization of instrumental use). Advocates push boundaries of the framework but are trapped within its logic — gains come through negotiation, not principle.
constraint_indexing:constraint_classification(animal_status__welfare_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ANIMAL AGRICULTURE AND RESEARCH INDUSTRIES (ROPE) — Primary beneficiaries. The welfare constraint solves a coordination problem: it legitimizes continued instrumental use while reducing moral friction through minimum standards. Industries benefit from moral cover (constraint certifies the use as ethical), market access (welfare certification enables trade), and stability (welfare framework prevents more restrictive alternatives). Effective extraction χ toward beneficiary is negative — constraint subsidizes their use patterns.
constraint_indexing:constraint_classification(animal_status__welfare_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: RIGHTS-BASED REFORM MOVEMENT (SCAFFOLD) — Organized agents (abolitionist advocates, legal reformers, institutional challengers) see the welfare reading as a temporary stage in a longer moral arc. The constraint has a structural sunset: as moral recognition of animal sentience deepens, the welfare framework becomes increasingly unstable — the logical endpoint is abolition of instrumental use. Reformers experience the constraint as a scaffold they are deliberately transcending, with visibility timelines of 20-50 years depending on cultural adoption.
constraint_indexing:constraint_classification(animal_status__welfare_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: LEGACY LEGAL FRAMEWORKS (PITON) — Traditional property law and animal-use regulations persist through institutional inertia despite increasing recognition that the frameworks fail to match the constraint's stated premise (sentience = moral consideration). The legal scaffolding (animal cruelty statutes, animal welfare regulations) is largely performative — it manages the tension between acknowledged sentience and permitted use without resolving it. Theater ratio is high because the regulatory apparatus (inspection regimes, welfare certifications, standard-setting bodies) is substantial but functionally weak at preventing harm.
constraint_indexing:constraint_classification(animal_status__welfare_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: INSTITUTIONAL REFORMERS (TANGLED ROPE) — Powerful actors (corporations, governments, professional bodies) experience the welfare constraint as a coordination mechanism that also extracts reputational cost. The constraint permits continued use but increasingly requires visible welfare improvements, supply chain transparency, and moral justification. Reformers benefit from the constraint (market positioning, risk mitigation, moral cover) and bear costs (investment in welfare infrastructure, regulatory compliance, reputational vulnerability to escalating standards). The framework coordinates genuine improvements with managed extraction of compliance costs.
constraint_indexing:constraint_classification(animal_status__welfare_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / SENTIENCE AS NATURAL LIMIT (MOUNTAIN) — From a civilizational perspective, sentience itself appears as an immutable foundation: if an animal is sentient, it has interests; if it has interests, those interests constrain how it can be legitimately treated. The analytical perspective sees sentience as a natural fact that generates logical closure on permissible action. However, the structural data contradicts this — the welfare framework systematically exempts instrumental use from the sentience constraint's logical implications. The false summit reveals that 'sentience grounds moral constraint' is naturalized while 'sentience permits instrumental use under welfare conditions' is constructed.
constraint_indexing:constraint_classification(animal_status__welfare_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status__welfare_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(animal_status__welfare_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(animal_status__welfare_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(animal_status__welfare_reading, TR),
    TR >= 0.70.

:- end_tests(animal_status__welfare_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.45): The constraint exhibits genuine hybrid extraction-coordination structure. Base measurement reflects moderate asymmetric benefit to animal-use industries (who capture moral cover and market access) and moderate cost to animals (who experience constrained but permitted harm). The value sits at the tangled rope / snare boundary because the welfare framework legitimizes use while constraining its expression, creating a coordinated extraction mechanism. Not as severe as pure snare (ε≥0.46) because some welfare protections are genuine; not as benign as rope (ε≤0.45) because the primary beneficiary is the user, not the used. Suppression (0.58): Moderate-high. Significant barriers to challenging the framework include: economic dependence on animal products, cultural normalization of animal use, institutional investment in welfare-certified systems, and epistemic barriers (credentialed expertise within the welfare frame). But suppression is not total — organized abolition movements exist, moral progress is visible (welfare regulations have tightened historically), and alternative systems (plant-based, cultivated meat) are emerging. Theater ratio (0.64): Substantial performative element. Regulatory inspection, welfare certification, humane labeling, and standard-setting bodies create visible apparatus of ethical concern while harm outcomes remain largely unchanged across welfare-compliant systems. The theater has increased over the measurement interval as certification standards have proliferated, suggesting Goodhart drift (compliance with visible metrics replacing actual harm reduction). This rising trajectory supports the piton classification from the legal framework perspective.
 *
 * PERSPECTIVAL GAP:
 *   The welfare reading generates sharp perspectival divergence across the constraint hierarchy. Animals experience the constraint as snare — permitted harm justified by human interest. Welfare advocates experience tangled rope — genuine protections negotiated through constraint. Industries experience rope — coordination solving their moral cover problem. Reformers experience scaffold — temporary stage being transcended toward abolition. Legal frameworks experience piton — performative apparatus maintaining an incoherent boundary. Corporate actors experience tangled rope — requirements for visible welfare investment. The analytical observer risks experiencing mountain — naturalizing the welfare reading's sentience-permits-use premise as a natural fact rather than recognizing it as one contested reading of the animal_status kernel. This perspectival range is diagnostic: when six perspectives produce six different types, the observer position matters more than the structural facts. The welfare reading itself is what produces this diversity — it holds sentience constant while varying the use boundary across different observers and time horizons.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d values) for each perspective is derived from agent power, exit options, and beneficiary/victim status. Animals: d=0.95 (full victim, trapped, powerless) → f(d)≈1.42 → high experienced extractiveness. Welfare advocates: d≈0.60 (mixed victim/beneficiary, constrained) → f(d)≈0.90 → moderate extraction. Industries: d=0.05 (full beneficiary, arbitrage, institutional) → f(d)≈-0.12 → negative extraction (subsidized by constraint). Reformers: d≈0.40 (organized resistance, constrained exit) → f(d)≈0.40 → moderate extraction. Legal frameworks: d=0.00 (institutional beneficiary) → f(d)≈-0.12 → institutional canonical. Scope modifier σ(S)=1.2 (global scale) amplifies extraction derived at this scope — the constraint coordinates harm at planetary scale. The analytical perspective uses canonical d=0.73 (analytical) → f(d)≈1.15, scaling to global scope σ=1.0 (universal) produces χ≈0.84, but the mountain classification from this perspective overrides χ calculation and asserts immutability via the sentience axiom.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATE ATROPHY SIGNAL: This welfare reading exhibits incipient mandatrophy — a gap between the constraint's stated justification (sentience generates moral consideration) and its enforcement outcome (continued instrumental use with minor protections). The mandate (sentience shall constrain harm) is slowly eroding into theater (sentience justifies regulatory apparatus without preventing harm). The rising theater_ratio (0.55→0.64) over the measurement interval signals this drift. The constraint is at the boundary where mandatrophy becomes visible: welfare frameworks have proliferated while animal use has intensified and industrialized, suggesting the constraint's real function is to legitimize use, not to constrain it. The welfare reading remains stable at ε=0.45 because the mandate has not yet fully atrophied — genuine protections exist, and reformers are actively resisting mandatrophy by redefining necessity boundaries. However, the measurement trend (extractiveness holding stable while theater rises) suggests the constraint is drifting from tangled rope toward piton. The mandatrophy is not yet resolved, but the structural pressure toward it is visible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sentience_to_constraint_closure,
    'Does recognized sentience logically entail prohibition of instrumental use, or only constraints on how that use is conducted?',
    'Formal logical analysis of sentience premises in various philosophical frameworks (utilitarian, deontological, capabilities-based). Empirical observation of which constraints philosophers committed to sentience actually endorse (welfare vs abolition).',
    'If sentience entails prohibition: this welfare reading forecloses its own premise — recognizing sentience while permitting use is self-contradictory, and the reading collapses into the abolitionist reading. If sentience entails only constraints on use: the welfare reading is coherent but its boundary (what uses are ''necessary''?) remains under-determined.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sentience_to_constraint_closure, conceptual, 'Logical relationship between sentience recognition and permissibility of instrumental use').

omega_variable(
    necessity_exemption_stability,
    'What stable definition of ''necessary'' human uses exempts animals from the sentience constraint? Does the category expand or contract over time?',
    'Historical analysis of uses classified as necessary (food, medicine, research, clothing, labor) across jurisdictions and time periods. Correlation with economic conditions, technology availability, and cultural values. Measurement of policy creep (definition widening) vs tightening.',
    'If expanding: necessity becomes a cover story for extraction, scaffolding collapses, welfare reading forecloses itself. If contracting: welfare frame is genuinely limiting instrumental use, supporting the tangled rope classification. If stable: necessity is a real boundary maintained by enforcement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(necessity_exemption_stability, empirical, 'Temporal stability of ''necessity'' category in exempting animal use from sentience constraint').

omega_variable(
    welfare_standard_convergence_or_divergence,
    'Are welfare standards globally converging toward a common minimal floor, or fracturing into competing regimes (high-welfare certifications vs low-cost production)?',
    'Comparative analysis of welfare regulations across jurisdictions (EU, US, China, India). Measurement of trade barriers protecting divergent standards. Analysis of certification scheme proliferation.',
    'If converging: welfare constraint is institutionalizing a genuine coordination function, supporting rope or tangled rope. If diverging: standards fragmentation enables arbitrage and extraction evasion, shifting classification toward snare (from industry perspective) or revealing the welfare reading as theater (piton).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(welfare_standard_convergence_or_divergence, empirical, 'Global welfare standard convergence or fragmentation').

omega_variable(
    kernel_reading_instability,
    'Is the welfare reading a stable philosophical position, or a transitional stage forced by incoherence between sentience recognition and instrumental use?',
    'Tracking of philosopher positions over time: how many explicitly defend welfare frameworks vs how many have migrated to abolitionism or property frameworks? Institutional stability of welfare positions in law, policy, and corporate practice.',
    'If transitional: the welfare reading is scaffolding toward abolition (supports scaffold classification from reform perspective). If stable: welfare is a coherent equilibrium position. If collapsing: the reading forecloses itself via its own internal contradictions, and must reclassify toward abolitionist reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_instability, conceptual, 'Welfare reading''s stability as a defended philosophical position').

omega_variable(
    false_summit_sentience_kernel,
    'Is the analytical observer''s ''sentience as natural law'' claim a genuine natural limit on legitimate action, or a contested kernel where multiple readings naturalize different boundaries?',
    'Analysis of whether sibling readings (abolitionist, property) claim the same sentience fact but reach different conclusions. If yes: sentience is the contested kernel and boundaries are reading-dependent, not natural. If no: readings differ at the factual level and classification follows from facts.',
    'If kernel contest: the mountain classification is a false summit (naturalization of a reading choice). The analytical perspective should route to tangled rope or piton depending on whose reading is treated as natural. If fact-based: mountain classification is legitimate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_sentience_kernel, conceptual, 'Whether sentience is a natural fact or a contested kernel with multiple readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__welfare_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aswf_theater_t0, animal_status__welfare_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(aswf_theater_t5, animal_status__welfare_reading, theater_ratio, 5, 0.6).
narrative_ontology:measurement(aswf_theater_t10, animal_status__welfare_reading, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(aswf_extract_t0, animal_status__welfare_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(aswf_extract_t5, animal_status__welfare_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(aswf_extract_t10, animal_status__welfare_reading, base_extractiveness, 10, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status__welfare_reading, resource_allocation).
narrative_ontology:affects_constraint(animal_status__welfare_reading, animal_status__abolitionist_reading).
narrative_ontology:affects_constraint(animal_status__welfare_reading, animal_status__property_reading).

% DUAL FORMULATION NOTE:
% The animal_status kernel decomposes into three structurally distinct constraint stories, each with different ε values and different philosophical premises. The welfare reading (this file) has ε=0.45 (tangled rope boundary), recognizing sentience as constraint on harm but not use. The abolitionist reading has higher ε (~0.65, snare) because it classifies continued use as categorical harm. The property reading has lower ε (~0.15, mountain or rope) depending on whether property status is treated as natural or constructed. All three link via network.affects_constraints to signal they are readings of one kernel, not independent constraints. The ε values differ because each reading identifies different victim sets and exemption structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(animal_status__welfare_reading, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
