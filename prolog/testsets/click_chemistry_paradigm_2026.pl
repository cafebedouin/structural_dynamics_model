% ============================================================================
% CONSTRAINT STORY: click_chemistry_paradigm_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_click_chemistry_paradigm_2026, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: click_chemistry_paradigm_2026
 *   human_readable: Click Chemistry Paradigm as Extraction and Coordination
 *   domain: chemical_synthesis/materials_science/research_methodology
 *
 * SUMMARY:
 *   Click chemistry, formalized by Sharpless and colleagues around 2000-2002,
 *   has become the dominant paradigm in organic synthesis and chemical
 *   biology. It combines genuine coordination benefits — modular reaction
 *   design, predictable outcomes, scalable synthesis of complex molecules —
 *   with significant extractive mechanisms that displace alternative
 *   synthetic traditions and concentrate research resources. The constraint
 *   exhibits all the hallmarks of Tangled Rope: real coordination function
 *   solving synthesis bottlenecks (modularity enables rapid prototyping,
 *   broad applicability across materials and biologics), active enforcement
 *   through funding allocation and publication prioritization (NSF and DOE
 *   strategic plans privilege click-compatible approaches), asymmetric
 *   extraction favoring established research groups and disfavoring displaced
 *   expertise (classical synthetic chemists, alternative coupling
 *   methodologies, biomimetic synthesis traditions), and beneficiaries that
 *   are identifiable (click chemistry establishment, modular-design-focused
 *   funding agencies) alongside victims (displaced synthetic traditions,
 *   early-career chemists locked into methodology-specific expertise). The
 *   constraint's evolution over 24 years shows rising theater_ratio (initial
 *   genuine utility at 0.35, peaking at 0.68 as pedagogical emphasis became
 *   divorced from practical application), rising extractiveness (from 0.18 at
 *   paradigm emergence to 0.58 at maturity), and recently stabilizing
 *   suppression (reaching 0.48-0.52 as alternative synthesis communities
 *   organize coordinated responses). This trajectory is consistent with a
 *   paradigm that began as genuine coordination breakthrough but accumulated
 *   extractive institutional layers (departmental hiring requiring click
 *   expertise, textbook canonization creating path dependency, funding agency
 *   lock-in).
 *
 * KEY AGENTS:
 *   - Click Chemistry Research Establishment: Institutional beneficiaries (institutional/arbitrage) — Sharpless group, leading click-chemistry labs, established practitioners. Experience genuine coordination (modularity solves real problems) with no extraction because they set the paradigm terms.
 *   - Funding Agencies (NSF, DOE, ERC): Policy-level beneficiaries (powerful/mobile) — embed click paradigm in strategic priorities. See coordination function (rational synthesis portfolio design) with planned sunset as alternative methodologies mature.
 *   - Classical Synthetic Chemists: Primary victims (powerless/trapped) — mid-to-late career chemists trained in traditional methods. Face career diminishment, reduced funding, institutional pressure to retrain. Trapped for biographical horizon.
 *   - Early-Career Chemists (Click-Trained): Secondary victims (moderate/constrained) — trained primarily in click chemistry; locked into methodology-dependent expertise with modest exit options.
 *   - Alternative Synthesis Communities: Organized victims (organized/constrained) — C-H activation, catalytic coupling, biocatalysis, flow chemistry researchers. Experience underfunding and publication bias despite methodological legitimacy.
 *   - Organic Chemistry Curriculum: Institutional constraint vehicle (institutional/arbitrage) — textbooks, courses, exams perpetuate click paradigm through inertia despite declining functional relevance to actual synthesis practice.
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing click paradigm dominance as inevitable scientific convergence, obscuring contingent institutional mechanisms.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(click_chemistry_paradigm_2026, 0.58).
domain_priors:suppression_score(click_chemistry_paradigm_2026, 0.48).
domain_priors:theater_ratio(click_chemistry_paradigm_2026, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(click_chemistry_paradigm_2026, extractiveness, 0.58).
narrative_ontology:constraint_metric(click_chemistry_paradigm_2026, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(click_chemistry_paradigm_2026, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(click_chemistry_paradigm_2026, tangled_rope).
narrative_ontology:human_readable(click_chemistry_paradigm_2026, "Click Chemistry Paradigm as Extraction and Coordination").
narrative_ontology:topic_domain(click_chemistry_paradigm_2026, "chemical_synthesis/materials_science/research_methodology").

domain_priors:requires_active_enforcement(click_chemistry_paradigm_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(click_chemistry_paradigm_2026, click_chemistry_research_establishment).
narrative_ontology:constraint_beneficiary(click_chemistry_paradigm_2026, funding_agencies_prioritizing_modularity).
narrative_ontology:constraint_victim(click_chemistry_paradigm_2026, displaced_synthetic_traditions).
narrative_ontology:constraint_victim(click_chemistry_paradigm_2026, alternative_synthesis_methodologies).
narrative_ontology:constraint_victim(click_chemistry_paradigm_2026, synthetic_chemists_trained_in_classical_methods).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CLASSICAL SYNTHETIC CHEMIST (SNARE) — Mid-career chemist trained in traditional coupling methods (Wittig, Grignard, Suzuki) finds their expertise devalued as click chemistry dominates funding, publication, and institutional priority. Research funding increasingly requires click chemistry justification. Students are trained primarily in modular approaches. Traditional synthetic routes are rejected as 'inefficient' despite proven track records. The constraint operates as pure extraction: career diminishment, reduced funding access, institutional pressure to retrain. No meaningful exit option — retraining requires years and carries risk of incomplete adoption. Maximum experienced extraction because the trap is biographical (career-long) and the agent is powerless to shift paradigm allocation.
constraint_indexing:constraint_classification(click_chemistry_paradigm_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EMERGING SYNTHETIC CHEMIST (TANGLED ROPE) — Early-career chemist trained primarily in click chemistry. Experiences genuine coordination benefits: clear design rules, predictable outcomes, modular assembly enable rapid prototyping. Also experiences extraction: career path dependent on continued dominance of click paradigm; lateral moves to alternative methods carry professional risk. Can exit to classical methods but at cost of career momentum and publication disadvantage. Mixed beneficiary-victim position: benefits from paradigm dominance during training window but locked into it as professional identity hardens.
constraint_indexing:constraint_classification(click_chemistry_paradigm_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CLICK CHEMISTRY RESEARCH ESTABLISHMENT (ROPE) — The founding research groups, established practitioners, and institutional nodes (Sharpless group, other leading labs) experience click chemistry as pure coordination. It solves real synthesis problems: modularity, predictability, scalability. These agents have arbitrage capacity — they can shift methods if needed without career damage, but click chemistry dominance is aligned with their interests. No experienced extraction because they benefit from the coordination function and have exit capacity. The paradigm is genuinely useful for their work.
constraint_indexing:constraint_classification(click_chemistry_paradigm_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ALTERNATIVE SYNTHESIS COMMUNITIES (TANGLED ROPE) — Organized research communities developing competing methodologies (C-H activation, catalytic coupling, flow chemistry, biocatalysis) experience the constraint as coordination with embedded extraction. Click chemistry's dominance creates both coordination problem (click modules can be integrated into alternative approaches) and extraction (alternative methods are systematically underfunded, difficult to publish, require constant comparative justification against click paradigm). These communities have organizing capacity and some exit options (creating specialized journals, funding initiatives) but remain constrained by paradigm-level resource allocation.
constraint_indexing:constraint_classification(click_chemistry_paradigm_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: FUNDING AGENCIES AND POLICY ACTORS (SCAFFOLD) — Government and foundation funders (NSF, DOE, ERC) use click chemistry as a structuring principle for synthetic chemistry portfolios, embedding it in strategic planning and evaluation criteria. Experience the paradigm as a coordination mechanism that enabled rational funding allocation and measurable progress. However, this is a Scaffold perspective because the coordination function has a sunset clause: as click chemistry matures and saturation effects emerge (diminishing returns from further modularization), policy actors are shifting to emphasis on complementary methodologies. The constraint is temporary coordination with recognized exit: strategic pivots toward sustainability chemistry, catalysis diversity, and biomimetic synthesis suggest declining centrality of click paradigm within 10-15 years.
constraint_indexing:constraint_classification(click_chemistry_paradigm_2026, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ORGANIC CHEMISTRY CURRICULUM (PITON) — Textbooks, undergraduate curricula, and institutional teaching frameworks have adopted click chemistry as a central exemplar. The high theater_ratio (0.68 in measurements) reflects that pedagogical emphasis on click chemistry is substantially performative: students rarely perform click reactions in introductory labs, yet click chemistry occupies substantial textbook real estate and examination content. The constraint persists through inertia — textbook chapters, curriculum modules, and examination frameworks were written when click chemistry was ascendant and are maintained without regular revision. The primary function (teaching synthesis principles) has atrophied; the constraint persists because alternatives haven't fully replaced it in institutional practice.
constraint_indexing:constraint_classification(click_chemistry_paradigm_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a civilizational view, some methodological consolidation in chemistry is inherent to scientific progress: dominant paradigms inevitably emerge because they solve real problems better than alternatives, and this is a natural, inevitable feature of how disciplines organize themselves. This perspective risks naturalizing what is actually a contingent institutional arrangement driven by funding concentration, publication bias, and career incentive structures. The engine's false summit detector will identify this as a natural-law framing that obscures extractive institutional mechanisms.
constraint_indexing:constraint_classification(click_chemistry_paradigm_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(click_chemistry_paradigm_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(click_chemistry_paradigm_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(click_chemistry_paradigm_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(click_chemistry_paradigm_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(click_chemistry_paradigm_2026, TR),
    TR >= 0.70.

:- end_tests(click_chemistry_paradigm_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Click chemistry shows moderate-high extraction. The paradigm began with genuine utility (ε ≈ 0.18 at t=0) but accumulated extractive institutional layers as it became institutionalized. By t=24 (present), extractiveness has stabilized around 0.58, reflecting both real coordination value and significant institutional lock-in benefits for established practitioners. The plateau rather than continued growth suggests maturation rather than continued expansion. Suppression (0.48): Moderate-high. Alternative methodologies face systematic barriers: harder publication in high-impact journals, lower funding success rates, career risk for researchers pursuing alternatives. However, suppression is not total — specialized journals exist, alternative funding mechanisms (ARPA-E, foundation grants) support competing methodologies, and some universities maintain classical synthesis expertise. The slight decline from 0.52 to 0.48 in recent years reflects emerging organization of alternative communities and policy recognition of methodology diversity needs. Theater ratio (0.62): High and rising historically (0.35→0.68) but recently stabilizing. The rising theater reflects increasing gap between click chemistry's pedagogical emphasis and its practical role. Students learn click chemistry extensively in coursework despite rarely performing actual click reactions in labs. Advanced research increasingly finds click chemistry insufficient for complex synthesis goals, yet canonical status in curriculum persists through institutional inertia. The stabilization at 0.62 reflects that theater has reached equilibrium — it's institutionalized but not growing.
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces dramatic perspectival divergence across seven distinct positions. The classical chemist sees Snare (pure extraction, trapped career). The emerging chemist sees Tangled Rope (mixed benefits and costs, constrained but not trapped). The click establishment sees Rope (pure coordination, no extraction). The alternative research communities see Tangled Rope from outside (underfunded but viable, constrained by paradigm allocation). Funding agencies see Scaffold (temporary coordination with recognized sunset). The curriculum sees Piton (degraded ritual maintaining through inertia). The analytical observer risks seeing Mountain (natural law of synthesis efficiency). This full-spectrum classification within unified base properties demonstrates that perspectival position (power level, exit options, beneficiary/victim status) is the primary determinant of classification type, not the objective structural features of the constraint itself.
 *
 * DIRECTIONALITY LOGIC:
 *   The engine derives directionality (d) from each agent's structural relationship to the constraint. Classical synthetic chemists are victims with trapped exit (d ≈ 0.95), producing maximum experienced extraction. Click establishment are beneficiaries with arbitrage exit (d ≈ 0.05), producing negative/minimal effective extraction. Emerging chemists are mixed beneficiary-victim with constrained exit (d ≈ 0.60), producing moderate extraction. Alternative communities are organized victims with constrained exit (d ≈ 0.65-0.70), producing moderate-high extraction. Funding agencies are institutional beneficiaries with mobile exit (d ≈ 0.20), producing low extraction. The curriculum institution has arbitrage exit despite victim-like role in perpetuating paradigm inertia (d ≈ 0.10-0.15), reflecting institutional capacity to shift educational emphasis despite theatrical resistance. The analytical observer at universal scope applies canonical fallback d ≈ 0.73, which produces the mountain classification. The diversity of d values across perspectives generates the full classification spectrum from Snare (powerless/trapped) through Rope (institutional/arbitrage) to Mountain (analytical/universal). The false summit detection will flag the mountain classification because identifiable beneficiaries exist (click establishment, funding agencies) — the 'natural law' framing is actually contingent institutional dominance.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy question by demonstrating that all seven classifications are simultaneously valid from their respective positions. The question 'Is click chemistry a paradigm shift (Rope) or an extractive institutional lock-in (Snare)?' has no single answer — it depends entirely on the observer's structural relationship. For the beneficiary (click establishment), it is Rope. For the trapped victim (classical chemist), it is Snare. For the analytical observer, both classifications are empirically present but phenomenologically invisible from within each perspective. The mandatrophy resolution is not 'which classification is correct?' but 'what does the classification tell us about the observer's structural position?' The constraint is the same; the classification is the measurement of the observer's relationship to it. The Tangled Rope classification (beneficiaries, victims, enforcement) correctly captures the hybrid coordination-extraction nature. The Snare classification captures what trapped agents experience. The Rope classification captures what beneficiaries experience. The Scaffold classification anticipates the sunset. The Piton classification identifies institutional inertia. The Mountain classification marks where naturalization risks occurring.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    functional_utility_vs_paradigm_lock,
    'Is the dominance of click chemistry primarily driven by genuine functional utility for synthesis problems, or by institutional lock-in (funding, publication bias, career incentives)?',
    'Comparative efficiency studies across synthesis domains; analysis of publication trends in journals where alternative methodologies can compete fairly; funding allocation patterns pre- and post-click adoption',
    'If utility-driven: paradigm reflects efficient knowledge organization (Rope dominates). If lock-in-driven: paradigm reflects extractive concentration (Snare dominates from displaced perspectives). Most likely outcome is mixed, with utility dominance in some domains (bioconjugation, materials assembly) and lock-in dominance in others (academic funding, synthetic training).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functional_utility_vs_paradigm_lock, empirical, 'Functional utility vs institutional lock-in in click chemistry dominance').

omega_variable(
    alternative_methodology_suppression_mechanisms,
    'What is the causal pathway through which click chemistry paradigm dominance suppresses investment in alternative methodologies? Is it active gatekeeping, passive funding bias, or perception of market saturation?',
    'Analysis of funding rejection patterns for alternative synthesis proposals; interview data on perceived barriers from competing research groups; publication acceptance rates for alternative methodologies in high-impact journals controlled for submission volume',
    'If active gatekeeping: suppression is severe (0.60+). If passive bias with awareness: suppression is moderate (0.45-0.55). If perception-driven with limited actual barriers: suppression is lower (0.35-0.45) and scaffold/rope perspectives are stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_methodology_suppression_mechanisms, empirical, 'Suppression mechanism for alternative synthesis methodologies').

omega_variable(
    click_chemistry_maturation_and_saturation,
    'Is click chemistry experiencing saturation effects (diminishing returns, reduced novelty publication rate, stagnant methodological development) that signal approaching paradigm decline?',
    'Bibliometric analysis of publication growth rates (citations, novelty metrics) over 5-year windows; patent filing trends; emergence of competing ''next-generation'' synthesis frameworks in funding announcements',
    'If saturation evident: Scaffold perspective is validated — sunset clause is real and paradigm decline is structural. If growth continuing: Scaffold sunset is aspirational, and institutional dominance will persist longer than estimated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(click_chemistry_maturation_and_saturation, empirical, 'Maturation and saturation of click chemistry paradigm').

omega_variable(
    expertise_displacement_reversibility,
    'Can synthetic chemists trained exclusively in click chemistry efficiently learn classical synthetic methods later, or is classical expertise irreversibly degraded by exclusive early training in modular approaches?',
    'Longitudinal tracking of chemists who retrained in classical methods (C-H activation, coupling reactions) after click-dominant training; assessment of competence parity with classically-trained peers; career outcomes post-retraining',
    'If reversible: extraction is temporary (Scaffold perspective strengthened). If irreversible: extraction is permanent (Snare perspective confirmed as biographical lock-in). Most likely outcome is partial reversibility with efficiency cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expertise_displacement_reversibility, empirical, 'Reversibility of classical chemistry expertise displacement').

omega_variable(
    false_summit_mountain_candidate,
    'Does the framing of click chemistry as a ''natural law'' of synthesis methodology (inevitable convergence on modular design) obscure the role of institutional beneficiaries (funding agencies, dominant research groups) who benefit from paradigm stability?',
    'Counterfactual scenario analysis: what would synthesis methodology development look like if funding had been equally allocated across click chemistry and alternatives for the past 10 years? Comparison of progress rates across alternative synthesis communities when funding is equalized.',
    'If mountain framing is false summit: engine reclassification to Tangled Rope or Snare validates omega. If mountain framing is accurate: emergence naturally = true is justified and beneficiary identification is spurious.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_mountain_candidate, conceptual, 'False summit detection: is click dominance natural law or institutional arrangement?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(click_chemistry_paradigm_2026, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(click_theater_t0, click_chemistry_paradigm_2026, theater_ratio, 0, 0.35).
narrative_ontology:measurement(click_theater_t8, click_chemistry_paradigm_2026, theater_ratio, 8, 0.58).
narrative_ontology:measurement(click_theater_t16, click_chemistry_paradigm_2026, theater_ratio, 16, 0.68).
narrative_ontology:measurement(click_theater_t24, click_chemistry_paradigm_2026, theater_ratio, 24, 0.62).

% Extraction over time
narrative_ontology:measurement(click_extract_t0, click_chemistry_paradigm_2026, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(click_extract_t8, click_chemistry_paradigm_2026, base_extractiveness, 8, 0.38).
narrative_ontology:measurement(click_extract_t16, click_chemistry_paradigm_2026, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(click_extract_t24, click_chemistry_paradigm_2026, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(click_suppress_t0, click_chemistry_paradigm_2026, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(click_suppress_t8, click_chemistry_paradigm_2026, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(click_suppress_t16, click_chemistry_paradigm_2026, suppression_requirement, 16, 0.52).
narrative_ontology:measurement(click_suppress_t24, click_chemistry_paradigm_2026, suppression_requirement, 24, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(click_chemistry_paradigm_2026, resource_allocation).
narrative_ontology:boltzmann_floor_override(click_chemistry_paradigm_2026, 0.18).
narrative_ontology:affects_constraint(click_chemistry_paradigm_2026, organic_synthesis_methodology_diversity).
narrative_ontology:affects_constraint(click_chemistry_paradigm_2026, displaced_synthetic_expertise).
narrative_ontology:affects_constraint(click_chemistry_paradigm_2026, research_funding_concentration).

% DUAL FORMULATION NOTE:
% Click chemistry as paradigm (this file) represents the institutional constraint allocating resources and epistemic authority toward modular synthesis approaches. Structurally distinct constraints exist for: (1) specific alternative methodologies suppressed (C-H activation, coupling reactions) with their own ε values; (2) expertise displacement as a labor-market phenomenon with biographical extraction; (3) funding concentration in NSF/DOE strategic priorities. These decompose into separate constraint stories linked through affects_constraints network.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(click_chemistry_paradigm_2026, institutional, 0.68).
constraint_indexing:directionality_override(click_chemistry_paradigm_2026, organized, 0.67).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
