% ============================================================================
% CONSTRAINT STORY: hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hybrid_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hybrid_reading
 *   human_readable: Bifurcated Latin Legitimacy: Classical Norms for Literary Domains, Medieval Forms for Technical Domains
 *   domain: historical_linguistics/intellectual_history
 *
 * SUMMARY:
 *   The bifurcated Latin legitimacy constraint emerged during the transition
 *   from medieval to Renaissance intellectual practice (ca. 12th-16th
 *   centuries). This constraint instantiates the HYBRID READING of the
 *   contested kernel latin_correctness. The hybrid reading holds that both
 *   classical norms and medieval forms possess legitimate authority, but in
 *   different domains: classical standards govern literary, rhetorical, and
 *   prestige intellectual discourse; medieval pragmatic forms remain
 *   legitimate for technical, administrative, and practical writing. This
 *   reading differs structurally from the continuity reading (which sees
 *   medieval forms as valid evolutionary development of Latin without status
 *   hierarchy) and the rupture reading (which accepts only one correct form —
 *   either purely classical or purely medieval — as fully legitimate). The
 *   hybrid reading's core innovation is DOMAIN BIFURCATION: legitimacy is not
 *   uniform across all Latin writing but stratified by textual function. This
 *   creates a moderate extractive constraint because the bifurcated norm
 *   enforces a status hierarchy (literary > technical) that benefits humanist
 *   elites and classical traditionalists while pressuring technical writers
 *   into an impossible bind: classical standards prioritize elegance over
 *   precision, while medieval forms ensure functional clarity but carry
 *   stigma. Extractiveness (0.52) reflects this asymmetry. Suppression (0.58)
 *   reflects the enforcement mechanisms: institutional gatekeeping through
 *   manuscript prestige hierarchies, dismissive rhetoric against 'debased'
 *   forms, and circulation barriers for medieval-form texts. Theater ratio
 *   (0.64) reflects increasing performative invocation of classical
 *   correctness as printing standardizes orthography, making the functional
 *   justification for domain-specific variation increasingly thin.
 *
 * KEY AGENTS:
 *   - Humanist Literary Elite (institutional/arbitrage): Primary beneficiary — capture status and institutional authority by monopolizing classical standards; define 'correct' Latin through their own practice
 *   - Technical Writers & Scribes (powerless-to-moderate/trapped-to-constrained): Primary victims — pressured to adopt impossible classical standards for technical work; or stigmatized for adopting pragmatic medieval forms
 *   - Medieval Form Practitioners (moderate/constrained): Secondary victims — monastic scriptoria, notarial communities defending functional necessity of abbreviated forms against humanist dismissal
 *   - Printing Press Reformers (organized/mobile): Secondary actor — see bifurcation as temporary coordination problem; printing technology enables unified standardization
 *   - Ecclesiastical Authority Structure (institutional/arbitrage): Institutional actor maintaining bifurcated norm through theological legitimacy claims; experiences constraint as institutional identity (Piton perspective)
 *   - Analytical Observer (analytical/analytical): Risks naturalizing contingent institutional arrangement as immutable law of language
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hybrid_reading, 0.52).
domain_priors:suppression_score(hybrid_reading, 0.58).
domain_priors:theater_ratio(hybrid_reading, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hybrid_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(hybrid_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(hybrid_reading, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hybrid_reading, tangled_rope).
narrative_ontology:human_readable(hybrid_reading, "Bifurcated Latin Legitimacy: Classical Norms for Literary Domains, Medieval Forms for Technical Domains").
narrative_ontology:topic_domain(hybrid_reading, "historical_linguistics/intellectual_history").

domain_priors:requires_active_enforcement(hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(hybrid_reading, distributed).
narrative_ontology:cs_authority_grounding(hybrid_reading, lineage).
narrative_ontology:cs_interpretation_layer_present(hybrid_reading).
narrative_ontology:cs_kernel_id(hybrid_reading, latin_correctness).
narrative_ontology:cs_reading_relation(hybrid_reading, continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation(hybrid_reading, rupture_reading, coexists_with).
narrative_ontology:cs_axiom(hybrid_reading, foundational, domain_bifurcation_legitimate).
narrative_ontology:cs_axiom_status(domain_bifurcation_legitimate, holdable).
narrative_ontology:cs_axiom_grounding(hybrid_reading, domain_bifurcation_legitimate, conventional).
narrative_ontology:cs_axiom(hybrid_reading, secondary, status_hierarchy_justified_by_function).
narrative_ontology:cs_axiom_status(status_hierarchy_justified_by_function, overridden).
narrative_ontology:cs_axiom_grounding(hybrid_reading, status_hierarchy_justified_by_function, empirically_contingent).
narrative_ontology:cs_reference_frame(hybrid_reading, classical_literary_standard_with_technical_exception).
narrative_ontology:cs_drift_state(hybrid_reading, post_printing_press_era, gap(codification_collapse, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hybrid_reading, classical_literary_tradition).
narrative_ontology:constraint_beneficiary(hybrid_reading, status_hierarchy_maintainers).
narrative_ontology:constraint_victim(hybrid_reading, technical_writers).
narrative_ontology:constraint_victim(hybrid_reading, medieval_form_practitioners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TECHNICAL WRITER (SNARE) — Trapped between two incommensurable legitimacy regimes. Adopting classical standards renders technical precision difficult; adopting medieval/pragmatic forms invites dismissal as 'corrupt' or 'debased.' No exit from the constraint without abandoning either technical function or scholarly credibility. Maximum extraction.
constraint_indexing:constraint_classification(hybrid_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: MONASTERY SCRIBAL COMMUNITY (TANGLED ROPE) — Benefits from functional legitimacy of medieval forms for practical copying work; constrained by periodic pressure from humanist critiques dismissing their forms as insufficiently classical. The constraint coordinates copying practice while simultaneously extracting legitimacy hierarchically. Some agency (can defend pragmatic necessity) but high cost of transgression against classical ideal.
constraint_indexing:constraint_classification(hybrid_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: HUMANIST LITERARY ELITE (ROPE) — Experiences the bifurcated norm as pure coordination mechanism: classical standards define in-group membership and distinguish literary production from mere technical work. High status benefit, low extraction cost. Can freely adopt classical standards and gains authority from doing so. Exit option (arbitrage) allows selection into the prestige domain.
constraint_indexing:constraint_classification(hybrid_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: PRINTING PRESS REFORMERS (SCAFFOLD) — Organized movement (Aldus Manutius, other Renaissance printers) sees the bifurcated norm as temporary coordination failure awaiting resolution through standardization. New printing technology enables distributed enforcement of classical standards across all domains, creating a sunset clause for medieval forms: as print standardizes orthography and typography, the functional justification for domain-specific medieval variation disappears. Moderate extraction (enforcement required during transition) declining toward zero.
constraint_indexing:constraint_classification(hybrid_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: ECCLESIASTICAL AUTHORITY STRUCTURE (PITON) — Maintains bifurcated norm through institutional inertia and theological legitimacy claims (medieval forms are 'the language of the Church'). The functional necessity has largely disappeared (printing enables uniform standards), but the constraint persists through appeals to tradition and institutional identity. Theater ratio reflects performative invocation of liturgical-continuity rationale to justify what is now largely institutional conservatism.
constraint_indexing:constraint_classification(hybrid_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From the highest analytical distance, the bifurcation appears to reflect natural linguistic stratification: literary language 'naturally' conserves classical standards while technical language 'naturally' adopts pragmatic shortcuts. This reading risks naturalizing what is actually a contingent institutional arrangement. The constraint's structural data (identifiable beneficiaries, enforced pressure, status hierarchy) will trigger false summit detection.
constraint_indexing:constraint_classification(hybrid_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hybrid_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hybrid_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hybrid_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hybrid_reading, TR),
    TR >= 0.70.

:- end_tests(hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate. The bifurcated norm creates genuine asymmetry favoring classical over medieval forms, but the extraction is not maximal because functional legitimacy of technical forms remains defensible and some institutional spaces (monasteries, notarial offices) maintain autonomous authority. The extractiveness increased over the interval (0.38 → 0.52) as humanist gatekeeping intensified and printing created technological capacity for unified standardization, making the 'functional necessity' defense increasingly weak. Suppression (0.58): Moderate-high. Multiple mechanisms enforce the bifurcation: institutional gatekeeping (manuscripts with classical forms circulate widely; medieval-form texts marginalized), rhetorical dismissal (classical defenders label medieval forms as 'corrupt,' 'barbaric,' 'debased'), publication barriers (printers prefer classical forms for prestige texts), and career incentives (literary advancement requires classical competence). But suppression is not total because monastic scriptoria and technical offices maintain operational autonomy. Theater ratio (0.64): Moderate-high. Increasingly performative. Early in the period (t=0), the bifurcation served genuine functional purposes — medieval abbreviations enhanced copying efficiency, classical forms enabled literary ornament. By the Renaissance period (t=300-400), printing has eliminated the copying-efficiency rationale, yet the bifurcation persists through appeals to tradition and correctness. The theater has risen over time, indicating that the constraint's functional component is being replaced by performative enforcement.
 *
 * PERSPECTIVAL GAP:
 *   This hybrid reading produces a pronounced perspectival gap between the beneficiary and victim views. The humanist literary elite sees the bifurcated norm as pure coordination (Rope) — classical standards elegantly distinguish literary discourse and mark in-group membership. Technical writers see the same norm as pure extraction (Snare) — trapped between two incommensurable legitimacy regimes with no escape. The printing press reformers see a temporary coordination problem approaching resolution (Scaffold) — unified standardization is technologically feasible and institutionally approaching. The ecclesiastical authority sees its own degraded ritual (Piton) — maintaining medieval forms through appeals to tradition rather than functional necessity. The monastery scribal community experiences the actual mixed mechanism (Tangled Rope) — genuine coordination of copying practice plus asymmetric status extraction. The analytical observer risks the false summit — seeing the bifurcation as natural linguistic stratification rather than contingent institutional arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's effective extractiveness (χ) derives from the agent's structural position relative to the bifurcated norm. Beneficiaries of the status hierarchy (humanist elite, institutional gatekeepers) have low directionality (d ≈ 0.15-0.20) — they gain from the constraint and experience negative extraction. Victims trapped in the dual standard (technical writers) have high directionality (d ≈ 0.85-0.90) — they lose under either choice. Agents with operational autonomy (monastery scriptoria) have moderate directionality (d ≈ 0.55-0.65) — they benefit from functional legitimacy but lose from status hierarchy. The printmakers' organized exit option reduces their directionality despite being situated between the regimes. The ecclesiastical institutional actor's arbitrage exit option (can adopt classical standards selectively) produces low directionality despite nominally defending medieval forms.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading avoids mandatrophy because it explicitly acknowledges both coordination (bifurcation serves different functional needs) and extraction (status hierarchy benefits classical promoters). The constraint is not misidentified as pure rope (it is not pure coordination) nor as pure snare (technical forms remain functionally legitimate). The tangled rope classification resolves the contradiction: genuine coordination function exists (domain-specific form matching) alongside asymmetric extraction (status hierarchy). The scaffold perspective (printing-driven sunset) avoids mandate collapse by identifying a structural exit mechanism: when printing standardizes orthography, the functional justification for bifurcation disappears, allowing convergence. The piton perspective captures institutional degradation: as functional necessity fades, the constraint persists through performative tradition-invocation rather than structural coherence. The false summit (mountain perspective) represents the mandate trap: naturalizing contingent institutional arrangement as immutable law prevents recognition of the extractive mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_differentiation_kernel,
    'Which reading of the Latin correctness kernel does this constraint instantiate, and how does it differ from the continuity reading and rupture reading?',
    'Analysis of how each reading resolves the classical-vs-medieval bifurcation: continuity reading sees medieval forms as valid evolutionary development; rupture reading sees only one correct form (either purely classical or purely medieval); hybrid reading sees legitimate domain-specific bifurcation. Authority appeals and core axioms differentiate the readings.',
    'Continuity reading: ε ≈ 0.15-0.25 (Rope or Mountain). Rupture reading: ε ≈ 0.70+ (Snare). Hybrid reading: ε ≈ 0.52 (Tangled Rope). The readings are not observational variants — they represent genuinely different structural claims about legitimacy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_differentiation_kernel, conceptual, 'Which reading of the Latin correctness kernel this constraint instantiates').

omega_variable(
    status_hierarchy_legitimacy,
    'Is the literary-over-technical status hierarchy intrinsic to the forms themselves, or socially constructed through enforcement mechanisms?',
    'Counterfactual: in an alternative institutional structure (e.g., monastery-led standardization rather than humanist-led), would medieval forms carry equal status? Analysis of patronage flows, manuscript prestige markers, and institutional gatekeeping.',
    'If intrinsic: classical forms genuinely coordinate literary discourse more elegantly; medieval forms genuinely suited to technical domains (supports rope/mountain reading). If constructed: status hierarchy is extractive enforcement, not functional necessity (confirms snare and tangled_rope readings).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(status_hierarchy_legitimacy, empirical, 'Whether status hierarchy is intrinsic to forms or socially constructed').

omega_variable(
    functional_necessity_medieval,
    'Do medieval forms serve a genuine functional purpose in technical/practical writing (abbreviation density, scribal efficiency), or is the ''functional necessity'' claim a post-hoc rationalization for institutional conservatism?',
    'Paleographic and codicological analysis: correlation between text function and scriptural form (does notarial writing actually require breviary abbreviations for efficiency, or could classical forms achieve same efficiency with different encoding?). Cross-cultural comparison: did non-Latin technical traditions converge on abbreviated forms for functional reasons?',
    'If genuine: bifurcation is legitimate coordination mechanism (Rope perspective more accurate). If rationalization: bifurcation is status enforcement masquerading as functionality (Snare and Tangled Rope perspectives confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functional_necessity_medieval, empirical, 'Whether medieval forms serve genuine technical functions').

omega_variable(
    transition_completion_sunset,
    'Does printing technology''s capacity to standardize orthography actually eliminate the functional necessity of medieval forms, or does domain-specific variation persist despite technological capability for standardization?',
    'Historical trajectory: Post-Aldus printing period shows either convergence toward classical standards across all domains (sunset confirmed) or persistent bifurcation despite printing standardization (sunset does not materialize). Track orthographic/typographic choices in technical printed works across 16th-17th centuries.',
    'If sunset confirmed: Scaffold perspective is structurally accurate — constraint is genuinely temporary. If bifurcation persists: Scaffold classification is aspirational; the constraint may be re-classified as Piton (degraded form) or re-analyzed as failed coordination attempt.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(transition_completion_sunset, empirical, 'Whether printing technology eliminates functional necessity of medieval forms').

omega_variable(
    false_summit_naturalization,
    'Does the analytical observer''s mountain classification represent genuine immutability or merely naturalization of a contingent institutional arrangement?',
    'Comparative history: did other linguistic traditions (Greek, vernacular) develop similar bifurcations, or is the Latin case particular to its historical power structure and institutional authority? If bifurcation is cultural-specific rather than universal, the mountain is false.',
    'If false summit: the constraint is Tangled Rope or Piton, not Mountain. The ''natural law'' framing obscures the extractive status hierarchy. If genuine universal pattern: mountain classification may be legitimate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_naturalization, empirical, 'Whether analytical mountain is genuine or false summit').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hybrid_reading, 0, 400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hybrid_theater_t0, hybrid_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(hybrid_theater_t150, hybrid_reading, theater_ratio, 150, 0.62).
narrative_ontology:measurement(hybrid_theater_t300, hybrid_reading, theater_ratio, 300, 0.64).
narrative_ontology:measurement(hybrid_theater_t400, hybrid_reading, theater_ratio, 400, 0.61).

% Extraction over time
narrative_ontology:measurement(hybrid_extract_t0, hybrid_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(hybrid_extract_t150, hybrid_reading, base_extractiveness, 150, 0.5).
narrative_ontology:measurement(hybrid_extract_t300, hybrid_reading, base_extractiveness, 300, 0.52).
narrative_ontology:measurement(hybrid_extract_t400, hybrid_reading, base_extractiveness, 400, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hybrid_reading, information_standard).
narrative_ontology:affects_constraint(hybrid_reading, continuity_reading).
narrative_ontology:affects_constraint(hybrid_reading, rupture_reading).

% DUAL FORMULATION NOTE:
% The hybrid reading is one decomposition of the latin_correctness kernel. The continuity reading models medieval forms as legitimate evolutionary path; the rupture reading models exclusive legitimacy of one form. All three readings share the same underlying kernel but decompose it differently. They are not observational variants of a single constraint — they represent genuinely different structural claims about legitimacy authority. Each has its own ε, beneficiary/victim structure, and classification type. The three stories are linked via network.affects_constraints to show kinship and shared kernel origin.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hybrid_reading, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
