% ============================================================================
% CONSTRAINT STORY: classical_latin_standard__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_classical_latin_standard__hybrid_reading, []).

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
 *   constraint_id: classical_latin_standard__hybrid_reading
 *   human_readable: Classical Latin Standard (Hybrid Reading)
 *   domain: historical_linguistics/philology/commitment_systems
 *
 * SUMMARY:
 *   The classical Latin standard exists as a contested kernel with three live
 *   readings: continuity (living practice as standard), reconstruction
 *   (Classical texts as standard), and hybrid (both Classical fidelity and
 *   legitimate post-Classical technical domains). This constraint
 *   instantiates the HYBRID_READING, which resolves the kernel by selective
 *   accommodation. The hybrid reading legitimizes ecclesiastical and legal
 *   Latin innovations as necessary technical vocabulary while delegitimizing
 *   popular medieval drift as barbarism. This reading enables institutional
 *   actors (Church, legal apparatus) to maintain both textual authority
 *   (Classical sources) and practical communication (evolved technical
 *   terms), but at the cost of suppressing vernacular and non-institutional
 *   forms. The constraint exhibits tangled_rope structure: genuine
 *   coordination function (ecclesiastical and legal domains require stable
 *   technical vocabulary; Classical appeals require textual fidelity) paired
 *   with asymmetric extraction (selective legitimation privileges
 *   institutional actors, suppresses non-institutional drift). The
 *   theater_ratio (0.55) reflects the selective legitimation mechanism: the
 *   standard is maintained through institutional practice and exemplary
 *   correction rather than through explicit principle justifying why
 *   ecclesiastical terms are legitimate but popular formations are not.
 *
 * KEY AGENTS:
 *   - Ecclesiastical Institution (Church): Institutional beneficiary (institutional/arbitrage) — benefits from both Classical appeals to authority and legitimized technical vocabulary for doctrinal development
 *   - Legal/Administrative Apparatus: Institutional beneficiary (institutional/arbitrage) — benefits from Classical formality + legitimized technical terminology for contracts and official documents
 *   - Vernacular Speaker/Popular Practice: Primary victim (powerless/trapped) — medieval forms excluded as barbarism unless institutionally legitimized; no exit without full Classical adoption
 *   - Medieval Copyist/Scribe: Secondary victim/partial beneficiary (moderate/constrained) — benefits through ecclesiastical vocabulary access, constrained by correction pressure for non-sanctioned drift
 *   - Humanist Philological Movement: Organized actor (organized/constrained) — benefits from textual fidelity requirement, constrained by accommodation of post-Classical forms
 *   - Reformation Movement: Organized actor with exit path (organized/mobile) — sees hybrid as temporary institutional arrangement, advocates for reconstruction reading with sunset logic
 *   - Academic Latin Tradition: Institutional maintainer (institutional/arbitrage) — maintains standard through practice and inertia; sees own function as degraded (piton perspective)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the institutional compromise as linguistic law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(classical_latin_standard__hybrid_reading, 0.38).
domain_priors:suppression_score(classical_latin_standard__hybrid_reading, 0.48).
domain_priors:theater_ratio(classical_latin_standard__hybrid_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(classical_latin_standard__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(classical_latin_standard__hybrid_reading, "Classical Latin Standard (Hybrid Reading)").
narrative_ontology:topic_domain(classical_latin_standard__hybrid_reading, "historical_linguistics/philology/commitment_systems").

domain_priors:requires_active_enforcement(classical_latin_standard__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(classical_latin_standard__hybrid_reading, '7cf04595-fe01-4af7-857b-e8b9f6198e46').
narrative_ontology:cs_kernel_codification('7cf04595-fe01-4af7-857b-e8b9f6198e46', fixed_text).
narrative_ontology:cs_authority_grounding('7cf04595-fe01-4af7-857b-e8b9f6198e46', extraction).
narrative_ontology:cs_interpretation_layer_present('7cf04595-fe01-4af7-857b-e8b9f6198e46').
narrative_ontology:cs_reading_relation('7cf04595-fe01-4af7-857b-e8b9f6198e46', classical_latin_standard__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('7cf04595-fe01-4af7-857b-e8b9f6198e46', classical_latin_standard__reconstruction_reading, influences).
narrative_ontology:cs_axiom('7cf04595-fe01-4af7-857b-e8b9f6198e46', foundational, institutional_domains_justify_innovation).
narrative_ontology:cs_axiom_status(institutional_domains_justify_innovation, holdable).
narrative_ontology:cs_axiom_grounding('7cf04595-fe01-4af7-857b-e8b9f6198e46', institutional_domains_justify_innovation, conventional).
narrative_ontology:cs_axiom('7cf04595-fe01-4af7-857b-e8b9f6198e46', foundational, textual_fidelity_preserves_authority).
narrative_ontology:cs_axiom_status(textual_fidelity_preserves_authority, holdable).
narrative_ontology:cs_axiom_grounding('7cf04595-fe01-4af7-857b-e8b9f6198e46', textual_fidelity_preserves_authority, deontological).
narrative_ontology:cs_reference_frame('7cf04595-fe01-4af7-857b-e8b9f6198e46', classical_textual_authority).
narrative_ontology:cs_drift_state('7cf04595-fe01-4af7-857b-e8b9f6198e46', medieval_ecclesiastical_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7cf04595-fe01-4af7-857b-e8b9f6198e46', '').
narrative_ontology:cs_kernel_id(classical_latin_standard__hybrid_reading, classical_latin_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(classical_latin_standard__hybrid_reading, institutional_ecclesiastical_users).
narrative_ontology:constraint_beneficiary(classical_latin_standard__hybrid_reading, technical_domain_specialists).
narrative_ontology:constraint_victim(classical_latin_standard__hybrid_reading, popular_medieval_forms).
narrative_ontology:constraint_victim(classical_latin_standard__hybrid_reading, vernacular_practitioners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VERNACULAR SPEAKER (SNARE) — Structurally trapped by the standard's selective legitimation. Their native medieval forms are delegitimized as 'barbarism' unless they happen to fall into recognized technical domains (law, liturgy). Suppression is asymmetric: some drift is permitted (ecclesiastical vocabulary), other drift (popular formations) is excluded. Maximum extraction from this position — no exit without adopting Classical forms entirely.
constraint_indexing:constraint_classification(classical_latin_standard__hybrid_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: MEDIEVAL COPYIST/SCRIBE (TANGLED ROPE) — Beneficiary through access to legitimized ecclesiastical vocabulary (genuine coordination need: Church requires stable Latin forms). Victim through constant correction pressure for non-sanctioned drift. Constrained exit: can adopt Classical norms for formal texts but loses connection to living practice. Moderate extraction — the constraint enables their professional function (scribal authority) while suppressing their actual linguistic competence.
constraint_indexing:constraint_classification(classical_latin_standard__hybrid_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ECCLESIASTICAL INSTITUTION (ROPE) — Benefits from the hybrid standard: Classical textual fidelity legitimizes doctrinal authority (appeals to ancient sources), while permitted post-Classical technical vocabulary enables institutional communication (canon law, liturgical refinement). Net beneficiary with arbitrage: can switch between Classical appeals and practical accommodation. Experiences constraint as coordination of textual authority and operational vocabulary.
constraint_indexing:constraint_classification(classical_latin_standard__hybrid_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LEGAL/ADMINISTRATIVE APPARATUS (ROPE) — Benefits from legitimized technical terminology (legal Latin, administrative vocabulary) that distinguishes formal documents from vernacular. Experiences constraint as pure coordination: the hybrid standard enables both Classical formality (contracts, charters) and necessary technical precision. Arbitrage exit: can deploy Classical forms for authority, technical forms for precision, switch freely.
constraint_indexing:constraint_classification(classical_latin_standard__hybrid_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: HUMANIST PHILOLOGICAL MOVEMENT (TANGLED ROPE) — Organized actors (14th–16th century) who both benefit from and are constrained by the hybrid standard. Benefit: the standard's textual fidelity requirement enables their core method (recovery of Classical sources). Victim: the standard's accommodation of post-Classical technical forms limits their purism — they must defend why *some* medieval innovations are legitimate while others are not. Constrained: the standard prevents wholesale embrace of vernacular evolution but does not give them full Classical restoration. Coordination function (textual recovery) + extraction (legitimacy hierarchy constrains their freedom).
constraint_indexing:constraint_classification(classical_latin_standard__hybrid_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: REFORMATION MOVEMENT (SCAFFOLD) — Organized actors (16th century) who see the hybrid standard as a temporary institutional arrangement with a sunset. Their critique: the standard's selective legitimation of medieval forms reflects Catholic institutional capture, not linguistic principle. The escape path: abandon the hybrid compromise, return to pure Classical sources (reconstruction_reading). Theater: the hybrid standard's selective accommodation is exposed as maintaining institutional authority rather than serving linguistic coherence. This perspective has mobile exit because alternative readings of the kernel are available. Sunset logic: once the standard's institutional utility is exposed, actors can defect to reconstruction without degradation. Temporary structure: the hybrid resolves the medieval/Classical tension only by deferring the choice, not by answering it.
constraint_indexing:constraint_classification(classical_latin_standard__hybrid_reading, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ACADEMIC LATIN TRADITION (PITON) — The hybrid standard persists through institutional inertia and performative maintenance. Its function (enabling both textual authority and institutional communication) is real, but the way it is maintained (selective legitimation, category hierarchies, implicit rules about which domains permit which forms) is largely theatrical — the tradition asserts the standards through practice and example rather than explicit principle. Theater_ratio high: students learn 'correct' forms by imitation and correction, not through codified rules justifying why ecclesiastical terms are legitimate but popular formations are not. The tradition sees itself as degraded — 'real' Latin is either fully Classical (reconstruction) or living (continuity), and the hybrid is a compromise neither position fully endorses.
constraint_indexing:constraint_classification(classical_latin_standard__hybrid_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a linguistic universals perspective, the tension between textual fidelity and living practice is inherent to language standardization itself: any prescriptive standard must choose between historical authority and practical use, and no standard can fully reconcile both. The hybrid standard's selective legitimation reflects an inevitable feature of how languages are codified. This perspective naturalizes the institutional compromise as a law of linguistic structure. However, the structural data reveals this as a false summit: the specific forms legitimized or excluded in the hybrid reading are not linguistically necessary — they reflect institutional power (Church authority, legal tradition) not linguistic principle.
constraint_indexing:constraint_classification(classical_latin_standard__hybrid_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(classical_latin_standard__hybrid_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(classical_latin_standard__hybrid_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(classical_latin_standard__hybrid_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(classical_latin_standard__hybrid_reading, TR),
    TR >= 0.70.

:- end_tests(classical_latin_standard__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The hybrid reading permits selective accommodation of post-Classical forms, reducing the base extractiveness compared to pure reconstruction (which would have ε ≈ 0.52). However, the selectivity itself creates extraction: institutional actors determine which innovations are legitimate, granting themselves authority to evolve technical vocabulary while suppressing non-institutional drift. The 0.38 value reflects genuine coordination gains (ecclesiastical and legal domains do require stable technical vocabulary) offset by institutional gatekeeping of legitimacy. Suppression (0.48): Moderate. The standard suppresses popular medieval forms but permits institutional technical innovations. Suppression is asymmetric: not total (institutional actors can expand technical domains) but real (vernacular practice cannot). The moderate value reflects partial accommodation — some drift is legitimized, other drift is excluded on institutional grounds. Theater ratio (0.55): Moderate-high. The selective legitimation mechanism is maintained through institutional practice (teaching, correction, exemplary usage) rather than through explicit principle. The standard asserts hierarchies of legitimacy (ecclesiastical forms > legal forms > popular forms) through practice, not through codified rules explaining the hierarchy. The hybrid reading's theater is lower than pure reconstruction (which would have theater ≈ 0.72, performing absolute Classical restoration) but higher than pure continuity (which would have theater ≈ 0.35, naturalizing living practice). The moderate value reflects the standard's middle position: acknowledging institutional need for technical innovation while maintaining Classical textual appeals.
 *
 * PERSPECTIVAL GAP:
 *   The hybrid reading produces sharp perspectival gaps across institutional position. Institutional beneficiaries (Church, legal apparatus) experience rope — genuine coordination + modest leverage through legitimacy gatekeeping. Organized reformers experience scaffold — the hybrid is a temporary compromise with an exit path (reconstruction reading). The academic tradition experiences piton — the standard persists through inertia despite being neither fully Classical nor fully alive. Vernacular speakers experience snare — suppressed without coordination benefit. The analytical observer risks false-summit classification (naturalizing institutional compromise as linguistic law). The constraint demonstrates how the same kernel (correct Latin) produces incompatible classifications depending on the observer's structural position relative to the institution that wields the standard.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (Church, legal apparatus) experience low effective extraction because they control the legitimacy categories; they have arbitrage exit (can deploy Classical or technical forms strategically). Victims (vernacular speakers, popular practice) experience high effective extraction because they cannot participate in the standard's legitimacy determination; they face trapped or constrained exit. The moderate institutional actors (copyists, scholars) experience mid-range extraction because they partially benefit from access to legitimized vocabulary but are constrained by correction pressure. The organized reformers have mobile exit (alternative readings available). The directionality derivation confirms the tangled_rope classification: beneficiaries with arbitrage produce low d → negative χ (experienced as coordination); victims with trapped/constrained exit produce high d → high χ (experienced as extraction); the constraint has both flows simultaneously.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading resolves mandatrophy by showing that the classical_latin_standard kernel cannot be answered univocally. The three readings (continuity, reconstruction, hybrid) are not three perspectives on one constraint but three distinct institutional commitments to different resolutions of the kernel's contradiction: textual authority vs. living practice. The hybrid reading's tangled_rope classification reflects this: it is tangled because it attempts to maintain both poles (Classical fidelity + technical accommodation) without resolving the underlying tension. The mandatrophy is not resolved but deferred — the hybrid maintains institutional power (Church authority, legal formality) by permitting controlled evolution within institutional domains while suppressing uncontrolled vernacular change. The analytical observer's mountain perspective (naturalizing as linguistic law) is a false summit: the apparent inevitability of the hybrid compromise reflects institutional dominance, not linguistic principle. The constraint's stability depends on the Church's institutional authority to enforce the legitimacy boundaries — once that authority wanes (Reformation), the hybrid reading loses institutional backing and the kernel contest re-emerges in reconstruction vs. continuity conflict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    domain_boundary_ambiguity,
    'What distinguishes a ''legitimate technical domain'' (ecclesiastical, legal) that justifies post-Classical forms from a ''barbarism'' (popular drift) that does not?',
    'Historical analysis of which institutions had codification power (Church, royal courts, legal schools) vs. which did not (vernacular communities, merchants, peasants). Test whether legitimized forms correlate with institutional embeddedness rather than linguistic principle.',
    'If institutional: the standard''s suppression is extractive (institutional actors determine legitimacy). If linguistic: the standard''s suppression is coordinating (technical precision requires specialized vocabulary). This is the core omega determining whether the tangled_rope classification holds or reverts to snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(domain_boundary_ambiguity, empirical, 'Criterion for distinguishing legitimate technical domains from barbarisms').

omega_variable(
    reconstruction_vs_hybrid_foreclosure,
    'Does the hybrid reading''s acceptance of legitimate post-Classical forms logically foreclose the reconstruction reading''s demand for pure Classical restoration?',
    'Examine the reading_relations logic: if the hybrid axiom holds that some post-Classical forms are legitimate by linguistic principle, does this rule out the reconstruction axiom that only Classical forms are legitimate? Or can both coexist as different institutional commitments (one for ecclesiastical domains, one for philosophical/literary domains)?',
    'If foreclosure: the readings are in direct logical conflict; only one framework can hold both (classical_latin_standard kernel forces a choice). If coexistence: each reading can be institutionalized in different domains or by different communities (medieval Church uses hybrid, humanist scholars use reconstruction). This determines whether the oracle gap analysis shows genuine incompatibility or perspectival complementarity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reconstruction_vs_hybrid_foreclosure, conceptual, 'Whether hybrid reading forecloses or coexists with reconstruction reading').

omega_variable(
    ecclesiastical_authority_drift,
    'As ecclesiastical institutions evolve (medieval to early modern to modern), does the set of ''legitimate technical domains'' expand, contract, or remain stable? Does expansion signal institutional capture masquerading as principle?',
    'Historical corpus analysis: compare ecclesiastical Latin vocabulary across centuries. Track which post-Classical innovations are progressively legitimized. Correlate legitimization events with institutional expansion or contraction of Church authority.',
    'If legitimization expands: suggests the standard''s boundaries are contingent (institutional capture hypothesis confirmed). If stable: suggests principle-based domain distinction. Affects whether the suppression metric should be revised upward (if institutional capture) or remains stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecclesiastical_authority_drift, empirical, 'Drift in the set of legitimized post-Classical domains over time').

omega_variable(
    kernel_reading_contest_grounding,
    'What is the ultimate grounding for the choice between continuity_reading, hybrid_reading, and reconstruction_reading? Is it empirical (which forms are actually used), normative (which forms should be used), or conventional (which forms the authority says are correct)?',
    'Analyze each reading''s foundational axioms: continuity anchors in living practice (empirical), reconstruction anchors in textual authority (historical normative), hybrid anchors in institutional domains (institutional conventional). Each reading is coherent within its grounding, but they are incommensurable across groundings. The contest is not resolvable by facts alone.',
    'Confirms the constraint''s classification as tangled_rope rather than mountain or rope: the apparent coordination function (establishing Latin standards) masks an underlying unresolved kernel contest. The compromise is not stable — it depends on institutional power to suppress the other readings, not on a principled answer.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_grounding, conceptual, 'Incommensurability of reading groundings (empirical vs normative vs conventional)').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(classical_latin_standard__hybrid_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clat_hyb_tr_t0, classical_latin_standard__hybrid_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(clat_hyb_tr_t3, classical_latin_standard__hybrid_reading, theater_ratio, 3, 0.52).
narrative_ontology:measurement(clat_hyb_tr_t6, classical_latin_standard__hybrid_reading, theater_ratio, 6, 0.55).

% Extraction over time
narrative_ontology:measurement(clat_hyb_be_t0, classical_latin_standard__hybrid_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(clat_hyb_be_t3, classical_latin_standard__hybrid_reading, base_extractiveness, 3, 0.35).
narrative_ontology:measurement(clat_hyb_be_t6, classical_latin_standard__hybrid_reading, base_extractiveness, 6, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(clat_hyb_su_t0, classical_latin_standard__hybrid_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(clat_hyb_su_t3, classical_latin_standard__hybrid_reading, suppression_requirement, 3, 0.46).
narrative_ontology:measurement(clat_hyb_su_t6, classical_latin_standard__hybrid_reading, suppression_requirement, 6, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(classical_latin_standard__hybrid_reading, information_standard).
narrative_ontology:affects_constraint(classical_latin_standard__hybrid_reading, classical_latin_standard__continuity_reading).
narrative_ontology:affects_constraint(classical_latin_standard__hybrid_reading, classical_latin_standard__reconstruction_reading).

% DUAL FORMULATION NOTE:
% The classical_latin_standard kernel decomposes into three structurally distinct constraints, each with different epsilon values and readings of the legitimacy foundation. The hybrid_reading (this file) has ε≈0.38, moderate suppression through institutional gatekeeping, and tangled_rope classification. The continuity_reading would have ε≈0.12 (living practice as coordinate, minimal suppression), classification rope. The reconstruction_reading would have ε≈0.52 (demand for Classical purity, high suppression of medieval drift), classification snare. Each reading is a coherent institutional commitment; the contest between readings is not resolvable by evidence alone but depends on which grounding (institutional function, textual authority, living practice) is adopted.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(classical_latin_standard__hybrid_reading, institutional, 0.22).
constraint_indexing:directionality_override(classical_latin_standard__hybrid_reading, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
