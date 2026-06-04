% ============================================================================
% CONSTRAINT STORY: roman_republican_constitution__legal_codification_twelve_tables
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_roman_republican_constitution__legal_codification_twelve_tables, []).

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
 *   constraint_id: roman_republican_constitution__legal_codification_twelve_tables
 *   human_readable: The Twelve Tables: Legal Codification and Public Access in the Roman Republic
 *   domain: political/historical/constitutional_authority
 *
 * SUMMARY:
 *   The Twelve Tables represent the Republic's first and most fundamental
 *   written codification of law — a moment of political contest in which
 *   plebeian pressure forced the patrician-dominated legal system to codify
 *   and publicly post the rules that had been held in pontifical memory. This
 *   constraint captures a single but decisive institutional innovation: the
 *   move from oral, priestly-monopolized law to written, publicly accessible
 *   law. The constraint is one reading of a larger constitutional kernel —
 *   the Roman Republic itself — that competitors argue was primarily
 *   constituted by emergency machinery (dictatorship), institutional checks
 *   (magistracies and collegiality), popular sovereignty (assemblies and
 *   tribunate), or senatorial authority. The legal codification reading
 *   frames the constraint as fundamentally about suppression of arbitrary
 *   adjudication through the transparency mechanism of public law. The
 *   extractiveness of the pre-codification regime (0.68) reflects the high
 *   power asymmetry: priesthood controls adjudication, plebeians have no
 *   recourse, no appeal, and no way to predict judicial outcome. After
 *   codification (0.35), extractiveness drops substantially but persists
 *   because patrician magistrates still interpret and enforce the code, and
 *   literacy barriers exclude many plebeians from access to written law. This
 *   is tangled_rope: genuine coordination mechanism (written law enables
 *   predictable, consistent adjudication) alongside asymmetric extraction
 *   (patricians retain interpretive and enforcement authority). The
 *   theater_ratio trajectory shows that oral tradition is highly performative
 *   (0.85 — priesthood controls both content and legitimacy claims), while
 *   written law is substantially more functional (0.55 at maturity) but
 *   retains performative elements (magistrates still stage formal
 *   proceedings, written law still requires ritualized interpretation).
 *
 * KEY AGENTS:
 *   - Plebeian collective: Primary beneficiary and victim (powerless/trapped initially → constrained/organized after codification) — benefits from transparency, bears cost of ongoing patrician interpretive authority and enforcement discretion
 *   - Pontifical college: Primary victim (institutional) — monopoly on law knowledge is broken; legal interpretive authority is degraded from exclusive to shared
 *   - Patrician magistrates: Secondary beneficiary (institutional) — retain interpretive authority and enforcement discretion while gaining coordination benefits of clear written rules
 *   - Popular assemblies / tribunes: Organized secondary beneficiary (organized) — gain ability to hold magistrates accountable by reference to written text; gain leverage through collective assertion of rights
 *   - Analytical observer: Analytical position — risks naturalizing the contingent political achievement of codification as inevitable institutional evolution
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(roman_republican_constitution__legal_codification_twelve_tables, 0.35).
domain_priors:suppression_score(roman_republican_constitution__legal_codification_twelve_tables, 0.48).
domain_priors:theater_ratio(roman_republican_constitution__legal_codification_twelve_tables, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(roman_republican_constitution__legal_codification_twelve_tables, extractiveness, 0.35).
narrative_ontology:constraint_metric(roman_republican_constitution__legal_codification_twelve_tables, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(roman_republican_constitution__legal_codification_twelve_tables, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(roman_republican_constitution__legal_codification_twelve_tables, tangled_rope).
narrative_ontology:human_readable(roman_republican_constitution__legal_codification_twelve_tables, "The Twelve Tables: Legal Codification and Public Access in the Roman Republic").
narrative_ontology:topic_domain(roman_republican_constitution__legal_codification_twelve_tables, "political/historical/constitutional_authority").

domain_priors:requires_active_enforcement(roman_republican_constitution__legal_codification_twelve_tables).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(roman_republican_constitution__legal_codification_twelve_tables, '7638e556-213f-4a12-b6f5-4a8a05c61c87').
narrative_ontology:cs_kernel_codification('7638e556-213f-4a12-b6f5-4a8a05c61c87', formalized).
narrative_ontology:cs_authority_grounding('7638e556-213f-4a12-b6f5-4a8a05c61c87', extraction).
narrative_ontology:cs_interpretation_layer_present('7638e556-213f-4a12-b6f5-4a8a05c61c87').
narrative_ontology:cs_reading_relation('7638e556-213f-4a12-b6f5-4a8a05c61c87', roman_republican_constitution__crisis_machinery, coexists_with).
narrative_ontology:cs_reading_relation('7638e556-213f-4a12-b6f5-4a8a05c61c87', roman_republican_constitution__magistracies_and_collegiality, influences).
narrative_ontology:cs_reading_relation('7638e556-213f-4a12-b6f5-4a8a05c61c87', roman_republican_constitution__popular_assemblies_and_tribunate, coexists_with).
narrative_ontology:cs_reading_relation('7638e556-213f-4a12-b6f5-4a8a05c61c87', roman_republican_constitution__senate_authority, influences).
narrative_ontology:cs_axiom('7638e556-213f-4a12-b6f5-4a8a05c61c87', foundational, law_suppression_by_transparency).
narrative_ontology:cs_axiom_status(law_suppression_by_transparency, holdable).
narrative_ontology:cs_axiom_grounding('7638e556-213f-4a12-b6f5-4a8a05c61c87', law_suppression_by_transparency, instrumental).
narrative_ontology:cs_axiom('7638e556-213f-4a12-b6f5-4a8a05c61c87', secondary, patrician_interpretive_authority_persistence).
narrative_ontology:cs_axiom_status(patrician_interpretive_authority_persistence, holdable).
narrative_ontology:cs_axiom_grounding('7638e556-213f-4a12-b6f5-4a8a05c61c87', patrician_interpretive_authority_persistence, empirically_contingent).
narrative_ontology:cs_reference_frame('7638e556-213f-4a12-b6f5-4a8a05c61c87', oral_priestly_legal_monopoly).
narrative_ontology:cs_drift_state('7638e556-213f-4a12-b6f5-4a8a05c61c87', late_republican_institutionalization, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('7638e556-213f-4a12-b6f5-4a8a05c61c87', '').
narrative_ontology:cs_kernel_id(roman_republican_constitution__legal_codification_twelve_tables, roman_republican_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(roman_republican_constitution__legal_codification_twelve_tables, plebeian_citizens).
narrative_ontology:constraint_beneficiary(roman_republican_constitution__legal_codification_twelve_tables, non_patrician_populace).
narrative_ontology:constraint_victim(roman_republican_constitution__legal_codification_twelve_tables, pontifical_legal_monopoly).
narrative_ontology:constraint_victim(roman_republican_constitution__legal_codification_twelve_tables, patrician_arbitrary_adjudication).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PLEBEIAN LITIGANT BEFORE CODIFICATION (SNARE) — Before the Twelve Tables, the plebeian faces the pontifical monopoly on law. Legal rules are held in priestly memory, adjudication is opaque, and there is no defense against arbitrary interpretation. The plebeian is trapped — cannot exit civic participation without loss of status, cannot challenge judicial decisions without knowledge of the rules being applied. Maximum extraction: the litigant bears full cost of judicial uncertainty with no recourse.
constraint_indexing:constraint_classification(roman_republican_constitution__legal_codification_twelve_tables, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: PLEBEIAN LITIGANT AFTER TWELVE TABLES (ROPE) — After public codification and posting in the Forum, the plebeian can read the law, anticipate judicial reasoning, and contest decisions by reference to written text. The constraint shifts from snare (opaque extraction) to rope (genuine coordination). The litigant still faces inequality in resources and advocacy, but the codification provides a shared reference frame that enables collective assertion of rights. Theater ratio drops: the law is now functional, not performative.
constraint_indexing:constraint_classification(roman_republican_constitution__legal_codification_twelve_tables, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: PLEBEIAN COLLECTIVE (TANGLED ROPE) — The plebeian assembly and tribunes constitute organized collective agents with some agency. They benefit from codification (can now articulate collective grievances by reference to written law) while bearing costs of enforcement (must continuously pressure magistrates to enforce the code against patrician resistance). This perspective sees genuine coordination (codification enables collective action) alongside asymmetric extraction (patricians retain interpretive authority and can slow enforcement). Extractiveness remains moderate because the organized collective has leverage — the threat of secession or tribunician veto gives them capacity to resist arbitrary adjudication.
constraint_indexing:constraint_classification(roman_republican_constitution__legal_codification_twelve_tables, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: MAGISTRATE / PATRICIAN ADJUDICATOR (TANGLED ROPE) — The magistrate who applies the Twelve Tables experiences coordination (clear rules reduce docket complexity, enable consistent judgment) and extraction (cannot longer adjudicate by purely personal or familial preference — the rules bind the magistrate too). Extractiveness from this perspective is moderate because the magistrate retains interpretive discretion within the written frame. The codification is an inconvenience but also an advantage: written law shifts authority from individual magistrate to institutional office, enabling succession and reducing challenge to decisions.
constraint_indexing:constraint_classification(roman_republican_constitution__legal_codification_twelve_tables, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: PONTIFICAL COLLEGE (PITON) — The pontiffs' legal authority begins to degrade after codification. Their monopoly on law interpretation is broken — any literate citizen can read the Tables and challenge priestly judgment. The pontifical college persists and maintains significant interpretive authority (they are still consulted on religious law, they still control ritual), but their position as sole authoritative interpreters of secular law is substantially diminished. This is piton classification: institutional inertia maintains pontifical prestige long after the functional basis for their legal monopoly has been removed. Theater ratio rises from the pontifical perspective — their continued role becomes increasingly performative as lay interpretation spreads.
constraint_indexing:constraint_classification(roman_republican_constitution__legal_codification_twelve_tables, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the Twelve Tables instantiate a universal principle: any complex legal system must eventually codify and publish rules to function reliably. Oral-only law is inherently unstable at scale; written, public, posted law is a structural necessity for large-scale societies. This perspective sees the codification not as a political achievement but as a natural law of institutional development. However, the structural data contradicts this classification: the codification was politically contested (plebeian/patrician struggle), contingent (could have failed), and extractive (removed monopolistic power). The engine's false summit detector will identify this as naturalization of a contingent historical outcome.
constraint_indexing:constraint_classification(roman_republican_constitution__legal_codification_twelve_tables, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(roman_republican_constitution__legal_codification_twelve_tables_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(roman_republican_constitution__legal_codification_twelve_tables, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(roman_republican_constitution__legal_codification_twelve_tables, TypeOther, context(agent_power(powerless), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(roman_republican_constitution__legal_codification_twelve_tables, TR),
    TR >= 0.70.

:- end_tests(roman_republican_constitution__legal_codification_twelve_tables_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35, measurement end): The final measurement (t=3) reflects the constraint after codification has normalized within the Republic. Extractiveness is moderate, not low, because arbitrary adjudication persists within the interpretive authority retained by magistrates and because literacy barriers prevent plebeian access. The trajectory from 0.68 → 0.35 shows substantial but incomplete reduction in extraction through codification. Suppression (0.48): Moderate-high. The written law does suppress some arbitrary adjudication (reduces information asymmetry, creates written record for appeal), but suppression of plebeian alternatives remains high (no legal profession, no formal appellate process, patrician judges still control courtroom). Theater ratio (0.55): Moderate. Written law is substantially more functional than oral tradition (reduces performative content), but legal proceedings still retain ritual and ceremonial elements. The trajectory from 0.85 → 0.55 shows the shift from purely performative oral monopoly to mixed functional-performative written law.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same institutional innovation (codification) appears as complete extraction suppression (snare → rope transformation from plebeian view), as modest constraint on magistracial discretion (tangled_rope from magistrate view), as institutional degradation (piton from pontifical view), or as inevitable institutional law (mountain from analytical view). The plebeian perspective shows the largest shift (snare before, rope after) because the transparency mechanism most directly affects agents with no other legal resources. The magistrate perspective shows moderate constraint because written rules bind the magistrate but retain space for interpretation. The pontifical perspective shows loss of exclusive authority but continued influence. The analytical perspective risks naturalizing this as inevitable rather than contingent on plebeian political pressure.
 *
 * DIRECTIONALITY LOGIC:
 *   Plebeian agents begin with high d (0.95, full target of extraction) in the pre-codification regime because they are powerless and trapped, with no escape from arbitrary adjudication. After codification, d decreases to ~0.55 (moderate victim) because they gain some agency through literacy and collective organization, though they remain structurally disadvantaged. The pontifical college's d shifts upward from beneficiary status (~0.15, receiving extraction benefit) toward neutral (0.50) as their monopoly is broken and their interpretive authority becomes merely one voice among many. Magistrates retain low d as beneficiaries of retained interpretive authority. The overall constraint's directionality reflects the power redistribution: from pure extraction (high d targets with no alternatives) toward tangled rope (moderate targets with some alternatives alongside coordination benefits). The false summit mountain classification assumes d=0.72 (analytical), masking the actual asymmetric extraction structure.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION VIA KERNEL READING: The mandatrophy is resolved by specifying that this is one reading of a contested constitutional kernel, not 'the' constitution. The tangled_rope classification captures the actual structure: genuine coordination (codification enables predictable, consistent law) alongside asymmetric extraction (patricians retain interpretive and enforcement authority). This avoids both poles of the false choice: the constraint is neither pure extraction (snare — pre-codification) nor pure coordination (rope — idealized post-codification). It is a real institutional hybrid where codification suppresses some arbitrary adjudication while preserving patrician interpretive advantage. The analytical mountain perspective represents the natural-law false summit: 'inevitable institutional evolution of complex legal systems.' The kernel context reveals this as contestation: different parties held different constitutional commitments. The legal_codification reading was one achievement, not destiny.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    oral_memory_verifiability,
    'How much of the pontifical legal tradition was actually lost or distorted through oral transmission vs. deliberately obscured?',
    'Comparative reconstruction: identify laws recorded in the Twelve Tables that conflict with or supersede pontifical traditions; examine whether conflicts reflect genuine uncertainty or active suppression of knowledge',
    'If significant distortion detected: pontifical monopoly was not merely interpretive authority but active gatekeeping of true law (increases classification of pre-codification regime as snare). If minimal: pontifical role was primarily interpretive coordination, and extractiveness of opaqueness is overstated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(oral_memory_verifiability, empirical, 'Degree of deliberate obscuration in oral legal transmission').

omega_variable(
    plebeian_literacy_and_access,
    'What proportion of plebeians could actually read the Twelve Tables after posting? Did literacy access vary by region or economic status?',
    'Historical analysis of Roman literacy rates by class and period; examination of legal sources for evidence of plebeian legal knowledge and citation; comparison of urban vs rural plebeian access to the text',
    'If literacy access is <30% of plebeians: public posting is largely performative (increases theater_ratio and suggests rope classification is aspirational). If >70%: posting genuinely enables plebeian agency (confirms rope classification for literate plebeians, snare for illiterate remainder).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(plebeian_literacy_and_access, empirical, 'Actual plebeian access to and comprehension of posted Twelve Tables').

omega_variable(
    pontifical_reinterpretation_continuity,
    'Did the pontifical college retain interpretive authority over the Twelve Tables through religious law, auspices, and ritual authority after secular law was codified?',
    'Historical documentation of pontifical consultation in legal disputes post-codification; examination of religious law domains where pontiffs maintained interpretive monopoly; analysis of plebeian ability to challenge pontifical religious interpretations',
    'If pontiffs retained substantial interpretive authority through religious law: extractiveness of arbitrary adjudication is reduced but not eliminated (supports tangled_rope classification across longer timeframe). If pontifical authority is substantially eliminated: constraint moves toward rope classification and the false summit mountain is further invalidated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pontifical_reinterpretation_continuity, empirical, 'Continuation of pontifical interpretive authority after codification').

omega_variable(
    kernel_reading_contest,
    'Which constitutional reading of the Roman Republic best explains its stability and eventual collapse: legal codification (Twelve Tables), emergency machinery (dictatorship), institutional division (magistracies), popular sovereignty (assemblies), or senatorial auctoritas?',
    'Historical analysis of which institutional element(s) actually functioned as the binding constraint during Republic stability (509–264 BCE) vs breakdown (133–27 BCE); examination of which institution failed first during the civil wars; comparison of political theory in classical sources (Polybius, Cicero) with institutional reality',
    'If legal codification is the binding constraint: this reading''s classification as tangled_rope (coordination + extraction) is foundational. If legal codification is merely one instrument of a different primary constraint (e.g., magistracial collegiality, senatorial authority): this reading''s extractiveness may be overstated and its role in the constraint system less central than framed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, empirical, 'Which institutional element was the Republic''s primary constitutional constraint').

omega_variable(
    reading_foreclosure_test,
    'Does the legal codification reading foreclose the popular sovereignty reading? That is, can a constitution simultaneously prioritize written law (codification logic) and popular assembly supremacy (democratic logic) as its binding principle, or do these framings logically contradict?',
    'Philosophical analysis of codification vs democratic decision-making: can written law constrain democratic assemblies, or does democratic supremacy render written law subject to revision by vote? Historical examination of friction between popular assemblies and the written law of the Twelve Tables in actual Roman practice',
    'If foreclosure is real (codification and popular sovereignty are logically incompatible as primary constraints): reading_relations should include forecloses relation to popular_assemblies_and_tribunate reading. If no foreclosure (both can coexist): relation should be coexists_with. This determines the ds(kernel) (Deontic Structure of the kernel) classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_test, conceptual, 'Whether legal codification logically forecloses popular sovereignty as competing constitutional principle').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(roman_republican_constitution__legal_codification_twelve_tables, 0, 3).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_t0_oral_tradition, roman_republican_constitution__legal_codification_twelve_tables, theater_ratio, 0, 0.85).
narrative_ontology:measurement(theater_t1_early_written, roman_republican_constitution__legal_codification_twelve_tables, theater_ratio, 1, 0.72).
narrative_ontology:measurement(theater_t2_established_written, roman_republican_constitution__legal_codification_twelve_tables, theater_ratio, 2, 0.6).
narrative_ontology:measurement(theater_t3_functional_law, roman_republican_constitution__legal_codification_twelve_tables, theater_ratio, 3, 0.55).

% Extraction over time
narrative_ontology:measurement(extractiveness_t0_prelegal, roman_republican_constitution__legal_codification_twelve_tables, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(extractiveness_t1_early_codification, roman_republican_constitution__legal_codification_twelve_tables, base_extractiveness, 1, 0.52).
narrative_ontology:measurement(extractiveness_t2_codification_normalized, roman_republican_constitution__legal_codification_twelve_tables, base_extractiveness, 2, 0.4).
narrative_ontology:measurement(extractiveness_t3_late_republican, roman_republican_constitution__legal_codification_twelve_tables, base_extractiveness, 3, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(suppression_t0_oral_monopoly, roman_republican_constitution__legal_codification_twelve_tables, suppression_requirement, 0, 0.78).
narrative_ontology:measurement(suppression_t2_partial_enforcement, roman_republican_constitution__legal_codification_twelve_tables, suppression_requirement, 2, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(roman_republican_constitution__legal_codification_twelve_tables, enforcement_mechanism).
narrative_ontology:affects_constraint(roman_republican_constitution__legal_codification_twelve_tables, roman_republican_constitution__crisis_machinery).
narrative_ontology:affects_constraint(roman_republican_constitution__legal_codification_twelve_tables, roman_republican_constitution__magistracies_and_collegiality).
narrative_ontology:affects_constraint(roman_republican_constitution__legal_codification_twelve_tables, roman_republican_constitution__popular_assemblies_and_tribunate).
narrative_ontology:affects_constraint(roman_republican_constitution__legal_codification_twelve_tables, roman_republican_constitution__senate_authority).

% DUAL FORMULATION NOTE:
% The Twelve Tables reading is one of five constitutional readings of the roman_republican_constitution kernel. Each reading models a different institution as the binding constraint. These readings are siblings in a contested constitutional family — they compete, coexist, and influence one another. The legal_codification reading influences all others by establishing written law as the framework within which other institutions operate. The magistracies reading influences this by specifying who interprets and enforces the code. The popular_assemblies reading influences this by enabling collective challenge to code interpretation. The senate reading influences this by controlling magistrate selection and resource allocation for enforcement. The crisis_machinery reading operates orthogonally — it specifies how the system suspends itself rather than how it normally functions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(roman_republican_constitution__legal_codification_twelve_tables, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
