% ============================================================================
% CONSTRAINT STORY: magna_carta_constraint_authority__living_constitutionalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_constraint_authority__living_constitutionalism_reading, []).

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
 *   constraint_id: magna_carta_constraint_authority__living_constitutionalism_reading
 *   human_readable: Magna Carta Constraint Authority (Living Constitutionalism Reading)
 *   domain: constitutional_history/legal_philosophy
 *
 * SUMMARY:
 *   Magna Carta (1215, reissued 1217, 1225) establishes a foundational
 *   constraint on English royal prerogative through juridical precedent. The
 *   living constitutionalism reading interprets this medieval feudal covenant
 *   as a transmissible, evolving constraint on executive power that binds all
 *   subsequent rulers through constitutional inheritance and evolutionary
 *   interpretation. This reading holds that Magna Carta's core principle —
 *   that all persons, including the sovereign, are subject to law — survives
 *   regime transitions, expansions of the beneficiary class, and
 *   technological change through continuous reinterpretation by courts and
 *   parliaments. The constraint is classified as Rope (pure coordination)
 *   from the analytical perspective: monarch and subject both benefit from
 *   institutionalized legal restraint on arbitrary power. However, from the
 *   perspective of contemporaneous baronial interests (who authored the
 *   original charter), the constraint exhibits Tangled Rope characteristics —
 *   coordination benefit (shared legal framework) alongside asymmetric
 *   extraction (exclusion of non-propertied classes from beneficiary
 *   protections). The living constitutionalism reading emphasizes the
 *   constraint's evolutionary plasticity: successive reissues (1217, 1225),
 *   judicial expansions (Coke's invocations in the 1620s), parliamentary
 *   elaborations (Bill of Rights 1689, Reform Acts), and constitutional
 *   inheritance (adoption by former colonies and new democracies) demonstrate
 *   that the constraint's binding force persists through interpretive
 *   development rather than static fidelity to the original text. This is the
 *   distinctive marker of living constitutionalism: the constraint
 *   regenerates across time by being actively reread, not by being
 *   mechanically applied.
 *
 * KEY AGENTS:
 *   - King John and Reigning Monarch: Authority constrained (powerful/constrained) — bound by inherited precedent; benefits from legitimacy but loses discretionary prerogative
 *   - Barons and Parliamentary Classes: Authorized interpreters and beneficiaries (organized/constrained) — enforce and elaborate the constraint; gain voice in governance
 *   - Subjects and Citizens: Beneficiary class (moderate-to-powerless/mobile) — protected by due process shield and legal restraints on arbitrary power
 *   - Legal Profession and Judiciary: Custodians of interpretation (institutional/arbitrage) — maintain continuity of tradition through jurisprudential innovation; benefit from authority delegation
 *   - Revolutionary and Reform Movements: Organized challengers (organized/constrained) — invoke Magna Carta as legitimacy narrative for constraining monarchy during regime transition windows
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees the constraint as a genuine coordination mechanism that stabilizes governance through institutionalized restraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_constraint_authority__living_constitutionalism_reading, 0.28).
domain_priors:suppression_score(magna_carta_constraint_authority__living_constitutionalism_reading, 0.35).
domain_priors:theater_ratio(magna_carta_constraint_authority__living_constitutionalism_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_constraint_authority__living_constitutionalism_reading, rope).
narrative_ontology:human_readable(magna_carta_constraint_authority__living_constitutionalism_reading, "Magna Carta Constraint Authority (Living Constitutionalism Reading)").
narrative_ontology:topic_domain(magna_carta_constraint_authority__living_constitutionalism_reading, "constitutional_history/legal_philosophy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_constraint_authority__living_constitutionalism_reading, 'ec547e55-2854-4e16-a101-2be720082bda').
narrative_ontology:cs_kernel_codification('ec547e55-2854-4e16-a101-2be720082bda', fixed_text).
narrative_ontology:cs_authority_grounding('ec547e55-2854-4e16-a101-2be720082bda', lineage).
narrative_ontology:cs_interpretation_layer_present('ec547e55-2854-4e16-a101-2be720082bda').
narrative_ontology:cs_reading_relation('ec547e55-2854-4e16-a101-2be720082bda', magna_carta_constraint_authority__feudal_obsolescence_reading, coexists_with).
narrative_ontology:cs_reading_relation('ec547e55-2854-4e16-a101-2be720082bda', magna_carta_constraint_authority__parliamentary_sovereignty_reading, influences).
narrative_ontology:cs_axiom('ec547e55-2854-4e16-a101-2be720082bda', foundational, constraint_perpetually_reinterpreted).
narrative_ontology:cs_axiom_status(constraint_perpetually_reinterpreted, holdable).
narrative_ontology:cs_axiom_grounding('ec547e55-2854-4e16-a101-2be720082bda', constraint_perpetually_reinterpreted, conventional).
narrative_ontology:cs_axiom('ec547e55-2854-4e16-a101-2be720082bda', foundational, universal_rule_of_law_principle).
narrative_ontology:cs_axiom_status(universal_rule_of_law_principle, holdable).
narrative_ontology:cs_axiom_grounding('ec547e55-2854-4e16-a101-2be720082bda', universal_rule_of_law_principle, deontological).
narrative_ontology:cs_reference_frame('ec547e55-2854-4e16-a101-2be720082bda', evolutionary_constitutional_inheritance).
narrative_ontology:cs_drift_state('ec547e55-2854-4e16-a101-2be720082bda', contemporary_democratic_constitutional_order, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ec547e55-2854-4e16-a101-2be720082bda', '').
narrative_ontology:cs_kernel_id(magna_carta_constraint_authority__living_constitutionalism_reading, magna_carta_constraint_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__living_constitutionalism_reading, subjects_and_successors).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__living_constitutionalism_reading, rule_of_law_tradition).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: REIGNING MONARCH (ROPE) — A contemporary ruler bound by inherited due process norms sees Magna Carta as a coordination mechanism that constrains royal prerogative but enables legitimate governance through law. Exit is constrained (cannot simply repudiate constitutional inheritance without losing legitimacy) but not impossible. The monarch benefits from the constraint's legitimacy structure as much as the subject does — rule through law is more stable than arbitrary rule.
constraint_indexing:constraint_classification(magna_carta_constraint_authority__living_constitutionalism_reading, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: THE SUBJECT (ROPE) — Individual subjects experience Magna Carta as genuine coordination: lawful restraint on royal power protects their rights and enables predictable legal order. Mobile exit options (emigration, legal recourse) are theoretically available and structurally real. The constraint is experienced as beneficial coordination with legitimate authority, not extraction. Low suppression because subjects can invoke due process shields.
constraint_indexing:constraint_classification(magna_carta_constraint_authority__living_constitutionalism_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: BARONIAL-PARLIAMENTARY COALITION (TANGLED ROPE) — Organized agents (parliament, legal profession, nobility) who inherit and enforce Magna Carta's constraints benefit from the legitimacy structure (they are the authorized interpreters) but are also constrained by it. They experience both coordination (enabling common law development) and asymmetric extraction (exclusion of non-property-owning classes from due process protections). The constraint enables their power while limiting their scope.
constraint_indexing:constraint_classification(magna_carta_constraint_authority__living_constitutionalism_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: THE LEGAL PROFESSION (PITON) — Professional judges and lawyers maintain the Magna Carta interpretive tradition through ritualized invocation and evolutionary reading. Theater ratio is moderate-high (legal ceremony around 'ancient rights' performs continuity with 1215) but the functional constraint on royal power is real. The profession benefits from arbitrage (they are the authorized interpreters) and experiences the constraint as a coordinating infrastructure, not extraction. Piton classification reflects partial performativity of the 'ancient precedent' narrative alongside genuine legal function.
constraint_indexing:constraint_classification(magna_carta_constraint_authority__living_constitutionalism_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: REVOLUTIONARY-REFORM MOVEMENT (SCAFFOLD) — Organized reform agents invoking Magna Carta as a constitutional restraint on monarchy (English Civil War, 1688 Glorious Revolution, 19th century suffrage extensions) see the constraint as temporary scaffolding for regime transition. Scaffold logic: Magna Carta provides the legitimacy narrative for constraining monarchy during the window when absolute royal power is being dismantled. Once the transition completes (constitutional monarchy, parliamentary sovereignty, democratic representation), the scaffold's functional role decays. Sunset is endogenous — as democracy matures, appeals to inherited baronial privileges become anachronistic.
constraint_indexing:constraint_classification(magna_carta_constraint_authority__living_constitutionalism_reading, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (ROPE) — From the civilizational horizon, Magna Carta establishes a genuine coordination mechanism: all political agents (monarch and subject alike) benefit from institutionalized restraint on unilateral power. The constraint is low-extractive coordination that evolves through juridical interpretation rather than explicit renegotiation. Living constitutionalism (evolutionary reading) allows the constraint to adapt to new governance contexts without requiring formal amendment. This is textbook Rope: minimal coercion, genuine coordination benefit, coordination function visible to all perspectives.
constraint_indexing:constraint_classification(magna_carta_constraint_authority__living_constitutionalism_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_constraint_authority__living_constitutionalism_reading_tests).

test(piton_threshold) :-
    domain_priors:theater_ratio(magna_carta_constraint_authority__living_constitutionalism_reading, TR),
    TR >= 0.70.

:- end_tests(magna_carta_constraint_authority__living_constitutionalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-to-moderate. The living constitutionalism reading classifies Magna Carta as primarily a coordination mechanism with minimal asymmetric extraction. The original 1215 charter explicitly benefited the baronial class (extracting privilege from the crown while excluding commoners), giving it Tangled Rope characteristics from the contemporary baronial perspective. However, the living reading emphasizes evolutionary universalization: successive reinterpretations expand the beneficiary set from barons to all property-holders to all subjects to all citizens. From the analytical/civilizational perspective, this universalization trend reduces effective extractiveness — the constraint coordinates broad rule-of-law benefit rather than extracting narrowly for a privileged class. The extractiveness increases slightly over the interval (0.18 → 0.28) as the constraint's scope expands and more interpretive apparatus is required to maintain it, but it remains low because the functional benefit (legal restraint on arbitrary power) is genuine and broad. Suppression (0.35): Moderate, declining over time. In the original feudal context (1215), suppression is high (0.55) — enforcement of due process protections requires significant constraint on royal discretion and active baronial enforcement (threat of rebellion). Over subsequent centuries, suppression requirement decreases as the constraint becomes institutionalized in legal doctrine and parliamentary oversight. By the modern constitutional era, suppression (0.35) is moderate because the constraint is sustained by professional legal interpretation and constitutional convention rather than active coercive enforcement. Theater ratio (0.42): Moderate, declining over time. The original charter has low theater (0.25) — the constraint is a genuine military/political negotiation outcome between barons and king. Theater increases during reissues and reinterpretations (0.38-0.45) as the constraint becomes invoked as 'ancient precedent' and 'foundational law' through rhetorical elaboration. By the modern era, theater stabilizes at (0.42) — the constraint has both real institutional force and performative invocation of continuity with medieval origins. The living constitutionalism reading emphasizes that the constraint's continuity is genuine (evolutionary interpretation preserves binding force) rather than purely theatrical (ancient text invoked without functional content).
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gaps reveal why living constitutionalism is a distinct reading from feudal obsolescence and parliamentary sovereignty readings. (1) The reigning monarch sees Rope (coordination benefit through legitimacy) in living constitutionalism, whereas feudal obsolescence reading would classify as Snare (arbitrary royal power constrained without offsetting benefit). (2) The subject sees Rope (genuine legal protection) in living constitutionalism, whereas feudal obsolescence would classify as Mountain (constraint as natural law) and parliamentary sovereignty would classify differently based on whether parliament or crown is privileged. (3) The baronial-parliamentary coalition sees Tangled Rope (coordination + asymmetric privilege) in living constitutionalism, whereas parliamentary sovereignty reading makes parliament the primary beneficiary (shifting coalition membership). (4) The analytical observer sees Rope (genuine coordination) in living constitutionalism, whereas feudal obsolescence risks misclassifying as Mountain (erroneously treating medieval feudal relationship as natural law). The key difference: living constitutionalism emphasizes that the constraint evolves through continuous reinterpretation and expands its beneficiary class over time, whereas feudal obsolescence reading treats it as a dead letter (Piton inertia) and parliamentary sovereignty treats it as subsumed by parliamentary supremacy (constraint degraded).
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value (d) is derived from the agent's structural relationship to the constraint: (1) Reigning Monarch (powerful/constrained/beneficiary of legitimacy): d derives from constrained exit + beneficiary status (legitimacy benefits from rule-of-law framing) → low-to-moderate d. (2) Subject (moderate/mobile/beneficiary): d derives from mobile exit + clear beneficiary status (due process shield) → very low d (negative f(d)). (3) Baronial-Parliamentary Coalition (organized/constrained/mixed beneficiary-victim): d derives from constrained exit + beneficiary status offset by victim status (excluded from full universality in original, expanded over time) → moderate d. (4) Legal Profession (institutional/arbitrage/beneficiary): d derives from arbitrage exit + beneficiary status (authorized interpreters) → low d. The living constitutionalism reading produces lower d-values across agents than feudal obsolescence reading because living constitutionalism treats the constraint as coordinating genuine benefits for broad beneficiary classes, whereas feudal obsolescence would treat the constraint as primarily extractive (dead authority with inertial force). The measurement data confirms this: suppression requirement declines over time (0.55 → 0.35) as the constraint becomes institutionalized, which reduces effective extraction force.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy for Magna Carta is the tension between treating it as (a) a natural law of governance (immutable constitutional principle) vs. (b) a contingent historical artifact (feudal covenant now obsolete). Living constitutionalism resolves the mandatrophy by treating Magna Carta as a genuine coordination mechanism that is neither natural law nor obsolete, but rather a living institutional structure that regenerates through interpretation. The constraint is Rope (pure coordination) from the analytical perspective: all political agents benefit from rule-of-law restraint, making the constraint self-sustaining. The false-summit risk — treating Magna Carta as a natural law (Mountain) rather than a constructed constraint — is addressed by the omega variable on inherited-precedent binding: the constraint's binding force depends on performative legitimacy and institutional transmission, not on any natural property of law itself. The living constitutionalism reading avoids degradation to Piton (inertial maintenance of dead authority) by emphasizing continuous reinterpretation and evolutionary expansion of the beneficiary class — the constraint is alive because it is actively being reread, not merely invoked. The scaffold risk — treating Magna Carta as temporary scaffolding for regime transition — is addressed by the measurement data showing suppression requirement stabilizing in the modern era (no further decline after parliamentary supremacy is achieved), which suggests the constraint has transitioned from temporary scaffold to permanent coordination mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    living_vs_originalist_reading_identity,
    'Does living constitutionalism (this reading) genuinely instantiate a different constraint from strict originalism, or does it represent the same constraint read differently by the same interpretive authority?',
    'Examine whether living and originalist readings produce different directionality values for the same agents and observables. If d-values differ systematically, the readings instantiate structurally different constraints. If d-values are identical but classification diverges due to different time horizons, they are the same constraint with different perspective-relative classifications.',
    'If different constraints: originalist reading gets its own constraint_id and story file. If same constraint: both readings fold into perspectival variance within one story (not the current authoring approach). The current approach assumes they are different constraints with different ε values.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(living_vs_originalist_reading_identity, conceptual, 'Whether living constitutionalism and originalism are distinct constraints or same constraint with different readings').

omega_variable(
    inherited_precedent_binding_mechanism,
    'What makes inherited Magna Carta precedent binding on contemporary rulers? Is it formalized legal doctrine, performative legitimacy, path-dependent institutional inertia, or genuine consensual commitment to rule-of-law principle?',
    'Historical analysis of regime transitions and constitutional crises: when has a monarch or government explicitly repudiated Magna Carta binding force? What were the legitimacy costs? Did repudiation succeed or fail? Correlation between repudiation attempts and regime collapse.',
    'If binding via formalized doctrine: constraint is stable and transmissible across regimes (Rope classification holds). If binding via performative legitimacy alone: constraint is vulnerable to delegitimization and revocation (Piton classification more accurate). If binding via institutional inertia: constraint is stable but degrading (Piton precedence over Rope). If binding via genuine principle: classification stable across all readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inherited_precedent_binding_mechanism, empirical, 'Mechanism by which Magna Carta precedent binds contemporary rulers').

omega_variable(
    exclusivity_of_beneficiary_class,
    'Does the living constitutionalism reading genuinely extend due process protections universally (all subjects = beneficiaries), or does it preserve exclusion of non-propertied classes from the beneficiary set while performing universalism?',
    'Textual analysis of Magna Carta reissues and 13th-19th century legal interpretations: which classes are explicitly covered by ''free men'' / ''all subjects'' language? Correlation between formal beneficiary expansion (women, non-property-owners, non-citizens) and explicit constraint revisions vs. interpretive claims of continuity.',
    'If genuinely universal: constraint is Rope with low extractiveness and inclusive beneficiary set. If exclusionary with universalist performance: constraint is Tangled Rope or Snare (coordination benefit for in-group, extraction from out-group) and beneficiary set should list only the included classes. This reading''s claimed_type assumes living constitutionalism achieves real universality; empirical falsification would require reclassification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusivity_of_beneficiary_class, empirical, 'Whether Magna Carta''s due process protections extend universally or preserve class exclusion').

omega_variable(
    king_john_context_collapse,
    'To what extent does the living constitutionalism reading require bracketing or transcending the original 1215 context (feudal covenant between barons and king) in order to justify reading it as a universal rule-of-law constraint?',
    'Textual archaeology: identify which clauses of Magna Carta Carta are genuinely about constraining arbitrary power universally vs. which clauses are specific to feudal obligations, baronial privileges, or 13th-century jurisdictions. Quantify the proportion of the document that carries forward to contemporary application.',
    'If minimal context collapse required: Magna Carta''s core can sustain living constitutionalism reading (Rope classification valid). If substantial collapse required: living constitutionalism reading may be performative reinterpretation rather than evolutionary development (theater_ratio should be higher, Piton classification more accurate). This reading assumes minimal collapse; empirical evidence of heavy reinterpretation would suggest the constraint is degraded (inertial) rather than living.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(king_john_context_collapse, empirical, 'Extent of contextual reinterpretation required for living constitutionalism reading').

omega_variable(
    constraint_kernel_stability_under_regime_transition,
    'When regime transition occurs (monarchy to republic, absolute to constitutional monarchy, independent to post-colonial state), what fraction of the Magna Carta constraint''s structural force survives transplantation? Does the constraint rebind to new authority or does binding authority itself shift?',
    'Comparative constitutionalism: examine post-colonial constitutions, republican governance transitions, and revolutionary government legitimation narratives. Track which constraints explicitly invoke Magna Carta lineage vs. which repudiate it. Measure adoption rates of Magna Carta-derived due process provisions in new regimes.',
    'If high transplantation success (>70% of new regimes adopt Magna Carta-derived constraints): constraint is robust to regime change and Rope classification holds. If low success: constraint is authority-dependent and may collapse under regime transition (Scaffold classification more accurate). Living constitutionalism assumes high transplantation; empirical verification would confirm or refute.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constraint_kernel_stability_under_regime_transition, empirical, 'Magna Carta constraint survival under regime transitions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_constraint_authority__living_constitutionalism_reading, 0, 800).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mc_living_theater_1215, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(mc_living_theater_1365, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 150, 0.38).
narrative_ontology:measurement(mc_living_theater_1565, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 350, 0.45).
narrative_ontology:measurement(mc_living_theater_1715, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 500, 0.42).

% Extraction over time
narrative_ontology:measurement(mc_living_extract_1215, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(mc_living_extract_1365, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 150, 0.22).
narrative_ontology:measurement(mc_living_extract_1565, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 350, 0.28).
narrative_ontology:measurement(mc_living_extract_1715, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 500, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(mc_living_suppress_1215, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(mc_living_suppress_1365, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 150, 0.48).
narrative_ontology:measurement(mc_living_suppress_1565, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 350, 0.38).
narrative_ontology:measurement(mc_living_suppress_1715, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 500, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_constraint_authority__living_constitutionalism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__living_constitutionalism_reading, magna_carta_constraint_authority__feudal_obsolescence_reading).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__living_constitutionalism_reading, magna_carta_constraint_authority__parliamentary_sovereignty_reading).

% DUAL FORMULATION NOTE:
% Magna Carta constraint authority is realized through three structurally distinct readings: (1) living_constitutionalism_reading (this file) — evolutionary interpretation, universal rule-of-law benefit, Rope classification; (2) feudal_obsolescence_reading — constraint as dead medieval text maintained through inertia, Piton classification; (3) parliamentary_sovereignty_reading — constraint as legitimacy narrative for parliament's supremacy over crown, Tangled Rope classification. Each reading produces different ε values, different beneficiary sets, and different terminal classifications. The readings are related by the kernel 'magna_carta_constraint_authority' but are structurally distinct constraints under the ε-invariance principle. Empirical test: if measuring the constraint via doctrinal continuity (text-based interpretation) produces ε ≈ 0.08, but measuring via institutional power dynamics produces ε ≈ 0.45, then the 'constraint' label conflates multiple distinct structural phenomena. Each reading chooses a measurement basis that produces its own ε consistently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
