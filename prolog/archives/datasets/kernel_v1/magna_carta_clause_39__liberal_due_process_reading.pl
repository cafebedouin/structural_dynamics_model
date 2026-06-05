% ============================================================================
% CONSTRAINT STORY: magna_carta_clause_39__liberal_due_process_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_clause_39__liberal_due_process_reading, []).

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
 *   constraint_id: magna_carta_clause_39__liberal_due_process_reading
 *   human_readable: Magna Carta Clause 39 (Liberal Due Process Reading): Universal Individual Rights Against Arbitrary State Power
 *   domain: constitutional_law/legal_history/political_theory
 *
 * SUMMARY:
 *   Clause 39 of Magna Carta ("No free man shall be seized or imprisoned, or
 *   stripped of his rights or possessions, except by the lawful judgment of
 *   his equals or by the law of the land") is a contested kernel that has
 *   been read through three fundamentally different interpretive frames: the
 *   feudal_prerogative_reading (which treats it as a protection for baronial
 *   property rights against royal overreach, preserving estate-based
 *   hierarchy); the originalist_limitation_reading (which treats it as a
 *   formal limitation on state power establishing rule of law without
 *   necessarily universal applicability); and the liberal_due_process_reading
 *   instantiated here (which treats it as establishing universal individual
 *   rights against arbitrary state authority, grounding modern constitutional
 *   due process protections). This constraint story models ONLY the liberal
 *   reading: that Clause 39 constrains all state power, protects all
 *   individuals equally, and grounds an abstract principle of due process
 *   applicable universally. The structural properties of this reading reveal
 *   a tangled rope: genuine coordination function (both authority and subject
 *   commit to procedure-following) combined with asymmetric extraction (from
 *   unchecked executive discretion toward individual protection). The
 *   measurements show theater ratio rising from 0.15 in 1215 (when
 *   enforcement was direct — nobles physically enforced charter rights) to
 *   0.58 in 2015 (modern due process review is substantially performative,
 *   with procedural compliance masking substantive power asymmetries). Base
 *   extractiveness has risen from 0.35 to 0.48, indicating that the scope of
 *   the constraint (who it protects, what it protects from) has expanded
 *   while enforcement intensity has declined.
 *
 * KEY AGENTS:
 *   - Individual Subjects: Primary beneficiaries (powerless/trapped) — protected from arbitrary state seizure; bound within state jurisdiction but gain predictable legal interaction
 *   - Arbitrary Executive Authority: Primary victim in liberal reading (institutional/trapped) — forced to justify state action through procedure rather than will; cannot exit constraint without abandoning legitimacy
 *   - Propertied Classes: Secondary beneficiaries (moderate/constrained) — gain advantage from procedure-based protection; constrained because property rights depend on state system
 *   - Courts & Judicial System: Institutional gatekeepers (institutional/arbitrage) — interpret due process; now maintain performative theater while enabling extraction through other mechanisms
 *   - Rights-Based Internationalism: Organized reformers (organized/mobile) — treating due process as temporary scaffold toward post-state rights governance; see sunset clause in evolution of transnational accountability
 *   - Liberal Tradition: Ideological authority (analytical/analytical) — naturalizes due process as immutable principle; risk of false summit (naturalizing contingent institutional commitment)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_clause_39__liberal_due_process_reading, 0.48).
domain_priors:suppression_score(magna_carta_clause_39__liberal_due_process_reading, 0.52).
domain_priors:theater_ratio(magna_carta_clause_39__liberal_due_process_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_clause_39__liberal_due_process_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_clause_39__liberal_due_process_reading, "Magna Carta Clause 39 (Liberal Due Process Reading): Universal Individual Rights Against Arbitrary State Power").
narrative_ontology:topic_domain(magna_carta_clause_39__liberal_due_process_reading, "constitutional_law/legal_history/political_theory").

domain_priors:requires_active_enforcement(magna_carta_clause_39__liberal_due_process_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_clause_39__liberal_due_process_reading, 'f12d637b-eca3-46ee-bdd7-6a478c48fe87').
narrative_ontology:cs_kernel_codification('f12d637b-eca3-46ee-bdd7-6a478c48fe87', fixed_text).
narrative_ontology:cs_authority_grounding('f12d637b-eca3-46ee-bdd7-6a478c48fe87', lineage).
narrative_ontology:cs_interpretation_layer_present('f12d637b-eca3-46ee-bdd7-6a478c48fe87').
narrative_ontology:cs_reading_relation('f12d637b-eca3-46ee-bdd7-6a478c48fe87', magna_carta_clause_39__feudal_prerogative_reading, coexists_with).
narrative_ontology:cs_reading_relation('f12d637b-eca3-46ee-bdd7-6a478c48fe87', magna_carta_clause_39__originalist_limitation_reading, influences).
narrative_ontology:cs_axiom('f12d637b-eca3-46ee-bdd7-6a478c48fe87', foundational, individual_rights_are_universal_and_prior).
narrative_ontology:cs_axiom_status(individual_rights_are_universal_and_prior, holdable).
narrative_ontology:cs_axiom_grounding('f12d637b-eca3-46ee-bdd7-6a478c48fe87', individual_rights_are_universal_and_prior, deontological).
narrative_ontology:cs_axiom('f12d637b-eca3-46ee-bdd7-6a478c48fe87', foundational, state_authority_is_subordinate_to_procedure).
narrative_ontology:cs_axiom_status(state_authority_is_subordinate_to_procedure, holdable).
narrative_ontology:cs_axiom_grounding('f12d637b-eca3-46ee-bdd7-6a478c48fe87', state_authority_is_subordinate_to_procedure, deontological).
narrative_ontology:cs_reference_frame('f12d637b-eca3-46ee-bdd7-6a478c48fe87', universal_individual_due_process_protection).
narrative_ontology:cs_drift_state('f12d637b-eca3-46ee-bdd7-6a478c48fe87', contemporary_legal_systems, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f12d637b-eca3-46ee-bdd7-6a478c48fe87', '').
narrative_ontology:cs_kernel_id(magna_carta_clause_39__liberal_due_process_reading, magna_carta_clause_39).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__liberal_due_process_reading, individuals_protected_by_due_process).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__liberal_due_process_reading, check_on_executive_discretion).
narrative_ontology:constraint_victim(magna_carta_clause_39__liberal_due_process_reading, unchecked_state_authority).
narrative_ontology:constraint_victim(magna_carta_clause_39__liberal_due_process_reading, arbitrary_executive_power).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL SUBJECT (ROPE) — From the powerless subject's biographical perspective, Clause 39 establishes genuine coordination: a shared framework (due process) that allows predictable legal interaction and protection from arbitrary seizure. The individual is trapped within the state's jurisdiction but the constraint creates mutual obligation on the state itself. This is coordination because both parties (subject and authority) commit to procedure-following. The constraint solves a collective action problem: without due process norms, predatory authority extracts capriciously; with them, subjects can navigate legally. The individual perceives this as a binding but fair framework rather than pure extraction.
constraint_indexing:constraint_classification(magna_carta_clause_39__liberal_due_process_reading, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PROPERTIED CLASS (TANGLED ROPE) — Moderate power actors (early medieval barons, later propertied merchants) experience Clause 39 as both coordination and extraction. The clause genuinely enables their collective action against arbitrary royal seizure of lands — a coordination function. But it also restricts others' (the crown's) power while enhancing theirs. They gain asymmetric benefit: the clause constrains who can be arbitrarily seized and under what process, and the propertied have resources to invoke procedure-following. The constraint extracts from executive discretion in their favor. Exit is constrained because property rights depend on the state system; one cannot simply leave without abandoning holdings.
constraint_indexing:constraint_classification(magna_carta_clause_39__liberal_due_process_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MONARCH'S ARBITRARY AUTHORITY (SNARE) — From the perspective of unchecked executive discretion, Clause 39 is pure extraction with suppression. The monarch experiences the constraint as pure subordination to legal procedure with no functional coordination benefit — the monarch is forced to justify seizures through law rather than simply exercising will. The suppression is maximal: the monarch cannot exit the constraint without reneging on the charter itself, which carries legitimacy costs. This perspective sees the constraint as extraction directed against executive power. However, note: this is not the actual monarch's modern perspective, but the structural position of 'arbitrary authority' itself — the constraint specifically targets and extracts from unchecked power.
constraint_indexing:constraint_classification(magna_carta_clause_39__liberal_due_process_reading, snare,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 4: LIBERAL DEMOCRATIC TRADITION (MOUNTAIN) — From an organized liberal observer at generational scope, Clause 39 appears as an immutable principle: that individuals possess natural/inherent rights to due process that no authority can legitimately violate. The liberal reading treats due process as a foundational law of legitimate governance — unchangeable, universal, prior to state authority. This perspective naturalizes what is actually a contingent institutional commitment. The engine's false summit detector will flag this: the natural-law framing masks that due process norms are enforceable commitments (tangled rope) not natural laws. The generation-al timescale and global scope (applying the principle universally) reinforce the mountain classification, but the structural data shows constructed constraints.
constraint_indexing:constraint_classification(magna_carta_clause_39__liberal_due_process_reading, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: MODERN COURT SYSTEM (PITON) — Contemporary courts claim to enforce Clause 39 (or its modern equivalents — 5th Amendment due process, 14th Amendment equal protection) but often perform the ritual of due process review while enabling systematic extraction through other mechanisms (bail system, discovery costs, plea bargaining pressure). The court system sees itself as upholding due process, but the functional verification of arbitrary state power has degraded into procedural theater. Courts gate-keep the interpretation of what constitutes 'process' and arbitrate what constitutes 'law' — the constraint persists through institutional inertia and legitimacy theater rather than through active enforcement of its coordinative function. Theater ratio is high because procedural compliance can mask substantive arbitrariness.
constraint_indexing:constraint_classification(magna_carta_clause_39__liberal_due_process_reading, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: RIGHTS-BASED INTERNATIONALISM (SCAFFOLD) — The global rights movement (UN Universal Declaration of Human Rights, International Criminal Court, human rights NGOs) treats Clause 39 as the kernel of a temporary scaffolding system: establishing individual rights protections that will eventually make coercive state power structurally obsolete. This reading has low effective extraction because it includes an explicit sunset clause: as international norms, supranational courts, and transnational accountability mechanisms mature, the need for individual state-level due process claims diminishes. The constraint is transitional — it solves an immediate coordination problem (protecting individuals during the period of state-centric legal systems) while enabling exit toward post-state alternatives (cosmopolitan governance, transnational law).
constraint_indexing:constraint_classification(magna_carta_clause_39__liberal_due_process_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — At civilizational scope, this reading instantiates a genuine hybrid: Clause 39 coordinates legitimate state action (both state and subject commit to procedure) while extracting from unchecked discretionary power. The constraint is not a natural law but a socially enforced commitment with real enforcement costs. It enables subject protection AND subordinates executive authority to law. Both functions are genuine. The extractiveness (0.48) reflects that the constraint subordinates a power source (executive discretion) while enabling a benefit (individual protection). The suppression (0.52) reflects the active enforcement requirements: courts must exist, procedures must be codified, violations must trigger consequences. This is a constructive reading that avoids naturalizing the constraint as an inherent principle.
constraint_indexing:constraint_classification(magna_carta_clause_39__liberal_due_process_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_clause_39__liberal_due_process_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(magna_carta_clause_39__liberal_due_process_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(magna_carta_clause_39__liberal_due_process_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(magna_carta_clause_39__liberal_due_process_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(magna_carta_clause_39__liberal_due_process_reading, TR),
    TR >= 0.70.

:- end_tests(magna_carta_clause_39__liberal_due_process_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The liberal reading interprets Clause 39 as extracting from unchecked executive discretion — the state must subordinate its power to procedure, which constrains but does not eliminate state authority. The extractiveness is not higher because the constraint is genuinely coordinative (both authority and subject gain from predictable procedure). Rising extractiveness over time (0.35 → 0.48) reflects that the modern interpretation has expanded the scope of individual protection while the enforcement machinery has degraded — the constraint now claims to apply universally but enforcement is fragmented and procedurally complex. Suppression (0.52): Moderate-high. Suppression measures the barriers to exit and alternatives. Individuals cannot exit state jurisdiction (trapped within legal system); the state cannot exit due process claims without delegitimation. However, suppression is not total because both parties can attempt to subvert procedure — the state through systematic complexity, individuals through collective action or jurisdictional exit. Theater ratio (0.38 baseline, rising to 0.58): Moderate baseline reflecting that due process was genuinely enforceable in early charters but increasingly theatrical in modern application. Modern courts perform due process review while bail systems, discovery costs, and plea pressure enable systematic extraction through mechanisms formally outside due process scope. Claimed type (tangled_rope): Correct. The constraint has genuine coordinative function (procedure-following is mutually binding) and genuine extractive asymmetry (constraint on unchecked power is unequal burden on different actors — propertied classes bear lower burden than powerless).
 *
 * PERSPECTIVAL GAP:
 *   The reading produces maximum perspectival divergence across contexts. From the powerless individual's perspective, the clause is protective coordination (rope). From unchecked authority's perspective, it is pure extraction (snare). From the propertied class's perspective, it is mixed — both coordination and asymmetric benefit (tangled rope). From the liberal ideological tradition's perspective, it is a natural law (mountain — but a false summit). From the modern court system's perspective, it is degraded ritual (piton). From the international rights movement's perspective, it is temporary scaffolding with sunset (scaffold). The perspectives do not disagree about facts — they differ in structural position and temporal scope. This is the diagnostic signature of a genuine tangled rope: multiple coherent classifications from different structural positions, none wrong, all capturing different aspects of the same constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) maps each agent's structural position: do they benefit from or bear costs from this constraint? Powerless individuals have high d (bearers of arbitrary power) but the constraint reverses this — they become beneficiaries at low d. Arbitrary authority has d ≈ 1.0 (pure target of constraint). Propertied classes have d ≈ 0.4 (asymmetric benefit but constrained by system itself). Courts have d ≈ 0.3 (beneficiaries of legitimacy, arbitrage available). Liberal tradition has d ≈ 0.5 (symmetric — both benefits from naturalizing universal rights and costs from constraint on power). The engine derives d from beneficiary/victim declarations: individuals are beneficiaries (d toward 0), arbitrary authority is victim (d toward 1.0). Directionality feeds sigmoid f(d) which produces experienced extractiveness chi — different perspectives perceive different chi values from the same base extractiveness (0.48) due to different structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves the mandatrophy by clearly declaring its structural position: it interprets the clause as establishing universal individual rights (not estate-particular rights), as grounding due process protections (not merely formalizing existing discretion), and as extracting from unchecked authority (not coordinating among equals). The analytical observer perspective explicitly notes the risk of false summitry — the liberal tradition treats due process as an immutable principle, but this is naturalizing a contingent institutional commitment. The measurements show degradation: as enforcement has become more judicial (courts interpreting due process) and less direct (nobles physically enforcing charter rights), the theater ratio has risen and extractiveness has paradoxically increased — the constraint now claims broader scope but enforcement is weaker. This is the classic mandatrophy shape: claimed universality rising as actual enforcement power declines. The reading does not claim to resolve this tension — it documents it and leaves the tower's integrity question for omega variables.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    due_process_universality_vs_particularity,
    'Does Clause 39 establish universal individual rights applicable to all persons regardless of status, or particular protections for a defined group (originally free men, property holders)?',
    'Historical textual analysis of ''anyone'' vs ''free man'' language; comparative reading of feudal_prerogative_reading claim about estate-based rights; empirical documentation of actual expansion of clause scope over time (1215 → 1628 → 19th century → modern)',
    'If universal: this reading''s core claim holds — clause constrains arbitrary authority over all subjects. If particular: the reading has misidentified the kernel''s scope; the constraint is narrower than claimed and extraction mainly benefited the property-holding class. Sibling readings may be correct about the original intent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(due_process_universality_vs_particularity, empirical, 'Whether Clause 39 establishes universal or estate-particular protections').

omega_variable(
    natural_rights_vs_constructed_norms,
    'Is due process a pre-political natural right that the clause merely recognizes, or a constructed legal norm that the clause establishes and must continuously enforce?',
    'Philosophical lineage analysis (Locke, Hobbes, natural law theory vs legal positivism); documentation of periods when due process enforcement weakened or disappeared; evidence of deliberate norm construction vs discovery of inherent principle',
    'If natural right: mountain classification is defensible; the liberal reading tracks something immutable. If constructed norm: piton and tangled_rope perspectives are correct; the constraint requires active enforcement and is subject to degradation. This determines whether the false summit detection fires.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_rights_vs_constructed_norms, conceptual, 'Whether due process is a natural right or constructed legal norm').

omega_variable(
    extraction_directionality_ambiguity,
    'Does Clause 39 extract FROM arbitrary executive power (FOR individual protection) or extract FROM individual liberty (FOR state authority to enforce law)?',
    'Documentation of actual implementation: do clause applications protect individuals from state power, or do they enable state enforcement? Case law analysis showing whether the clause limits or enables state authority in specific domains (property seizure, criminal procedure, administrative action)',
    'If extracts from executive: this reading''s snare perspective on ''arbitrary authority'' is correct. If enables state: the originalist_limitation_reading may be correct that the clause enhances state power to enforce law formally. Directionality fundamentally determines classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_directionality_ambiguity, empirical, 'Whether the clause extracts from executive discretion or enables state enforcement').

omega_variable(
    scope_creep_vs_living_doctrine,
    'Is the modern expansion of Clause 39 protections (to all persons, all contexts, global application) a legitimate evolution of a living doctrine, or scope creep that has transformed a limited feudal protection into an overreaching universal claim?',
    'Comparative analysis of 1215 charter text vs modern interpretations; documentation of how each major legal/political epoch reinterpreted the clause; identification of explicit doctrinal breaks vs continuous interpretation',
    'If living doctrine: this reading''s universalism is correct and the constraint legitimately applies to all persons. If scope creep: the reading has extended beyond the kernel''s actual scope; originalist_limitation_reading is correct. Determines the reading''s internal coherence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_creep_vs_living_doctrine, conceptual, 'Whether modern expansion is legitimate doctrine evolution or scope creep').

omega_variable(
    enforcement_degradation_rate,
    'At what rate has the actual enforcement of due process protections degraded relative to the formal existence of the clause?',
    'Time-series measurement of piton indicators: ratio of due process claims filed vs granted; documentation of procedural theater growth (discovery burdens, plea pressure, bail system complexity); evidence of systematic substitution of formal procedure for substantive protection',
    'High degradation: piton perspective is correct; the constraint is increasingly performative. Low degradation: tangled_rope and rope perspectives hold; the constraint remains functionally coordinative. Measurement trajectory informs mandatrophy analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_degradation_rate, empirical, 'Rate of enforcement degradation relative to clause formality').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_clause_39__liberal_due_process_reading, 0, 800).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mc39_theater_1215, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(mc39_theater_1465, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 250, 0.28).
narrative_ontology:measurement(mc39_theater_1765, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 550, 0.42).
narrative_ontology:measurement(mc39_theater_2015, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 800, 0.58).

% Extraction over time
narrative_ontology:measurement(mc39_extract_1215, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(mc39_extract_1465, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 250, 0.4).
narrative_ontology:measurement(mc39_extract_1765, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 550, 0.45).
narrative_ontology:measurement(mc39_extract_2015, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 800, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_clause_39__liberal_due_process_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_clause_39__liberal_due_process_reading, magna_carta_clause_39__feudal_prerogative_reading).
narrative_ontology:affects_constraint(magna_carta_clause_39__liberal_due_process_reading, magna_carta_clause_39__originalist_limitation_reading).
narrative_ontology:affects_constraint(magna_carta_clause_39__liberal_due_process_reading, habeas_corpus_writ_protection).
narrative_ontology:affects_constraint(magna_carta_clause_39__liberal_due_process_reading, fifth_amendment_due_process).
narrative_ontology:affects_constraint(magna_carta_clause_39__liberal_due_process_reading, fourteenth_amendment_equal_protection).

% DUAL FORMULATION NOTE:
% Magna Carta Clause 39 is a single kernel with three structurally distinct constraint readings. Each reading instantiates different beneficiaries, victims, and extractiveness values. The liberal_due_process_reading (this constraint) treats the clause as universal individual rights against arbitrary authority (ε ≈ 0.48, tangled_rope). The feudal_prerogative_reading interprets it as baronial protection of property rights (ε ≈ 0.35, tangled_rope favoring nobles). The originalist_limitation_reading sees formal rule-of-law constraint without universal scope claims (ε ≈ 0.25, rope). These are not the same constraint viewed from different angles — they have different ε values and different scope assumptions. They are linked as siblings by network.affects_constraints to enable comparative analysis of how the same historical text grounds different institutional orderings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
