% ============================================================================
% CONSTRAINT STORY: australian_federation_1901__washminster_hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_australian_federation_1901__washminster_hybrid_reading, []).

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
 *   constraint_id: australian_federation_1901__washminster_hybrid_reading
 *   human_readable: Australia's Westminster-Federal Hybrid: 1901 Bargain Sustaining Doctrinal Tension
 *   domain: legal/constitutional/federalism
 *
 * SUMMARY:
 *   Australia's 1901 Constitution deliberately fused two incompatible parent
 *   logics: Westminster parliamentary sovereignty (cabinet government
 *   answering to the elected lower house) and American federalism
 *   (equal-state Senate, division of powers, judicial review of federalism
 *   boundaries). The bargain sustained because each parent model suppresses
 *   the other's most dangerous vetoes: Westminster majoritarianism is
 *   constrained by federalism's equal-state Senate; American federalism is
 *   constrained by Westminster's unified executive accountability. The design
 *   is neither contradiction nor synthesis — it is structured equilibrium
 *   maintained by mutual suppression. The 1975 dismissal detonated the
 *   hybrid's fault line: a Senate blocking supply and a Governor-General
 *   dismissing an elected government exposed that the two parent systems
 *   disagreed on who could invoke the deepest reserve powers. The constraint
 *   has since operated with raised temperature around the reserve-power
 *   mechanism but without formal resolution or collapse. The theater_ratio
 *   rose from 1901 (functional hybridity) through 1975 (crisis performance)
 *   and has stabilized at moderate levels as institutional actors learned to
 *   navigate around the fault line. Extractiveness peaked at the 1975 crisis
 *   moment (0.62) when doctrinal incompatibility became visible, and has
 *   settled at 0.52 as the hybrid has developed informal norms suppressing
 *   further resort to reserve powers. Suppression (enforcement of the
 *   veto-suppression on both parent models) rose through the mid-20th century
 *   and spiked at 1975, then moderately declined as conventions stabilized
 *   around which reserve powers remain available and which are effectively
 *   dormant.
 *
 * KEY AGENTS:
 *   - Federation constitutional stability: Beneficiary (institutional/arbitrage) — the hybrid design itself, seen as a bounded equilibrium. Benefits from both parent models' veto suppression because it prevents either from dominating.
 *   - Doctrinal coherence: Victim (powerless/trapped) — both Westminster and federalism doctrines are systematically suppressed; cannot exit without abandoning the Constitution.
 *   - Federal state governments: Organized victims and beneficiaries (organized/constrained) — benefit from equal Senate representation (federalism coordination), constrained by lower-house cabinet dominance (Westminster suppression of federalist logic).
 *   - Lower-house majority governments: Primary beneficiary (institutional/arbitrage) — form the cabinet, answer to lower house, experience the hybrid as coordination of their power through federal limits.
 *   - Senate: Institutional actor with constrained exit (institutional/constrained) — genuinely coordinates federal representation but systematically excluded from executive formation (post-1975 norm).
 *   - Judicial review system: Institutional piton (institutional/constrained) — maintains formal American-style review but defers massively to parliamentary sovereignty (Westminster logic).
 *   - Analytical observer: Sees the hybrid as a stable equilibrium (analytical/analytical) — recognizes the mutual-suppression mechanism and interprets the constraint as tangled_rope (genuine coordination + genuine extraction).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(australian_federation_1901__washminster_hybrid_reading, 0.52).
domain_priors:suppression_score(australian_federation_1901__washminster_hybrid_reading, 0.58).
domain_priors:theater_ratio(australian_federation_1901__washminster_hybrid_reading, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(australian_federation_1901__washminster_hybrid_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(australian_federation_1901__washminster_hybrid_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(australian_federation_1901__washminster_hybrid_reading, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(australian_federation_1901__washminster_hybrid_reading, tangled_rope).
narrative_ontology:human_readable(australian_federation_1901__washminster_hybrid_reading, "Australia's Westminster-Federal Hybrid: 1901 Bargain Sustaining Doctrinal Tension").
narrative_ontology:topic_domain(australian_federation_1901__washminster_hybrid_reading, "legal/constitutional/federalism").

domain_priors:requires_active_enforcement(australian_federation_1901__washminster_hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(australian_federation_1901__washminster_hybrid_reading, '64d46177-88b6-40d0-ad81-5c3d145faa31').
narrative_ontology:cs_kernel_codification('64d46177-88b6-40d0-ad81-5c3d145faa31', formalized).
narrative_ontology:cs_authority_grounding('64d46177-88b6-40d0-ad81-5c3d145faa31', extraction).
narrative_ontology:cs_interpretation_layer_present('64d46177-88b6-40d0-ad81-5c3d145faa31').
narrative_ontology:cs_reading_relation('64d46177-88b6-40d0-ad81-5c3d145faa31', australian_federation_1901__dismissal_1975_reading, influences).
narrative_ontology:cs_reading_relation('64d46177-88b6-40d0-ad81-5c3d145faa31', australian_federation_1901__referendum_rigidity_reading, coexists_with).
narrative_ontology:cs_axiom('64d46177-88b6-40d0-ad81-5c3d145faa31', foundational, mutual_veto_suppression_stabilizes_hybrid).
narrative_ontology:cs_axiom_status(mutual_veto_suppression_stabilizes_hybrid, holdable).
narrative_ontology:cs_axiom_grounding('64d46177-88b6-40d0-ad81-5c3d145faa31', mutual_veto_suppression_stabilizes_hybrid, instrumental).
narrative_ontology:cs_axiom('64d46177-88b6-40d0-ad81-5c3d145faa31', foundational, doctrinal_purity_is_extraction_cost).
narrative_ontology:cs_axiom_status(doctrinal_purity_is_extraction_cost, holdable).
narrative_ontology:cs_axiom_grounding('64d46177-88b6-40d0-ad81-5c3d145faa31', doctrinal_purity_is_extraction_cost, deontological).
narrative_ontology:cs_reference_frame('64d46177-88b6-40d0-ad81-5c3d145faa31', federated_parliamentary_equilibrium).
narrative_ontology:cs_drift_state('64d46177-88b6-40d0-ad81-5c3d145faa31', contemporary_2000s, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('64d46177-88b6-40d0-ad81-5c3d145faa31', '').
narrative_ontology:cs_kernel_id(australian_federation_1901__washminster_hybrid_reading, australian_federation_1901).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(australian_federation_1901__washminster_hybrid_reading, federation_constitutional_stability).
narrative_ontology:constraint_victim(australian_federation_1901__washminster_hybrid_reading, doctrinal_coherence).
narrative_ontology:constraint_victim(australian_federation_1901__washminster_hybrid_reading, single_parent_model_adherence).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DOCTRINAL PURITY ADVOCATES (SNARE) — Those committed to Westminster coherence or American federalism clarity are trapped. The hybrid design systematically suppresses both parent logics' internal consistency. A Westminster purist cannot exit without abandoning Australia's design; a federalist cannot exit without abandoning Senate equality. The constraint extracts doctrinal coherence as the price of constitutional stability. Powerless because the hybrid is locked in place; trapped because abandoning it requires rewriting the constitution.
constraint_indexing:constraint_classification(australian_federation_1901__washminster_hybrid_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FEDERAL STATE GOVERNMENTS (TANGLED ROPE) — Benefit from equal Senate representation (federalism coordination) while constrained by cabinet government answering to lower-house majorities (Westminster limitation on state power). The constraint coordinates federal stability AND extracts power upward to federal majorities. Not maximal extraction (states retain constitutional spheres) but significant — Senate equality is suppressed by lower-house dominance of executive formation.
constraint_indexing:constraint_classification(australian_federation_1901__washminster_hybrid_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FEDERAL EXECUTIVE (ROPE) — The lower-house majority that forms government experiences the hybrid as pure coordination: cabinet government delegates executive power to a body answering to the elected chamber; the federal frame provides scope and Senate restraint prevents pure majoritarianism. This agent sees coordination value in both parent logics simultaneously. Net beneficiary — the hybrid distributes power to the majority while the federalism frame provides institutional theater of restraint.
constraint_indexing:constraint_classification(australian_federation_1901__washminster_hybrid_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: SENATE AS INSTITUTION (TANGLED ROPE) — The Senate coordinates federal representation (two senators per state, genuinely limits lower-house monopoly) while systematically constrained by cabinet government logic. Senate cannot initiate supply or dismiss cabinets without triggering constitutional crisis (1975 precedent). Institutional gain (representation) mixed with extraction (powerlessness over executive formation in normal operation).
constraint_indexing:constraint_classification(australian_federation_1901__washminster_hybrid_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: JUDICIAL REVIEW APPARATUS (PITON) — Courts apply American judicial review doctrine over a Westminster system with parliamentary sovereignty tradition. The judicial role is substantially performative — courts review laws for federal validity but defer massively to parliamentary intention and executive action. High theater (separate power, formal review procedures) masking low structural check on elected branches. The review system persists through institutional inertia and legitimacy theater rather than as a functional veto.
constraint_indexing:constraint_classification(australian_federation_1901__washminster_hybrid_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / HYBRID STABILITY VIEW (TANGLED ROPE) — From a civilizational perspective, the design successfully coordinates two parent logics by suppressing each one's vetoes on the other. Westminster cabinet government is constrained by federalism; American federalism is constrained by Westminster majoritarianism. This mutual suppression is the bargain's enabling condition. The hybrid is neither pyramid nor contradiction — it is a structural equilibrium where each parent model limits the other's worst-case outcomes. This perspective sees the extraction (doctrinal purity sacrificed) as the functional cost of maintaining both coordination functions simultaneously.
constraint_indexing:constraint_classification(australian_federation_1901__washminster_hybrid_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(australian_federation_1901__washminster_hybrid_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(australian_federation_1901__washminster_hybrid_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(australian_federation_1901__washminster_hybrid_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(australian_federation_1901__washminster_hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(australian_federation_1901__washminster_hybrid_reading, TR),
    TR >= 0.70.

:- end_tests(australian_federation_1901__washminster_hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The hybrid extracts doctrinal coherence from both parent models. Westminster is suppressed by federalism (Senate equality limits lower-house majoritarianism); federalism is suppressed by Westminster (cabinet government makes federal states subordinate to federal executives answerable to lower-house majorities). The extraction is not minimal (doctrinal purity is genuinely sacrificed) but not maximal (both parent models retain functional space). The measurement trajectory shows how extractiveness rose as the Constitution aged and institutional actors tested boundaries (rising to 0.62 by 1975), peaked during the crisis when reserve powers were invoked, and then moderated to 0.52 as conventions developed that made some reserve powers effectively dormant. Suppression (0.58): Moderate-high. Both parent logics' veto capacities are suppressed. A Westminster purist cannot invoke simple parliamentary sovereignty to override federalism; a federalist cannot invoke pure federalism to override lower-house executive dominance. The suppression is enforced through constitutional convention rather than pure legal prohibition — the Constitution's text permits reserve-power invocation, but 1975 and post-1975 norms have established informal operating boundaries. Theater ratio (0.64): Moderate-high. The system maintains institutional theater: courts apply American-style judicial review despite Westminster sovereignty traditions; the Senate performs federal restraint despite lower-house dominance; the Governor-General maintains formal reserve powers that are rarely invoked. The theater reflects the hybrid's mode of operation — institutional forms borrowed from both parent systems, many of them performative rather than functionally decisive. The rise in theater_ratio from 1901 (0.45) to 1975 (0.68) reflects the increasing gap between formal institutional structure and actual decision-making power; the decline to 0.64 by 2000 reflects stabilization of conventions that restored some functional coherence to the theater.
 *
 * PERSPECTIVAL GAP:
 *   The hybrid produces maximum perspectival divergence. Westminster doctrinal purists see a snare (their logic is suppressed, coherence is extracted); federalism purists see the same snare (their logic is equally suppressed). Federal state governments see tangled_rope (they benefit from Senate equality but are constrained by cabinet dominance). The lower-house majority government sees rope (the hybrid coordinates their power). The Senate sees tangled_rope (genuinely represents states but is excluded from executive formation). The judicial review system appears to itself as piton (maintains form without decisive function). The analytical observer at the civilizational level sees tangled_rope (both parent models are genuinely required, their mutual suppression is the enabling condition, and the constraint is stable but not frictionless). The gaps reveal that the hybrid is not a unified constraint but rather two parent constraints held in permanent mutual suppression — collapse occurs if either parent model's veto capacity is reinvoked (1975 nearly triggered this).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality differs sharply between beneficiary and victims. The federation constitutional stability (beneficiary, institutional power, arbitrage exit) experiences low d — the hybrid benefits them by suppressing both parent models' most dangerous powers. The doctrinal purity victim (powerless, trapped) experiences maximum d — cannot exit without rewriting the Constitution, bears full cost of incoherence. Federal states experience mid-d (organized power, constrained exit) — they benefit from Senate equality but lose federalism veto on executive formation. The lower-house government experiences low d (institutional, arbitrage) — the constraint distributes power to them while the federal frame provides restraint. The Senate experiences mid-d (institutional, constrained) — genuine federal coordination benefit mixed with exclusion from executive formation. The analytical observer applies d ≈ 0.72 (canonical for analytical context) — sees the full structure. The chi formula applies the sigmoid f(d) to these d values: beneficiary with low d → negative or near-zero chi (no extraction experienced); victims with high d → high chi (extraction keenly experienced); organized actors with mid-d → moderate chi (mixed experience). Scope modifier σ(S) applies national scope (σ=1.0), so chi = ε × f(d) × 1.0.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that the hybrid's classification as tangled_rope (from the analytical observer's civilizational perspective) is not a compromise but a structural necessity. Both parent logics are genuinely required — abandon one and the other's worst-case outcomes become possible. Westminster alone risks federalism's obliteration; American federalism alone risks majoritarianism without restraint. The hybrid is tangled_rope not because it fails to be either rope or snare, but because it is structurally both: it coordinates two incompatible parent models (rope function) by enforcing mutual veto suppression (snare function). The mandatrophy dissolves when you recognize that the question 'is this coordination or extraction?' has the answer: 'yes, structurally.' The 1975 crisis tested whether the suppression mechanism would hold (it did, through post-crisis norms); the constitutional amendment rigidity (separate under the referendum_rigidity_reading) tests whether the federation bargain can be modified without invoking one parent model's veto. The three sibling readings decompose this single constraint family across three structural properties: this reading addresses hybrid stability/mutual suppression; dismissal_1975_reading addresses reserve-power invocation and fault-line activation; referendum_rigidity_reading addresses amendment rigidity as a secondary effect of hybrid structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    id_1975_precedent_amplitude,
    'Does the 1975 dismissal establish a reserve power reserve, or did it destabilize the hybrid''s implicit operating norms permanently?',
    'Post-1975 institutional behavior: has the Senate re-tested supply blocking? Have Governors-General articulated dismissal criteria? Frequency of reserve power invocation vs. expected norms.',
    'If reserve powers remain stable/dormant: the hybrid contains the fault line (Tangled Rope taxonomy holds). If reserve powers become normalized/weaponized: the hybrid is degrading toward instability (Snare or Scaffold taxonomy, depending on whether institutional actors can coordinate around limits).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(id_1975_precedent_amplitude, empirical, 'Whether 1975 destabilized the hybrid or established stable restraint norms').

omega_variable(
    doctrinal_purity_loss_measurement,
    'What is the magnitude of doctrinal incoherence the hybrid imposes? Is the suppression of Westminster clarity vs. federalist clarity symmetrical?',
    'Analysis of judicial review doctrine (American-derived vs. Westminster deference patterns); analysis of Senate power in practice (veto frequency, blocking patterns, legislative timing). Comparison of constitutional text gaps vs. convention fill.',
    'If asymmetrical (one parent model suppressed more than other): the hybrid is not a true equilibrium but a masked dominance (one parent colonizes the other). If roughly symmetrical: the tangled_rope classification holds — both parents are genuinely constrained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_purity_loss_measurement, empirical, 'Degree of symmetry in doctrinal suppression between parent logics').

omega_variable(
    referendum_amendment_ceiling,
    'Is the double-majority referendum gate (Section 128) an intrinsic property of Australian constitutionalism, or does it reflect the hybrid''s resistance to revision because revision requires consensus across constituencies whose interests the hybrid divides?',
    'Comparative constitutional amendment rates (other federal systems, other Westminster systems); analysis of failed referenda (do they cluster around hybrid-integrity questions — Senate powers, executive-Senate balance — or distribute across policy domains?). Survey of elite constitutional intention around amendment.',
    'If intrinsic: rigidity is a primary constraint (separate story under referendum_rigidity_reading). If structural to the hybrid: rigidity is a secondary effect of hybrid stability — the hybrid works by suppressing each parent model''s revision, making amendment nearly impossible because no amendment can satisfy both. This omega is resolved in both directions by the sibling reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(referendum_amendment_ceiling, conceptual, 'Whether constitutional rigidity is intrinsic or a structural consequence of the hybrid').

omega_variable(
    reserve_power_codification_gap,
    'Should the Governor-General''s reserve powers (dismissal, appointment, dissolution) be formally codified, or does the hybrid rely on their ambiguity as a stabilizing feature?',
    'Comparison with Commonwealth realms that have formalized reserve powers; analysis of constitutional convention stability (does ambiguity increase or decrease compliance with norms?); elite interviews with constitutional actors about their understanding of dismissal/appointment criteria.',
    'If codification would stabilize: the hybrid is underdetermined and relies on convention fragility (risk: another 1975). If codification would destabilize: the hybrid''s stability depends on leaving worst-case powers ambiguous (structural feature, not bug). This is a preference-class omega — the resolution depends on whether actors prefer explicit limits or stabilizing ambiguity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reserve_power_codification_gap, preference, 'Whether reserve powers should be codified or remain conventional').

omega_variable(
    cabinet_accountability_westminster_purity,
    'Does Australia''s cabinet system genuinely instantiate Westminster accountability (ministers responsible to parliament, dismissible via supply votes), or is the lower-house domination so strong that Westminster accountability is theater masking executive dominance?',
    'Analysis of ministerial resignations (forced vs. voluntary); analysis of legislative defeats on major executive initiatives; frequency of government defeats in lower house; comparative data on parliamentary checks on executives in other Westminster systems.',
    'If genuine accountability: Westminster coordination function is real (tangled_rope classification holds). If theater: the Westminster frame is piton-like degradation (the system appears to have Westminster restraint but functionally the majority controls both legislative and executive branches).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cabinet_accountability_westminster_purity, empirical, 'Whether Westminster accountability mechanisms actually constrain executive power').

omega_variable(
    federalism_equal_state_representation_functionality,
    'Does Senate equal-state representation actually function as a federalism coordinate that restrains lower-house majorities, or is it systematically suppressed by party discipline and lower-house executive dominance?',
    'Analysis of Senate voting patterns (party discipline vs. state interest alignment); frequency of Senate amendments to lower-house legislation; analysis of Senate institutional power in budget formation and treaty ratification; comparison with other federal systems'' upper chambers.',
    'If functional federalism coordinate: the federal parent model contributes genuine coordination (tangled_rope, hybrid stability real). If systematically suppressed: federalism is piton-like (institutional form without functional content).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(federalism_equal_state_representation_functionality, empirical, 'Whether Senate equal representation functions as a federalism restraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(australian_federation_1901__washminster_hybrid_reading, 1901, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aushy_tr_t1901, australian_federation_1901__washminster_hybrid_reading, theater_ratio, 1901, 0.45).
narrative_ontology:measurement(aushy_tr_t1950, australian_federation_1901__washminster_hybrid_reading, theater_ratio, 1950, 0.58).
narrative_ontology:measurement(aushy_tr_t1975, australian_federation_1901__washminster_hybrid_reading, theater_ratio, 1975, 0.68).
narrative_ontology:measurement(aushy_tr_t2000, australian_federation_1901__washminster_hybrid_reading, theater_ratio, 2000, 0.64).

% Extraction over time
narrative_ontology:measurement(aushy_be_t1901, australian_federation_1901__washminster_hybrid_reading, base_extractiveness, 1901, 0.35).
narrative_ontology:measurement(aushy_be_t1950, australian_federation_1901__washminster_hybrid_reading, base_extractiveness, 1950, 0.48).
narrative_ontology:measurement(aushy_be_t1975, australian_federation_1901__washminster_hybrid_reading, base_extractiveness, 1975, 0.62).
narrative_ontology:measurement(aushy_be_t2000, australian_federation_1901__washminster_hybrid_reading, base_extractiveness, 2000, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(aushy_su_t1901, australian_federation_1901__washminster_hybrid_reading, suppression_requirement, 1901, 0.4).
narrative_ontology:measurement(aushy_su_t1950, australian_federation_1901__washminster_hybrid_reading, suppression_requirement, 1950, 0.52).
narrative_ontology:measurement(aushy_su_t1975, australian_federation_1901__washminster_hybrid_reading, suppression_requirement, 1975, 0.72).
narrative_ontology:measurement(aushy_su_t2000, australian_federation_1901__washminster_hybrid_reading, suppression_requirement, 2000, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(australian_federation_1901__washminster_hybrid_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(australian_federation_1901__washminster_hybrid_reading, australian_federation_1901__dismissal_1975_reading).
narrative_ontology:affects_constraint(australian_federation_1901__washminster_hybrid_reading, australian_federation_1901__referendum_rigidity_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of the australian_federation_1901 kernel decomposition. The kernel admits three structurally distinct readings with different ε values and different primary mechanisms. washminster_hybrid_reading (this constraint, ε=0.52) addresses the functional equilibrium maintained by mutual veto suppression. dismissal_1975_reading (ε≈0.68) addresses reserve-power invocation and the hybrid's fault line. referendum_rigidity_reading (ε≈0.45) addresses constitutional amendment rigidity as a structural property. These three stories link bidirectionally via network.affects_constraints because the hybrid stability (this reading) explains why amendment is rigid (second sibling) and why 1975 was a crisis (first sibling). Each story gets its own ε, its own beneficiary/victim set, and its own metrics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(australian_federation_1901__washminster_hybrid_reading, organized, 0.55).
constraint_indexing:directionality_override(australian_federation_1901__washminster_hybrid_reading, institutional, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
