% ============================================================================
% CONSTRAINT STORY: bill_of_rights_1791__reserved_powers_amendments
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bill_of_rights_1791__reserved_powers_amendments, []).

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
 *   constraint_id: bill_of_rights_1791__reserved_powers_amendments
 *   human_readable: Reserved Powers and Unenumerated Rights (Ninth and Tenth Amendments)
 *   domain: political/legal/constitutional_interpretation
 *
 * SUMMARY:
 *   The Ninth and Tenth Amendments stand as the Bill of Rights' residuary
 *   clauses, reserving to the people and states the rights and powers not
 *   enumerated in the Constitution. This reading of the contested kernel
 *   'Bill of Rights 1791' presents the reserved powers doctrine as the
 *   fundamental constraint binding federal authority — a coordinate principle
 *   with the explicit procedural protections (Fourth through Eighth
 *   Amendments) and expressive protections (First Amendment) that dominate
 *   contemporary doctrine. The structural claim is that the enumeration of
 *   specific rights and federal powers is not exhaustive but exemplary; the
 *   Ninth and Tenth Amendments supply the doctrinal apparatus to prevent the
 *   enumerated powers from absorbing all legitimate governance authority into
 *   federal hands. Over 235 years, this reading has experienced erosion in
 *   enforceability alongside rising judicial theater (the doctrine is invoked
 *   but rarely constrains federal action), and suppression has increased as
 *   federal interpretation of enumerated powers (Commerce Clause, Spending
 *   Clause, Necessary and Proper Clause) has expanded to cover domains
 *   historically understood as reserved to the states and people.
 *
 * KEY AGENTS:
 *   - Individual Rights Claimants: Persons asserting unenumerated rights (privacy, movement, bodily autonomy); structurally powerless, trapped within federal jurisdiction, bearing full cost of establishing novel rights; victim of the constraint's suppression
 *   - State Governments: Beneficiaries of the reserved powers doctrine in principle but constrained in practice by federal supremacy and broad interpretation of enumerated powers; experience mixed coordination (doctrine protects nominal sphere) and extraction (sphere continuously eroded)
 *   - Federal Government (Living Constitution Coalition): Institutional beneficiary; experiences the reserved powers clauses as flexibility mechanism enabling implied powers and doctrinal evolution; arbitrage exit available through reinterpretation of enumerated powers
 *   - Federal Judiciary: Enforcer of the doctrine whose enforcement has degraded into performative review; maintains doctrinal structure while deferring to federal action; piton perspective
 *   - Federalism Revival Coalition: Organized actors (state attorneys general, originalist scholars, conservative judges) attempting to revive enforcement of the Tenth Amendment and limit federal implied powers; scaffold perspective treating reserved powers as a temporary failure amenable to doctrinal correction
 *   - Federal Power Maximalists: Institutional actors (federal agencies, enforcement bureaucracies, regulatory coalitions) whose interests lie in expansive interpretation of federal authority; victims of the reading if enforced, but protected by current doctrine's atrophy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bill_of_rights_1791__reserved_powers_amendments, 0.48).
domain_priors:suppression_score(bill_of_rights_1791__reserved_powers_amendments, 0.62).
domain_priors:theater_ratio(bill_of_rights_1791__reserved_powers_amendments, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bill_of_rights_1791__reserved_powers_amendments, extractiveness, 0.48).
narrative_ontology:constraint_metric(bill_of_rights_1791__reserved_powers_amendments, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(bill_of_rights_1791__reserved_powers_amendments, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bill_of_rights_1791__reserved_powers_amendments, tangled_rope).
narrative_ontology:human_readable(bill_of_rights_1791__reserved_powers_amendments, "Reserved Powers and Unenumerated Rights (Ninth and Tenth Amendments)").
narrative_ontology:topic_domain(bill_of_rights_1791__reserved_powers_amendments, "political/legal/constitutional_interpretation").

domain_priors:requires_active_enforcement(bill_of_rights_1791__reserved_powers_amendments).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bill_of_rights_1791__reserved_powers_amendments, '1d7653c4-9a8e-45bb-80f7-7e79e8a899d6').
narrative_ontology:cs_kernel_codification('1d7653c4-9a8e-45bb-80f7-7e79e8a899d6', formalized).
narrative_ontology:cs_authority_grounding('1d7653c4-9a8e-45bb-80f7-7e79e8a899d6', lineage).
narrative_ontology:cs_interpretation_layer_present('1d7653c4-9a8e-45bb-80f7-7e79e8a899d6').
narrative_ontology:cs_reading_relation('1d7653c4-9a8e-45bb-80f7-7e79e8a899d6', bill_of_rights_1791__criminal_procedure_amendments, coexists_with).
narrative_ontology:cs_reading_relation('1d7653c4-9a8e-45bb-80f7-7e79e8a899d6', bill_of_rights_1791__expression_conscience_amendments, coexists_with).
narrative_ontology:cs_reading_relation('1d7653c4-9a8e-45bb-80f7-7e79e8a899d6', bill_of_rights_1791__security_arms_amendments, coexists_with).
narrative_ontology:cs_axiom('1d7653c4-9a8e-45bb-80f7-7e79e8a899d6', foundational, enumeration_is_exemplary_not_exhaustive).
narrative_ontology:cs_axiom_status(enumeration_is_exemplary_not_exhaustive, holdable).
narrative_ontology:cs_axiom_grounding('1d7653c4-9a8e-45bb-80f7-7e79e8a899d6', enumeration_is_exemplary_not_exhaustive, deontological).
narrative_ontology:cs_axiom('1d7653c4-9a8e-45bb-80f7-7e79e8a899d6', foundational, federalism_as_enforceable_structural_limit).
narrative_ontology:cs_axiom_status(federalism_as_enforceable_structural_limit, holdable).
narrative_ontology:cs_axiom_grounding('1d7653c4-9a8e-45bb-80f7-7e79e8a899d6', federalism_as_enforceable_structural_limit, deontological).
narrative_ontology:cs_reference_frame('1d7653c4-9a8e-45bb-80f7-7e79e8a899d6', original_federalist_enumeration_doctrine).
narrative_ontology:cs_drift_state('1d7653c4-9a8e-45bb-80f7-7e79e8a899d6', contemporary_2024, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('1d7653c4-9a8e-45bb-80f7-7e79e8a899d6', '2026-02-26T18:45:00Z').
narrative_ontology:cs_kernel_id(bill_of_rights_1791__reserved_powers_amendments, bill_of_rights_1791).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bill_of_rights_1791__reserved_powers_amendments, rights_claimants).
narrative_ontology:constraint_beneficiary(bill_of_rights_1791__reserved_powers_amendments, state_governments).
narrative_ontology:constraint_beneficiary(bill_of_rights_1791__reserved_powers_amendments, federalist_limiting_coalition).
narrative_ontology:constraint_victim(bill_of_rights_1791__reserved_powers_amendments, federal_power_maximalists).
narrative_ontology:constraint_victim(bill_of_rights_1791__reserved_powers_amendments, implied_powers_doctrine).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL RIGHTS CLAIMANT (SNARE) — A person asserting a right not explicitly enumerated in the Constitution (e.g., privacy, bodily autonomy, movement) faces extraction through jurisdictional suppression. Federal courts treat unenumerated rights as suspect, requiring heavy doctrinal lifting (substantive due process, penumbral reasoning) to gain recognition. The trapped exit reflects that no real alternative venue exists for defending novel rights claims; state courts may be hostile or inconsistent. Maximum experienced extraction — the claimant bears full burden of judicially establishing legitimacy for rights the Ninth Amendment nominally reserves to them.
constraint_indexing:constraint_classification(bill_of_rights_1791__reserved_powers_amendments, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: STATE GOVERNMENT / FEDERALIST COALITION (TANGLED ROPE) — States benefit from the reserved powers doctrine: the Tenth Amendment nominally protects their regulatory authority from federal preemption. But the coordination function is real — states need assurance that federal commerce power won't consume all legislative space, and the reserved powers clause provides that assurance. Yet extraction is embedded: states remain subordinate to the Supremacy Clause, and judicial interpretation of enumerated federal powers (commerce, spending, enforcement) continuously erodes state regulatory space. States experience mixed coordination (genuine protection of sphere of action) and extraction (asymmetric shrinkage of that sphere over time). Exit constrained by constitutional position and resource dependence on federal spending.
constraint_indexing:constraint_classification(bill_of_rights_1791__reserved_powers_amendments, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FEDERAL GOVERNMENT / LIVING CONSTITUTION COALITION (ROPE) — From the federal maximalist perspective, the Ninth and Tenth Amendments are coordination mechanisms, not extraction traps. They enable flexibility: the enumerated powers doctrine remains formally intact (coordination function), but implicit and evolving powers can be derived without amending the text (arbitrage function). Federal actors experience the reserved powers clauses as pure coordination — they preserve the appearance of constitutional constraint while enabling policy adaptation. No extraction is perceived; the mechanism is seen as enabling efficient governance. Arbitrage exit available: federal actors can reinterpret enumerated powers to cover new domains (New Deal Commerce Clause expansion) without formally violating the reserved powers principle.
constraint_indexing:constraint_classification(bill_of_rights_1791__reserved_powers_amendments, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: JUDICIAL ENFORCEMENT APPARATUS (PITON) — The federal judiciary's treatment of the Ninth and Tenth Amendments has degraded into largely performative review. Courts invoke the reserved powers principle while consistently deferring to federal action under broad interpretations of enumerated powers (Commerce Clause, Spending Clause, Necessary and Proper Clause). The Ninth Amendment is almost never the basis for striking down federal legislation; the Tenth is invoked primarily in symbolic dissents. The mechanism persists through institutional inertia — judges maintain the doctrinal structure because it has been canonized, not because it functions as originally intended. Theater ratio (0.58) reflects the gap between the formal doctrine (reserves powers to the states and people) and its actual enforcement (no real doctrinal force). The piton classification captures that the apparatus performs its constraint-recognition role while its constraining power has atrophied.
constraint_indexing:constraint_classification(bill_of_rights_1791__reserved_powers_amendments, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: STATE RIGHTS MOVEMENT / FEDERALISM REVIVAL COALITION (SCAFFOLD) — The New Federalism movement (1990s–present) treats the reserved powers doctrine as a temporary failure needing correction through renewed enforcement. Organized actors (conservative legal scholars, state governments, originalist judges) see the Ninth and Tenth Amendments as having a sunset clause: if judicial doctrine can be reformed to take the reserved powers seriously (via the Lopez/Morrison line of Commerce Clause limits, or via renewed 11th Amendment enforcement), the extraction mechanism dissolves. Suppression remains high (federal supremacy is entrenched), but the organized coalition perceives actionable exit paths — constitutional interpretation reform, strategic litigation, state coordinated action. Theater is moderate because the revival is genuinely attempting to alter doctrinal force, not merely performing compliance.
constraint_indexing:constraint_classification(bill_of_rights_1791__reserved_powers_amendments, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW FEDERALISM VIEW (MOUNTAIN) — From a civilizational perspective, the reserved powers principle is framed as a natural law of federalist systems: any distributed authority structure must have a residuary clause reserving unlisted powers to the lower tier (states/people). The Ninth and Tenth Amendments appear as immutable logical consequences of federalism itself — they could not be otherwise without abandoning the federal structure. This perspective naturalizes what is actually a contested interpretive choice. The engine's false summit detector will identify this as a false summit: the structural data shows identifiable beneficiaries (federalists, rights claimants, state governments) whose interests depend on enforcing the reserved powers doctrine. Natural law framing obscures the extractive struggle between those benefiting from federal supremacy and those resisting it.
constraint_indexing:constraint_classification(bill_of_rights_1791__reserved_powers_amendments, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bill_of_rights_1791__reserved_powers_amendments_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bill_of_rights_1791__reserved_powers_amendments, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bill_of_rights_1791__reserved_powers_amendments, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(bill_of_rights_1791__reserved_powers_amendments, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(bill_of_rights_1791__reserved_powers_amendments, TR),
    TR >= 0.70.

:- end_tests(bill_of_rights_1791__reserved_powers_amendments_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high, increasing over time. At ratification (0.35), the reserved powers doctrine was freshly articulated and had some doctrinal salience. By the Reconstruction era (1876, value 0.48), federal expansion via the Fourteenth Amendment and expanded Commerce/Spending Clauses had begun eroding state sovereignty, raising the extraction cost for rights-claimants and states defending their reserved sphere. By the post-New Deal era (1941, value 0.55), the extraction has intensified — the doctrine is invoked in theory but has minimal judicial enforceability, yet it continues to frame the debate. Suppression (0.62): High and rising. Federal supremacy is entrenched through doctrinal deference, resource concentration in federal agencies, and the Supremacy Clause's clear priority. Alternatives to federal governance are suppressed through practical barriers (cost of federal litigation, jurisdictional structure) and doctrinal barriers (rational basis review of state laws, broad construction of federal enumerated powers). Theater ratio (0.58): Rising over time. At ratification, the doctrine had real bite; federal courts treated federalism as a meaningful constraint. By the modern era, the doctrine is performed (invoked in opinions, taught in law schools) but lacks enforcement teeth. Courts cite the Ninth and Tenth Amendments while upholding federal action under increasingly broad interpretations of Commerce and Spending powers. The theater rise reflects the gap between doctrinal commitment and actual adjudication.
 *
 * PERSPECTIVAL GAP:
 *   The reserved powers reading generates one of the largest perspectival gaps in constitutional doctrine. A rights-claimant asserting an unenumerated right sees a snare — the constraint suppresses their ability to establish novel rights through judicial process. A federalist state government sees tangled rope — genuine coordination (the doctrine protects a nominal sphere of state authority) embedded in extraction (the sphere is continuously eroded). The federal government sees rope — pure coordination enabling flexible governance. The judiciary sees piton — a degraded doctrine it maintains through habit. The Federalism Revival coalition sees scaffold — a temporary doctrinal failure amenable to correction via renewed interpretation. The analytical observer risks seeing mountain — naturalizing federalism as an immutable principle. This perspectival distribution reveals that the reserved powers reading's classification cannot be uniform across observers; the gap itself is diagnostic. The claimed_type (tangled_rope) represents the dominant structural reality: mixed coordination (the doctrine does enable a nominal federal/state divide) and extraction (federal expansion suppresses state and individual reserved powers). But this classification would be rejected by federal maximalists (rope perspective) and rights-claimants (snare perspective) — the gap between perspectives is the constraint's defining feature.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's experienced extractiveness (chi) is derived from base extractiveness (0.48), their power level, exit options, and the scope of the constraint (national). Federal Government (institutional/arbitrage): d ≈ 0.05 (full beneficiary with escape option); f(d) ≈ -0.12; χ ≈ 0.48 × -0.12 × 1.0 ≈ -0.06 (negative extraction — benefit experienced). Rights-Claimants (powerless/trapped): d ≈ 0.95 (full victim with no escape); f(d) ≈ 1.42; χ ≈ 0.48 × 1.42 × 1.0 ≈ 0.68 (high extraction). State Governments (organized/constrained, federalist fraction): d ≈ 0.40 (mixed beneficiary/victim); f(d) ≈ 0.40; χ ≈ 0.48 × 0.40 × 1.0 ≈ 0.19 (moderate extraction). Federal Judiciary (institutional/arbitrage, by doctrinal role): d ≈ 0.15 (nominal enforcer, actual deferrer); f(d) ≈ -0.01; χ ≈ 0.48 × -0.01 × 1.0 ≈ -0.005 (near-zero extraction). The directionality heterogeneity reflects the reading's core tension: the doctrine is nominally constraining but actually enabling federal expansion.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exemplifies the mandatrophy at the constitutional level. The reserved powers reading claims to resolve the indeterminacy of unenumerated rights and federal limits by providing a residuary principle — what's not enumerated belongs to the people/states. But this resolution is itself disputed: federal maximalists read the enumeration as enabling implied powers that subsume the residuum; federalism revivalists read the enumeration as merely exemplary and the residuum as binding. The mandatrophy is not 'which type is correct?' but 'which interpretive tradition controls doctrine?' Because the kernel contest (Bill of Rights 1791) remains unresolved, the reserved powers reading cannot achieve mandatrophy closure. The structure is tangled rope (mixed coordination and extraction) from the systemic view, but this classification is unstable — if the living constitution reading (federal maximalist rope perspective) captures doctrine permanently, the reserved powers reading degrades into piton (performative). If the federalism revival (scaffold perspective) succeeds in renewing enforcement, the reading clarifies into rope (genuine coordination via doctrinal constraint). The permanence of the constraint's type depends on which reading remains institutionally dominant.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enumeration_as_limit_or_exemplar,
    'Is the enumeration of specific rights (First through Eighth Amendments) meant to establish the LIMIT of protected rights, or merely EXEMPLARY instances of the broad reservoir protected by the Ninth Amendment?',
    'Historical analysis of Framers'' intent via ratification debates, Federalist Papers, state convention records; comparison with parallel reserved-powers language in Articles of Confederation and state constitutions',
    'If enumeration = limit: unenumerated rights have weak doctrinal footing, and federal expansion can justify itself by pointing to unstated powers. If enumeration = exemplar: the enumeration/Ninth Amendment duality is a genuine constraint on federal authority, and many contemporary rights (privacy, bodily autonomy, movement) are protected by the Ninth''s residuum. Classification shifts from Piton (performative) to Tangled Rope (genuinely mixed coordination/extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enumeration_as_limit_or_exemplar, empirical, 'Whether enumeration limits or exemplifies protected rights').

omega_variable(
    federalism_as_textual_or_structural_principle,
    'Is federalism a textual constraint binding the judiciary (the Tenth Amendment must be actively enforced to have meaning), or a structural principle that permits implied powers as necessary to federal function?',
    'Doctrinal analysis of judicial review standards: strict scrutiny with meaningful limits (enforceable federalism gate) vs rational basis review with deference (federalism as structural background not judicially enforceable)',
    'If textual: the Ninth and Tenth Amendments are enforceable constraints, and current doctrine is a breach. If structural: the Amendments are coordination devices enabling federal flexibility without creating judicially cognizable rights for states or individuals. The first reading supports the Federalism Revival reading (Scaffold); the second supports the Federal Maximalist reading (Rope as federal perspective).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(federalism_as_textual_or_structural_principle, conceptual, 'Whether federalism is textually binding or structurally enabling').

omega_variable(
    rights_unenumerated_identifiability,
    'Can courts identify which rights are reserved to the people via the Ninth Amendment without substituting judicial judgment for democratic process?',
    'Empirical test: comparison of Ninth Amendment jurisprudence outcomes with referendum/democratic outcomes on claimed rights; analysis of whether judicial identification of unenumerated rights correlates with independent democratic recognition or represents pure judicial substitution',
    'If courts can reliably identify unenumerated rights: the Ninth Amendment is enforceable and the rights-claimant perspective (Snare) is resolvable into Rope (genuine coordination between judiciary and rights-claimants). If courts cannot: the Ninth Amendment remains performative (Piton persists), and rights-claimants remain trapped (Snare). Current doctrine suggests the second outcome — courts avoid Ninth Amendment adjudication entirely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rights_unenumerated_identifiability, empirical, 'Whether courts can reliably identify unenumerated rights').

omega_variable(
    reading_kernel_ambiguity,
    'This constraint is one reading of the contested kernel ''Bill of Rights 1791'': the foundational dispute over which reading is correct (which Amendments are the CORE binding constraints on federal authority) remains unresolved in doctrine. Is the reserved-powers reading a genuine alternative to the criminal-procedure reading, or is it subordinate to whatever interpretive tradition has captured doctrine?',
    'Longitudinal doctrinal analysis: which reading has been institutionally dominant across eras? Does doctrine accept the reserved-powers reading as a co-equal constraint on federal action, or treat it as a supplemental principle subordinate to other Amendments'' explicit protections?',
    'If co-equal: the kernel is genuinely contested, and all four readings are structural alternatives with different extractiveness profiles. If subordinate: the reserved-powers reading is performative scaffolding around a dominant reading, and its autonomy as a constraint is illusory. This omega documents the committer-frame structural fact that this reading''s legitimacy is bound up with the kernel contest itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_ambiguity, conceptual, 'Whether reserved-powers reading is autonomous constraint or subordinate to dominant reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bill_of_rights_1791__reserved_powers_amendments, 0, 235).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bor_reserved_theater_1791, bill_of_rights_1791__reserved_powers_amendments, theater_ratio, 0, 0.25).
narrative_ontology:measurement(bor_reserved_theater_1876, bill_of_rights_1791__reserved_powers_amendments, theater_ratio, 85, 0.45).
narrative_ontology:measurement(bor_reserved_theater_1941, bill_of_rights_1791__reserved_powers_amendments, theater_ratio, 150, 0.58).

% Extraction over time
narrative_ontology:measurement(bor_reserved_extract_1791, bill_of_rights_1791__reserved_powers_amendments, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(bor_reserved_extract_1876, bill_of_rights_1791__reserved_powers_amendments, base_extractiveness, 85, 0.48).
narrative_ontology:measurement(bor_reserved_extract_1941, bill_of_rights_1791__reserved_powers_amendments, base_extractiveness, 150, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(bor_reserved_supp_1791, bill_of_rights_1791__reserved_powers_amendments, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(bor_reserved_supp_1876, bill_of_rights_1791__reserved_powers_amendments, suppression_requirement, 85, 0.6).
narrative_ontology:measurement(bor_reserved_supp_1941, bill_of_rights_1791__reserved_powers_amendments, suppression_requirement, 150, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bill_of_rights_1791__reserved_powers_amendments, enforcement_mechanism).
narrative_ontology:affects_constraint(bill_of_rights_1791__reserved_powers_amendments, bill_of_rights_1791__criminal_procedure_amendments).
narrative_ontology:affects_constraint(bill_of_rights_1791__reserved_powers_amendments, bill_of_rights_1791__expression_conscience_amendments).
narrative_ontology:affects_constraint(bill_of_rights_1791__reserved_powers_amendments, bill_of_rights_1791__security_arms_amendments).
narrative_ontology:affects_constraint(bill_of_rights_1791__reserved_powers_amendments, fourteenth_amendment_incorporation).
narrative_ontology:affects_constraint(bill_of_rights_1791__reserved_powers_amendments, commerce_clause_implied_powers).

% DUAL FORMULATION NOTE:
% The reserved powers reading is one of four competing readings of the Bill of Rights kernel. The kernel contest is about which amendments constitute the CORE binding constraint on federal authority. This reading argues that the Ninth and Tenth Amendments are coordinate with the explicit procedural and expressive protections, not subordinate. Each sibling reading (criminal procedure, expression/conscience, security/arms) can be evaluated independently as a distinct constraint with its own epsilon value. They are linked by network relationships reflecting the kernel's unified interpretive field. All four readings share the same period (1791–present) and domain (constitutional law). Their classification types may differ (e.g., criminal procedure reading might classify as Rope; reserved powers as Tangled Rope; security/arms as Piton due to modern atrophy), reflecting the different structural positions each reading occupies in contemporary doctrine.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bill_of_rights_1791__reserved_powers_amendments, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
