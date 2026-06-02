% ============================================================================
% CONSTRAINT STORY: equal_protection_clause__substantive_equality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_clause__substantive_equality_reading, []).

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
 *   constraint_id: equal_protection_clause__substantive_equality_reading
 *   human_readable: Equal Protection Clause: Substantive Equality Reading
 *   domain: constitutional_law/civil_rights
 *
 * SUMMARY:
 *   The substantive equality reading of the Equal Protection Clause
 *   constrains maintenance of structures that reproduce group subordination
 *   regardless of their facial neutrality. This constraint emerges from the
 *   civil rights movement's challenge to formal equality doctrine — the older
 *   reading that equal protection requires only that laws apply to all
 *   persons without explicit race, gender, or other protected-class
 *   classifications. The substantive equality reading shifts the evaluative
 *   frame: a facially neutral policy (e.g., residential lending criteria,
 *   school funding formulas, voter identification requirements) can violate
 *   equal protection if it produces disparate impact on historically
 *   marginalized groups and cannot be justified by compelling state interest.
 *   This reading restructures the victim set (bringing
 *   subordination-reproducing structures and their beneficiaries within the
 *   constraint's scope) and elevates the extractiveness coefficient by making
 *   structural inequality itself a matter of constitutional concern. The
 *   constraint exhibits tangled rope structure: it coordinates doctrinal
 *   development toward reducing subordination while imposing enforcement
 *   costs and political resistance on state institutions and
 *   subordination-maintaining entities. The theater ratio remains moderate
 *   because substantive equality doctrine has institutional legitimacy
 *   (judicial precedent, statutory basis in Civil Rights Act, enforcement
 *   through federal agencies) even as its real-world enforcement remains
 *   contested.
 *
 * KEY AGENTS:
 *   - Subordinated Groups (powerless/trapped): Victims of structural subordination embedded in facially neutral policies. Stand to benefit from enforcement but bear costs of persistent legal battles and partial enforcement.
 *   - Civil Rights Enforcement Coalition (moderate/constrained): Courts, federal agencies, advocacy organizations. Benefit from substantive equality as tool for litigation and policy pressure; constrained by resources and political backlash.
 *   - Academic Theorists (institutional/arbitrage): Critical legal scholars, civil rights theorists. Develop and defend the substantive equality reading; benefit through intellectual authority.
 *   - State Regulatory Apparatus (powerful/constrained): Housing authorities, education systems, employment agencies, voting administrations. Coordinated toward reducing disparate impact; constrained by compliance costs and litigation risk.
 *   - Subordination-Reproducing Institutions (organized/constrained): Schools, employers, housing markets, law enforcement relying on facially neutral policies. Face legal vulnerability under substantive equality reading; constrained from abandoning subordinating structures.
 *   - Federal Judiciary (institutional/arbitrage): Interprets and applies substantive equality doctrine. Benefits from judicial authority; maintains arbitrage through doctrinal flexibility and appointment-sensitive shifts.
 *   - Formal Equality Defenders (powerful/mobile): Conservative legal theorists, business interests, institutional stakeholders. Defend 'colorblindness' and facial neutrality. Operate through piton persistence rather than active function.
 *   - Analytical Observer (analytical/analytical): Risks naturalizing the substantive equality reading as inevitable constitutional meaning rather than contingent interpretation emerging from political struggle.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_clause__substantive_equality_reading, 0.58).
domain_priors:suppression_score(equal_protection_clause__substantive_equality_reading, 0.68).
domain_priors:theater_ratio(equal_protection_clause__substantive_equality_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_clause__substantive_equality_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(equal_protection_clause__substantive_equality_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(equal_protection_clause__substantive_equality_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_clause__substantive_equality_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_clause__substantive_equality_reading, "Equal Protection Clause: Substantive Equality Reading").
narrative_ontology:topic_domain(equal_protection_clause__substantive_equality_reading, "constitutional_law/civil_rights").

domain_priors:requires_active_enforcement(equal_protection_clause__substantive_equality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_clause__substantive_equality_reading, 'c38eafa9-71f6-4158-a218-ed9a873f1532').
narrative_ontology:cs_kernel_codification('c38eafa9-71f6-4158-a218-ed9a873f1532', fixed_text).
narrative_ontology:cs_authority_grounding('c38eafa9-71f6-4158-a218-ed9a873f1532', lineage).
narrative_ontology:cs_interpretation_layer_present('c38eafa9-71f6-4158-a218-ed9a873f1532').
narrative_ontology:cs_reading_relation('c38eafa9-71f6-4158-a218-ed9a873f1532', equal_protection_clause__formal_equality_reading, coexists_with).
narrative_ontology:cs_reading_relation('c38eafa9-71f6-4158-a218-ed9a873f1532', equal_protection_clause__anti_subordination_reading, influences).
narrative_ontology:cs_axiom('c38eafa9-71f6-4158-a218-ed9a873f1532', foundational, structural_subordination_legally_cognizable).
narrative_ontology:cs_axiom_status(structural_subordination_legally_cognizable, holdable).
narrative_ontology:cs_axiom_grounding('c38eafa9-71f6-4158-a218-ed9a873f1532', structural_subordination_legally_cognizable, deontological).
narrative_ontology:cs_axiom('c38eafa9-71f6-4158-a218-ed9a873f1532', secondary, disparate_impact_as_violation_proxy).
narrative_ontology:cs_axiom_status(disparate_impact_as_violation_proxy, holdable).
narrative_ontology:cs_axiom_grounding('c38eafa9-71f6-4158-a218-ed9a873f1532', disparate_impact_as_violation_proxy, empirically_contingent).
narrative_ontology:cs_reference_frame('c38eafa9-71f6-4158-a218-ed9a873f1532', structural_equality_framework).
narrative_ontology:cs_drift_state('c38eafa9-71f6-4158-a218-ed9a873f1532', contemporary_conservative_retrenchment, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c38eafa9-71f6-4158-a218-ed9a873f1532', '').
narrative_ontology:cs_kernel_id(equal_protection_clause__substantive_equality_reading, equal_protection_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_clause__substantive_equality_reading, historical_marginalized_groups).
narrative_ontology:constraint_beneficiary(equal_protection_clause__substantive_equality_reading, civil_rights_enforcement_agencies).
narrative_ontology:constraint_victim(equal_protection_clause__substantive_equality_reading, subordination_reproduction_structures).
narrative_ontology:constraint_victim(equal_protection_clause__substantive_equality_reading, state_coercive_apparatus).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBORDINATED GROUP (SNARE) — Trapped by structural barriers embedded in facially neutral policies (housing, education, employment, voting). The substantive equality reading brings these into the victim set via disparate impact doctrine. No exit from the constraint without material reorganization of social structures. Maximum extraction — the group bears costs of subordination-reproducing policies they did not consent to.
constraint_indexing:constraint_classification(equal_protection_clause__substantive_equality_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CIVIL RIGHTS ENFORCEMENT COALITION (TANGLED ROPE) — Benefits from substantive equality doctrine as a tool for challenging structural subordination, but constrained by litigation costs, political resistance, and enforcement capacity limits. The reading creates both coordination (marshaling evidence of disparate impact) and asymmetric extraction (victory in one case may trigger backlash limiting future victories).
constraint_indexing:constraint_classification(equal_protection_clause__substantive_equality_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ACADEMIC THEORISTS (ROPE) — Substantive equality reading provides interpretive framework for scholarly work on constitutional meaning and civil rights doctrine. Benefits through intellectual authority and policy influence. Minimal suppression — theorists have significant autonomy to develop and defend the reading.
constraint_indexing:constraint_classification(equal_protection_clause__substantive_equality_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: STATE REGULATORY APPARATUS (TANGLED ROPE) — Coordination function: substantive equality doctrine shapes housing, education, employment, voting policy toward reducing subordination. But extraction: enforcement creates compliance costs, litigation exposure, political backlash. State apparatus is both constrained (must respond to legal doctrine) and benefits (gets legitimacy claim of pursuing equality). Constrained exit — cannot simply abandon the doctrine without constitutional crisis.
constraint_indexing:constraint_classification(equal_protection_clause__substantive_equality_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: SUBORDINATION-REPRODUCING INSTITUTIONS (SNARE) — Schools, housing markets, employers, law enforcement, voting administrations that benefit from facially neutral policies generating disparate impact. The substantive equality reading makes their structural arrangements legally vulnerable. Constrained exit — cannot abandon the subordinating structures without confronting the reading's legal force. High suppression of alternatives.
constraint_indexing:constraint_classification(equal_protection_clause__substantive_equality_reading, snare,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: FEDERAL JUDICIARY (TANGLED ROPE) — Coordinates constitutional meaning through doctrine development (substantive equality reading enables litigation strategy). Benefits from judicial authority to adjudicate civil rights claims. But extraction: doctrine creates precedent burdens, political pressure, and competing interpretations (formal equality vs substantive equality vs anti-subordination). Arbitrage exit — can shift doctrine through appointment of new justices with different constitutional views.
constraint_indexing:constraint_classification(equal_protection_clause__substantive_equality_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: FORMAL EQUALITY DEFENDERS (PITON) — Defend the older reading that equal protection requires only facial neutrality, not attention to disparate impact. This perspective has institutional support (conservative jurisprudence, business interests) but operates through inertia rather than functional verification. The substantive equality reading has displaced it as the leading edge of doctrine, yet formal equality persists as performative alternative ('colorblindness' as aspirational goal rather than descriptive fact).
constraint_indexing:constraint_classification(equal_protection_clause__substantive_equality_reading, piton,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a position of complete abstraction, equal protection doctrine might appear as a timeless principle of natural law — the inherent requirement that laws treat all persons with equal concern and respect. The analytically detached observer risks seeing the substantive equality reading as merely revealing what equal protection always meant, not as a constructed interpretation that emerged from political struggle, civil rights organizing, and doctrinal innovation.
constraint_indexing:constraint_classification(equal_protection_clause__substantive_equality_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_clause__substantive_equality_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(equal_protection_clause__substantive_equality_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(equal_protection_clause__substantive_equality_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(equal_protection_clause__substantive_equality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(equal_protection_clause__substantive_equality_reading, TR),
    TR >= 0.70.

:- end_tests(equal_protection_clause__substantive_equality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The substantive equality reading identifies and constrains structures that benefit from subordination reproduction — housing discrimination, educational segregation, employment discrimination, voting suppression. The extraction coefficient reflects that the constraint imposes real costs on beneficiaries of subordinating structures (institutional reorganization, litigation exposure, reduced subordinating capacity) while the payoff to subordinated groups is conditional on enforcement. The measurement trajectory (0.32 → 0.58 over 60 time units, roughly 1954–2014) reflects the doctrine's increasing enforcement intensity: early cases (Brown v Board, Loving v Virginia) established formal prohibition on explicit discrimination; mid-period doctrine (disparate impact cases) expanded to cover facially neutral policies; recent cases (Shelby County v Holder, Parents Involved v Seattle Schools) show increasing doctrinal resistance. Suppression (0.68): High and rising. Suppression includes legal barriers to enforcement (high litigation costs, proof burdens, remedial limitations), political suppression (backlash against 'reverse discrimination' claims, Senate opposition to civil rights enforcement), and institutional suppression (Congress limiting agency authority, judicial appointment of doctrine-skeptical justices). The measurement trajectory (0.42 → 0.68) reflects both doctrinal consolidation (suppression mechanisms became more sophisticated and institutionalized) and reactive suppression (increased resistance as enforcement became more effective). Theater ratio (0.55): Moderate. Substantive equality doctrine has legitimate doctrinal basis in constitutional text and precedent, producing moderate theater — the doctrine is not purely performative (courts do apply it, agencies do enforce it) but also not fully functional (enforcement remains partial, remedies incomplete, political opposition significant). Theater stays relatively flat across the interval because the doctrine's performative content (announcing commitment to equality without ensuring material equality) remains constant even as enforcement intensity rises.
 *
 * PERSPECTIVAL GAP:
 *   The substantive equality reading produces maximal perspectival divergence. For subordinated groups, the constraint is snare (trapped by structural subordination regardless of facial neutrality). For civil rights enforcers, it is tangled rope (benefit through litigation strategy, constrained by enforcement gaps). For the state apparatus, it is tangled rope (coordination toward equality, extraction through compliance costs). For subordination-reproducing institutions, it is snare (constrained by legal vulnerability). For formal equality defenders, it is piton (institutional persistence of 'colorblindness' despite being displaced by substantive doctrine). For the analytical observer, it threatens to be mountain (natural constitutional law) — a false summit that naturalizes contingent interpretation. The perspectival gap reveals that 'equal protection of the laws' is not a transparent constitutional principle but a site of genuine interpretive struggle: the kernel permits multiple readings with structurally different consequences.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position within the constraint. Subordinated groups bear the costs of subordination-reproducing policies; they are victims with trapped exit options, producing high d (structural victim position). Civil rights enforcer coalition benefits from the reading as a tool but faces resistance and constraint; moderate power with constrained exit produces mid-range d. State apparatus is both constrained (must respond to doctrine) and benefits (legitimacy); powerful actor with constrained options produces mid-range d tilted toward the victim side. Formal equality defenders have powerful institutional backing but operate through inertia; powerful with mobile options produces lower d. Theoretical architects benefit with minimal suppression; institutional power with arbitrage exit produces low d. The perspectival gap emerges from this directionality structure: victims perceive snare (no exit, maximum extraction cost); beneficiaries perceive rope (coordination benefit outweighs constraint); organized resisters perceive snare (high suppression, constrained exit); analytically detached observer risks perceiving mountain (naturalizing the constraint as constitutional law rather than contingent interpretation).
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING OF EQUAL PROTECTION: This constraint instantiates one reading of a contested kernel. The Equal Protection Clause ('nor deny to any person within its jurisdiction the equal protection of the laws') has been read three structurally distinct ways: (1) formal equality — equal protection requires only facial neutrality, not attention to outcomes; (2) substantive equality (this reading) — equal protection requires attention to disparate impact and structural subordination; (3) anti-subordination — equal protection prohibits hierarchical social ordering regardless of intent or mechanism. These are not empirical disputes resolvable by evidence gathering — they are interpretive choices about what the kernel means. The mandatrophy here is the reading's own self-undermining: if substantive equality doctrine is a contingent interpretation that could have been foreclosed by different judicial appointments, then its authority rests not on inevitable constitutional meaning but on contingent power to maintain a specific reading against alternatives. Yet if substantive equality is inevitable constitutional meaning, the reading claims to reveal what equal protection always meant, not to construct new meaning — which risks naturalizing the constraint as mountain (timeless constitutional principle) rather than tangled rope (contingent institutional practice). The resolution is perspectival: subordinated groups experiencing the constraint as snare see it as real structural force regardless of its contingency; formal equality defenders see it as interpretive overreach; analytical observers see the indeterminacy. All three assessments are structurally accurate from their respective positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substantive_vs_formal_boundary,
    'Is the distinction between substantive and formal equality a genuine structural boundary in constitutional interpretation, or a false dichotomy obscuring the reading''s political character?',
    'Historical genealogy of formal vs substantive language in case law; examination of whether the boundary tracks onto real interpretive differences or rhetorical positioning',
    'If genuine structural boundary: substantive equality is a defensible reading distinct from formal equality. If false dichotomy: both readings obscure the anti-subordination principle that grounds the constraint, and the boundary is performative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substantive_vs_formal_boundary, conceptual, 'Whether substantive/formal distinction is structural or rhetorical').

omega_variable(
    disparate_impact_proof_asymmetry,
    'Does requiring proof of disparate impact (statistical racial disparity in outcomes) adequately capture subordination-reproducing structures, or does it privilege measurable harms while obscuring unmeasurable ones?',
    'Comparative analysis of harms caught by disparate impact doctrine (housing discrimination, education segregation, voting access) vs harms not caught (cultural erasure, epistemic marginalization, relational subordination without statistical trace)',
    'If adequate: substantive equality reading identifies the constraint''s extraction mechanisms correctly. If inadequate: the reading naturalizes certain types of subordination as ''not susceptible to legal remedy,'' potentially reinforcing those structures.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disparate_impact_proof_asymmetry, empirical, 'Proof adequacy for capturing subordination via disparate impact').

omega_variable(
    judicial_enforcement_capacity,
    'Can federal courts actually enforce substantive equality doctrine against structural subordination, or does legal recognition without material enforcement capacity constitute pure theater masking persistent subordination?',
    'Longitudinal analysis of disparate impact judgments vs actual implementation; tracking of remedies imposed and remedies complied with; comparison of pre-judgment and post-judgment inequality metrics',
    'If capacity sufficient: substantive equality reading is operative constraint (tangled rope, snare). If capacity insufficient: reading is piton (performative declaration without functional enforcement) or false summit (naturalizes legal remedy as equivalent to material equality).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(judicial_enforcement_capacity, empirical, 'Judicial enforcement capacity for substantive equality remedies').

omega_variable(
    reading_contingency_vs_inevitability,
    'Is the substantive equality reading a contingent interpretation that emerged from political struggle and could have been foreclosed by different historical choices, or does it represent the inevitable logical development of equal protection doctrine?',
    'Counterfactual history: would different appointments to the Court in 1960s–1980s have foreclosed substantive equality, or would doctrinal logic have produced it anyway? Analysis of how formal equality could have satisfied equivalent constitutional values.',
    'If contingent: the reading is a constructed constraint with real but revisable political foundations. If inevitable: the reading reveals equal protection''s true meaning, and formal equality was always unstable. Affects assessment of whether the reading can be foreclosed or merely suppressed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contingency_vs_inevitability, conceptual, 'Contingency vs historical inevitability of substantive equality reading').

omega_variable(
    kernel_reading_under_determination,
    'Does the Equal Protection Clause kernel support only one defensible reading (substantive equality), or do multiple readings (formal equality, anti-subordination, minimal rational basis) represent genuine alternative meanings coexisting within the text?',
    'Textual analysis of the clause (''equal protection of the laws'') across interpretive traditions; examination of whether formal and substantive readings each find adequate textual purchase',
    'If single reading: substantive equality forecloses formal equality within any adequate constitutional framework. If multiple coexisting readings: substantive equality coexists with formal equality, and the engine must model both readings as live options even if one has doctrinal dominance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_under_determination, conceptual, 'Kernel under-determination across equal protection readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_clause__substantive_equality_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epc_subst_theater_t0, equal_protection_clause__substantive_equality_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(epc_subst_theater_t30, equal_protection_clause__substantive_equality_reading, theater_ratio, 30, 0.52).
narrative_ontology:measurement(epc_subst_theater_t60, equal_protection_clause__substantive_equality_reading, theater_ratio, 60, 0.55).

% Extraction over time
narrative_ontology:measurement(epc_subst_extr_t0, equal_protection_clause__substantive_equality_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(epc_subst_extr_t30, equal_protection_clause__substantive_equality_reading, base_extractiveness, 30, 0.48).
narrative_ontology:measurement(epc_subst_extr_t60, equal_protection_clause__substantive_equality_reading, base_extractiveness, 60, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(epc_subst_supp_t0, equal_protection_clause__substantive_equality_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(epc_subst_supp_t30, equal_protection_clause__substantive_equality_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(epc_subst_supp_t60, equal_protection_clause__substantive_equality_reading, suppression_requirement, 60, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_clause__substantive_equality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_protection_clause__substantive_equality_reading, equal_protection_clause__formal_equality_reading).
narrative_ontology:affects_constraint(equal_protection_clause__substantive_equality_reading, equal_protection_clause__anti_subordination_reading).
narrative_ontology:affects_constraint(equal_protection_clause__substantive_equality_reading, voting_rights_act__preclearance_regime).
narrative_ontology:affects_constraint(equal_protection_clause__substantive_equality_reading, fair_housing_act__disparate_impact_doctrine).
narrative_ontology:affects_constraint(equal_protection_clause__substantive_equality_reading, title_vii_employment_discrimination__disparate_impact_standard).

% DUAL FORMULATION NOTE:
% The equal_protection_clause kernel is read through three distinct interpretive frames. Each frame produces a different constraint story with different epsilon, different victim sets, different enforcement mechanisms. Substantive equality reading has highest epsilon (0.58) because it makes structural subordination itself the target of equal protection doctrine. Formal equality reading has lower epsilon (~0.20) because it permits facially neutral policies regardless of disparate impact. Anti-subordination reading has highest epsilon (~0.68) because it targets hierarchical social ordering as inherent violation. These are not observable-dependent variations on one constraint — they are genuinely distinct constraints produced by competing readings of a single kernel. All three are live positions in constitutional discourse.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(equal_protection_clause__substantive_equality_reading, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
