% ============================================================================
% CONSTRAINT STORY: us_constitution_interpretive__popular_constitutionalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_interpretive__popular_constitutionalism_reading, []).

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
 *   constraint_id: us_constitution_interpretive__popular_constitutionalism_reading
 *   human_readable: Popular Constitutionalism: Constitutional Meaning Through Democratic Contestation
 *   domain: constitutional_law/political_theory/interpretive_authority
 *
 * SUMMARY:
 *   The popular constitutionalism reading of the U.S. Constitution asserts
 *   that constitutional meaning is not determined by judicial interpretation
 *   alone, but emerges through contestation among the branches of government
 *   and popular political movements. Under this reading, 'We the People' are
 *   not historical Framers but the ongoing body politic — constitutional
 *   meaning evolves when sustained popular movements reinterpret the
 *   Constitution through political struggle. This reading directly challenges
 *   judicial supremacy (the presumption that courts are the final arbiters of
 *   constitutional meaning) by distributing interpretive authority across
 *   branches and claim-making constituencies. The constraint exhibits Tangled
 *   Rope structure: it coordinates legitimate democratic participation in
 *   constitutional meaning-making (genuine coordination function) while
 *   simultaneously extracting protections from vulnerable groups dependent on
 *   counter-majoritarian judicial authority (asymmetric extraction). The
 *   beneficiaries are organized popular movements, legislative majorities,
 *   and anti-elitist challengers who gain interpretive authority through
 *   political mobilization. The victims are those who depend on judicial
 *   finality (institutional stability dependents, long-term planners),
 *   minorities whose rights have been protected by counter-majoritarian
 *   judicial intervention, and the judiciary itself (whose authority is
 *   subordinated to political contestation). The theater_ratio (0.64)
 *   reflects that much of the performance of constitutional governance under
 *   this reading is legitimation: framing majoritarian political outcomes as
 *   'constitutional meaning' rather than policy preference requires
 *   substantial rhetorical work to distinguish genuine constitutional
 *   reinterpretation from ordinary legislation.
 *
 * KEY AGENTS:
 *   - Popular Movements: Primary beneficiary (organized/mobile) — gain interpretive authority and can claim constitutional meaning through political struggle
 *   - Legislative Majorities: Secondary beneficiary (powerful/arbitrage) — elevated from interpreters within judicial constraints to co-equal constitutional authorities
 *   - Anti-Elitist Challengers: Tertiary beneficiary (organized/mobile) — benefit from reading that delegitimizes elite judicial monopoly on constitutional meaning
 *   - Judicial Finality Advocates: Primary victim (powerless/trapped) — cannot exit commitment to stable judicial settlement; see constitutional meaning constantly reopened
 *   - Constitutional Minorities: Secondary victim (moderate/identity_locked) — structurally mobile but identity-fused with reliance on counter-majoritarian judicial protection; exposed to majoritarian reinterpretation
 *   - Institutional Stability Dependents: Tertiary victim (moderate/constrained) — bear cost of uncertainty as constitutional meaning shifts through political contestation
 *   - The Federal Judiciary: Institutional actor (institutional/constrained) — constrained by role; loses institutional authority as judicial interpretation becomes merely one voice among many
 *   - Analytical Observer: Observational perspective (analytical/analytical) — sees both the coordination function (democracy shapes meaning) and the extraction mechanism (judicial protection for minorities is subordinated)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_interpretive__popular_constitutionalism_reading, 0.52).
domain_priors:suppression_score(us_constitution_interpretive__popular_constitutionalism_reading, 0.58).
domain_priors:theater_ratio(us_constitution_interpretive__popular_constitutionalism_reading, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_interpretive__popular_constitutionalism_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_interpretive__popular_constitutionalism_reading, "Popular Constitutionalism: Constitutional Meaning Through Democratic Contestation").
narrative_ontology:topic_domain(us_constitution_interpretive__popular_constitutionalism_reading, "constitutional_law/political_theory/interpretive_authority").

domain_priors:requires_active_enforcement(us_constitution_interpretive__popular_constitutionalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_interpretive__popular_constitutionalism_reading, 'c380bfc6-d2dd-4883-9e35-c43f8477cb48').
narrative_ontology:cs_kernel_codification('c380bfc6-d2dd-4883-9e35-c43f8477cb48', fixed_text).
narrative_ontology:cs_authority_grounding('c380bfc6-d2dd-4883-9e35-c43f8477cb48', distributed).
narrative_ontology:cs_reading_relation('c380bfc6-d2dd-4883-9e35-c43f8477cb48', us_constitution_interpretive__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('c380bfc6-d2dd-4883-9e35-c43f8477cb48', us_constitution_interpretive__living_constitution_reading, coexists_with).
narrative_ontology:cs_axiom('c380bfc6-d2dd-4883-9e35-c43f8477cb48', foundational, interpretive_authority_is_distributed).
narrative_ontology:cs_axiom_status(interpretive_authority_is_distributed, holdable).
narrative_ontology:cs_axiom_grounding('c380bfc6-d2dd-4883-9e35-c43f8477cb48', interpretive_authority_is_distributed, conventional).
narrative_ontology:cs_axiom('c380bfc6-d2dd-4883-9e35-c43f8477cb48', foundational, constitutional_meaning_emerges_from_political_contestation).
narrative_ontology:cs_axiom_status(constitutional_meaning_emerges_from_political_contestation, holdable).
narrative_ontology:cs_axiom_grounding('c380bfc6-d2dd-4883-9e35-c43f8477cb48', constitutional_meaning_emerges_from_political_contestation, empirically_contingent).
narrative_ontology:cs_axiom('c380bfc6-d2dd-4883-9e35-c43f8477cb48', secondary, judicial_supremacy_is_contingent).
narrative_ontology:cs_axiom_status(judicial_supremacy_is_contingent, holdable).
narrative_ontology:cs_axiom_grounding('c380bfc6-d2dd-4883-9e35-c43f8477cb48', judicial_supremacy_is_contingent, instrumental).
narrative_ontology:cs_reference_frame('c380bfc6-d2dd-4883-9e35-c43f8477cb48', judicial_interpretive_supremacy_framework).
narrative_ontology:cs_drift_state('c380bfc6-d2dd-4883-9e35-c43f8477cb48', contemporary_popular_contestation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c380bfc6-d2dd-4883-9e35-c43f8477cb48', '').
narrative_ontology:cs_kernel_id(us_constitution_interpretive__popular_constitutionalism_reading, us_constitution_interpretive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__popular_constitutionalism_reading, popular_movements).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__popular_constitutionalism_reading, legislative_majorities).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__popular_constitutionalism_reading, anti_elitist_challengers).
narrative_ontology:constraint_victim(us_constitution_interpretive__popular_constitutionalism_reading, judicial_finality_advocates).
narrative_ontology:constraint_victim(us_constitution_interpretive__popular_constitutionalism_reading, minorities_requiring_counter_majoritarian_protection).
narrative_ontology:constraint_victim(us_constitution_interpretive__popular_constitutionalism_reading, institutional_stability_dependents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: JUDICIAL FINALITY ADVOCATES (SNARE) — Trapped within a constitutional order whose meaning is contested and subject to reinterpretation through political struggle. They cannot exit this constraint; their core commitment to stable, judicially-determined constitutional settlement is perpetually vulnerable to popular movement contestation. Maximum experienced extraction — no agency to enforce interpretive closure.
constraint_indexing:constraint_classification(us_constitution_interpretive__popular_constitutionalism_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MINORITIES DEPENDENT ON JUDICIAL PROTECTION (SNARE) — Identity-locked within a constitutional order that nominally protects them through counter-majoritarian judicial authority, but the popular constitutionalism reading undermines that protection by subordinating judicial authority to popular political movements. Structurally mobile (can organize, advocate, exit jurisdictions) but cannot escape the identity fusion with reliance on judicial protection as a minority group. High extraction masked by the appearance of democratic legitimacy.
constraint_indexing:constraint_classification(us_constitution_interpretive__popular_constitutionalism_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 3: INSTITUTIONAL STABILITY DEPENDENTS (TANGLED ROPE) — Corporations, long-term investors, settled institutional actors that benefit from stable constitutional settlement but also participate in the political processes that shape meaning. Constrained by the cost of relocating institutional infrastructure, but gain genuine benefits from the coordination function (constitutional meaning provides predictability even if contested). Mixed experience of both extraction and benefit.
constraint_indexing:constraint_classification(us_constitution_interpretive__popular_constitutionalism_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: POPULAR MOVEMENTS (ROPE) — Organized agents with mobile exit options (can deploy political pressure, mobilize constituencies, exit failed coalitions). The popular constitutionalism reading directly benefits them by legitimating their claim to interpretive authority. They experience this constraint as pure coordination — a mechanism for channeling mass democratic will into constitutional meaning. Low or positive effective extraction; genuine agency and win conditions.
constraint_indexing:constraint_classification(us_constitution_interpretive__popular_constitutionalism_reading, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGISLATIVE MAJORITIES (ROPE) — Powerful agents with arbitrage options (can pass legislation, alter constitutional amendment processes, shift interpretive boundaries through legislative action and political pressure). The popular constitutionalism reading elevates their role from subordinate to co-equal with courts in constitutional interpretation. Pure beneficiary — experiences low extraction and high coordination benefit.
constraint_indexing:constraint_classification(us_constitution_interpretive__popular_constitutionalism_reading, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: THE FEDERAL JUDICIARY (TANGLED ROPE) — Constrained by institutional role and constitutional structure, but also benefits from the coordination function (legitimate exercise of judicial review requires some grounding in constitutional interpretation). However, the popular constitutionalism reading directly attacks judicial supremacy in interpretation, extracting authority and institutional prestige. The judiciary is forced to accept popular movements as legitimate co-interpreters of the Constitution, which subordinates judicial interpretive authority and creates constant pressure from extra-judicial constitutional meaning-making.
constraint_indexing:constraint_classification(us_constitution_interpretive__popular_constitutionalism_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: THE ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational/analytical perspective, the popular constitutionalism reading describes a genuine structural feature of constitutional governance: constitutional meaning DOES emerge from political struggle across branches and popular mobilization, not solely from judicial pronouncement. However, this reading also contains significant extraction mechanisms: it subordinates those dependent on judicial protection to majoritarian political processes, and it destabilizes the institutional settlements that minorities and vulnerable groups rely on. The coordination function (democracy determines constitutional meaning) coexists with asymmetric extraction (minorities lose judicial protection). This is irreducibly tangled.
constraint_indexing:constraint_classification(us_constitution_interpretive__popular_constitutionalism_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_interpretive__popular_constitutionalism_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_constitution_interpretive__popular_constitutionalism_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_constitution_interpretive__popular_constitutionalism_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_interpretive__popular_constitutionalism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_interpretive__popular_constitutionalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The reading redistributes interpretive authority from courts to political movements, which benefits majorities and organized constituencies but extracts protection from minorities dependent on judicial authority. The extraction is not maximal (Snare-level) because institutional mechanisms (constitutional text, amendment procedures, structural limits on executive power) still constrain political reinterpretation. The rise from 0.38 to 0.52 over the interval reflects increasing acceptance of the reading in legal scholarship and political practice, raising extractiveness as popular constitutionalism gains institutional legitimacy. Suppression (0.58): Moderate-high. Significant suppression mechanisms include (1) the marginalization of counter-majoritarian judicial authority in constitutional discourse, (2) the difficulty for minorities dependent on judicial protection to mobilize politically against constitutional movements backed by popular majorities, (3) the concentration of interpretive resources among organized movements that can mobilize constituencies, and (4) the elimination of institutional veto points for those opposing constitutional reinterpretation. Theater ratio (0.64): Moderate-high. The performance of popular constitutionalism requires substantial rhetorical work to distinguish genuine constitutional reinterpretation (grounded in sustained popular will across branches) from ordinary majoritarian legislation masquerading as constitutional meaning. The judiciary itself performs 'deference to popular constitutionalism,' legitimating political outcomes as constitutional rather than contingent. The theater increases from 0.55 to 0.64 as the reading becomes more widely invoked to justify political outcomes, raising its performative burden.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence. Popular movements and legislative majorities classify the reading as Rope — they see a coordination mechanism that enables their voice in constitutional interpretation. Minorities and stability dependents classify it as Snare or Tangled Rope — they see extraction and loss of protection. The judiciary occupies the tangled position: constrained to accept popular constitutionalism as legitimate (cannot exit the constraint) but also constrained to defend its own interpretive role against total absorption by political movements. The judicial finality advocates are trapped in a constraint where the very meaning of the Constitution is perpetually contestable. The analytical observer sees the reading as Tangled Rope — genuine coordination (democracy shapes meaning) plus genuine extraction (judicial protection for minorities is sacrificed).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies dramatically by agent structural position. Popular movements and legislative majorities are beneficiaries with mobile/arbitrage exit options, yielding low d and negative/minimal effective extraction (they experience Rope or benefit from Rope coordination). Minorities dependent on judicial protection are identity-locked victims with no exit, yielding high d and maximum experienced extraction (they experience Snare). Institutional stability dependents are constrained victims with some adaptive capacity, yielding moderate d and moderate extraction (they experience Tangled Rope). The judiciary itself is constrained within its institutional role as a branch of government, yielding moderate d — the reading extracts judicial authority but does not eliminate the judiciary's role, only subordinates it (Tangled Rope). The perspectival gaps are large because the same constraint redistributes power: what is rope for majorities is snare for minorities; what is coordination for movements is extraction for those requiring finality.
 *
 * MANDATROPHY ANALYSIS:
 *   The popular constitutionalism reading avoids simple Snare misclassification (pure extraction) by maintaining genuine coordination benefit for popular movements and legislative majorities — the reading does solve a real coordination problem (how to allow constitutional meaning to evolve with democratic will without requiring formal amendment each time). However, the reading also manifestly creates asymmetric extraction: minorities lose judicial protection, institutional stability is sacrificed, and those dependent on constitutional finality bear high costs. This is precisely the Tangled Rope signature — irreducible coexistence of coordination function and asymmetric extraction. The mandatrophy is resolved by recognizing that from SOME perspectives (majorities, movements), this is legitimate pure coordination, while from OTHER perspectives (minorities, stability advocates), this is genuine snare. The presheaf over observer positions captures the full structure: no single type is correct, but the distribution of types is diagnostically meaningful.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_supremacy_or_coordinate_authority,
    'Is judicial authority over constitutional meaning a coordinate branch function (equal to legislative and executive interpretation) or a supreme interpretive authority that can be overridden by popular movements?',
    'Historical analysis of constitutional moments where popular movements have successfully reinterpreted the Constitution against judicial authority (Reconstruction, New Deal, Civil Rights); examination of whether judicial reversal of those movements constitutes legitimate constitutional reclamation or illegitimate counter-revolution.',
    'If coordinate authority: popular constitutionalism is rope (coordination mechanism). If supreme authority: popular constitutionalism is snare (extraction from those dependent on judicial finality). If authority is contextual (coordinate in some domains, supreme in others): constraint type varies by doctrinal domain.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judicial_supremacy_or_coordinate_authority, conceptual, 'Whether judicial authority is coordinate or supreme in constitutional interpretation').

omega_variable(
    majoritarian_tyranny_vs_democratic_legitimacy,
    'Does popular constitutionalism create legitimate democratic control over constitutional meaning or does it unleash majoritarian tyranny against constitutional minorities?',
    'Empirical study of outcomes for minority groups under (a) judicial supremacy interpretation and (b) popular constitutionalism interpretation across historical periods; correlation between constitutional movement success and minority protection outcomes; analysis of constitutional amendments that explicitly overrode judicial decisions.',
    'If majoritarian tyranny dominates: popular constitutionalism extracts from minorities (Snare). If minority protection outcomes improve under popular constitutionalism: constraint reclassifies toward Rope. If outcomes are mixed: Tangled Rope confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majoritarian_tyranny_vs_democratic_legitimacy, empirical, 'Whether popular constitutionalism protects or harms constitutional minorities').

omega_variable(
    popular_vs_populist_distinction,
    'Can popular constitutionalism distinguish between legitimate popular constitutional interpretation (by diverse, deliberative movements) and populist demagogy (by concentrated power disguised as popular will)?',
    'Formalization of criteria for legitimate popular movement (diverse constituency, sustained mobilization, reference to constitutional text, cross-partisan support) vs populist appropriation (concentrated leadership, ephemeral mobilization, rhetoric over text, single-party mobilization); historical analysis of whether the popular constitutionalism reading supports legitimate movements or enables demagogy.',
    'If distinction is sharp and enforceable: popular constitutionalism can avoid elite capture (Rope from popular movements perspective). If distinction collapses: demagogues can claim popular constitutionalism authority to overcome judicial constraints (Snare from minorities perspective, Piton as the judiciary becomes performative).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(popular_vs_populist_distinction, conceptual, 'Whether popular constitutionalism can distinguish genuine popular will from populist demagogy').

omega_variable(
    kernel_reading_identity,
    'Is this constraint one reading of the contested kernel ''US Constitution Interpretive Authority'', or does the popular constitutionalism reading constitute a rejection of the kernel itself (i.e., the claim that any single unified constitutional meaning exists)?',
    'Analysis of whether popular constitutionalism scholars (Ackerman, Kramer, Tushnet) treat constitutional meaning as determinate (but plurally-authored) or as inherently contestable across generations; examination of whether the reading accepts judicial interpretation as ONE valid source of meaning or rejects the very concept of unified meaning.',
    'If one reading of a unified kernel: fits the kernel framework; coexists with originalist and living constitution readings. If rejection of unified meaning: this is NOT a reading but a meta-challenge to the kernel itself; should be modeled as a separate constraint about whether the Constitution has determinate meaning.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether popular constitutionalism is a reading of the constitutional meaning kernel or a rejection of kernel determinacy').

omega_variable(
    reform_vs_revolutionary_potential,
    'Does popular constitutionalism enable incremental constitutional reform through democratic contestation (Tushnet: ''thin'' constitution, democratic amendment), or does it risk revolutionary constitutional displacement by mobilized factions (Ackerman: ''We the People'' moments can overturn prior constitutional orders)?',
    'Comparative analysis of outcomes when popular constitutionalism frameworks were invoked: Reconstruction (revolutionary displacement of antebellum constitution), New Deal (contested reform of judicial authority without constitutional text amendment), Civil Rights (incremental reinterpretation of existing provisions). Examination of scholar positions on whether popular constitutionalism includes or excludes revolutionary constitutional displacement.',
    'If incremental only: constraint serves populist movements seeking reform (Rope). If revolutionary potential included: minorities dependent on settled constitutional order face maximum exposure (Snare). If variability: constraint type depends on whether mobilization is incremental or revolutionary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reform_vs_revolutionary_potential, empirical, 'Whether popular constitutionalism enables incremental reform or revolutionary displacement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_interpretive__popular_constitutionalism_reading, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(popcon_tr_t0, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(popcon_tr_t2, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 2, 0.6).
narrative_ontology:measurement(popcon_tr_t4, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 4, 0.64).

% Extraction over time
narrative_ontology:measurement(popcon_be_t0, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(popcon_be_t2, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 2, 0.45).
narrative_ontology:measurement(popcon_be_t4, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 4, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(popcon_su_t0, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(popcon_su_t2, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 2, 0.53).
narrative_ontology:measurement(popcon_su_t4, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 4, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_interpretive__popular_constitutionalism_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_interpretive__popular_constitutionalism_reading, 0.18).
narrative_ontology:affects_constraint(us_constitution_interpretive__popular_constitutionalism_reading, us_constitution_interpretive__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_interpretive__popular_constitutionalism_reading, us_constitution_interpretive__living_constitution_reading).
narrative_ontology:affects_constraint(us_constitution_interpretive__popular_constitutionalism_reading, judicial_supremacy_doctrine).
narrative_ontology:affects_constraint(us_constitution_interpretive__popular_constitutionalism_reading, counter_majoritarian_judicial_authority).

% DUAL FORMULATION NOTE:
% The popular constitutionalism reading is a distinct reading of the constitutional interpretive authority kernel. It differs structurally from the originalist and living constitution readings in the SOURCE of authority (popular movements + branches vs. judicial reasoning alone) and the SCOPE of authority (distributed vs. concentrated in courts). All three readings share the same base property extractiveness range because the kernel's contestation itself creates extraction mechanisms. However, each reading produces different beneficiary/victim structures: popular constitutionalism benefits organized movements and majorities while extracting from minorities and stability advocates.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution_interpretive__popular_constitutionalism_reading, moderate, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
