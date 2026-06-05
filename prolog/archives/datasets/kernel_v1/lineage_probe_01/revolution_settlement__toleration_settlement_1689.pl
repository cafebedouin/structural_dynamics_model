% ============================================================================
% CONSTRAINT STORY: revolution_settlement__toleration_settlement_1689
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_revolution_settlement__toleration_settlement_1689, []).

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
 *   constraint_id: revolution_settlement__toleration_settlement_1689
 *   human_readable: The Toleration Act 1689: Licensed Nonconformity and Calibrated Exclusion
 *   domain: political/historical
 *
 * SUMMARY:
 *   The Toleration Act of 1689 is the Glorious Revolution's religious
 *   settlement: a truce that legalized dissenting worship for trinitarian
 *   Protestants while leaving the established church's tests and privileges
 *   intact, and explicitly excluding Catholics and radical sects. This
 *   reading understands the constraint as a calibrated reduction of
 *   suppression through licensed exclusion — toleration for some bought by
 *   the explicit exclusion of others. The act is not a universal principle
 *   but a political bargain that benefits trinitarian dissenters and the
 *   established church while victimizing the Catholic population and
 *   non-trinitarian groups. The constraint exhibits tangled_rope dynamics:
 *   genuine coordination (ending religious persecution) embedded in
 *   asymmetric extraction (maintaining civil disabilities and legal
 *   exclusions). From the trinitarian dissenter's perspective, it is
 *   liberation with persistent constraints (tangled rope). From the Catholic
 *   perspective, it is pure extraction — a relief granted to rivals while
 *   Catholics remain criminalized (snare). From the established church's
 *   perspective, it is coordination that stabilizes royal authority without
 *   surrendering privileges (rope). From a parliamentary coalition
 *   perspective, it is a temporary settlement with a built-in sunset
 *   mechanism through successive reformations (scaffold). From a
 *   civilizational perspective, it risks naturalizing contingent political
 *   arrangements as inevitable features of religious pluralism (mountain,
 *   false summit).
 *
 * KEY AGENTS:
 *   - Trinitarian Dissenters: Beneficiary (moderate/constrained) — legalized worship but face Test Acts, office-holding restrictions, and social marginalization
 *   - Established Church and Crown: Primary beneficiary (institutional/arbitrage) — toleration reinforces establishment by settling conflict without surrendering privilege or power
 *   - Catholic Population: Primary victim (powerless/trapped) — explicitly excluded from toleration, recusancy penalties persist, no coordination benefit
 *   - Non-Trinitarian Groups: Secondary victim (powerless/trapped) — explicitly excluded, treated as sectarian threats, denied even limited toleration
 *   - Parliamentary Coalition (Whigs, William III, moderates): Organized actor (organized/mobile) — negotiated the settlement as temporary accommodation with generational sunset built in
 *   - Institutional Memory of Compromise: Piton actor (institutional/arbitrage) — constraint becomes performative over centuries, invoked as proof of liberalism while substantive exclusions persist
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(revolution_settlement__toleration_settlement_1689, 0.48).
domain_priors:suppression_score(revolution_settlement__toleration_settlement_1689, 0.52).
domain_priors:theater_ratio(revolution_settlement__toleration_settlement_1689, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(revolution_settlement__toleration_settlement_1689, extractiveness, 0.48).
narrative_ontology:constraint_metric(revolution_settlement__toleration_settlement_1689, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(revolution_settlement__toleration_settlement_1689, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(revolution_settlement__toleration_settlement_1689, tangled_rope).
narrative_ontology:human_readable(revolution_settlement__toleration_settlement_1689, "The Toleration Act 1689: Licensed Nonconformity and Calibrated Exclusion").
narrative_ontology:topic_domain(revolution_settlement__toleration_settlement_1689, "political/historical").

domain_priors:requires_active_enforcement(revolution_settlement__toleration_settlement_1689).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(revolution_settlement__toleration_settlement_1689, '6eaddde8-684a-4b4d-a1bc-a11751a6d931').
narrative_ontology:cs_kernel_codification('6eaddde8-684a-4b4d-a1bc-a11751a6d931', formalized).
narrative_ontology:cs_authority_grounding('6eaddde8-684a-4b4d-a1bc-a11751a6d931', extraction).
narrative_ontology:cs_interpretation_layer_present('6eaddde8-684a-4b4d-a1bc-a11751a6d931').
narrative_ontology:cs_reading_relation('6eaddde8-684a-4b4d-a1bc-a11751a6d931', revolution_settlement__bill_of_rights_1689, coexists_with).
narrative_ontology:cs_reading_relation('6eaddde8-684a-4b4d-a1bc-a11751a6d931', revolution_settlement__act_of_settlement_1701, coexists_with).
narrative_ontology:cs_axiom('6eaddde8-684a-4b4d-a1bc-a11751a6d931', foundational, conscience_indivisibility_principle).
narrative_ontology:cs_axiom_status(conscience_indivisibility_principle, holdable).
narrative_ontology:cs_axiom_grounding('6eaddde8-684a-4b4d-a1bc-a11751a6d931', conscience_indivisibility_principle, deontological).
narrative_ontology:cs_axiom('6eaddde8-684a-4b4d-a1bc-a11751a6d931', foundational, trinitarian_doctrinal_boundary).
narrative_ontology:cs_axiom_status(trinitarian_doctrinal_boundary, holdable).
narrative_ontology:cs_axiom_grounding('6eaddde8-684a-4b4d-a1bc-a11751a6d931', trinitarian_doctrinal_boundary, theological).
narrative_ontology:cs_reference_frame('6eaddde8-684a-4b4d-a1bc-a11751a6d931', established_church_with_licensed_dissent).
narrative_ontology:cs_drift_state('6eaddde8-684a-4b4d-a1bc-a11751a6d931', contemporary_post_emancipation, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('6eaddde8-684a-4b4d-a1bc-a11751a6d931', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(revolution_settlement__toleration_settlement_1689, revolution_settlement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(revolution_settlement__toleration_settlement_1689, trinitarian_dissenters).
narrative_ontology:constraint_beneficiary(revolution_settlement__toleration_settlement_1689, crown_religious_authority).
narrative_ontology:constraint_victim(revolution_settlement__toleration_settlement_1689, catholic_population).
narrative_ontology:constraint_victim(revolution_settlement__toleration_settlement_1689, non_trinitarian_groups).
narrative_ontology:constraint_victim(revolution_settlement__toleration_settlement_1689, religious_establishment_conformity_requirement).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRINITARIAN DISSENTER (TANGLED ROPE) — Beneficiary of legalized worship but constrained by Test Acts, office-holding restrictions, and social stigma. Experiences genuine coordination (worship now lawful) alongside persistent extraction (civil disabilities remain). Can organize congregations but faces career barriers. Mixed experience: freedom and constraint coexist.
constraint_indexing:constraint_classification(revolution_settlement__toleration_settlement_1689, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: CATHOLIC POPULATION (SNARE) — Explicitly excluded from toleration. Worship remains illegal; penalties for recusancy persist. No coordination benefit. Pure extraction: bears costs of establishment conformity requirement while denied even the limited relief granted trinitarians. Trapped by legal prohibition and fear of prosecution.
constraint_indexing:constraint_classification(revolution_settlement__toleration_settlement_1689, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: ESTABLISHED CHURCH AND CROWN (ROPE) — Primary beneficiary. Toleration Act *reinforces* establishment by legalizing worship outside it under strict conditions. Settles the religious conflict without dismantling establishment tests or privileges. Benefits from coordination (religious peace) with minimal loss (dissenters still excluded from office and honors). Experiences constraint as pure coordination mechanism that stabilizes royal authority.
constraint_indexing:constraint_classification(revolution_settlement__toleration_settlement_1689, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PARLIAMENTARY COALITION FOR SETTLEMENT (SCAFFOLD) — Organized agents (Whigs, moderates, William III) see toleration as a temporary settlement mechanism with a generational sunset: gradual relaxation of Test Acts (repealed 1828, 1829) and eventual Catholic Emancipation (1829) are built into the settlement logic. The constraint has explicit exit pathway — successive parliaments can revise the accommodation. Low effective extraction because the coalition that negotiated the 1689 settlement anticipated its own evolution.
constraint_indexing:constraint_classification(revolution_settlement__toleration_settlement_1689, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: INSTITUTIONAL MEMORY OF RELIGIOUS COMPROMISE (PITON) — Over centuries, the Toleration Act becomes primarily performative: invoked as proof of England's religious liberalism while the substantive machinery (Test Acts, church-state nexus, Catholic exclusion) persists or transforms. The ritual of 'we tolerated dissenters in 1689' replaces functional verification that toleration actually operates. Theater_ratio high because the constraint's legitimacy increasingly depends on its historical narrative rather than its operative mechanisms.
constraint_indexing:constraint_classification(revolution_settlement__toleration_settlement_1689, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational analytical perspective, religious pluralism might be seen as an inevitable feature of complex societies: the gap between state authority and individual conscience is ontologically given, toleration is the natural settlement of an immutable asymmetry. However, this naturalizes what is actually a contingent 1689 political bargain. The engine will detect this as a false summit: the constraint is socially constructed (benefits a specific coalition, excludes specific groups) despite appearing natural or inevitable.
constraint_indexing:constraint_classification(revolution_settlement__toleration_settlement_1689, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(revolution_settlement__toleration_settlement_1689_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(revolution_settlement__toleration_settlement_1689, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(revolution_settlement__toleration_settlement_1689, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(revolution_settlement__toleration_settlement_1689, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(revolution_settlement__toleration_settlement_1689, TR),
    TR >= 0.70.

:- end_tests(revolution_settlement__toleration_settlement_1689_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The Toleration Act reduces suppression for trinitarians but does not end extraction — Test Acts remain in place, office-holding is restricted, social disabilities persist. The act benefits the established church (coordination of settling religious conflict) and the Crown (stabilized authority) at cost to Catholics (explicit exclusion, ongoing penalties) and non-trinitarians (classified as sectarian threats). The extractiveness is not as high as outright persecution (which would be 0.65+) because there is genuine coordination benefit (nonconformist worship is legal, congregations can organize), but extraction persists through civil disabilities and legal exclusions. The intermediate value reflects the constraint's hybrid nature. Over the 30-year interval, extractiveness rises from 0.38 to 0.52 as the constraint hardens: early implementation is tentative and localized; by 1710-1720, the constraint is institutionalized, Test Acts are vigorously enforced, and the expected path toward broader relief stalls. Suppression (0.52): Moderate. Initially (1689) suppression is highest (0.62) because the constraint operates through legal prohibition and fear. Catholic recusancy penalties are enforced; nonconformist congregations operate under legal precarity. Over 30 years, suppression declines (0.42) as enforcement becomes routine and normalized — the constraint no longer requires active coercion, it is woven into institutional practice. Theater ratio (0.35): Low-moderate. The Toleration Act's operative mechanism is relatively straightforward: license dissenters, exclude Catholics, enforce Test Acts. Little performative content early on. Theater rises toward 0.35 as the constraint becomes institutionalized — by the 1720s, the act is invoked as proof of English religious liberty while the substantive Test Acts block office-holding and the machinery of exclusion persists. The theater reflects growing gap between the constraint's narrative (England tolerates dissenters) and its operative mechanics (dissenters are licensed but subordinated).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perspectival fragmentation across six power positions. The trinitarian dissenter sees toleration-with-restrictions (tangled rope) — they are partially liberated but subordinated. The Catholic sees pure extraction (snare) — they are explicitly targeted and excluded while dissenters gain relief. The established church and Crown see coordination (rope) — the constraint settles religious conflict and stabilizes authority without loss. The parliamentary coalition sees a temporary settlement being revised in Parliament (scaffold) — later parliaments will repeal Test Acts (1828–1829) and enact Catholic Emancipation (1829), following the generational sunset logic built into the 1689 bargain. The institutional memory sees ritual invocation of toleration replacing operative toleration (piton) — 'we tolerated dissenters in 1689' becomes the constraint's performative legitimacy as the actual Test Acts fade or change. The civilizational analytical observer risks seeing toleration as an inevitable response to religious pluralism (mountain) — but the structural data reveals this as naturalization: the constraint is contingent on 1689 coalition politics, explicit exclusion of Catholics, and specific power asymmetries. The mountain classification is a false summit, revealing that the constraint's legitimacy depends on narrating political choices as natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each agent's structural relationship to the constraint. Trinitarian dissenters are beneficiaries (toleration granted) but also constrained (Test Acts apply); their d is moderate (~0.50–0.55). The established church is a clear beneficiary with arbitrage options (can negotiate terms with Crown); d is low (~0.15–0.20). Catholics are pure victims with no exit options (recusancy penalties, no path to toleration under this reading); d is maximal (~0.95). The parliamentary coalition negotiated the settlement; they experience low d because they have agency and exit (they can revise the settlement in later parliaments). The piton institutional memory has arbitrage options (can reinterpret the constraint in rhetorical terms); low d. For each power atom, the engine derives d from these beneficiary/victim relationships and applies the sigmoid f(d) to produce experienced extractiveness chi. Beneficiaries with institutional power and arbitrage options experience negative or near-zero chi (coordination, not extraction). Victims with powerless status and trapped exit experience high chi (maximum experienced extraction).
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING RESOLUTION: The toleration_settlement_1689 reading resolves the mandatrophy by identifying this specific constraint (religious toleration through licensed exclusion) as distinct from the bill_of_rights_1689 reading (reordering of Crown powers) and act_of_settlement_1701 reading (succession by statute). Each reading is one structural facet of the settlement. This reading's ε=0.48 (tangled rope) reflects that the constraint is a hybrid: genuine coordination (religious peace) embedded in asymmetric extraction (civil disabilities and exclusions). The constraint is neither pure coordination nor pure extraction, neither natural law nor pure contingency. It is a political settlement that provides real relief (toleration) through manufactured boundaries (trinitarian/non-trinitarian, dissenting/established). The mandatrophy is resolved by recognizing that the settlement's legitimacy depends on maintaining this hybrid structure — any shift to pure coordination (full equality) or to pure extraction (persecution) would destabilize the settlement. The constraint persists because it is structurally unstable but practically necessary: unstable because the boundary (trinitarian toleration) is arbitrary and cannot be maintained indefinitely without appearing unjust; necessary because the alternative (either universal toleration or universal persecution) threatens the political coalition that negotiated the settlement. The piton perspective (institutional memory) captures the constraint's long-term trajectory: over centuries, the operative mechanism (Test Acts, exclusions) decays while the narrative (England tolerates dissenters) persists, until later reform movements (Catholic Emancipation) explicitly replace the 1689 boundary with a new settlement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    trinitarian_boundary_contingency,
    'Why exclude non-trinitarians from toleration if the principle is conscience-based? What makes the trinitarian boundary a principled limit rather than arbitrary exclusion?',
    'Historical analysis: examine pamphlet discourse, parliamentary debates, and theological justifications for why trinity was the boundary. Compare to Catholic and radical exclusions (security vs. doctrinal framing). Test whether the boundary reflects genuine principle or contingent political coalition size.',
    'If principled (genuine security or doctrinal commitment): toleration is a limited settlement constrained by coherent framework. If arbitrary (coalition size, political expediency): reveals extraction mechanism disguised as principled exclusion. Classification shifts from tangled_rope (mixed coordination/extraction) toward snare (extraction camouflaged as principle).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trinitarian_boundary_contingency, conceptual, 'Whether trinitarian boundary reflects principle or arbitrary coalition politics').

omega_variable(
    nonconformist_agency_counterfactual,
    'Did nonconformists secure toleration through negotiating strength, or did the Crown grant it as a tactic to avoid broader religious settlement demands?',
    'Comparative power analysis: nonconformist petition networks, parliamentary representation, threat capacity vs. Crown preferences. Counterfactual: what would the settlement look like if nonconformists had had greater bargaining power? (Likely: no Test Acts at all, immediate office-holding rights, Catholic inclusion.)',
    'If nonconformists had negotiating strength: toleration is a partial victory (tangled_rope classification holds). If Crown tactic: toleration is containment of dissent (shifts toward snare). This is the core ambiguity in whether the constraint represents genuine coordination or managed extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nonconformist_agency_counterfactual, empirical, 'Whether nonconformists negotiated toleration or received it as Crown containment strategy').

omega_variable(
    catholic_exclusion_necessity,
    'Was explicit Catholic exclusion from toleration a structural necessity for settling the religious conflict, or a choice shaped by anti-Catholic ideology and foreign policy (William III''s anti-French positioning)?',
    'Comparative historical analysis: did other Protestant settlements (Dutch, German) require Catholic exclusion, or was it England-specific? Examine whether the 1689 settlement''s wording left logical room for later Catholic inclusion (answer: it did — Emancipation in 1829 required new legislation, not reinterpretation). If room existed in 1689 language, exclusion was choice not necessity.',
    'If necessity: victim set (Catholic population) is a structural feature of the constraint, and classification is stable. If choice: victim set reflects 1689 political coalitions, and the constraint''s extractiveness from Catholics is manufactured extraction, not coordination cost. This determines whether the constraint is tangled_rope (mixed) or closer to snare (suppression mechanism).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(catholic_exclusion_necessity, empirical, 'Whether Catholic exclusion was structural necessity or political choice').

omega_variable(
    reading_kernel_ambiguity,
    'Is the kernel of the Glorious Revolution Settlement a religious truce (this reading) or a constitutional reordering of Crown powers (Bill of Rights reading) or succession law (Act of Settlement reading)?',
    'Examine which constraint (religious toleration, parliamentary supremacy, succession) is most generative of subsequent conflict and reform. The constraint that generates the most persistent political tension is likely the reading that captures the settlement''s true structural content. Prediction: succession law (Act of Settlement) generates least tension; religious toleration generates moderate tension; Bill of Rights generates maximum tension because it leaves executive power ambiguous across centuries.',
    'If religious truce is primary: this reading (toleration_settlement_1689) is the settlement''s core. If constitutional order is primary: this reading is secondary and the Bill of Rights reading is the true kernel. If succession is primary: both are secondary. This affects how much weight the toleration act carries in the settlement''s legitimacy structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kernel_ambiguity, conceptual, 'Which constraint reading (religious, constitutional, succession) captures the kernel''s true structural content').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(revolution_settlement__toleration_settlement_1689, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tolact_tr_t0, revolution_settlement__toleration_settlement_1689, theater_ratio, 0, 0.22).
narrative_ontology:measurement(tolact_tr_t15, revolution_settlement__toleration_settlement_1689, theater_ratio, 15, 0.3).
narrative_ontology:measurement(tolact_tr_t30, revolution_settlement__toleration_settlement_1689, theater_ratio, 30, 0.35).

% Extraction over time
narrative_ontology:measurement(tolact_be_t0, revolution_settlement__toleration_settlement_1689, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(tolact_be_t15, revolution_settlement__toleration_settlement_1689, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(tolact_be_t30, revolution_settlement__toleration_settlement_1689, base_extractiveness, 30, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(tolact_su_t0, revolution_settlement__toleration_settlement_1689, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(tolact_su_t15, revolution_settlement__toleration_settlement_1689, suppression_requirement, 15, 0.55).
narrative_ontology:measurement(tolact_su_t30, revolution_settlement__toleration_settlement_1689, suppression_requirement, 30, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(revolution_settlement__toleration_settlement_1689, identity_coordination).
narrative_ontology:affects_constraint(revolution_settlement__toleration_settlement_1689, revolution_settlement__bill_of_rights_1689).
narrative_ontology:affects_constraint(revolution_settlement__toleration_settlement_1689, revolution_settlement__act_of_settlement_1701).
narrative_ontology:affects_constraint(revolution_settlement__toleration_settlement_1689, test_acts_civil_disabilities).
narrative_ontology:affects_constraint(revolution_settlement__toleration_settlement_1689, catholic_emancipation_1829).

% DUAL FORMULATION NOTE:
% The toleration settlement is one reading of the revolution_settlement kernel. It is structurally distinct from the Bill of Rights reading (which focuses on Crown power) and the Act of Settlement reading (which focuses on succession). All three readings affect constraints in the post-1689 religious and constitutional order: Test Acts, Catholic Emancipation, Repeal of Test Acts. Decomposition follows from the ε-invariance principle: each reading has a stable ε derived from its own structural properties (religious coordination for toleration; constitutional power reordering for Bill of Rights; succession for Act of Settlement). The readings are linked because they are all part of the same historical settlement, but they are distinct constraints because they operate through different mechanisms with different beneficiary/victim structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(revolution_settlement__toleration_settlement_1689, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
