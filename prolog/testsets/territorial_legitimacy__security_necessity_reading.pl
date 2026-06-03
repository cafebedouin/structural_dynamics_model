% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy__security_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy__security_necessity_reading, []).

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
 *   constraint_id: territorial_legitimacy__security_necessity_reading
 *   human_readable: Territorial Legitimacy via Security Necessity (1967 + Strategic Depth Reading)
 *   domain: political_theory/international_law/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of the contested kernel
 *   'territorial legitimacy' — the security-necessity reading, which grounds
 *   Israeli territorial control of the West Bank and Golan Heights in
 *   defensive necessity and strategic buffer logic. The reading claims that
 *   maintaining control of territory beyond the 1967 borders (pre-war
 *   armistice lines) is justified because Palestinian statehood without
 *   strategic limitations poses an existential threat to Israel; therefore,
 *   Israeli security control is a necessary coordination mechanism, not
 *   extractive occupation. This reading coexists with the partition reading
 *   (legitimacy via UN Resolution 181 and international recognition of 1948
 *   borders) and the indigenous continuity reading (legitimacy via continuous
 *   Palestinian habitation and anti-colonial self-determination). The
 *   security-necessity reading is one institutional framework's answer to the
 *   question: 'What makes territorial control legitimate?' Its empirical
 *   claim is that the threat to Israel justifies the constraint; its
 *   normative claim is that security necessity overrides territorial
 *   partition norms when existential stakes are present. The constraint
 *   exhibits a sharp perspectival gap: Israeli security actors and settlement
 *   advocates experience it as rational coordination (rope); Palestinian
 *   civilians experience it as pure extraction (snare); the international
 *   legal system experiences it as generating both coordination benefits
 *   (security buffer precedent) and extraction costs (norm violation).
 *   Extractiveness has risen over the 40-year interval from 0.38 (early
 *   occupation, arguably temporary) to 0.68 (contemporary, with deep
 *   institutional entrenchment and settlement expansion). Suppression has
 *   similarly risen from 0.58 to 0.72, indicating that the enforcement
 *   machinery required to maintain the constraint has intensified over time.
 *
 * KEY AGENTS:
 *   - Israeli State Security Establishment: Primary beneficiary (institutional/arbitrage) — maintains strategic depth and territorial buffer; primary framework for evaluating constraint as rational
 *   - Palestinian Civilian Population: Primary victim (powerless/trapped) — bears costs of security regime, settlement expansion, military occupation; no exit options
 *   - Palestinian Authority: Secondary actor (moderate/constrained) — has limited agency within occupation framework; dependent on external legitimacy; bears administrative costs
 *   - Israeli Settlement Movement / Right-Wing Coalition: Organized beneficiary (organized/arbitrage) — uses security-necessity framing to justify territorial expansion; high organizational capacity to enforce constraint
 *   - International Legal System / Global Security Framework: Institutional observer (institutional/mobile) — experiences constraint as generating precedent for security exceptions to territorial integrity norm; has theoretically high exit capacity but institutional inertia blocks enforcement
 *   - Academic/Policy Security Studies Community: Piton institutional actor (institutional/constrained) — performs repeated threat assessment without fundamental updating; maintains constraint through discourse legitimation
 *   - Analytical Observer / Security Realism: Civilizational observer (analytical/analytical) — risks naturalizing constraint as immutable feature of anarchic international system
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy__security_necessity_reading, 0.68).
domain_priors:suppression_score(territorial_legitimacy__security_necessity_reading, 0.72).
domain_priors:theater_ratio(territorial_legitimacy__security_necessity_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy__security_necessity_reading, snare).
narrative_ontology:human_readable(territorial_legitimacy__security_necessity_reading, "Territorial Legitimacy via Security Necessity (1967 + Strategic Depth Reading)").
narrative_ontology:topic_domain(territorial_legitimacy__security_necessity_reading, "political_theory/international_law/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy__security_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy__security_necessity_reading, 'da3fe7ac-beca-4336-9de5-70ca2b9c06a6').
narrative_ontology:cs_kernel_codification('da3fe7ac-beca-4336-9de5-70ca2b9c06a6', formalized).
narrative_ontology:cs_authority_grounding('da3fe7ac-beca-4336-9de5-70ca2b9c06a6', extraction).
narrative_ontology:cs_interpretation_layer_present('da3fe7ac-beca-4336-9de5-70ca2b9c06a6').
narrative_ontology:cs_reading_relation('da3fe7ac-beca-4336-9de5-70ca2b9c06a6', territorial_legitimacy__partition_reading, coexists_with).
narrative_ontology:cs_reading_relation('da3fe7ac-beca-4336-9de5-70ca2b9c06a6', territorial_legitimacy__indigenous_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('da3fe7ac-beca-4336-9de5-70ca2b9c06a6', foundational, existential_threat_justifies_territorial_buffer).
narrative_ontology:cs_axiom_status(existential_threat_justifies_territorial_buffer, holdable).
narrative_ontology:cs_axiom_grounding('da3fe7ac-beca-4336-9de5-70ca2b9c06a6', existential_threat_justifies_territorial_buffer, empirically_contingent).
narrative_ontology:cs_axiom('da3fe7ac-beca-4336-9de5-70ca2b9c06a6', foundational, military_occupation_necessarily_temporary_security_measure).
narrative_ontology:cs_axiom_status(military_occupation_necessarily_temporary_security_measure, overridden).
narrative_ontology:cs_axiom_grounding('da3fe7ac-beca-4336-9de5-70ca2b9c06a6', military_occupation_necessarily_temporary_security_measure, deontological).
narrative_ontology:cs_reference_frame('da3fe7ac-beca-4336-9de5-70ca2b9c06a6', temporary_occupation_security_emergency).
narrative_ontology:cs_drift_state('da3fe7ac-beca-4336-9de5-70ca2b9c06a6', contemporary_57_year_entrenchment, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('da3fe7ac-beca-4336-9de5-70ca2b9c06a6', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(territorial_legitimacy__security_necessity_reading, territorial_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy__security_necessity_reading, israeli_state_security_apparatus).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__security_necessity_reading, israeli_settlement_movement).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__security_necessity_reading, right_wing_coalition_parties).
narrative_ontology:constraint_victim(territorial_legitimacy__security_necessity_reading, palestinian_civilian_population).
narrative_ontology:constraint_victim(territorial_legitimacy__security_necessity_reading, palestinian_state_sovereignty).
narrative_ontology:constraint_victim(territorial_legitimacy__security_necessity_reading, post_1967_border_viability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PALESTINIAN CIVILIANS (SNARE) — Trapped under perpetual security regime justified by existential threat narrative. Exit options (relocation, sovereignty recognition, economic mobility) are structurally blocked by the constraint itself. No viable exit; maximum experienced extraction. Sees the constraint as pure coercion with no coordination benefit.
constraint_indexing:constraint_classification(territorial_legitimacy__security_necessity_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: PALESTINIAN AUTHORITY (TANGLED ROPE) — Constrained by dependence on external legitimacy (UN recognition, Arab League endorsement) while bearing costs of administrative oversight under security regime. Has some agency (negotiating capacity, institutional structure) but high costs to exit (state fragmentation, loss of international standing). Mixed experience: some coordination functions (inter-Palestinian governance) alongside significant extraction (military occupation framework).
constraint_indexing:constraint_classification(territorial_legitimacy__security_necessity_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ISRAELI SECURITY ESTABLISHMENT (ROPE) — Primary beneficiary. Experiences the constraint as coordination mechanism: maintaining territorial buffer reduces existential threat perception and provides strategic depth. High exit capacity (can choose alternative security models); exits from this constraint would mean fundamental strategic reorientation. Sees constraint as solving coordination problem of defensive perimeter maintenance.
constraint_indexing:constraint_classification(territorial_legitimacy__security_necessity_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: SETTLEMENT MOVEMENT / RIGHT-WING COALITION (ROPE) — Organized beneficiary with high exit capacity. Experiences constraint as enabling their central narrative and policy agenda (territorial expansion justified by security, Zionist historical claim validated by strategic necessity). Can exit via ideological shift but chooses not to because constraint serves their fundamental objective.
constraint_indexing:constraint_classification(territorial_legitimacy__security_necessity_reading, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: INTERNATIONAL LEGAL SYSTEM (TANGLED ROPE) — Globalist perspective. The constraint generates both coordination benefits (precedent for territorial buffer logic, security exception doctrine for international law) and significant extraction (undermines post-1945 territorial integrity norm, enables sovereignty-exceptional occupation regimes). Mobile exit capacity (can enforce alternative norms) but institutional inertia and major-power use of same exception logic blocks collective action.
constraint_indexing:constraint_classification(territorial_legitimacy__security_necessity_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: SECURITY STUDIES DISCOURSE (PITON) — Academic and policy community treating security-necessity framing as established fact rather than contestable reading. Performative citation of threat assessments; theater ratio reflects repeated invocation of same threat models without updating empirical verification. The discourse persists through institutional inertia (career incentives for continuity, reputational cost of revisionism) rather than through active updating of threat assessment. Theater ratio 0.58 indicates moderate performativity.
constraint_indexing:constraint_classification(territorial_legitimacy__security_necessity_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / SECURITY REALISM (MOUNTAIN) — From civilizational/universal perspective, territorial buffer legitimacy follows from anarchic international system structure: states lacking strategic depth face extinction risk; security dilemma makes buffer acquisition rational regardless of good faith. This perspective risks naturalizing the constraint as an immutable law of state survival. False summit candidate: structural data (identifiable beneficiaries, suppression of alternatives, measurable extraction) suggests the 'natural necessity' framing obscures a contingent institutional arrangement.
constraint_indexing:constraint_classification(territorial_legitimacy__security_necessity_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy__security_necessity_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(territorial_legitimacy__security_necessity_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(territorial_legitimacy__security_necessity_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy__security_necessity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(territorial_legitimacy__security_necessity_reading, TR),
    TR >= 0.70.

:- end_tests(territorial_legitimacy__security_necessity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The security-necessity reading justifies sustained Israeli control of Palestinian territory and settlements. The original research group (Israeli security apparatus) captures strategic advantage (territorial buffer, settlement expansion rights, control of water resources, military positioning) during the indefinite 'security emergency.' The extraction is measured at 0.68 rather than higher because: (1) some genuine coordination functions exist (preventing hostile forces from adjacent territory is a real strategic problem); (2) the constraint is partially offset by Palestinian Authority institutional capacity (though severely limited); (3) some ambiguity remains about whether the threat magnitude actually justifies the territorial extent. The extractiveness trajectory (0.38 → 0.68 over 40 years) shows accumulation of extraction as the temporary security measure became permanent institutional arrangement with settlement expansion. Suppression (0.72): High. Significant barriers to Palestinian sovereignty autonomy include: Israeli military control of territory, restriction on Palestinian military capacity, control of border crossings and water resources, settlement expansion blocking contiguous Palestinian territory, administrative detention and security detention mechanisms, restriction on freedom of movement. These are not merely regulatory barriers but structural blockades to exit. Theater ratio (0.58): Moderate-high. The security-necessity framing involves substantial performative elements: repeated invocation of threat scenarios without updating threat assessment, security discourse that conflates security infrastructure needs with settlement expansion justification, rhetorical separation of 'temporary' occupation from 'permanent' territorial settlement despite 57-year entrenchment. The theater is not complete (actual security infrastructure is operationally functional) but the legitimating discourse around it has high performative content.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The Israeli security establishment sees rope (solving the coordination problem of defensive perimeter). The settlement movement sees rope (territorial acquisition justified by security need). The Palestinian civilians see snare (pure extraction, no exit). The Palestinian Authority sees tangled rope (some institutional functions but high costs). The international legal system sees tangled rope (both coordination benefits from precedent and extraction costs from norm violation). The security studies discourse sees piton (performative invocation of threat narratives). The analytical observer risks seeing mountain (naturalizing security-necessity as immutable feature of international anarchy) — false summit candidate. The security-necessity reading coexists with partition reading (both live positions) but influences the indigenous continuity reading (creates structural pressure against anti-colonial framing).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) in this constraint are derived from structural position. Israeli security establishment: beneficiary with arbitrage exit → low d → negative effective extraction (they experience the constraint as a benefit, not cost). Palestinian civilians: victims with trapped exit → high d (0.95) → high f(d) (1.42) → experienced chi well above base extractiveness. Palestinian Authority: victims with constrained exit (higher cost than arbitrage but lower than trapped) → moderate-high d → moderate-high chi. The perspectival gap reflects these divergent d values computed from the same ε. No directionality overrides are necessary; the derivation chain from beneficiary/victim + exit options produces the correct perspectival gap.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: This constraint is classified as snare at the primary (powerless/trapped) perspective and represents genuine extraction with high suppression — the mandatrophy is resolved by accepting that security-necessity framing does not eliminate the snare classification at the victim perspective. The constraint solves a real coordination problem (Israeli security) but does so through structural extraction (Palestinian territorial loss and sovereignty limitation). The mandatrophy is not 'is this coordination or extraction?' but 'can a constraint be both?' YES — when one agent's coordination need is another agent's extraction mechanism. The security-necessity reading classifies the constraint as snare from the victim perspective while rope from the beneficiary perspective. Both classifications are correct for their respective agents. The mandatrophy is fully resolved at ε=0.68 by the perspectival framework itself: the constraint IS legitimated as security necessity from the beneficiary position and IS experienced as pure extraction from the victim position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threat_assessment_empiricism,
    'Is the existential threat from Palestinian autonomy empirically validated, or does the threat magnitude justify the constraint only under particular strategic doctrine assumptions?',
    'Longitudinal threat assessment comparison: post-1967 vs pre-1967 attack rates, capability analysis of Palestinian military capacity relative to Israeli defense systems, counterfactual modeling of Palestinian state military limits under international agreement',
    'If threat is empirically validated: constraint classification remains snare (unavoidable security necessity). If threat is doctrine-dependent: constraint reclassifies as tangled_rope or worse (extractive arrangement justified by selective threat narrative).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threat_assessment_empiricism, empirical, 'Whether the claimed existential threat is empirically validated or doctrine-dependent').

omega_variable(
    alternative_security_architecture_feasibility,
    'Are demilitarized Palestinian state, international guarantees, technological defense (missile systems, cyber), and early-warning arrangements genuinely insufficient alternatives to territorial buffer acquisition?',
    'Technical security analysis: comparative defense modeling under alternative scenarios. Historical case studies of states without territorial buffers (island nations, nuclear-armed states, treaty-protected states). Feasibility assessment of multilateral security guarantees and enforcement mechanisms.',
    'If alternatives are insufficient: snare classification stands (unavoidable extraction). If alternatives are viable: constraint reclassifies as tangled_rope or snare with reduced justification (extraction mechanism has alternatives but is maintained for political/ideological reasons).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_security_architecture_feasibility, empirical, 'Whether alternative security architectures are genuinely insufficient').

omega_variable(
    reading_framework_committer_ambiguity,
    'Is security-necessity a foundational organizing principle for territorial legitimacy, or a contingent post-hoc justification for territorial claims grounded in other commitments (settler-colonial ideology, religious claim, historical trauma)?',
    'Historical genealogy of the security-necessity reading: timeline of when security rationale became primary in Israeli policy discourse vs. when territorial claims originated. Counterfactual test: if security threat dissolved (through multilateral agreement, technological shift, threat elimination), would the constraint be voluntarily relinquished? Policy statements and settlement expansion patterns as indicators of underlying commitment ordering.',
    'If security is foundational: reading stands as stated. If security is post-hoc: constraint instantiates a different reading (hidden settler-colonial or theological reading) wearing security-necessity language; the actual constraint structure is misclassified until underlying reading is made explicit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_framework_committer_ambiguity, conceptual, 'Whether security-necessity is foundational or post-hoc justification for territorial claims').

omega_variable(
    axiom_overriding_drift,
    'Has the ''military occupation as temporary security measure'' axiom been overridden by 57+ years of entrenchment, institutional development, and settlement expansion that make the regime''s temporary character increasingly implausible?',
    'Timeline analysis of occupation institutional development (administrative structure growth, settlement count/area, integration of Palestinian territory into Israeli infrastructure). Policy statements and legal frameworks treating occupation as permanent vs. temporary. International legal challenges based on axiom override (occupation must be temporary by definition).',
    'If axiom is overridden: drift_state shows substantial magnitude authority_erosion. Constraint reclassifies to explicitly extractive snare (dropped the temporary security measure language). If axiom remains holdable: constraint maintains current classification (snare justified by security necessity, not entrenched settlement).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(axiom_overriding_drift, empirical, 'Whether the temporary occupation axiom has been overridden by 57 years of institutional entrenchment').

omega_variable(
    settlement_legitimacy_boundary,
    'Do Israeli settlements represent legitimate security presence (military forward positions) or colonial settlement (civilian territorial acquisition), and does the security-necessity reading coherently justify both simultaneously?',
    'Classification of settlement types: military/security infrastructure vs. civilian housing. Temporal analysis of settlement expansion relative to security threats (do expansions track actual threat increases or do they follow political cycles?). Legal status analysis: are settlements treated as security infrastructure (temporary, revocable) or private property (permanent, transferable)?',
    'If settlements are security infrastructure: reading is internally coherent. If settlements are civilian colonial settlement: they violate the security-necessity logic; constraint reclassifies as snare wearing security language (actual function is territorial acquisition, not security).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(settlement_legitimacy_boundary, empirical, 'Whether settlements are security infrastructure or colonial settlement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy__security_necessity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_sec_theater_t0, territorial_legitimacy__security_necessity_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(terr_sec_theater_t20, territorial_legitimacy__security_necessity_reading, theater_ratio, 20, 0.5).
narrative_ontology:measurement(terr_sec_theater_t40, territorial_legitimacy__security_necessity_reading, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(terr_sec_extract_t0, territorial_legitimacy__security_necessity_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(terr_sec_extract_t20, territorial_legitimacy__security_necessity_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(terr_sec_extract_t40, territorial_legitimacy__security_necessity_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(terr_sec_suppress_t0, territorial_legitimacy__security_necessity_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(terr_sec_suppress_t20, territorial_legitimacy__security_necessity_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(terr_sec_suppress_t40, territorial_legitimacy__security_necessity_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy__security_necessity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(territorial_legitimacy__security_necessity_reading, territorial_legitimacy__partition_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__security_necessity_reading, territorial_legitimacy__indigenous_continuity_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__security_necessity_reading, palestinian_state_viability).
narrative_ontology:affects_constraint(territorial_legitimacy__security_necessity_reading, settlement_expansion_dynamics).
narrative_ontology:affects_constraint(territorial_legitimacy__security_necessity_reading, international_humanitarian_law_exception_doctrine).

% DUAL FORMULATION NOTE:
% This constraint is ONE reading of the territorial_legitimacy kernel. The sibling constraints (partition_reading, indigenous_continuity_reading) are structurally distinct constraint stories with different ε values and beneficiary/victim structures. All three stories should be generated and linked via network.affects_constraints to model the kernel decomposition. The security_necessity_reading has high extractiveness (0.68) and snare classification; the partition_reading has moderate extractiveness and tangled_rope classification; the indigenous_continuity_reading has high extractiveness and snare classification. The network edges indicate that these three stories are readings of the same kernel and structurally influence one another.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
