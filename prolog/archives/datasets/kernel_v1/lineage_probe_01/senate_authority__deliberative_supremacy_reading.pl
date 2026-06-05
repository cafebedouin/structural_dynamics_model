% ============================================================================
% CONSTRAINT STORY: senate_authority__deliberative_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_senate_authority__deliberative_supremacy_reading, []).

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
 *   constraint_id: senate_authority__deliberative_supremacy_reading
 *   human_readable: Senate Deliberative Supremacy in the Roman Republic
 *   domain: legal/doctrinal/institutional_authority
 *
 * SUMMARY:
 *   The Senate of the Roman Republic occupies a contested constitutional
 *   position: it is officially advisory to the annually elected magistrates
 *   and assemblies, yet it holds continuous deliberative authority over
 *   finance and foreign policy because 'someone had to hold them between
 *   elections.' This constraint is ONE READING of a kernel contested across
 *   three structural interpretations. The deliberative_supremacy_reading
 *   instantiates the constraint as a coordination mechanism with embedded
 *   extraction: the Senate's continuous deliberation solves the coordination
 *   problem of annual discontinuity (the beneficiary is policy coherence and
 *   institutional memory), but this same continuity suppresses the
 *   magistrate's autonomy and the assembly's voice (the victims are annual
 *   authority and democratic parity). The constraint exhibits tangled_rope
 *   classification from most perspectives because it genuinely coordinates
 *   while also genuinely extracting — the Senate's authority is neither pure
 *   coordination (a Rope) nor pure extraction (a Snare), but a hybrid where
 *   coordination function legitimizes extraction.
 *
 * KEY AGENTS:
 *   - Annual Magistrates: Primary victims (powerless/trapped) — hold formal authority but find finance and foreign policy predetermined by the Senate; face maximum suppression because refusal to serve is political death
 *   - Popular Assembly: Secondary victim (moderate/constrained) — theoretically sovereign but cannot initiate finance or foreign policy; experiences both coordination benefit (stable policy) and extraction (inability to override)
 *   - Experienced Senate Magistrates: Primary beneficiary (institutional/arbitrage) — experience the constraint as coordination that enables their effectiveness; grow in power with age and tenure
 *   - Oligarchic Patrician Families: Organized beneficiary (organized/constrained) — benefit from Senate dominance but also constrained by collective decision-making when dissenting
 *   - Policy Continuity Across Elections: Structural beneficiary (analytical) — abstract but real — the constraint's primary function is to prevent fiscal and diplomatic chaos from magistrate turnover
 *   - Analytical Observer: Seeing the whole structure (analytical/analytical) — risks naturalizing the deliberative_supremacy reading as inevitable necessity rather than recognizing it as one of three contested readings of the same kernel
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(senate_authority__deliberative_supremacy_reading, 0.52).
domain_priors:suppression_score(senate_authority__deliberative_supremacy_reading, 0.68).
domain_priors:theater_ratio(senate_authority__deliberative_supremacy_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(senate_authority__deliberative_supremacy_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(senate_authority__deliberative_supremacy_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(senate_authority__deliberative_supremacy_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(senate_authority__deliberative_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(senate_authority__deliberative_supremacy_reading, "Senate Deliberative Supremacy in the Roman Republic").
narrative_ontology:topic_domain(senate_authority__deliberative_supremacy_reading, "legal/doctrinal/institutional_authority").

domain_priors:requires_active_enforcement(senate_authority__deliberative_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(senate_authority__deliberative_supremacy_reading, 'a45a7afd-4735-4ba8-b3ec-3b928e44a788').
narrative_ontology:cs_kernel_codification('a45a7afd-4735-4ba8-b3ec-3b928e44a788', formalized).
narrative_ontology:cs_authority_grounding('a45a7afd-4735-4ba8-b3ec-3b928e44a788', lineage).
narrative_ontology:cs_interpretation_layer_present('a45a7afd-4735-4ba8-b3ec-3b928e44a788').
narrative_ontology:cs_reading_relation('a45a7afd-4735-4ba8-b3ec-3b928e44a788', senate_authority__advisory_only_reading, coexists_with).
narrative_ontology:cs_reading_relation('a45a7afd-4735-4ba8-b3ec-3b928e44a788', senate_authority__oligarchic_capture_reading, influences).
narrative_ontology:cs_axiom('a45a7afd-4735-4ba8-b3ec-3b928e44a788', foundational, continuous_deliberation_coordinative_necessity).
narrative_ontology:cs_axiom_status(continuous_deliberation_coordinative_necessity, holdable).
narrative_ontology:cs_axiom_grounding('a45a7afd-4735-4ba8-b3ec-3b928e44a788', continuous_deliberation_coordinative_necessity, empirically_contingent).
narrative_ontology:cs_axiom('a45a7afd-4735-4ba8-b3ec-3b928e44a788', foundational, senatorial_expertise_legitimacy).
narrative_ontology:cs_axiom_status(senatorial_expertise_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('a45a7afd-4735-4ba8-b3ec-3b928e44a788', senatorial_expertise_legitimacy, conventional).
narrative_ontology:cs_reference_frame('a45a7afd-4735-4ba8-b3ec-3b928e44a788', continuous_deliberative_republican_authority).
narrative_ontology:cs_drift_state('a45a7afd-4735-4ba8-b3ec-3b928e44a788', late_republic_emperors, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('a45a7afd-4735-4ba8-b3ec-3b928e44a788', '').
narrative_ontology:cs_kernel_id(senate_authority__deliberative_supremacy_reading, senate_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(senate_authority__deliberative_supremacy_reading, policy_continuity_across_elections).
narrative_ontology:constraint_beneficiary(senate_authority__deliberative_supremacy_reading, experienced_magistrates).
narrative_ontology:constraint_victim(senate_authority__deliberative_supremacy_reading, popular_assembly_voice).
narrative_ontology:constraint_victim(senate_authority__deliberative_supremacy_reading, annual_magistrate_authority).
narrative_ontology:constraint_victim(senate_authority__deliberative_supremacy_reading, constitutional_parity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ANNUAL MAGISTRATE (SNARE) — Newly elected consul, praetor, or quaestor discovers that finance and foreign policy are already decided by the Senate of 300 ex-magistrates. The magistrate holds formal authority but cannot reverse deliberations or access the revenue streams and diplomatic channels the Senate controls. Exit is impossible: refusal to serve means political death; service means managing the Senate's predetermined policy within constraints. High suppression, high extraction — the magistrate experiences maximum constraint. The 'Republic' is an illusion; the Senate's continuity makes the magistrate a steward, not a ruler.
constraint_indexing:constraint_classification(senate_authority__deliberative_supremacy_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: POPULAR ASSEMBLY (TANGLED ROPE) — Over a generational horizon, the assembly has some coordination benefit: the Senate's continuity prevents chaotic reversals of foreign alliances and prevents fiscal collapse from magistrate mismanagement. The assembly benefits from predictable policy. However, the assembly also bears extraction: it cannot override the Senate's decisions, cannot direct revenue, and cannot initiate foreign policy. Exit is constrained but real: the assembly could theoretically force magistrates to act against the Senate, but the cost is constitutional crisis and the Senate's organized power makes this costly. Tangled rope captures this mixed experience — real coordination function alongside asymmetric extraction.
constraint_indexing:constraint_classification(senate_authority__deliberative_supremacy_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EXPERIENCED MAGISTRATES (ROPE) — The Senate benefits from coordination it creates. By holding finance and foreign policy continuously, experienced senators solve the collective action problem of annual disruption: magistrates coordinate through the Senate's standing committees and precedent rather than each reinventing policy. The Senate experiences the constraint as beneficial coordination — the mechanism that enables effective governance. They exit costlessly: membership is automatic, and their power grows with age. Rope classification captures genuine coordination benefit that the Senate uses to legitimize its authority.
constraint_indexing:constraint_classification(senate_authority__deliberative_supremacy_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REPUBLICAN CONSTITUTIONAL FORM (PITON) — Over civilizational time, the Senate's authority has become largely performative: the constitutional mythology of 'the Republic' persists (annually elected magistrates, sovereign assembly) while actual power concentrates in the continuous deliberative body. The theatrical elements include formal assembly votes that ratify Senate decisions, magisterial ceremonies that invoke authority magistrates no longer hold, and legal fictions that obscure the Senate's real power. Theater ratio reflects the gap between constitutional form and actual operation. The institution persists through inertia and because alternatives haven't fully replaced it — the Emperor system will eventually do so, but during the Republic's height, the Piton classification captures the degraded, partially theatrical nature of formal institutions.
constraint_indexing:constraint_classification(senate_authority__deliberative_supremacy_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL NECESSITY (MOUNTAIN) — From a civilizational/universal perspective, the constraint appears immutable: **someone** must hold finance and foreign policy continuously, and annual magistrates cannot do it. The gap between elections creates a coordination problem that ANY complex polity must solve. The Senate's solution appears structurally inevitable — the people who know finance and foreign policy are the experienced ex-magistrates, so they will hold it. This perspective risks naturalizing what is actually a doctrinal reading: the claim that the Senate's authority is naturally necessary. The engine's false summit detector will identify this as a false summit, revealing that necessity is contingent on the deliberative_supremacy reading itself.
constraint_indexing:constraint_classification(senate_authority__deliberative_supremacy_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: OLIGARCHIC PATRICIAN FACTION (TANGLED ROPE) — Organized patrician families experience the Senate as a coordination mechanism that benefits their collective interests while also constraining individual families' ability to override others. The faction's power depends on Senate continuity and collective decision-making. However, the faction also experiences the constraint as extraction when dissenting from the consensus: a senior senator with a minority position is constrained by the binding nature of Senate decisions. This perspective shows that even within the beneficiary institutional group, extraction exists — the tangled rope reflects mixed benefit and constraint at the organized power level.
constraint_indexing:constraint_classification(senate_authority__deliberative_supremacy_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(senate_authority__deliberative_supremacy_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(senate_authority__deliberative_supremacy_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(senate_authority__deliberative_supremacy_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(senate_authority__deliberative_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(senate_authority__deliberative_supremacy_reading, TR),
    TR >= 0.70.

:- end_tests(senate_authority__deliberative_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The Senate extracts significant decision-making power from magistrates and the assembly, capturing finance and foreign policy for the continuous body. However, the extraction is NOT as severe as pure capture would be, because the Senate legitimately solves a real coordination problem: annual magistrates genuinely cannot competently manage complex foreign relations and accumulated fiscal commitments. The extractiveness reflects both the real value of continuity and the real asymmetry of power. The measurement trajectory (0.38 → 0.52 over 10 time points) shows rising extraction as oligarchic control hardened — early Republic had more magistrate autonomy; late Republic shows increasing Senate dominance. Suppression (0.68): High. The constraint suppresses magistrate autonomy, assembly initiative, and constitutional parity through multiple mechanisms: (1) control of information and precedent by the continuous body, (2) coordination costs of overriding Senate decisions (requires organizing multiple actors), (3) career consequences for magistrates who defy the Senate, (4) cultural legitimacy of the Senate's 'experience.' The suppression is not total — magistrates occasionally override, assemblies occasionally assert power — but the default state is suppression. Theater ratio (0.55): Moderate. The performative element includes the formal ceremonial roles of magistrates (they appear to hold authority they don't actually wield) and the assembly's nominal sovereignty (it votes on matters the Senate has already decided). However, the theater is not dominant: the Senate's authority rests substantially on real power and real expertise, not pure illusion. The ratio increases over time (0.42 → 0.55) as the gap widens between constitutional form and actual practice.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the perspectival gap across institutional power levels and exit options. The annual magistrate (powerless/trapped) experiences maximum extraction and sees a Snare. The experienced Senate member (institutional/arbitrage) experiences beneficial coordination and sees a Rope. The popular assembly (moderate/constrained) experiences mixed coordination and suppression and sees a Tangled Rope. The analytical observer (analytical/analytical) risks seeing the constraint as a natural necessity (Mountain — 'someone must decide continuously') until omega variables surface the contestation: the constraint is NOT inevitable, but one reading of an ambiguous kernel that could equally support the advisory_only or oligarchic_capture interpretations. The perspectival gap reveals that the 'necessity' claimed by the deliberative_supremacy_reading is actually a doctrinal choice, not a law of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are derived from structural position. Senate magistrates are beneficiaries with arbitrage-level exit (they can decline future service but retain status through seniority) — low d, negative chi, experience the constraint as beneficial (Rope). Annual magistrates are victims with trapped-level exit (refusal means political death, service means reduced autonomy) — high d, high chi, experience maximum extraction (Snare). The assembly is a mixed victim (has theoretical power but suppressed exit options to exercise it) — moderate d, moderate chi (Tangled Rope). The oligarchic faction is an organized beneficiary with constrained exit (they benefit from Senate dominance but dissent is costly) — moderate-low d (Tangled Rope at organized power level). The perceptual gap emerges because the same constraint produces opposite d values for agents at different structural positions: beneficiaries see coordination, victims see extraction, and the same institution's action is simultaneously both.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by recognizing that the deliberative_supremacy_reading CANNOT be reduced to a single type — it exhibits Rope, Tangled Rope, Snare, and Piton functions simultaneously. The mandatrophy question is not 'what type is this?' but 'which reading of the kernel is correct, and what structural phenomena does each reading illuminate?' The false summit (Mountain perspective) tries to naturalize the deliberative_supremacy reading as inevitable ('someone must decide continuously'), but omega variables expose the contestation: the advisory_only reading denies binding force; the oligarchic_capture reading inverts the beneficiary logic. The constraint's actual classification is stable across perspectives when the kernel reading is held constant — it is tangled_rope from the perspective of experienced senators, snare from magistrates, rope from the coordination standpoint — but the classification shifts radically if the kernel reading changes. This is NOT a measurement problem or perspective error; it is a structural feature of contested authority: the same institutional arrangement admits multiple readings, and each reading produces a coherent classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deliberative_supremacy_vs_oligarchic_capture,
    'Is the Senate''s continuous holding of finance and foreign policy a necessary coordination mechanism (''someone must decide''), or is it an institutional capture by ex-magistrates defending their class monopoly?',
    'Historical analysis of Senate decision reversals: how often does the Senate change course on previous decisions? How often do assembled magistrates successfully override or contradict Senate positions? If reversal rate > 30%, mechanism is coordinating rather than capturing. If < 10%, mechanism is capture.',
    'If coordination-driven: deliberative_supremacy reading holds; oligarchic_capture reading is derivative interpretation of the same constraint. If capture-driven: oligarchic_capture reading accurately describes the constraint; deliberative_supremacy reading is legitimizing narrative. Classification shifts from tangled_rope (mixed) toward snare (pure extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(deliberative_supremacy_vs_oligarchic_capture, empirical, 'Whether the Senate''s authority is coordinative necessity or oligarchic capture').

omega_variable(
    constitutional_binding_of_senate_decrees,
    'Do Senate decrees legally bind magistrates and the assembly, or is Senate authority purely advisory and contingent on magisterial/assembly action?',
    'Doctrinal analysis of legal texts, mos maiorum (customary law), and historical cases: did magistrates routinely overturn Senate decisions? Did assemblies authorize contrary actions? The advisory_only_reading hinges on empirical facts about legal bindingness — either Senate decrees had force through precedent and political cost, or they did not.',
    'If binding: deliberative_supremacy reading describes actual authority structure. If advisory: advisory_only_reading is correct, and the constraint is lower-extraction rope or even scaffold (reversible coordination). Classification shifts significantly based on legal vs. political bindingness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_binding_of_senate_decrees, empirical, 'Whether Senate decrees are legally binding or purely advisory').

omega_variable(
    assembly_impulsiveness_suppression,
    'Does the Senate''s control of finance and foreign policy actually suppress popular assembly action, or does it enable assembly action by preventing chaotic reversals?',
    'Comparative analysis: periods when the assembly had direct control vs. periods of Senate dominance; measure policy stability, fiscal sustainability, and foreign policy coherence in each era.',
    'If suppression is real and harmful: the constraint is primarily extractive (snare/tangled_rope). If assembly impulsiveness is genuinely costly and Senate control prevents worse outcomes: the constraint is primarily coordinative (rope/tangled_rope with coordination-first interpretation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(assembly_impulsiveness_suppression, empirical, 'Whether Senate control suppresses harmful assembly impulsiveness or enables assembly power').

omega_variable(
    reading_foreclosure_test,
    'Does the deliberative_supremacy reading logically foreclose the advisory_only_reading within a single constitutional framework, or do both readings coexist as different interpretations of ambiguous texts?',
    'Doctrinal logic: if the Senate is ''the Republic''s mind'' with continuous control over finance and foreign policy, can Senate decrees simultaneously be non-binding advice? The core claims appear contradictory. But historical sources are ambiguous — Senate authority rests on mos maiorum (custom) rather than written law. Ambiguous texts permit coexisting readings.',
    'If foreclosing: deliberative_supremacy reading rules out advisory_only reading within any coherent constitutional reading. If coexisting: both readings are live positions that different historical actors genuinely held. The relation type between the readings shifts from forecloses (logically incompatible) to coexists_with (different camps).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_test, conceptual, 'Whether the readings are logically incompatible or coexist as ambiguous interpretations').

omega_variable(
    extractiveness_timeline,
    'Did the Senate''s extractiveness increase over the Republic''s lifespan as oligarchic capture hardened, or did the constraint maintain a stable structure from the early Republic through the late Republic?',
    'Historical periodization: measure magistrate autonomy, Senate override instances, and oligarchic family dominance across Early/Middle/Late Republic. Rising extraction trajectory suggests oligarchic_capture reading becomes more accurate. Stable extraction suggests deliberative_supremacy reading is consistent across time.',
    'If rising: the constraint drifts from coordination (early) toward capture (late), and periodical stories may differ in classification. If stable: single reading applies across the Republic''s lifespan.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extractiveness_timeline, empirical, 'Extractiveness trajectory over the Republic''s lifespan').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(senate_authority__deliberative_supremacy_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(senad_tr_t0, senate_authority__deliberative_supremacy_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(senad_tr_t5, senate_authority__deliberative_supremacy_reading, theater_ratio, 5, 0.48).
narrative_ontology:measurement(senad_tr_t10, senate_authority__deliberative_supremacy_reading, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(senad_be_t0, senate_authority__deliberative_supremacy_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(senad_be_t5, senate_authority__deliberative_supremacy_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(senad_be_t10, senate_authority__deliberative_supremacy_reading, base_extractiveness, 10, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(senad_su_t0, senate_authority__deliberative_supremacy_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(senad_su_t5, senate_authority__deliberative_supremacy_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(senad_su_t10, senate_authority__deliberative_supremacy_reading, suppression_requirement, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(senate_authority__deliberative_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(senate_authority__deliberative_supremacy_reading, senate_authority__advisory_only_reading).
narrative_ontology:affects_constraint(senate_authority__deliberative_supremacy_reading, senate_authority__oligarchic_capture_reading).
narrative_ontology:affects_constraint(senate_authority__deliberative_supremacy_reading, magistrate_annual_discontinuity_problem).
narrative_ontology:affects_constraint(senate_authority__deliberative_supremacy_reading, roman_fiscal_accumulation_control).

% DUAL FORMULATION NOTE:
% The Senate's authority is a single institutional phenomenon instantiated via three structurally distinct constraint readings. Each reading produces different ε and classification outcomes because they interpret the kernel differently. The deliberative_supremacy_reading treats the Senate's continuity as coordinative necessity (ε=0.52, tangled_rope). The advisory_only_reading treats the same institution as advisory rather than binding (lower ε, rope classification). The oligarchic_capture_reading treats it as pure extraction of ex-magistrate power (higher ε, snare classification). The three stories are linked via the kernel: they are competing interpretations of the same contested authority structure, not separate constraints. The engine's constraint_affects relation models the factual interdependence: all three readings operate on the same Senate institution; changing the interpretation of one affects the others' plausibility.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(senate_authority__deliberative_supremacy_reading, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
