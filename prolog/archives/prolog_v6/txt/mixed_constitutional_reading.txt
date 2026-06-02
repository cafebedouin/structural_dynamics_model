% ============================================================================
% CONSTRAINT STORY: mixed_constitutional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mixed_constitutional_reading, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: mixed_constitutional_reading
 *   human_readable: Mixed Constitutional Authority (Inherited + Delegated Legitimacy)
 *   domain: political_philosophy/constitutional_theory
 *
 * SUMMARY:
 *   Mixed constitutional authority — the framework in which legitimacy
 *   derives from both inherited sources (dynasty, historical continuity,
 *   bloodline succession) and delegated sources (parliament, elected
 *   institutions, constitutional procedures) — creates a distinctive
 *   structural constraint. This reading models the specific case where BOTH
 *   legitimacy sources are treated as equally binding and mutually
 *   constraining. When inherited authority claims prerogative outside
 *   constitutional limits, delegated institutions can block it; when
 *   delegated authority acts outside its chartered scope, inherited authority
 *   can challenge it. This dual-constraint structure generates persistent
 *   institutional tension. Subjects caught between conflicting orders from
 *   the two sources bear extraction with no adjudication authority they can
 *   trust. Constitutional courts, tasked with resolving the ambiguity,
 *   extract power through interpretation monopoly. Delegated institutions
 *   benefit from the constitutional frame but remain contingent on inherited
 *   permission. Inherited authority benefits from the legitimacy
 *   stabilization the constitution provides. The constraint's theater ratio
 *   (0.58) reflects that ceremonial legitimacy claims (coronations, oaths,
 *   symbolic succession) remain high despite declining functional force in
 *   practice. The extractiveness (0.48) reflects that the extraction is real
 *   but intermittent — it escalates during succession crises or authority
 *   conflicts, subsides during stable periods. This constraint is ONE reading
 *   of the contested sovereign_legitimacy kernel; sibling readings
 *   (monarchical and republican) would model different distributional
 *   assumptions about which legitimacy source is primary.
 *
 * KEY AGENTS:
 *   - Inherited Authority (Monarchy/Dynasty): Institutional beneficiary — benefits from constitutional legitimacy stabilization; constrained by constitutional limits but those limits are precisely what make the authority legitimate. Powers include succession control, prerogative claims, ceremonial authority.
 *   - Delegated Authority (Parliament/Legislature): Institutional moderately-extracted — coordinates genuine collective action (legislation, policy) but authority remains contingent on inherited authority's recognition; can be dissolved, overridden, or have powers recalled.
 *   - Constitutional Court/Arbiter: Institutional powerful actor — extracts authority through interpretation monopoly; benefits from the ambiguity between inherited and delegated sources because their role is indispensable for conflict resolution.
 *   - Subject/Citizen Caught in Authority Deadlock: Powerless victim — during periods when inherited and delegated authorities conflict, subjects must obey contradictory commands with no resolution mechanism and no exit.
 *   - Excluded Delegated Authority Claimants: Moderate victims — parties claiming legitimacy through delegated authority but excluded by inherited framework (e.g., revolutionary movements, constitutional reformers).
 *   - Ceremonial/Symbolic Authority Layer: Institutional degraded actor (Piton perspective) — maintains legitimacy theater with high performative content but declining functional force.
 *   - Analytical Observer: Civilizational observer — at risk of naturalizing the mixed framework as inevitable feature of political organization, masking the specific institutional choice.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mixed_constitutional_reading, 0.48).
domain_priors:suppression_score(mixed_constitutional_reading, 0.52).
domain_priors:theater_ratio(mixed_constitutional_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mixed_constitutional_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(mixed_constitutional_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(mixed_constitutional_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mixed_constitutional_reading, tangled_rope).
narrative_ontology:human_readable(mixed_constitutional_reading, "Mixed Constitutional Authority (Inherited + Delegated Legitimacy)").
narrative_ontology:topic_domain(mixed_constitutional_reading, "political_philosophy/constitutional_theory").

domain_priors:requires_active_enforcement(mixed_constitutional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(mixed_constitutional_reading, formalized).
narrative_ontology:cs_authority_grounding(mixed_constitutional_reading, extraction).
narrative_ontology:cs_interpretation_layer_present(mixed_constitutional_reading).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mixed_constitutional_reading, institutional_continuity_agents).
narrative_ontology:constraint_beneficiary(mixed_constitutional_reading, constitutional_gatekeepers).
narrative_ontology:constraint_victim(mixed_constitutional_reading, agents_caught_in_authority_deadlock).
narrative_ontology:constraint_victim(mixed_constitutional_reading, excluded_delegated_authority_claimants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBJECT IN CONSTITUTIONAL DEADLOCK (SNARE) — During periods when inherited and delegated authority sources conflict (e.g., executive overreach vs legislative assertion, monarchical prerogative vs constitutional limits), subjects face maximum extraction: they must obey contradictory commands, have no appeal mechanism, and bear costs of both authority sources' enforcement attempts. No exit available — territorial and legal jurisdiction are total. Pure extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(mixed_constitutional_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DELEGATED AUTHORITY (TANGLED ROPE) — Parliament, legislatures, and delegated bodies genuinely coordinate collective action (legislation, resource allocation, policy-making) while simultaneously suffering asymmetric extraction: their authority is contingent on inherited authority's permission; they can be dissolved, overridden, or have powers recalled. They benefit from the coordination function; they are constrained by the inherited framework.
constraint_indexing:constraint_classification(mixed_constitutional_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INHERITED AUTHORITY (ROPE) — The constitutional monarch or inherited authority source experiences the framework primarily as coordination: the constitutional limit on their power is precisely the mechanism that makes their authority legitimate across time. Without the constitutional frame, they would face endemic challenges and instability; with it, they benefit from stability and recognized succession. The inherited authority is the beneficiary of dual-legitimacy stabilization.
constraint_indexing:constraint_classification(mixed_constitutional_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSTITUTIONAL COURT (TANGLED ROPE) — Judicial or constitutional bodies tasked with resolving conflicts between inherited and delegated authority coordinate dispute resolution (genuine function) while extracting significant power for themselves through interpretation monopoly. They benefit from the authority ambiguity — their role is indispensable precisely because the two sources conflict. They have mobile options (they could theoretically resign or cede authority) but rarely do because their position is enabled by the constraint.
constraint_indexing:constraint_classification(mixed_constitutional_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: CEREMONIAL AUTHORITY (PITON) — Formal state rituals, oath-taking, coronation ceremonies, and symbolic authority declarations persist with high theater but declining functional force. In stable constitutional systems, this layer is substantially performative — it maintains inherited legitimacy symbolically while delegated authority operates practically. Theater ratio is elevated; actual coordination or extraction through this channel has degraded.
constraint_indexing:constraint_classification(mixed_constitutional_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW FRAMING (MOUNTAIN) — From a civilizational/universal analytical perspective, the dual-legitimacy structure can appear as an inevitable feature of institutional stability: all complex polities require both continuity anchors (inherited/dynastic) and responsive mechanisms (delegated/participatory). The constraint appears as natural to political organization as gravity is to physics. However, this risks false summitry — the 'necessity' of mixed authority naturalizes what is actually a specific institutional reading.
constraint_indexing:constraint_classification(mixed_constitutional_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mixed_constitutional_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(mixed_constitutional_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(mixed_constitutional_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(mixed_constitutional_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(mixed_constitutional_reading, TR),
    TR >= 0.70.

:- end_tests(mixed_constitutional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high, trending upward. The constraint begins at 0.32 (periods of stable dual authority coexistence) and rises to 0.48 (increasing conflict between sources). This trajectory reflects institutional drift: as delegated authority consolidates power and inherited authority reasserts traditional prerogatives, the tension between them creates more frequent extraction. The upward trend over the interval suggests the constraint is becoming more extractive over time, not less — a signal of constitutional brittleness rather than stability. Suppression (0.52): Moderate. Agents caught in authority deadlock face significant barriers to exiting (territorial jurisdiction, legal dependency) but some organizational and political recourse exists (petition, legislative appeal, constitutional reform campaigns). Theater ratio (0.58): Moderate-high. Ceremonial legitimacy claims remain substantial (coronation ceremonies, oath-taking, formal succession rituals) despite declining functional verification. The theater is meaningful but not dominant — the constraint still performs some genuine institutional work even as its performative content increases.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits the full range of experienced classification across positions. Inherited authority sees Rope (coordination, legitimacy stabilization, recognized succession path). Delegated institutions see Tangled Rope (genuine legislative function plus extraction through contingency). Subjects in deadlock see Snare (maximum extraction, no exit, no coordination benefit). Constitutional arbiters see Tangled Rope or Rope (depending on whether they recognize their own extraction via interpretation monopoly). The analytical observer risks seeing Mountain (mixed authority as natural political law) but this is a false summit — the constraint naturalizes a contingent institutional choice. The perspectival spread (from Rope to Snare) is maximal, indicating high structural tension: different observers have fundamentally incompatible experiences of the same institutional framework.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values reflect each agent's structural position relative to authority extraction. Inherited authority, as primary beneficiary of dual-legitimacy stabilization, derives low d (0.10-0.20): they capture citation authority and legitimacy surplus while their power is constrained. Delegated institutions derive moderate d (0.50-0.65): they coordinate genuine function but bear contingency costs. Subjects caught in deadlock derive maximum d (0.90-0.95): they bear costs of conflicting commands with no exit. The constitutional court derives moderate-high d (0.55-0.70) despite institutional power because their extracted authority relies on the constraint's persistence — if the dual-legitimacy ambiguity were resolved, the court's interpretive power would diminish. This creates a perverse incentive: the arbiter benefits from maintaining ambiguity. The engine's directionality computation from these structural positions produces chi values aligned with the Tangled Rope classification: coordination function (delegated authority genuinely legislates) plus asymmetric extraction (contingency costs flow from delegated to inherited).
 *
 * MANDATROPHY ANALYSIS:
 *   MIXED CONSTITUTIONAL READING MANDATROPHY: This constraint resolves mandatrophy by clearly separating coordination function from extraction mechanism. Coordination function: delegated authority (parliament, legislatures) genuinely coordinates collective action — they pass laws, allocate resources, represent constituencies. Extraction mechanism: inherited authority maintains contingency over delegated authority, preventing full autonomy and extracting legitimacy surplus through the constitutional frame. The tangled rope type correctly captures both: it is neither pure coordination (rope) nor pure extraction (snare), but a hybrid where both mechanisms operate simultaneously. The mandate of mixed constitutional authority is both to enable governance (coordination) and to preserve inherited authority (extraction). Resolving mandatrophy requires refusing to collapse the hybrid into either pure type and instead recognizing that the system's design embeds both functions inextricably.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_decomposition_sibling_readings,
    'What is the relationship between mixed_constitutional_reading and its sibling readings (monarchical_reading, republican_reading)?',
    'This constraint instantiates ONE specific reading of the contested sovereign_legitimacy kernel. Sibling readings would generate separate constraint stories with different ε values, different victim sets, and different classification patterns. The monarchical reading emphasizes inherited authority as primary (lower ε, rope-dominant); the republican reading emphasizes delegated authority as primary (higher ε, tangled_rope or snare-dominant). Mixed reading treats both as equally binding — dual legitimacy sources create structural deadlock.',
    'If the monarchical reading is correct: ε should be ~0.25, beneficiaries shift to dynastic continuity only. If the republican reading is correct: ε should be ~0.62, beneficiaries shift to participatory institutions only. If mixed reading is correct: ε ≈ 0.48, deadlock between sources creates persistent extraction of agents caught between them.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_decomposition_sibling_readings, conceptual, 'Relationship between this reading and sibling readings of sovereign_legitimacy kernel').

omega_variable(
    constitutional_deadlock_empirical_trigger,
    'Under what empirical conditions does the mixed-authority constraint transition from latent (suppressed, low theater) to active (high extraction, high theater)?',
    'Historical case analysis: periods of stable constitutional balance vs periods of authority conflict (e.g., 1936-1938 UK abdication crisis, 2019-2021 Australian constitutional questions, ongoing Israeli constitutional debates). Measurement: extractiveness and suppression during stable periods vs crisis periods.',
    'If deadlock is frequent/regular: constraint is Tangled Rope or Snare even at stable time points (structural, not cyclical). If deadlock is rare/triggered: constraint is Rope or Scaffold at rest, escalates only during specific triggers (cyclical/temporal).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_deadlock_empirical_trigger, empirical, 'Empirical conditions triggering constitutional deadlock escalation').

omega_variable(
    interpretation_monopoly_extraction,
    'Does the constitutional court''s authority to interpret the dual-legitimacy structure constitute genuine dispute resolution (coordination) or extractive authority monopoly?',
    'Comparative analysis: courts in mixed constitutional systems (UK, Canada, Netherlands) vs systems with explicit supreme arbiter (US); measurement of discretionary interpretation expansion over time; analysis of when courts rule in favor of inherited vs delegated authority (asymmetry indicates capture).',
    'If courts genuinely arbitrate: they are net coordinators (lower d for judicial institution). If courts extract via interpretation monopoly: they are net extractors (higher d, possibly beneficiaries of the ambiguity). Resolving this shifts the court perspective from rope to snare or vice versa.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretation_monopoly_extraction, empirical, 'Whether constitutional interpretation is coordination or extractive monopoly').

omega_variable(
    succession_constraint_interaction,
    'Does the mixed constitutional framework constrain succession dynamics (bloodline meets constitutional limits) in ways that create unique extraction mechanisms distinct from purely inherited or purely delegated systems?',
    'Comparative succession analysis: cases where constitutional limits prevented dynastic succession or where bloodline legitimacy prevented constitutional deadlock resolution. Measurement: frequency and severity of succession crises in mixed vs pure systems.',
    'If succession is genuinely constrained: creates distinctive victim set (dynastic heirs excluded by constitutional process, or constitutional officers blocked by hereditary claims). If succession is unconstrained: the dual-legitimacy structure is primarily theatrical, ε should be lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(succession_constraint_interaction, empirical, 'Unique succession constraints created by mixed constitutional framework').

omega_variable(
    delegated_authority_permanence,
    'In mixed constitutional systems, can delegated authority (parliament, legislature) become a genuine independent power source, or does it remain permanently contingent on inherited authority''s permission?',
    'Historical trajectory analysis: does delegated authority consolidate power over generations (UK parliament 1200-2026), or does inherited authority periodically reassert primacy (pattern of dissolution/reconvocation)? Measurement: ratio of inherited authority interventions to delegated authority autonomy over generational timescales.',
    'If delegation consolidates: constraint transitions toward Rope or Scaffold (sunset logic as inherited authority becomes vestigial). If delegation remains contingent: constraint remains Tangled Rope or Snare (extraction is structural, not temporary).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(delegated_authority_permanence, empirical, 'Whether delegated authority consolidates independence or remains permanently contingent').

omega_variable(
    false_summit_mixed_authority_natural_necessity,
    'Is the mixed constitutional framework (dual legitimacy, inherited + delegated) presented as a natural and inevitable feature of stable governance, when it is actually a contingent institutional choice?',
    'Discourse analysis: do constitutional theorists, legal authorities, and institutional actors frame mixed authority as ''how complex polities must be organized'' vs ''how this specific polity chose to organize itself''? Historical analysis: alternative constitutional architectures without mixed legitimacy (pure republican, pure monarchical, pure delegated) and their stability profiles.',
    'If presented as necessary natural law: the mountain perspective gains traction, and ε appears lower (necessity framing suppresses recognition of extraction). If recognized as contingent choice: ε remains higher, and the snare/tangled_rope classifications hold. This is a false summit candidate — beneficiaries (constitutional gatekeepers, judicial interpreters) benefit from naturalizing the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_mixed_authority_natural_necessity, conceptual, 'Risk of false summit: naturalizing contingent mixed authority as inevitable governance law').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mixed_constitutional_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mixed_const_theater_t0, mixed_constitutional_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(mixed_const_theater_t5, mixed_constitutional_reading, theater_ratio, 5, 0.55).
narrative_ontology:measurement(mixed_const_theater_t10, mixed_constitutional_reading, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(mixed_const_extract_t0, mixed_constitutional_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(mixed_const_extract_t5, mixed_constitutional_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(mixed_const_extract_t10, mixed_constitutional_reading, base_extractiveness, 10, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mixed_constitutional_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(mixed_constitutional_reading, monarchical_reading).
narrative_ontology:affects_constraint(mixed_constitutional_reading, republican_reading).
narrative_ontology:affects_constraint(mixed_constitutional_reading, constitutional_succession_deadlock).
narrative_ontology:affects_constraint(mixed_constitutional_reading, regulatory_capture_institutional_authority).

% DUAL FORMULATION NOTE:
% Mixed constitutional authority is one decomposition of the sovereign_legitimacy kernel (constraint family: monarchical_reading, republican_reading, mixed_constitutional_reading). Each reading generates a separate constraint story with distinct ε values, victim sets, and classification patterns. The readings are NOT alternatives to be chosen, but structurally distinct constraints that coexist in the institutional ecosystem. A polity implementing mixed authority instantiates all three constraints simultaneously: the monarchical reading as inherited authority's structural position, the republican reading as delegated authority's structural position, and this mixed reading as the institutional framework managing the conflict between them. Network linking enables contamination analysis: if the mixed reading's stability degrades (ε rises above 0.70), both sibling readings escalate as the breakdown becomes explicit.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
