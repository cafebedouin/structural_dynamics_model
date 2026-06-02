% ============================================================================
% CONSTRAINT STORY: second_amendment_arms_right__collective_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_arms_right__collective_right_reading, []).

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
 *   constraint_id: second_amendment_arms_right__collective_right_reading
 *   human_readable: Second Amendment: Collective Right Reading (State Militia Authority)
 *   domain: constitutional_law/political_philosophy/legal_interpretation
 *
 * SUMMARY:
 *   The Second Amendment collective-right reading frames constitutional
 *   protection for arms as a structural guarantee to STATE GOVERNMENTS, not
 *   individuals. Under this interpretation, the right protects the capacity
 *   of states to maintain militia organizations independent of federal
 *   disarmament; it does NOT protect civilian firearm ownership outside
 *   militia context. States retain plenary authority to regulate, restrict,
 *   or prohibit individual possession of firearms. This reading generates a
 *   tangled coordination-extraction hybrid because it legitimately solves a
 *   federalism problem (preventing federal control of state militia) while
 *   enabling state extraction from individuals seeking civilian access to
 *   arms. The collective-right reading was the dominant constitutional
 *   interpretation for much of the 20th century but has been significantly
 *   challenged by the individual-right reading since the 2000s. The reading
 *   is coherent but depends on a specific understanding of the Amendment's
 *   functional purpose (militia protection rather than individual
 *   self-defense) and a specific historical frame (18th-century state militia
 *   autonomy). The measurement trajectory shows rising extractiveness and
 *   theater_ratio over the interval (time_point 0–200), reflecting erosion of
 *   the reading's apparent functional necessity (as modern militias become
 *   integrated with federal structures) and accumulation of regulatory
 *   extraction (as states exercise their asserted plenary authority). The
 *   suppression requirement remains high throughout because individuals
 *   outside militia context have limited exit options and no federal
 *   constitutional appeal under this reading.
 *
 * KEY AGENTS:
 *   - State Governments: Primary beneficiary (institutional/arbitrage) — retain full authority to regulate arms; extract from civilians seeking access while maintaining coordination with federal boundaries
 *   - Militia Organizations / National Guard: Secondary beneficiary (organized/constrained) — protected from federal disarmament but subject to state hierarchical control; genuine coordination function with state-level extraction
 *   - Individuals Outside Militia Context: Primary victim (powerless/trapped) — no federal constitutional protection; subject to state plenary authority; no meaningful exit from regulatory jurisdiction
 *   - Federal Government: Structural counterparty (institutional/arbitrage) — constrained from imposing uniform firearms policy; benefits from federalism boundary protection; low chi because the constraint is designed to limit federal extraction
 *   - Firearm Owners Seeking Multi-State Access: Secondary victim (moderate/constrained) — face fragmented state regulation; can relocate but at high cost; no federal constitutional standard
 *   - Analytical Constitutional Observer: Positioned as risk of naturalizing contingent historical reading as immutable law (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_arms_right__collective_right_reading, 0.52).
domain_priors:suppression_score(second_amendment_arms_right__collective_right_reading, 0.65).
domain_priors:theater_ratio(second_amendment_arms_right__collective_right_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_arms_right__collective_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_arms_right__collective_right_reading, "Second Amendment: Collective Right Reading (State Militia Authority)").
narrative_ontology:topic_domain(second_amendment_arms_right__collective_right_reading, "constitutional_law/political_philosophy/legal_interpretation").

domain_priors:requires_active_enforcement(second_amendment_arms_right__collective_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_arms_right__collective_right_reading, 'uuid-placeholder-collective-reading').
narrative_ontology:cs_kernel_codification('uuid-placeholder-collective-reading', fixed_text).
narrative_ontology:cs_authority_grounding('uuid-placeholder-collective-reading', lineage).
narrative_ontology:cs_interpretation_layer_present('uuid-placeholder-collective-reading').
narrative_ontology:cs_reading_relation('uuid-placeholder-collective-reading', second_amendment_arms_right__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('uuid-placeholder-collective-reading', second_amendment_arms_right__civic_republican_reading, coexists_with).
narrative_ontology:cs_axiom('uuid-placeholder-collective-reading', foundational, right_held_by_state_not_individual).
narrative_ontology:cs_axiom_status(right_held_by_state_not_individual, holdable).
narrative_ontology:cs_axiom_grounding('uuid-placeholder-collective-reading', right_held_by_state_not_individual, conventional).
narrative_ontology:cs_axiom('uuid-placeholder-collective-reading', foundational, militia_protection_justifies_state_regulatory_authority).
narrative_ontology:cs_axiom_status(militia_protection_justifies_state_regulatory_authority, holdable).
narrative_ontology:cs_axiom_grounding('uuid-placeholder-collective-reading', militia_protection_justifies_state_regulatory_authority, instrumental).
narrative_ontology:cs_reference_frame('uuid-placeholder-collective-reading', state_militia_autonomy_framework).
narrative_ontology:cs_drift_state('uuid-placeholder-collective-reading', contemporary_national_guard_integration, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('uuid-placeholder-collective-reading', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(second_amendment_arms_right__collective_right_reading, second_amendment_arms_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__collective_right_reading, state_governments).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__collective_right_reading, militia_authority_holders).
narrative_ontology:constraint_victim(second_amendment_arms_right__collective_right_reading, individuals_outside_militia_context).
narrative_ontology:constraint_victim(second_amendment_arms_right__collective_right_reading, civilian_firearm_access).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CIVILIAN OUTSIDE MILITIA CONTEXT (SNARE) — No meaningful exit from regulatory jurisdiction; state has plenary authority to restrict or prohibit individual firearm ownership. Under this reading, civilian arms access is a privilege subject to state police power, not a protected right. Maximum suppression: no legal recourse, no appeal to federal constitutional protection, regulatory alternatives unavailable.
constraint_indexing:constraint_classification(second_amendment_arms_right__collective_right_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MILITIA ORGANIZATION / STATE NATIONAL GUARD (TANGLED ROPE) — Genuine coordination function: the Second Amendment protects state militia capacity to organize and maintain armed forces independent of federal control. Real coordination benefit: federal government cannot disarm state militias. But asymmetric extraction: state authority over militia arms is hierarchical; individual members have constrained choices; state retains final authority over deployment and mission. The constraint coordinates federal-state power while extracting compliance from militia members.
constraint_indexing:constraint_classification(second_amendment_arms_right__collective_right_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE GOVERNMENT / POLICE AUTHORITY (ROPE) — Benefits from the constraint as coordination mechanism: Second Amendment under this reading protects state police authority, reserves arms regulation to states, prevents federal gun control mandates. Low extraction because the beneficiary (state) has full arbitrage: they can set regulation policy, delegate to local authorities, or maintain status quo. The constraint solves a coordination problem (federalism boundary) that the state legitimately controls.
constraint_indexing:constraint_classification(second_amendment_arms_right__collective_right_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: FIREARM OWNERS SEEKING CONSISTENT ACCESS ACROSS STATES (SNARE) — Constrained by fragmented state-level regulation; uniform federal protection unavailable under this reading. Can relocate (exit is constrained, not trapped), but relocation costs are high. High suppression: no direct federal constitutional appeal; competing state regulations create arbitrage costs; collective action to change regulation is difficult across state lines.
constraint_indexing:constraint_classification(second_amendment_arms_right__collective_right_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: HISTORICAL MILITIA STRUCTURES (PITON) — The well-regulated militia as described in the Second Amendment (18th-century state militias) has been largely displaced by the National Guard, federal military, and professional law enforcement. The collective-right reading's core justification (protecting state militia capacity) now applies to a functionally different institutional form. Theater_ratio high because contemporary militia rhetoric persists despite structural changes to what militias actually are and do. The constraint is maintained through institutional inertia and constitutional text attachment rather than functional necessity.
constraint_indexing:constraint_classification(second_amendment_arms_right__collective_right_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / ORIGINALIST NATURAL LAW VIEW (MOUNTAIN) — From an originalist analytical stance, the Second Amendment's text and founding-era understanding establish an immutable, unchangeable protection for state militia authority. Under this frame, the right is grounded in natural law or constitutional law's fixed meaning — not a contingent policy choice but a structural limit on federal power. The analytical observer risks treating the historical reading as a legal fact rather than one reading among contested interpretations.
constraint_indexing:constraint_classification(second_amendment_arms_right__collective_right_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_arms_right__collective_right_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(second_amendment_arms_right__collective_right_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(second_amendment_arms_right__collective_right_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_arms_right__collective_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(second_amendment_arms_right__collective_right_reading, TR),
    TR >= 0.70.

:- end_tests(second_amendment_arms_right__collective_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): The collective-right reading permits significant state extraction from individuals seeking civilian firearm access (plenary regulatory authority), but the extraction is not maximal (0.70+) because the reading's functional purpose — militia protection — is genuine even if contested. The reading extracts because it denies individuals a federal constitutional appeal, forcing them into state regulatory hierarchies with no fallback. The trajectory from 0.35 to 0.58 reflects rising pressure as the functional justification (militia autonomy) becomes less coherent with modern National Guard integration and as states accumulate regulatory restrictions. Suppression (0.65): High. Individuals outside militia context face material barriers (state law, regulatory cost, relocation barriers) and legal barriers (no federal constitutional protection under this reading). The reading itself is part of the suppression mechanism — it denies the victim a constitutional language in which to frame resistance. Targets cannot exit, cannot appeal to higher authority, cannot organize federal-level redress. Theater ratio (0.58): Moderate-high. The collective-right reading contains significant performative elements: state militia rhetoric is invoked to justify plenary arms regulation that has little to do with militia functionality; the reading's invocation of 18th-century militia structure masks contemporary regulatory extraction; constitutional interpretation appears to be discovering the Amendment's fixed meaning rather than choosing among interpretive options. The trajectory upward (0.42 to 0.58) reflects accumulating theater as the functional rationale diverges from actual state practice.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap in this constraint is dramatic. The beneficiary (state government, institutional perspective) experiences the constraint as Rope — a coordination mechanism protecting federalism boundaries. The militia organization experiences Tangled Rope — genuine protection from federal disarmament but hierarchical state control. The powerless individual experiences Snare — plenary state authority with no exit. The firearm owner seeking multi-state access experiences Snare — fragmented regulation with high arbitrage cost. The vestigial militia structures experience Piton — the constraint's functional purpose has eroded but the constitutional text persists. The analytical observer risks naturalizing the reading as immutable law (Mountain) rather than recognizing it as one interpretation competing with others. The gap between these perspectives (Rope → Snare, Rope → Piton, Rope → Mountain) reveals that the constraint's classification is not determined by the base properties alone but by the observer's structural position relative to state authority.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from beneficiary/victim structure and exit options. State governments are beneficiaries with arbitrage options (can set regulation, delegate, shift policy) → d ≈ 0.15 → f(d) ≈ -0.01 → institutional perspective sees low/negative chi (Rope). Militia organizations are secondary beneficiaries with constrained exit (integrated with state, subject to state hierarchy) → d ≈ 0.35 → f(d) ≈ 0.30 → organized perspective sees mixed chi (Tangled Rope). Individuals outside militia are victims with trapped exit → d ≈ 0.93 → f(d) ≈ 1.38 → powerless perspective sees maximum chi (Snare). Firearm owners seeking multi-state consistency are victims with constrained exit (can relocate but at cost) → d ≈ 0.75 → f(d) ≈ 1.10 → moderate perspective sees high chi (Snare). The analytical perspective derives d from the canonical fallback (d ≈ 0.73 for analytical) → f(d) ≈ 1.15, yielding what appears to be Mountain classification until the false summit detector examines whether the naturalness is genuine or constructed. The directionality values confirm that the constraint flows extraction toward state authority (beneficiary) and away from individuals outside militia (victims).
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is NOT resolved in this constraint (extractiveness 0.52 does not exceed the 0.70 threshold). However, the constraint demonstrates mandatrophy-type tensions: the reading claims to be about coordination (militia protection) but enables extraction (state regulation of civilian access). The coordination function is real but historically contingent (18th-century militia autonomy no longer applies to modern National Guard). The extraction is real but is framed as legitimate state police power rather than extractive privilege. The omega variables document the irreducible uncertainties: whether the functional rationale (militia protection) applies to modern institutional forms, whether the reading's suppression accurately captures individual mobility, whether the readings logically foreclose each other or coexist as live interpretations. Resolution would require either: (1) demonstrating that the coordination function is no longer operative (pushing extractiveness toward snare), or (2) demonstrating that the coordination function is fundamental to federalism and the extraction is a legitimate price for that coordination (pushing the constraint toward hybrid tangled rope with higher coordination weight). The present classification (Tangled Rope, extractiveness 0.52) holds the tension without resolving it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    militia_functionality_drift,
    'Does the collective-right reading remain coherent when applied to the modern National Guard, which is subject to federal command and control, rather than autonomous state militias?',
    'Historical analysis of militia structure and autonomy across time periods; examination of whether the functional justification for protecting state militia arms authority applies to contemporary militia structures',
    'If modern militias are too tightly integrated with federal structures: the rationale for protecting state militia arms authority is substantially weakened, suggesting the reading naturalizes a historical arrangement that no longer exists. If state militias retain meaningful autonomy: the reading''s functional justification survives. Either way, the constraint''s extractiveness may increase (from 0.52 to 0.65+) if the foundational logic is no longer operative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(militia_functionality_drift, empirical, 'Whether the collective-right reading''s militia-protection rationale applies to modern National Guard structures').

omega_variable(
    individual_militia_participation_path,
    'Does the collective-right reading permit individuals a constitutional path to arms access through militia membership, or does it reserve arms access entirely to state discretion?',
    'Legal analysis of state laws and court interpretations; examination of whether individuals have a right to form or join militia organizations and bear arms in that context; whether state can exclude individuals from militia access',
    'If individuals have a constitutional right to militia participation and arms bearing within militia: the snare classification (perspective 1) is softened; exits exist at biographical timescale. If state has plenary authority to exclude and restrict: snare classification holds. This distinction affects whether the constraint''s suppression value (0.65) accurately reflects the target''s structural mobility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(individual_militia_participation_path, conceptual, 'Whether the collective-right reading provides individuals a constitutional path to militia arms access').

omega_variable(
    reading_foundational_incompatibility,
    'Is the collective-right reading logically incompatible with the individual-right reading, or do both readings coexist as live positions held by different constitutional interpreters?',
    'Analysis of whether both readings can be held within a single legal framework without direct contradiction; examination of whether they make competing empirical claims or competing normative claims about constitutional authority',
    'If forecloses (logically incompatible): the sibling readings should be marked as coexists_with but the omega documents that the incompatibility is performative rather than logical. If coexists_with (both live): the constraint should acknowledge that the reading relations reflect actual interpretive pluralism, not logical constraint. This affects how the engine models constitutional uncertainty.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foundational_incompatibility, conceptual, 'Logical vs. interpretive relationship between collective-right and individual-right readings').

omega_variable(
    regulatory_extraction_coverage,
    'Does the collective-right reading''s allowance for state regulation cover all extraction mechanisms (licensing, background checks, feature restrictions, firearm bans), or are there residual individual protections even under this reading?',
    'Legal analysis of state-level arms regulations upheld under this reading; identification of any regulatory measures courts have rejected even when acknowledging state authority; examination of whether any constraint on state regulation appears in supporting legal opinions',
    'If state has truly plenary regulatory authority: extractiveness (0.52) is understated; should increase to 0.60+. If courts have found some residual individual protections (e.g., handgun bans rejected, core self-defense preserved): extractiveness correctly values partial state authority rather than complete plenary authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_extraction_coverage, empirical, 'Scope of state regulatory authority under the collective-right reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_arms_right__collective_right_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sa_collective_theater_t0, second_amendment_arms_right__collective_right_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(sa_collective_theater_t100, second_amendment_arms_right__collective_right_reading, theater_ratio, 100, 0.52).
narrative_ontology:measurement(sa_collective_theater_t200, second_amendment_arms_right__collective_right_reading, theater_ratio, 200, 0.58).

% Extraction over time
narrative_ontology:measurement(sa_collective_extractiveness_t0, second_amendment_arms_right__collective_right_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sa_collective_extractiveness_t100, second_amendment_arms_right__collective_right_reading, base_extractiveness, 100, 0.52).
narrative_ontology:measurement(sa_collective_extractiveness_t200, second_amendment_arms_right__collective_right_reading, base_extractiveness, 200, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(sa_collective_suppression_t0, second_amendment_arms_right__collective_right_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(sa_collective_suppression_t100, second_amendment_arms_right__collective_right_reading, suppression_requirement, 100, 0.61).
narrative_ontology:measurement(sa_collective_suppression_t200, second_amendment_arms_right__collective_right_reading, suppression_requirement, 200, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_arms_right__collective_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_arms_right__collective_right_reading, second_amendment_arms_right__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_arms_right__collective_right_reading, second_amendment_arms_right__civic_republican_reading).
narrative_ontology:affects_constraint(second_amendment_arms_right__collective_right_reading, federalism_militia_authority).
narrative_ontology:affects_constraint(second_amendment_arms_right__collective_right_reading, state_police_power_regulation).

% DUAL FORMULATION NOTE:
% The Second Amendment arms right is a contested kernel instantiated by three structurally distinct readings: COLLECTIVE-RIGHT (state militia authority, this constraint), INDIVIDUAL-RIGHT (civilian arms ownership), and CIVIC-REPUBLICAN (civic participation with individual duty). Each reading has different ε values, different beneficiary/victim structures, and different extractiveness profiles. The collective-right reading (ε=0.52) coordinates federal-state boundaries but enables state extraction from civilians. The individual-right reading (ε elsewhere) protects civilian access but may reduce state regulatory capacity. The civic-republican reading (ε elsewhere) places individual duty within collective defense structures. All three link to the parent kernel; see the kernel_context field and cs_structure.reading_relations for the network topology.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
