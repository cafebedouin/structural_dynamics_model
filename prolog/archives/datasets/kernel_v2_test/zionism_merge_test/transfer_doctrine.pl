% ============================================================================
% CONSTRAINT STORY: transfer_doctrine
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_transfer_doctrine, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: transfer_doctrine
 *   human_readable: Transfer Doctrine: Arab Population Removal as Demographic Solution
 *   domain: political_history/nationalism/settler_colonialism
 *
 * SUMMARY:
 *   The transfer doctrine emerged in Zionist leadership discourse as a
 *   proposed solution to the 'demographic problem' — the presence of a
 *   substantial Arab majority in Palestine that threatened the viability of a
 *   Jewish state. From the Peel Commission response (1937) through Plan Dalet
 *   implementation (1948), the doctrine evolved from proposal to policy to
 *   implementation. This constraint represents the ideological and practical
 *   commitment to Arab population removal, manifesting in leadership
 *   statements, military planning, and systematic expulsion during 1948. The
 *   doctrine created approximately 750,000 Palestinian refugees and 150,000
 *   internally displaced 'present absentees' — Israeli citizens legally
 *   barred from returning to their villages. The constraint exhibits high
 *   extractiveness (0.88) reflecting massive property transfer and permanent
 *   displacement, high suppression (0.92) reflecting military force and legal
 *   barriers to return, and moderate theater ratio (0.35) reflecting that
 *   while some humanitarian rhetoric existed, the implementation was
 *   substantially functional rather than performative. The constraint is
 *   downstream of the demographic_engineering_imperative but represents a
 *   distinct structural mechanism: where the upstream constraint is the
 *   perceived necessity of Jewish majority, this constraint is the specific
 *   solution of population removal.
 *
 * KEY AGENTS:
 *   - Palestinian Refugees: Primary victims (powerless/trapped) — 750,000 displaced persons with no return rights across three generations; maximum extraction through loss of land, property, citizenship
 *   - Internally Displaced Palestinians: Secondary victims (powerless/identity_locked) — 'present absentees' with Israeli citizenship but legal bar to village return; identity-locked by citizenship status that prevents acknowledgment of displacement
 *   - Zionist Leadership (Ben-Gurion Circle): Primary beneficiaries (institutional/arbitrage) — implemented transfer doctrine to achieve demographic goals; captured land and property; controlled narrative
 *   - Dissenting Zionist Intellectuals: Constrained actors (moderate/constrained) — Ahad Ha'am, Magnes, Buber advocating binational solutions; marginalized but not excluded; experienced both coordination and extraction
 *   - International Humanitarian Regime: Organized actors (organized/mobile) — UNRWA, refugee law frameworks treating displacement as temporary; sunset logic of eventual resolution
 *   - Peace Process Framework: Institutional actors (institutional/constrained) — Oslo and subsequent negotiations; atrophied function maintained as performance
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees systematic extraction through demographic engineering; identifies coordination cover story
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transfer_doctrine, 0.88).
domain_priors:suppression_score(transfer_doctrine, 0.92).
domain_priors:theater_ratio(transfer_doctrine, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transfer_doctrine, extractiveness, 0.88).
narrative_ontology:constraint_metric(transfer_doctrine, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(transfer_doctrine, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transfer_doctrine, snare).
narrative_ontology:human_readable(transfer_doctrine, "Transfer Doctrine: Arab Population Removal as Demographic Solution").
narrative_ontology:topic_domain(transfer_doctrine, "political_history/nationalism/settler_colonialism").

domain_priors:requires_active_enforcement(transfer_doctrine).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(transfer_doctrine, 'd2360e27-bd20-4d3f-8c58-9859c1525bc9').
narrative_ontology:cs_kernel_codification('d2360e27-bd20-4d3f-8c58-9859c1525bc9', distributed).
narrative_ontology:cs_authority_grounding('d2360e27-bd20-4d3f-8c58-9859c1525bc9', lineage).
narrative_ontology:cs_interpretation_layer_present('d2360e27-bd20-4d3f-8c58-9859c1525bc9').
narrative_ontology:cs_reading_relation('d2360e27-bd20-4d3f-8c58-9859c1525bc9', transfer_doctrine__settler_colonial_reading, forecloses).
narrative_ontology:cs_reading_relation('d2360e27-bd20-4d3f-8c58-9859c1525bc9', transfer_doctrine__religious_restoration_reading, coexists_with).
narrative_ontology:cs_axiom('d2360e27-bd20-4d3f-8c58-9859c1525bc9', foundational, persecution_driven_necessity).
narrative_ontology:cs_axiom_status(persecution_driven_necessity, holdable).
narrative_ontology:cs_axiom_grounding('d2360e27-bd20-4d3f-8c58-9859c1525bc9', persecution_driven_necessity, empirically_contingent).
narrative_ontology:cs_axiom('d2360e27-bd20-4d3f-8c58-9859c1525bc9', foundational, indigenous_return_legitimacy).
narrative_ontology:cs_axiom_status(indigenous_return_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('d2360e27-bd20-4d3f-8c58-9859c1525bc9', indigenous_return_legitimacy, deontological).
narrative_ontology:cs_axiom('d2360e27-bd20-4d3f-8c58-9859c1525bc9', secondary, demographic_sustainability_requirement).
narrative_ontology:cs_axiom_status(demographic_sustainability_requirement, holdable).
narrative_ontology:cs_axiom_grounding('d2360e27-bd20-4d3f-8c58-9859c1525bc9', demographic_sustainability_requirement, instrumental).
narrative_ontology:cs_reference_frame('d2360e27-bd20-4d3f-8c58-9859c1525bc9', national_liberation_necessity).
narrative_ontology:cs_drift_state('d2360e27-bd20-4d3f-8c58-9859c1525bc9', post_nakba_historiography, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d2360e27-bd20-4d3f-8c58-9859c1525bc9', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(transfer_doctrine, jewish_state_builders).
narrative_ontology:constraint_beneficiary(transfer_doctrine, zionist_leadership).
narrative_ontology:constraint_beneficiary(transfer_doctrine, israeli_state_apparatus).
narrative_ontology:constraint_victim(transfer_doctrine, palestinian_refugees).
narrative_ontology:constraint_victim(transfer_doctrine, palestinian_arab_population).
narrative_ontology:constraint_victim(transfer_doctrine, displaced_communities).
narrative_ontology:constraint_vindicates(transfer_doctrine, demographic_determinism).
narrative_ontology:constraint_vindicates(transfer_doctrine, territorial_exclusivity_principle).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PALESTINIAN REFUGEES (SNARE) — Trapped by military force, legal exclusion, and international abandonment. No exit from refugee status across three generations. Maximum extraction: loss of land, property, citizenship, and return rights. The constraint operates as pure extraction with no coordination function from this position.
constraint_indexing:constraint_classification(transfer_doctrine, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: INTERNALLY DISPLACED PALESTINIANS (SNARE) — Present absentees: physically within Israel but legally barred from return to villages. Identity-locked by citizenship status that prevents acknowledgment of displacement. Structural mobility (Israeli citizens) but functionally trapped by legal framework that denies internal refugee status. High extraction through property confiscation and denial of return.
constraint_indexing:constraint_classification(transfer_doctrine, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 3: DISSENTING ZIONIST INTELLECTUALS (TANGLED ROPE) — Figures like Ahad Ha'am, Judah Magnes, Martin Buber who advocated binational solutions. Constrained by institutional marginalization and historical trajectory but benefited from Zionist institutional infrastructure. Experienced both coordination (shared national project) and extraction (suppression of alternative visions). Career costs for dissent but not total exclusion.
constraint_indexing:constraint_classification(transfer_doctrine, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ZIONIST LEADERSHIP (ROPE) — Primary beneficiaries. Experienced transfer doctrine as coordination mechanism solving the 'demographic problem' that threatened Jewish majority. Arbitrage-level exit: could modulate implementation, control narrative, access international support. Net beneficiary of the constraint's operation.
constraint_indexing:constraint_classification(transfer_doctrine, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERNATIONAL HUMANITARIAN REGIME (SCAFFOLD) — UNRWA, refugee law frameworks, right of return resolutions. Organized actors treating displacement as temporary problem requiring transitional support. Sunset logic: refugee status meant to be resolved through repatriation or resettlement. Mobile: can redirect resources, reframe mandates. Sees constraint as coordination failure with intended resolution, not permanent extraction.
constraint_indexing:constraint_classification(transfer_doctrine, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: PEACE PROCESS FRAMEWORK (PITON) — Oslo Accords and subsequent negotiations treating refugee issue as negotiable final status question. Original function (conflict resolution) atrophied into performative diplomacy. Maintained through institutional inertia and international investment despite failure to address core displacement. Theater ratio reflects gap between negotiation ritual and structural reality.
constraint_indexing:constraint_classification(transfer_doctrine, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From civilizational/universal perspective, transfer doctrine represents systematic extraction: deliberate population removal to achieve demographic engineering goals. High extractiveness (0.88), high suppression (0.92), identifiable victims, no genuine coordination function. The 'demographic problem' framing naturalizes what is structural violence. Analytical classification: snare.
constraint_indexing:constraint_classification(transfer_doctrine, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(transfer_doctrine_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(transfer_doctrine, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(transfer_doctrine, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(transfer_doctrine, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(transfer_doctrine, TR),
    TR >= 0.70.

:- end_tests(transfer_doctrine_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.88): Very high. The constraint produced massive property transfer (estimated 300,000+ hectares), permanent displacement of 750,000+ people, and multi-generational refugee status. The extraction is not merely incidental to state-building but constitutive: land acquisition and demographic transformation were explicit goals. The value reflects that this is near-maximal extraction — total dispossession for the target population. Suppression (0.92): Very high. Military force during 1948, legal barriers to return (Absentee Property Law, citizenship law), international abandonment of return rights, and ongoing prevention of repatriation. Suppression increased sharply during implementation (1948) and has been maintained for 75+ years. The value reflects that exits are almost completely blocked: refugees cannot return, internally displaced cannot reclaim property, and alternative frameworks (binational state, right of return) are actively suppressed. Theater ratio (0.35): Moderate-low. While humanitarian rhetoric existed and some leaders expressed regret, the implementation was substantially functional: military orders, systematic village destruction, prevention of return. The theater increased slightly over time as the 'war necessity' narrative replaced explicit transfer advocacy, but the constraint remained primarily functional extraction rather than performance. The value reflects that this is not a piton (degraded ritual) but an actively maintained extraction mechanism with some cover story.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates extreme perspectival divergence. Zionist leadership experienced transfer doctrine as coordination (Rope) — solving the demographic problem that threatened state viability. They were net beneficiaries of land acquisition and demographic transformation. Palestinian refugees experienced pure extraction (Snare) — total dispossession with no coordination function and no exit. The 'demographic problem' framing that appears as legitimate concern from the beneficiary position appears as extraction justification from the victim position. Dissenting Zionist intellectuals experienced mixed coordination and extraction (Tangled Rope) — they benefited from Zionist institutional infrastructure while being marginalized for opposing transfer. International humanitarian regime experienced temporary coordination failure (Scaffold) — treating displacement as problem to be resolved through repatriation or resettlement, with sunset logic. Peace process framework experienced degraded ritual (Piton) — negotiations treating refugee issue as bargaining chip while structural displacement persists. Analytical observer identifies systematic extraction (Snare) — the coordination story (demographic sustainability) is cover for deliberate population removal. The gap between Rope (beneficiary) and Snare (victim/analytical) is the core measurement: what appears as legitimate state-building from one position appears as ethnic cleansing from another.
 *
 * DIRECTIONALITY LOGIC:
 *   Palestinian refugees are full targets (d ≈ 0.95): powerless, trapped, bearing total extraction. The engine derives high d from victim status + trapped exit, producing maximum effective extraction. Internally displaced Palestinians are also high-target (d ≈ 0.85): powerless but identity-locked rather than trapped — the citizenship status creates cognitive binding rather than pure structural barrier. Dissenting Zionist intellectuals are mixed (d ≈ 0.45): moderate power, constrained exit, both beneficiaries (Zionist infrastructure) and victims (marginalization). The engine derives mid-range d from mixed structural position. Zionist leadership are full beneficiaries (d ≈ 0.05): institutional power, arbitrage exit, primary beneficiaries. The engine derives very low d, producing negative effective extraction (subsidy). International humanitarian regime is low-target (d ≈ 0.25): organized power, mobile exit, treating constraint as coordination problem. Peace process framework is moderate-target (d ≈ 0.40): institutional power but constrained exit, experiencing atrophied function. Analytical observer uses analytical context, seeing the structural extraction clearly.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by showing that the classification depends on structural position and the question being asked. From the beneficiary position (Zionist leadership), transfer doctrine solved a genuine coordination problem: how to establish a Jewish-majority state in a territory with Arab majority. From this position, Rope classification is structurally accurate — they experienced coordination. From the victim position (Palestinian refugees), the constraint is pure extraction: dispossession with no coordination function and no exit. Snare classification is structurally accurate from this position. The analytical observer asks a different question: is the 'demographic problem' itself a natural constraint (Mountain) or a constructed justification for extraction (Snare)? The analytical classification is Snare because the demographic anxiety, while genuinely felt by Zionist leadership, was not an immutable natural law but a consequence of the exclusivist state-building project. Alternative frameworks (binational state, cantonal federation) were suppressed, not structurally impossible. The mandatrophy is resolved by recognizing that 'coordination for whom?' and 'extraction from whom?' are the questions that determine classification, and different positions yield different but equally valid answers within their contexts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inevitability_vs_agency,
    'Was large-scale Palestinian displacement inevitable consequence of 1948 war, or deliberate implementation of pre-existing transfer doctrine?',
    'Historiographic analysis: comparison of leadership statements (1937-1948), military orders (Plan Dalet), expulsion patterns vs combat zones, prevention of return policies. Archival evidence of intent vs opportunistic exploitation of war conditions.',
    'If inevitable: reduces extractiveness score (war logic vs demographic engineering). If deliberate: confirms snare classification and high extractiveness. Affects whether constraint is war byproduct or implemented policy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inevitability_vs_agency, empirical, 'Whether displacement was war consequence or policy implementation').

omega_variable(
    coordination_function_existence,
    'Did transfer doctrine solve genuine coordination problem (demographic sustainability of Jewish state) or create extraction mechanism (land acquisition through displacement)?',
    'Counterfactual analysis: binational state viability, demographic projections under alternative frameworks, comparison with other multi-ethnic state formations. Assessment of whether ''demographic problem'' was structural constraint or constructed justification.',
    'If genuine coordination: shifts toward tangled_rope (coordination + extraction). If constructed problem: confirms snare (pure extraction with coordination cover story). Determines whether demographic anxiety was structural or ideological.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_function_existence, conceptual, 'Whether demographic problem was genuine coordination challenge').

omega_variable(
    alternative_pathway_suppression,
    'Were binational or cantonal alternatives structurally viable, or was Jewish majority requirement non-negotiable given persecution context?',
    'Historical analysis of suppressed alternatives: Magnes-Buber proposals, Hashomer Hatzair positions, Arab-Jewish cooperation attempts. Assessment of whether alternatives were rejected due to structural impossibility or ideological commitment to exclusivity.',
    'If alternatives viable: increases suppression score and confirms snare (active suppression of exits). If structurally impossible: reduces suppression (no real alternatives existed). Affects whether constraint suppressed genuine options or only theoretical ones.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_pathway_suppression, conceptual, 'Viability of suppressed binational alternatives').

omega_variable(
    reading_foreclosure_mechanism,
    'Does the settler_colonial_reading logically foreclose the national_liberation_reading, or do they coexist as competing frameworks held by different communities?',
    'Logical analysis: can a movement be simultaneously indigenous return AND externally-originated colonial transplantation? Examination of whether frameworks are mutually exclusive within single analytical frame or represent incommensurable perspectives.',
    'If foreclosing: settler_colonial_reading rules out national_liberation_reading within coherent framework. If coexisting: both remain live interpretive options for different communities. Determines reading_relations structure in cs_structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_mechanism, conceptual, 'Whether settler-colonial and national-liberation readings are mutually exclusive').

omega_variable(
    suppression_mechanism_internalization,
    'For internally displaced Palestinians (present absentees), is suppression primarily structural (legal barriers to return) or internalized (identity-lock through citizenship status)?',
    'Post-legal-change trajectory analysis: if Absentee Property Law were repealed, would return occur immediately (structural) or would identity-lock persist (internalized)? Comparison with other internally displaced populations'' return patterns after legal barrier removal.',
    'If structural: suppression is purely external legal framework. If internalized: suppression includes cognitive dimension where citizenship status prevents acknowledgment of displacement. Affects whether identity_locked exit option accurately captures mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether present absentee suppression is structural or internalized').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transfer_doctrine, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(transfer_theater_1937, transfer_doctrine, theater_ratio, 0, 0.15).
narrative_ontology:measurement(transfer_theater_1943, transfer_doctrine, theater_ratio, 6, 0.2).
narrative_ontology:measurement(transfer_theater_1948, transfer_doctrine, theater_ratio, 11, 0.25).
narrative_ontology:measurement(transfer_theater_1952, transfer_doctrine, theater_ratio, 15, 0.35).

% Extraction over time
narrative_ontology:measurement(transfer_extract_1937, transfer_doctrine, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(transfer_extract_1940, transfer_doctrine, base_extractiveness, 3, 0.62).
narrative_ontology:measurement(transfer_extract_1943, transfer_doctrine, base_extractiveness, 6, 0.75).
narrative_ontology:measurement(transfer_extract_1946, transfer_doctrine, base_extractiveness, 9, 0.82).
narrative_ontology:measurement(transfer_extract_1948, transfer_doctrine, base_extractiveness, 11, 0.88).
narrative_ontology:measurement(transfer_extract_1952, transfer_doctrine, base_extractiveness, 15, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(transfer_suppress_1937, transfer_doctrine, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(transfer_suppress_1943, transfer_doctrine, suppression_requirement, 6, 0.55).
narrative_ontology:measurement(transfer_suppress_1948, transfer_doctrine, suppression_requirement, 11, 0.85).
narrative_ontology:measurement(transfer_suppress_1952, transfer_doctrine, suppression_requirement, 15, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transfer_doctrine, identity_coordination).

% DUAL FORMULATION NOTE:
% Transfer doctrine is downstream of demographic_engineering_imperative (the perceived necessity of Jewish majority) but represents a distinct constraint: the specific solution of population removal. The upstream constraint has its own extractiveness reflecting the demographic anxiety itself; this constraint has its own extractiveness reflecting the implementation of removal. They are linked but structurally separate: one could accept demographic engineering as goal while rejecting transfer as method (dissenting Zionists), or reject demographic engineering entirely (binational advocates).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
