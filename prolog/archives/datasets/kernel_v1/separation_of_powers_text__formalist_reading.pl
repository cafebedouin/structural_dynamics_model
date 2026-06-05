% ============================================================================
% CONSTRAINT STORY: separation_of_powers_text__formalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_separation_of_powers_text__formalist_reading, []).

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
 *   constraint_id: separation_of_powers_text__formalist_reading
 *   human_readable: Separation of Powers (Formalist Reading): Strict Constitutional Boundaries
 *   domain: constitutional_law/political_theory/administrative_law
 *
 * SUMMARY:
 *   The formalist reading of separation of powers doctrine establishes
 *   strict, impermeable boundaries between legislative, executive, and
 *   judicial authority, with particular emphasis on the prohibition of
 *   congressional delegation of legislative power to administrative agencies.
 *   Under this reading, Article I, Section 1 ('All legislative Powers herein
 *   granted shall be vested in a Congress of the United States') creates an
 *   absolute boundary: Congress cannot transfer its legislative authority to
 *   the executive branch, and any agency rule that exercises discretion in
 *   determining legal obligations exceeds constitutional bounds. This reading
 *   produces a constraint whose structure is a snare: administrative agencies
 *   and the regulatory capacity they provide are the primary victims; the
 *   legislative branch and originalist constitutional theory are the primary
 *   beneficiaries. The constraint has grown more suppressive over the
 *   interval (1926-present) as the complexity of modern governance has
 *   increased while the doctrine's scope has expanded. The extractiveness has
 *   risen as agencies have been forced to operate in increasing legal
 *   ambiguity (non-major rule exemptions, guidance documents, memoranda of
 *   understanding) to accomplish necessary governance functions while
 *   nominally respecting the constitutional boundary the formalist reading
 *   enforces. The theater ratio remains low because the constraint is
 *   substantive rather than performative — the doctrine actually prevents
 *   certain regulatory actions, not merely ritualizes them. This is one
 *   reading of the contested kernel 'separation of powers doctrine'; sibling
 *   readings (functionalist, unitary executive) would produce different
 *   victim sets and different constraint classifications.
 *
 * KEY AGENTS:
 *   - Administrative Agencies: Primary victims (powerless/trapped) — cannot legally exercise delegated legislative authority; trapped within constraint regardless of functional necessity
 *   - Executive Branch (short-term): Beneficiary-in-appearance (institutional/arbitrage) — gains formal constitutional supremacy in its domain; actually constrained by inability to regulate effectively
 *   - Executive Branch (long-term/generational): Secondary victim (powerful/mobile-but-suppressed) — prevented from adapting governance to new challenges; regulatory capacity ossifies
 *   - Congress / Legislative Branch: Primary beneficiary (institutional/arbitrage) — doctrine preserves and formalizes congressional prerogative; has exit option (amendment, reinterpretation) at low practical cost
 *   - Regulated Industries: Secondary victim (moderate/constrained) — suffer dual extraction: regulatory uncertainty + unpredictable legislative micromanagement + inability of agencies to provide coherent rules
 *   - Constitutional Textualists / Originalists: Ideational beneficiary (analytical/arbitrage) — reading validates their interpretive method and enhances their authority in constitutional discourse
 *   - Analytical Observer: Sees committer structure (analytical/analytical) — recognizes that the reading is one coherent framing among coexisting alternatives, each with different beneficiary/victim distributions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(separation_of_powers_text__formalist_reading, 0.68).
domain_priors:suppression_score(separation_of_powers_text__formalist_reading, 0.72).
domain_priors:theater_ratio(separation_of_powers_text__formalist_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(separation_of_powers_text__formalist_reading, snare).
narrative_ontology:human_readable(separation_of_powers_text__formalist_reading, "Separation of Powers (Formalist Reading): Strict Constitutional Boundaries").
narrative_ontology:topic_domain(separation_of_powers_text__formalist_reading, "constitutional_law/political_theory/administrative_law").

domain_priors:requires_active_enforcement(separation_of_powers_text__formalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(separation_of_powers_text__formalist_reading, '155a862e-f2ef-4984-9fcb-e31150a66602').
narrative_ontology:cs_kernel_codification('155a862e-f2ef-4984-9fcb-e31150a66602', formalized).
narrative_ontology:cs_authority_grounding('155a862e-f2ef-4984-9fcb-e31150a66602', lineage).
narrative_ontology:cs_interpretation_layer_present('155a862e-f2ef-4984-9fcb-e31150a66602').
narrative_ontology:cs_reading_relation('155a862e-f2ef-4984-9fcb-e31150a66602', separation_of_powers_text__functionalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('155a862e-f2ef-4984-9fcb-e31150a66602', separation_of_powers_text__unitary_executive_reading, coexists_with).
narrative_ontology:cs_axiom('155a862e-f2ef-4984-9fcb-e31150a66602', foundational, legislative_authority_nondelegable).
narrative_ontology:cs_axiom_status(legislative_authority_nondelegable, holdable).
narrative_ontology:cs_axiom_grounding('155a862e-f2ef-4984-9fcb-e31150a66602', legislative_authority_nondelegable, deontological).
narrative_ontology:cs_axiom('155a862e-f2ef-4984-9fcb-e31150a66602', foundational, strict_textual_originalism).
narrative_ontology:cs_axiom_status(strict_textual_originalism, holdable).
narrative_ontology:cs_axiom_grounding('155a862e-f2ef-4984-9fcb-e31150a66602', strict_textual_originalism, conventional).
narrative_ontology:cs_reference_frame('155a862e-f2ef-4984-9fcb-e31150a66602', vested_congressional_supremacy).
narrative_ontology:cs_drift_state('155a862e-f2ef-4984-9fcb-e31150a66602', contemporary_administrative_state, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('155a862e-f2ef-4984-9fcb-e31150a66602', '').
narrative_ontology:cs_kernel_id(separation_of_powers_text__formalist_reading, separation_of_powers_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(separation_of_powers_text__formalist_reading, legislative_branch_institutional_power).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__formalist_reading, constitutional_textualists).
narrative_ontology:constraint_victim(separation_of_powers_text__formalist_reading, administrative_agencies).
narrative_ontology:constraint_victim(separation_of_powers_text__formalist_reading, executive_regulatory_capacity).
narrative_ontology:constraint_victim(separation_of_powers_text__formalist_reading, specialized_governance_function).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ADMINISTRATIVE AGENCY (SNARE) — Trapped within constitutional doctrine that denies legitimacy to delegated rulemaking authority. Cannot exit the constraint without fundamentally violating the constitutional framework it operates within. Bears maximum extraction: constrained to advisory/ministerial functions despite functional necessity for specialized governance. No alternative legal pathway.
constraint_indexing:constraint_classification(separation_of_powers_text__formalist_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REGULATED INDUSTRY (SNARE) — Faces extraction through dual mechanism: (a) inability of agencies to promulgate coherent, specialized rules creates regulatory uncertainty and fragmentation; (b) compensatory congressional micromanagement creates unpredictable legislative burdens. Constrained exit — can lobby but cannot escape the constraint's regime. Moderate power but high experienced extraction.
constraint_indexing:constraint_classification(separation_of_powers_text__formalist_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LEGISLATIVE BRANCH (ROPE) — Institutional beneficiary. The formalist reading protects legislative prerogative and preserves constitutional supremacy of Congress. Experiences coordination benefit: the constraint formalizes legislative control as fundamental rather than contingent. Arbitrage exit — can bypass the constraint through constitutional amendment (low practical cost given institutional power). Net beneficiary position.
constraint_indexing:constraint_classification(separation_of_powers_text__formalist_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: EXECUTIVE BRANCH / GENERATIONAL VIEW (SNARE) — Structurally immobilized: the formalist doctrine prevents executive adaptation to new governance challenges across generational timescales. Executive reorganization and agency rulemaking capacity cannot evolve in response to technological or ecological change. Mobile nominally (executive can propose alternative readings) but trapped in practice by institutional inertia of constitutional doctrine. High suppression of adaptive alternatives.
constraint_indexing:constraint_classification(separation_of_powers_text__formalist_reading, snare,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (TANGLED ROPE) — Recognizes that the formalist reading provides genuine coordination function (preserves constitutional hierarchy and legislative primacy) while simultaneously imposing asymmetric extraction (suppresses executive adaptive capacity and specialized governance function). The constraint coordinates constitutional authority distribution while extracting regulatory capacity. This perspective instantiates the committer frame: observes that the reading's foundational axioms (textual originalism, strict delegation prohibition) generate the snare classification for subordinate agents, and that sibling readings (functionalist, unitary executive) would produce different victim sets. Sees the reading as one coherent framing among coexisting alternatives rather than as natural law.
constraint_indexing:constraint_classification(separation_of_powers_text__formalist_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(separation_of_powers_text__formalist_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(separation_of_powers_text__formalist_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(separation_of_powers_text__formalist_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(separation_of_powers_text__formalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(separation_of_powers_text__formalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The formalist constraint prevents agencies from exercising regulatory authority even when functionally necessary. The extraction takes the form of suppressed governance capacity: specialized functions cannot be performed or must be performed through non-statutory, less accountable means (guidance documents, memoranda). Agencies are forced to operate in a shadow system of nominally advisory rules that exercise real power while claiming not to, increasing compliance costs for regulated entities and reducing rule legitimacy. Suppression (0.72): High and rising. The constraint actively prevents alternatives: agencies cannot adopt even minor rules without major questions doctrine scrutiny; Congress cannot effectively delegate without facing constitutional challenge; executive adaptation to new domains (AI governance, pandemic response, cyberattacks) is constitutionally blocked. The suppression has increased over the interval as modern challenges require governance solutions that the constraint forbids. Theater ratio (0.38): Low. The constraint is substantive, not performative. Agencies genuinely cannot promulgate certain rules; Congress genuinely must retain certain authority. The constraint prevents action rather than ritualizing it. The low theater reflects that the formalist reading takes itself seriously as a structural limit, unlike pitons which are theatrical masks for degraded function.
 *
 * PERSPECTIVAL GAP:
 *   The formalist reading produces a sharp perspectival gap. The legislative branch sees coordination (Rope) — protection of constitutional structure and legislative prerogative. Administrative agencies see extraction (Snare) — constitutional prohibition on the only authority that can legally exercise their function. The executive branch sees coordination at immediate timescales (rope: formal supremacy) but extraction at generational timescales (snare: inability to adapt). Regulated industries see dual snare (extraction through regulatory fragmentation and legislative uncertainty). The analytical observer sees that the reading is internally coherent and provides real coordination benefits (preserves legislative supremacy) while simultaneously imposing real extraction costs (suppresses adaptive governance). The gap reveals that the formalist reading's classification depends entirely on which agent's position is indexed — there is no single 'true' type, only perspectival types from (Power, Time, Exit, Scope) tuples. This is precisely the diagnostic value of indexical classification: the same constitutional text and the same structural mechanism produce snare from agency and regulated-industry perspectives, rope from legislative perspective, and mountain-or-piton from misguided naive analytical observers who mistake the constraint's formality for naturalism.
 *
 * DIRECTIONALITY LOGIC:
 *   The formalist reading's directionality structure is asymmetric. Legislative branch beneficiaries experience low d (0.1-0.2) due to arbitrage exit and net benefit — they can amend the constitution or reinterpret the text at relatively low institutional cost, and the constraint protects their constitutional prerogative. Administrative agencies experience high d (0.92-0.98) due to trapped exit and pure victim status — they cannot exit the constraint without ceasing to exist as functional entities, and the constraint prevents their core authority. Regulated industries experience moderate-high d (0.75-0.85) due to constrained exit and victim status — they can lobby but cannot escape the regime, and the constraint forces them to bear coordination costs (fragmented rules, legislative uncertainty). The executive branch (institutional perspective) experiences intermediate d values depending on time horizon: immediate perspective d ≈ 0.4 (beneficiary-in-form due to formal supremacy), generational perspective d ≈ 0.75 (victim-in-practice due to prevented adaptation). These d values feed the sigmoid f(d) to produce effective extractiveness chi: low d yields negative or low chi (beneficiaries experience net benefit), high d yields high chi (victims experience severe extraction). The perspectival gap in chi values (beneficiary experiences rope with χ ≈ 0.08, agency experiences snare with χ ≈ 0.88) is the core diagnostic signal that this constraint involves substantial structural asymmetry — it benefits some agents while harming others.
 *
 * MANDATROPHY ANALYSIS:
 *   The formalist reading resolves the mandatrophy by showing that the snare classification is the correct perspectival reading from victim and analytical positions, while the rope classification is correct from the beneficiary position. The constraint genuinely coordinates legislative supremacy (rope function) while extracting regulatory capacity (snare effect). The mandatrophy would be: 'Is this a coordination mechanism (rope) or an extraction mechanism (snare)?' The answer is: it is both, but from different perspectives. From the legislative branch perspective, it is rope — a legitimate coordination mechanism that structures authority distribution. From the agency perspective, it is snare — pure extraction with no coordination benefit. The analytical observer who examines the structure carefully sees tangled_rope at civilizational scope: genuine coordination (preserves constitutional hierarchy) combined with genuine extraction (prevents adaptive governance). This is not a paradox — it is the correct diagnosis of a hybrid constraint viewed from outside the beneficiary position. The resolving insight is that indexical classification requires specifying the index; single-perspective classification that reports 'the constraint is rope' without specifying whose rope is incomplete and misleading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    delegation_necessity_vs_constitutional_text,
    'Does the constitutional text''s silence on agency rulemaking authority prohibit delegated legislative power, or does it permit Congress to decide what counts as legislative?',
    'Historical analysis of Founding-era understanding of executive vs legislative functions; comparison with constitutional structures in peer democracies; examination of whether specialized governance domains existed at Founding and how they were handled',
    'If text prohibits delegation: formalist reading is defensible as textual constraint. If text is silent and Congress may decide: delegation is permissible, reclassifying agencies from victims to beneficiaries and snare→tangled_rope or rope. Entire classification shifts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(delegation_necessity_vs_constitutional_text, empirical, 'Whether constitutional text prohibits or permits delegated regulatory authority').

omega_variable(
    functional_necessity_containment,
    'Can specialized governance functions (environmental regulation, financial markets, telecommunications) be performed by non-delegated legislative authority, or is delegation functionally necessary?',
    'Case analysis: attempt pure legislative specification (Congress writes the rule) for modern regulatory domains. Measure outcomes against delegated agency approach on criteria of: speed, technical accuracy, responsiveness to changed conditions, compliance costs.',
    'If legislative specification is adequate: formalist reading incurs only moderate suppression cost. If delegation is functionally necessary: formalist reading produces systemic governance failure, reclassifying snare→catastrophic extraction, potentially mountain→piton (performative constitutionalism masking non-compliance).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functional_necessity_containment, empirical, 'Whether specialized governance functions require delegated authority').

omega_variable(
    reading_viability_under_modern_state,
    'Can the formalist reading sustain itself institutionally under conditions requiring rapid regulatory response to novel challenges (pandemics, cyberattacks, AI systems)?',
    'Longitudinal analysis: track instances where Congress attempted pure legislative specification vs delegated agency response. Measure: time to rule, adaptability to new conditions, judicial invalidation rates, compensatory regulatory workarounds (non-statutory guidance, memoranda).',
    'If formalist reading proves unworkable: either (a) doctrine drifts toward functionalism (reading collapses), or (b) theater increases as agencies adopt nominally non-legislative forms while exercising legislative power (piton classification). Institutional viability is medium-confidence empirical claim, not a values question.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_viability_under_modern_state, empirical, 'Whether formalist reading can sustain institutional viability under modern governance demands').

omega_variable(
    committer_axiom_underdetermination,
    'Is the foundational formalist axiom (strict textual originalism about legislative power) grounded in constitutional text interpretation, or is it grounded in the committer''s preference for legislative control?',
    'Compare formalist reading''s textual arguments with functionalist reading''s textual arguments. Identify points where both cite text but arrive at opposite conclusions. Assess whether the divergence is driven by interpretation method (originalist vs living constitution) or by who benefits from each reading (legislative vs executive power concentration).',
    'If grounded in interpretation method: reading''s axiom is defensible as principled hermeneutics. If grounded in preference for legislative power: reading is a committer preference masked as constitutional requirement, reclassifying the constraint''s nature from snare-by-structure to snare-by-design. This does not change the classification (snare remains snare) but changes its epistemic status (natural reading vs constructed constraint).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_axiom_underdetermination, conceptual, 'Whether formalist axiom derives from interpretation method or power preference').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(separation_of_powers_text__formalist_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sopform_tr_t0, separation_of_powers_text__formalist_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(sopform_tr_t50, separation_of_powers_text__formalist_reading, theater_ratio, 50, 0.36).
narrative_ontology:measurement(sopform_tr_t100, separation_of_powers_text__formalist_reading, theater_ratio, 100, 0.38).

% Extraction over time
narrative_ontology:measurement(sopform_be_t0, separation_of_powers_text__formalist_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(sopform_be_t50, separation_of_powers_text__formalist_reading, base_extractiveness, 50, 0.62).
narrative_ontology:measurement(sopform_be_t100, separation_of_powers_text__formalist_reading, base_extractiveness, 100, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(sopform_su_t0, separation_of_powers_text__formalist_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(sopform_su_t50, separation_of_powers_text__formalist_reading, suppression_requirement, 50, 0.68).
narrative_ontology:measurement(sopform_su_t100, separation_of_powers_text__formalist_reading, suppression_requirement, 100, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(separation_of_powers_text__formalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(separation_of_powers_text__formalist_reading, separation_of_powers_text__functionalist_reading).
narrative_ontology:affects_constraint(separation_of_powers_text__formalist_reading, separation_of_powers_text__unitary_executive_reading).
narrative_ontology:affects_constraint(separation_of_powers_text__formalist_reading, major_questions_doctrine).
narrative_ontology:affects_constraint(separation_of_powers_text__formalist_reading, arbitrary_and_capricious_review_standard).

% DUAL FORMULATION NOTE:
% The separation_of_powers_text kernel has three competing readings: formalist (this constraint), functionalist, and unitary executive. Each reading instantiates a different constraint with different epsilon values and different victim/beneficiary sets. The formalist reading (ε=0.68) treats agencies as victims and Congress as beneficiary. The functionalist reading (ε≈0.35-0.45, expected tangled_rope) treats agencies and Congress as co-coordinators with balanced extraction. The unitary executive reading (ε≈0.40-0.55, expected tangled_rope or snare depending on executive power conception) treats agencies as presidential instruments and Congress as victim of executive power. Each is a distinct constraint story; they are linked via network relationships showing that each reading influences the others through institutional competition for doctrinal control.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(separation_of_powers_text__formalist_reading, institutional, 0.15).
constraint_indexing:directionality_override(separation_of_powers_text__formalist_reading, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
