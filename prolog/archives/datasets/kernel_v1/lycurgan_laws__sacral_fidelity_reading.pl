% ============================================================================
% CONSTRAINT STORY: lycurgan_laws__sacral_fidelity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lycurgan_laws__sacral_fidelity_reading, []).

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
 *   constraint_id: lycurgan_laws__sacral_fidelity_reading
 *   human_readable: Lycurgan Laws as Sacred, Unchangeable Divine Ordinance
 *   domain: political_philosophy/constitutional_theory/commitment_systems
 *
 * SUMMARY:
 *   The Lycurgan laws, as sacred and unchangeable divine ordinance, represent
 *   a commitment-system constraint where Spartan constitutional authority
 *   grounds itself in a legendary legislator (Lycurgus) whose laws are
 *   presented as immutable revelation. This reading instantiates the SACRAL
 *   FIDELITY frame: laws cannot be revised because they are not human
 *   artifacts but divine ordinances. Under this reading, constitutional
 *   change is metaphysically impossible and ethically impermissible — to
 *   alter Lycurgan law is to commit impiety against the founder-god and the
 *   social order itself. The constraint binds Spartan society through a dual
 *   mechanism: legal prohibition on revision (structural) combined with
 *   sacralization of immutability (cognitive/identity). The elite benefit
 *   from stability and predictable succession; the commons and helots bear
 *   the cost of absolute rigidity. Over 400 years of observable time (archaic
 *   through classical Sparta), the measurements show rising theater_ratio
 *   (immutability doctrine becomes increasingly performative as actual
 *   adaptive practice diverges from sacred law), rising extractiveness (the
 *   constraint's extraction function becomes more concentrated), and rising
 *   suppression_requirement (the enforcement burden grows as external
 *   pressures mount). The constraint reaches snare classification by the
 *   classical period despite originating as rope-like coordination mechanism
 *   — the escalating theater and suppression suggest that as the system
 *   becomes rigid, it must rely more heavily on performative legitimacy and
 *   coercive enforcement.
 *
 * KEY AGENTS:
 *   - Spartan Commons (powerless/trapped): Hoplites, non-elite warriors, craftspeople bound by absolute obedience to laws framed as sacred; no revision mechanism; experience maximum extraction
 *   - Helot Class (powerless/trapped): Permanently subjugated agrarian underclass; sacral framing immunizes constraint against reform arguments; total extraction with maximal suppression
 *   - Conservative Elite (institutional/arbitrage): Warrior caste, ephors, Agiadai lineage — primary beneficiaries experiencing constraint as coordination mechanism enabling warrior cohesion and predictable succession
 *   - Reform-Minded Factions (organized/constrained): Post-Persian War reformers, ephors seeking expanded authority, younger agiadai challenging conservative hegemony — constrained by immutability doctrine yet benefit from shared martial identity
 *   - The Gerousia (institutional/constrained): Council of elders maintaining ritual of Lycurgan fidelity while adapting through reinterpretation and evasion; high performative content masking institutional drift
 *   - Analytical Observer (analytical/analytical): Civilizational perspective risking naturalization of constructed immutability doctrine as immutable natural law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lycurgan_laws__sacral_fidelity_reading, 0.58).
domain_priors:suppression_score(lycurgan_laws__sacral_fidelity_reading, 0.72).
domain_priors:theater_ratio(lycurgan_laws__sacral_fidelity_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lycurgan_laws__sacral_fidelity_reading, snare).
narrative_ontology:human_readable(lycurgan_laws__sacral_fidelity_reading, "Lycurgan Laws as Sacred, Unchangeable Divine Ordinance").
narrative_ontology:topic_domain(lycurgan_laws__sacral_fidelity_reading, "political_philosophy/constitutional_theory/commitment_systems").

domain_priors:requires_active_enforcement(lycurgan_laws__sacral_fidelity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lycurgan_laws__sacral_fidelity_reading, '4cd998bd-e6df-4042-af9e-c015eec73487').
narrative_ontology:cs_kernel_codification('4cd998bd-e6df-4042-af9e-c015eec73487', fixed_text).
narrative_ontology:cs_authority_grounding('4cd998bd-e6df-4042-af9e-c015eec73487', lineage).
narrative_ontology:cs_interpretation_layer_present('4cd998bd-e6df-4042-af9e-c015eec73487').
narrative_ontology:cs_reading_relation('4cd998bd-e6df-4042-af9e-c015eec73487', lycurgan_laws__demographic_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('4cd998bd-e6df-4042-af9e-c015eec73487', lycurgan_laws__adaptive_fiction_reading, coexists_with).
narrative_ontology:cs_axiom('4cd998bd-e6df-4042-af9e-c015eec73487', foundational, lycurgan_immutability_divine_mandate).
narrative_ontology:cs_axiom_status(lycurgan_immutability_divine_mandate, holdable).
narrative_ontology:cs_axiom_grounding('4cd998bd-e6df-4042-af9e-c015eec73487', lycurgan_immutability_divine_mandate, theological).
narrative_ontology:cs_axiom('4cd998bd-e6df-4042-af9e-c015eec73487', foundational, constitutional_rigidity_as_virtue).
narrative_ontology:cs_axiom_status(constitutional_rigidity_as_virtue, holdable).
narrative_ontology:cs_axiom_grounding('4cd998bd-e6df-4042-af9e-c015eec73487', constitutional_rigidity_as_virtue, deontological).
narrative_ontology:cs_reference_frame('4cd998bd-e6df-4042-af9e-c015eec73487', lycurgan_sacred_constitution).
narrative_ontology:cs_drift_state('4cd998bd-e6df-4042-af9e-c015eec73487', classical_period_crisis, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4cd998bd-e6df-4042-af9e-c015eec73487', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(lycurgan_laws__sacral_fidelity_reading, lycurgan_laws).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lycurgan_laws__sacral_fidelity_reading, conservative_elite).
narrative_ontology:constraint_beneficiary(lycurgan_laws__sacral_fidelity_reading, warrior_caste).
narrative_ontology:constraint_victim(lycurgan_laws__sacral_fidelity_reading, spartan_commons).
narrative_ontology:constraint_victim(lycurgan_laws__sacral_fidelity_reading, helot_class).
narrative_ontology:constraint_victim(lycurgan_laws__sacral_fidelity_reading, reform_advocates).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SPARTAN COMMONS (SNARE) — Caught in absolute obedience to laws declared immutable by divine will. No legal mechanism for revision, no appeal beyond Lycurgus. Trapped by both structural barriers (no recognized legislative process) and cognitive framing (laws are sacred law, not human artifact). Maximum extraction with minimal coordination benefit — the commons exist to sustain the warrior caste.
constraint_indexing:constraint_classification(lycurgan_laws__sacral_fidelity_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: HELOT CLASS (SNARE) — Permanently subjugated under laws framed as sacred ordinance. The sacral framing immunizes the constraint against reform: arguing for helot freedom becomes impiety. Extraction is total and suppression is maximal — enforced through both material force and metaphysical closure.
constraint_indexing:constraint_classification(lycurgan_laws__sacral_fidelity_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: CONSERVATIVE ELITE (ROPE) — The warrior caste and ephors experience the sacred immutability as coordination: it binds together a warrior society with high cohesion and predictable succession. The elite have arbitrage options (they can reinterpret law, migrate, shift institutional roles); the sacral framing serves their interest in stability. Net beneficiary through coordination benefit.
constraint_indexing:constraint_classification(lycurgan_laws__sacral_fidelity_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: REFORM-MINDED FACTIONS (TANGLED ROPE) — Organized agents (Agiadai lineage challengers, ephors seeking to expand power, post-Persian War reformers) see the constraint as both coordinating shared martial culture AND extracting constraint on institutional adaptation. They benefit from the common identity and military cohesion yet are constrained by the sacral immutability doctrine when seeking to modify laws in response to external pressures. Moderate extraction with genuine coordination function underneath.
constraint_indexing:constraint_classification(lycurgan_laws__sacral_fidelity_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / IMMUTABLE LAW VIEW (MOUNTAIN) — From the frame of Lycurgus-as-sacred-legislator, the laws are treated as irreducible natural law: they emerge from divine will, cannot be altered without impiety, and form an immutable foundation for Spartan excellence. This perspective frames constitutional revision as metaphysically impossible. However, the structural data (beneficiaries, victims, enforcement mechanisms) will trigger false summit detection in the engine.
constraint_indexing:constraint_classification(lycurgan_laws__sacral_fidelity_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: GEROUSIA / INSTITUTIONAL APPARATUS (PITON) — The council of elders maintains the ritual of Lycurgan fidelity (theater_ratio = 0.68) while adapting law through reinterpretation and evasion (ephoral decisions, xenelasia adjustments, agoge curriculum drift). The institutional apparatus performs immutability while practicing gradual modification. High theater — the constraint's functional role has atrophied relative to its performative centrality.
constraint_indexing:constraint_classification(lycurgan_laws__sacral_fidelity_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lycurgan_laws__sacral_fidelity_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(lycurgan_laws__sacral_fidelity_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(lycurgan_laws__sacral_fidelity_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(lycurgan_laws__sacral_fidelity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(lycurgan_laws__sacral_fidelity_reading, TR),
    TR >= 0.70.

:- end_tests(lycurgan_laws__sacral_fidelity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts significant costs from commons and helots (enforced rigidity, zero revision capacity, compulsory military discipline) while benefiting the elite through stable succession and warrior cohesion. The extraction is not maximal (0.66+) because the constraint does provide genuine coordination benefits to all parties — shared martial identity, predictable institutional structure, clarity of obligation. The upward trend over the 400-year interval (0.42 → 0.58) reflects accumulating rigidity as external pressures (Persian invasions, Peloponnesian War, regional competition) mount, forcing more aggressive enforcement of immutability doctrine. Suppression (0.72): High. Multiple suppressive mechanisms: no legal revision process (structural), sacralization of immutability (cognitive), harsh penalties for reform advocacy (institutional), helot subjugation enforced through state terror (coercive). The suppression is not total (0.80+) because the gerousia can interpret law, ephors can evade through creative reinterpretation, and lower-class Spartans retain some agency within the fixed framework. Suppression rises over time as the system becomes more rigid and enforcement infrastructure intensifies. Theater ratio (0.68): High. The constraint's performative content increases significantly. By the classical period, the gerousia is maintaining ritual fidelity to Lycurgan immutability while adapting law through evasion and reinterpretation (ephoral decisions, agoge curriculum drift, foreign relations pragmatism). The public ceremony of immutability persists while actual practice diverges — a core piton signature. The upward trend reflects this growing gap: archaic period had lower theater because the immutability doctrine was less formalized; classical period shows higher theater as the doctrine becomes institutionalized and ritualized.
 *
 * PERSPECTIVAL GAP:
 *   The sacral fidelity reading produces sharp perspectival divergence. The conservative elite (rope perspective) experience the constraint as coordination — sacred law enables warrior cohesion and collective identity. The analytical observer (mountain perspective) risks treating immutability as natural law rather than constructed doctrine. The reform-minded factions (tangled rope perspective) see mixed coordination and extraction — they benefit from shared identity but are constrained by zero-revision doctrine. The commons and helots (snare perspective) experience pure extraction with no coordination benefit — they have no stake in elite stability and no exit option. The gerousia (piton perspective) sees the constraint's degradation: maintaining ritual immutability while practicing adaptive reinterpretation. This perspectival gap reveals the constraint's structural instability — the mountain classification (from the analytical observer's frame) is a false summit, naturalization of a contingent institutional commitment that benefits the elite and is maintained through escalating enforcement and theater.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from the agent's structural position relative to the constraint. Conservative elite (institutional power, arbitrage exit options, beneficiary status) experience low d (~0.12-0.15), producing negative or near-zero χ — they see the constraint as enabling, not extracting. Commons and helots (powerless, trapped exit, victim status) experience high d (~0.90-0.95), producing maximum χ — full extraction. Reform-minded factions (organized power, constrained exit, mixed victim/beneficiary status) experience moderate d (~0.45-0.55), producing moderate χ and tangled rope classification. The gerousia (institutional power, constrained exit, beneficiary status) experiences low-moderate d (~0.25-0.35), filtered through high theater ratio, producing piton classification. The analytical observer (analytical power, analytical exit, neutral position) derives canonical d (~0.72), producing mountain classification under the natural law frame — but the beneficiary presence and victim declarations trigger false summit detection.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy through explicit kernel decomposition and reading differentiation. The sacral fidelity reading instantiates ONE interpretation of the Lycurgan kernel. The sibling readings (demographic trap, adaptive fiction) instantiate different interpretations. This reading's snare classification at the analytical level (mountain false summit) is not in tension with the organizational-agent tangled rope classification because they are measuring different structural phenomena: this reading privileges the sacrality frame (producing mountain if natural law), while the organizational-agent perspective privileges the actual institutional adaptation (tangled rope). The mandatrophy is resolved by acknowledging that 'Lycurgan law' is a contested kernel with multiple legitimate readings, and that the classification varies by reading. There is no single correct type — the presheaf over readings IS the analytical content.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_will_vs_constructed_doctrine,
    'Is the sacrality of Lycurgan law grounded in genuine religious experience (divine revelation) or in institutional construction of a mythological founder?',
    'Historical archaeology of Lycurgus as historical figure vs legendary construct; comparison of Lycurgan law precursors in Dorian law codes; analysis of when the full sacred immutability doctrine emerges in Spartan institutional evolution',
    'If genuinely revealed (mountain logic): the constraint is a natural law of Spartan constitution. If constructed myth (snare logic): the ''divinity'' is a cover story for aristocratic extraction, and the constraint reclassifies to pure snare with higher χ.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_will_vs_constructed_doctrine, empirical, 'Whether Lycurgan sacrality is revelation or constructed myth').

omega_variable(
    immutability_doctrine_emergence_timing,
    'When does the doctrine of absolute Lycurgan immutability crystallize in Spartan practice — at Lycurgus''s actual time (8th-7th century), during the archaic period, or later during the classical crisis?',
    'Comparative analysis of constitutional rhetoric across periods; examination of actual legal modifications (ephoral expansion, agoge changes, foreign relations shifts) and contemporary legitimating narratives; chronology of texts claiming immutability vs texts permitting revision',
    'If immutability doctrine is archaic: it is deeply embedded and represents stable civilization commitment. If it crystallizes during crisis (e.g., post-Persian wars, post-Leuctra): it is a reactive defensive move masking systemic fragility, suggesting higher suppression_requirement and lower accessible_revision_threshold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(immutability_doctrine_emergence_timing, empirical, 'Timeline of immutability doctrine crystallization').

omega_variable(
    commons_cognitive_capture_depth,
    'Do Spartan commons (hoplites, non-elite warriors, craftspeople) genuinely internalize the Lycurgan sacrality as identity-locked agents, or do they experience it as external structural oppression (trapped)?',
    'Analysis of internal dissent, escape rates, participation in ephoral challenges, acceptance of law despite hardship; comparison with attested complaints or resistance in helot revolts and mercenary defections; ethnographic reconstruction of internalized piety vs grudging compliance',
    'If deeply internalized (identity_locked): the suppression is both structural and cognitive; exit would require identity dissolution. If primarily external (trapped): suppression is high but not as resistant to reform — changing the institutional structure could enable exit. Classification implications: identity_locked commons → higher effective suppression, lower perceived accessibility for change; trapped commons → high suppression but potential for rapid collapse if enforcement infrastructure breaks.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(commons_cognitive_capture_depth, empirical, 'Depth of commons cognitive capture by Lycurgan sacrality').

omega_variable(
    sibling_reading_foreclosure_status,
    'Does the sacral fidelity reading logically foreclose the demographic trap reading and adaptive fiction reading, or can a Spartan actor coherently hold the sacral immutability claim while also acknowledging systemic demographic failure or adaptive reinterpretation?',
    'Textual analysis of Spartan institutional rhetoric during crisis periods (4th century decline, post-Leuctra); examination of whether reform movements claimed Lycurgan authority for their proposals or rejected immutability altogether; assessment of whether the fiction of immutability persisted even as practice evolved',
    'If sacral fidelity forecloses the other readings: they are mutually exclusive accounts held by different parties (coexists_with relation). If sacral fidelity is compatible with institutional adaptation justified via Lycurgan reinterpretation: the readings coexist even within single actors'' frameworks (influences relation rather than foreclosure).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_status, conceptual, 'Whether sacral fidelity reading forecloses sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lycurgan_laws__sacral_fidelity_reading, 0, 400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lycurg_theater_t0, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(lycurg_theater_t200, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 200, 0.52).
narrative_ontology:measurement(lycurg_theater_t400, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 400, 0.68).

% Extraction over time
narrative_ontology:measurement(lycurg_extract_t0, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(lycurg_extract_t200, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 200, 0.5).
narrative_ontology:measurement(lycurg_extract_t400, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 400, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(lycurg_suppress_t0, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(lycurg_suppress_t200, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 200, 0.68).
narrative_ontology:measurement(lycurg_suppress_t400, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 400, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lycurgan_laws__sacral_fidelity_reading, identity_coordination).
narrative_ontology:affects_constraint(lycurgan_laws__sacral_fidelity_reading, lycurgan_laws__demographic_trap_reading).
narrative_ontology:affects_constraint(lycurgan_laws__sacral_fidelity_reading, lycurgan_laws__adaptive_fiction_reading).

% DUAL FORMULATION NOTE:
% The Lycurgan laws constraint family consists of three structurally distinct readings with different ε values and different classification profiles. The sacral fidelity reading (this file) ε=0.58, snare/mountain mix. The demographic trap reading ε=0.72, snare. The adaptive fiction reading ε=0.35, tangled rope/piton mix. These are not the same constraint viewed from different angles — they represent different epistemic frames for interpreting the same historical institution. Each reading has its own network of downstream institutional effects, its own identification of beneficiaries and victims, and its own measurement profile. They are linked via network.affects_constraints to indicate that the Lycurgan kernel is contested and that adopting one reading constrains but does not determine adoption of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(lycurgan_laws__sacral_fidelity_reading, organized, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
