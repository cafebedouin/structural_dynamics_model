% ============================================================================
% CONSTRAINT STORY: qwerty_persistence__incumbent_preservation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence__incumbent_preservation_reading, []).

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
 *   constraint_id: qwerty_persistence__incumbent_preservation_reading
 *   human_readable: QWERTY Persistence via Incumbent Preservation (Defensive Suppression Reading)
 *   domain: technology_standards/industrial_path_dependence/organizational_strategy
 *
 * SUMMARY:
 *   The QWERTY keyboard layout persists globally as the dominant standard
 *   despite documented ergonomic and efficiency limitations of its design —
 *   it was originally optimized for mechanical typewriter technology to
 *   prevent hammer jams, not for typing speed or human comfort. This
 *   constraint is ONE READING of a contested kernel: the 'incumbent
 *   preservation' reading claims that QWERTY dominance is actively maintained
 *   by manufacturers, training institutions, and platform vendors through
 *   defense mechanisms (marketing emphasis on familiarity, firmware defaults,
 *   training curriculum standardization, patent enforcement, suppression of
 *   alternative layout research). The sibling 'lapsed alternatives' reading
 *   claims alternatives (Dvorak, Colemak, BÉPO) failed on their own merits —
 *   coordination-level problems unrelated to incumbent action — and QWERTY
 *   won passively through critical mass equilibrium. This story instantiates
 *   the incumbent-preservation reading: QWERTY persists because identifiable
 *   beneficiaries (manufacturers, training institutions, trained typists with
 *   high switching costs) actively defend it and bear costs to suppress
 *   alternatives. The constraint is TANGLED ROPE: there is genuine
 *   coordination function (global interoperability, equipment compatibility,
 *   workforce transferability) AND asymmetric extraction (non-standard
 *   adopters bear individual switching costs while no coordinated mechanism
 *   compensates them for the collective efficiency loss).
 *
 * KEY AGENTS:
 *   - Keyboard Manufacturers (Institutional): Primary beneficiary (arbitrage exit, immediate time horizon) — capture monopoly rent through standardized production tooling, global supply chains, embedded firmware defaults. Active preservation via marketing, firmware design, training subsidies.
 *   - Trained Typist Workforce (Moderate, Constrained): Moderate victim — face high retraining costs if switching; also benefit from global standard compatibility (constrained exit). Experience moderate extraction.
 *   - Alternative Layout Adopters (Powerless, Trapped): Primary victim — bear full individual switching costs (training, incompatibility, stigma) with zero exit option at global scale. Experience maximum extraction (snare classification).
 *   - Training Institutions (Institutional, Arbitrage): Secondary beneficiary — stable curriculum, globally transferable skills, no motivation to change despite low cost to do so. Defense via curriculum standardization.
 *   - Computing Platform Vendors (Powerful, Mobile): Meta-level beneficiary — benefit from QWERTY standardization (lower OS/firmware complexity) but have sufficient power to shift if utility changed. Defend QWERTY through defaults and cultural narrative but not dependent on it.
 *   - Typing Standards Bodies (Institutional, Arbitrage): Vestigial authority — formal standards bodies maintain specifications but enforcement is atrophied; market lock-in is the actual enforcement mechanism (piton classification).
 *   - Analytical Observer (Analytical, Analytical): Risks naturalizing contingent incumbent defense as an immutable coordination law.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence__incumbent_preservation_reading, 0.52).
domain_priors:suppression_score(qwerty_persistence__incumbent_preservation_reading, 0.68).
domain_priors:theater_ratio(qwerty_persistence__incumbent_preservation_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence__incumbent_preservation_reading, tangled_rope).
narrative_ontology:human_readable(qwerty_persistence__incumbent_preservation_reading, "QWERTY Persistence via Incumbent Preservation (Defensive Suppression Reading)").
narrative_ontology:topic_domain(qwerty_persistence__incumbent_preservation_reading, "technology_standards/industrial_path_dependence/organizational_strategy").

domain_priors:requires_active_enforcement(qwerty_persistence__incumbent_preservation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence__incumbent_preservation_reading, 'd971d27f-f083-41b6-af97-0d9b18f0b9c2').
narrative_ontology:cs_kernel_codification('d971d27f-f083-41b6-af97-0d9b18f0b9c2', formalized).
narrative_ontology:cs_authority_grounding('d971d27f-f083-41b6-af97-0d9b18f0b9c2', extraction).
narrative_ontology:cs_interpretation_layer_present('d971d27f-f083-41b6-af97-0d9b18f0b9c2').
narrative_ontology:cs_reading_relation('d971d27f-f083-41b6-af97-0d9b18f0b9c2', qwerty_persistence__lapsed_alternatives_reading, coexists_with).
narrative_ontology:cs_axiom('d971d27f-f083-41b6-af97-0d9b18f0b9c2', foundational, incumbent_defense_necessary_for_persistence).
narrative_ontology:cs_axiom_status(incumbent_defense_necessary_for_persistence, holdable).
narrative_ontology:cs_axiom_grounding('d971d27f-f083-41b6-af97-0d9b18f0b9c2', incumbent_defense_necessary_for_persistence, empirically_contingent).
narrative_ontology:cs_axiom('d971d27f-f083-41b6-af97-0d9b18f0b9c2', secondary, alternative_layouts_technically_sufficient).
narrative_ontology:cs_axiom_status(alternative_layouts_technically_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('d971d27f-f083-41b6-af97-0d9b18f0b9c2', alternative_layouts_technically_sufficient, empirically_contingent).
narrative_ontology:cs_reference_frame('d971d27f-f083-41b6-af97-0d9b18f0b9c2', incumbent_defense_coordination_equilibrium).
narrative_ontology:cs_drift_state('d971d27f-f083-41b6-af97-0d9b18f0b9c2', contemporary_digital_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d971d27f-f083-41b6-af97-0d9b18f0b9c2', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(qwerty_persistence__incumbent_preservation_reading, qwerty_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, keyboard_manufacturers).
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, trained_typist_workforce).
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, training_institutions).
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, hardware_embedded_qwerty).
narrative_ontology:constraint_victim(qwerty_persistence__incumbent_preservation_reading, alternative_keyboard_adopters).
narrative_ontology:constraint_victim(qwerty_persistence__incumbent_preservation_reading, efficiency_seekers).
narrative_ontology:constraint_victim(qwerty_persistence__incumbent_preservation_reading, ergonomic_design_space).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ERGONOMIC ALTERNATIVE SEEKER (SNARE) — Trapped by installed-base lock-in and training costs. Cannot exit QWERTY without bearing full retraining cost individually (incompatible with global installed base). Suppression is structural: manufacturers defend QWERTY actively to protect production tooling. Experiences maximum extraction — bears transition costs with zero exit option.
constraint_indexing:constraint_classification(qwerty_persistence__incumbent_preservation_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NICHE ADOPTER COHORT (TANGLED ROPE) — Can exit QWERTY at regional scale (Dvorak communities, specialized ergonomic keyboards) but faces high costs: training time, software lock-in, incompatibility with shared typing environments, stigma as non-standard. Also genuinely benefits from QWERTY's coordination function: ability to type on any keyboard globally, compatibility with shared documents/machines. Extraction is significant but not total — has agency and some coordination benefit.
constraint_indexing:constraint_classification(qwerty_persistence__incumbent_preservation_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: KEYBOARD MANUFACTURERS (ROPE) — Primary beneficiary. Experiences QWERTY as pure coordination: standardization enables mass production, global supply chains, interchangeable tooling. No exit costs; can arbitrage any alternative layout through subsidiary product lines without threatening core business. Benefits flow toward this agent. Defense activities (lobbying against alternative standards, embedding QWERTY in firmware, supporting training institutions) are framed as 'maintaining interoperability,' a coordination narrative.
constraint_indexing:constraint_classification(qwerty_persistence__incumbent_preservation_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: TRAINING INSTITUTIONS (ROPE) — Secondary beneficiary. Invested in QWERTY curriculum, teaching materials, standardized typing tests. Benefits from global standardization (students' skills are globally transferable; curriculum is stable). Can arbitrage by adding alternative layouts to curriculum (low marginal cost) but prefer not to — stability is beneficial. No meaningful extraction from this agent's perspective.
constraint_indexing:constraint_classification(qwerty_persistence__incumbent_preservation_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: COMPUTING PLATFORM VENDORS (TANGLED ROPE) — Powerful institutional actors (OS vendors, device manufacturers) benefit from QWERTY standardization (lower design complexity, easier software adaptation to hardware variants) but have sufficient mobile exit capacity to adopt alternative layouts if utility shifted dramatically (they could redefine 'standard' at their platform layer). Defends QWERTY through firmware/OS defaults and cultural narrative-setting but does not depend on it for survival. Moderate extraction — benefits substantial but not binding.
constraint_indexing:constraint_classification(qwerty_persistence__incumbent_preservation_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: TYPING STANDARD BUREAU (PITON) — Formal standards bodies (ANSI, ISO keyboard layout standards) maintain QWERTY specifications through institutional inertia. The theatrical function is preserved (committees meet, standards are published) but the actual enforcement mechanism has atrophied — no vendor fears standards non-compliance because QWERTY dominance is already self-reinforcing. Theater ratio is moderate (standards bodies still have some legitimacy) but core function (enforcing layout standardization) is maintained by market lock-in, not by authority.
constraint_indexing:constraint_classification(qwerty_persistence__incumbent_preservation_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational scope, QWERTY persistence appears as an immutable consequence of network effects and installed-base lock-in: once a standard reaches critical mass, any alternative is mathematically impossible to adopt globally. This perspective naturalizes the constraint as an irreducible feature of coordination dynamics. However, the structural data contradicts this classification — this is a FALSE SUMMIT. The 'natural law' framing obscures that active incumbent defense (suppression cost 0.68) is a necessary condition for QWERTY persistence. Without that defense, alternatives could propagate at niche scale. The naturalization serves incumbent interests.
constraint_indexing:constraint_classification(qwerty_persistence__incumbent_preservation_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence__incumbent_preservation_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(qwerty_persistence__incumbent_preservation_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(qwerty_persistence__incumbent_preservation_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(qwerty_persistence__incumbent_preservation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(qwerty_persistence__incumbent_preservation_reading, TR),
    TR >= 0.70.

:- end_tests(qwerty_persistence__incumbent_preservation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   EXTRACTIVENESS (0.52): Moderate-high. Base value reflects that extraction exists — non-standard adopters incur switching costs (training time, incompatibility, social friction) without receiving compensation. However, extractiveness is NOT 0.72 because the constraint also delivers genuine coordination benefit: global typing interoperability is valuable and QWERTY enables it. The 0.52 figure reflects the balance: extraction sufficient to suppress alternatives + suppression is actively maintained (not passive equilibrium) + but coordination function is real. The measurement trajectory (0.35 → 0.52 over 60 years) shows increasing incumbent defense intensity in response to ergonomic and efficiency criticism. SUPPRESSION (0.68): High. Mechanisms include: (1) Firmware defaults embedding QWERTY at OS level, making alternative layouts require conscious override. (2) Training institution standardization on QWERTY curriculum. (3) Absence of manufacturer R&D investment in alternative layouts. (4) Marketing narrative emphasizing 'familiarity' and 'universal standard.' (5) Patent enforcement (historical attempts to slow Dvorak patent licensing). (6) Network-effect narrative — 'you must use QWERTY to be compatible' — which benefits incumbents by making alternatives seem irrational. Suppression is not brute-force (keyboards CAN be reprogrammed, alternatives CAN be learned) but structural: the institutional arrangement makes alternatives effortful and socially costly. THEATER RATIO (0.55): Moderate. The coordination function is real (interoperability genuinely matters) so theater is not high (not 0.70+). But some of the institutional defense has performative content: standards committees maintain specifications primarily because market lock-in is already established, not because enforcement is needed. The ratio increases over time (0.40 → 0.55) as incumbent defense mechanisms become more sophisticated (firmware defaults, platform narrative control) relative to the actual technical necessity of the standard.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates why tangled rope requires both beneficiaries and victims with asymmetric extraction. The beneficiaries (manufacturers, training institutions) genuinely coordinate — global QWERTY standardization enables supply chains and educational transfers that would not exist if every region had its own layout. That IS coordination benefit, not a cover story. But the asymmetry is real: non-standard adopters incur individual switching costs (training time, incompatibility, social friction) without receiving compensation or experiencing the coordination benefit at the same intensity. They are trapped by global coordination that does not serve them. The perspectival gap exists because the same institutional structure ('QWERTY is the standard') is experienced as beneficial coordination (manufacturers, training institutions) and as extraction suppression (alternative adopters). This is not a misperception — it is structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are computed from each agent's structural relationship to the constraint. Keyboard manufacturers start with d ≈ 0.05 (full beneficiary with arbitrage exit → low extraction from their perspective). Trained typists start with d ≈ 0.50-0.65 (both benefit from global standard AND bear switching costs if they want alternatives → moderate extraction). Alternative adopters have d ≈ 0.95 (full target of suppression with trapped exit → maximum extraction from their perspective). The sigmoid f(d) maps these to experienced extractiveness chi. The beneficiary-dominant structure (manufacturers and training institutions defending QWERTY actively, bearing costs to maintain suppression) generates the tangled-rope classification: there IS coordination (all parties benefit from interoperability), AND there IS asymmetric extraction (non-standard adopters bear unmapped switching costs). Directionality captures why the same constraint appears as pure coordination (rope) from the manufacturer perspective but as snare from the alternative-adopter perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by clearly declaring both coordination (genuine global interoperability benefit) and extraction (asymmetric cost distribution to non-standard adopters). Tangled rope classification prevents mislabeling this as pure coordination (rope) or pure extraction (snare). The constraint is neither. It is a coordination mechanism with embedded asymmetric extraction enforced by incumbent defense. The mandatrophy resolution is that BOTH elements are structural — you cannot have the coordination without the extraction, and you cannot solve the extraction without changing the coordination structure (which would harm the beneficiaries who depend on global standardization).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    incumbent_defense_sufficiency,
    'Is QWERTY persistence driven primarily by incumbent active defense (suppression) or by passive network effects and coordination equilibrium?',
    'Counterfactual analysis: modeling keyboard market outcomes if incumbent defense mechanisms (marketing emphasis, firmware defaults, training subsidies, patent enforcement) were removed while installed base remained. Historical case study: periods when incumbent defense was weaker (1950s-70s Dvorak adoption attempts, recent ergonomic keyboard surges) vs periods of strong defense (post-1985 computerization push by manufacturers).',
    'If primarily defense-driven: epsilon reduces to ~0.35 (tangled rope with lower suppression floor), extraction is contingent and reversible. If primarily network-effect-driven: epsilon holds at 0.52, constraint is harder to dislodge via policy intervention alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incumbent_defense_sufficiency, empirical, 'Whether QWERTY persistence requires active incumbent defense or emerges passively from coordination').

omega_variable(
    alternative_layout_technical_sufficiency,
    'Do credible alternative layouts (Dvorak, Colemak, BÉPO) provide documented ergonomic or efficiency gains sufficient to overcome training and switching costs IF network effects were neutralized?',
    'Meta-analysis of typing speed, error rate, and repetitive strain injury studies comparing QWERTY to alternatives under controlled conditions. Filtering for: studies using naive users (not QWERTY-trained), adequate training duration to reach asymptotic performance, blinded design. Comparison to actual workplace outcomes for small ergonomic-adopter cohorts.',
    'If alternatives genuinely superior by >15% efficiency/ergonomics: incumbent defense is suppressing a technologically superior alternative (higher victim extraction assessment). If alternatives marginal/context-dependent: coordination on any standard (including QWERTY) is defensible, reducing snare severity for victims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_layout_technical_sufficiency, empirical, 'Whether alternative keyboard layouts provide measurable superiority over QWERTY').

omega_variable(
    reading_boundary_demarcation,
    'Is this the ''incumbent-active-defense'' reading (beneficiaries deliberately suppress alternatives) or the ''lapsed-alternatives'' reading (alternatives failed on their own merits and incumbent passively dominated)? Where is the boundary?',
    'Document the core structural claim of THIS reading: incumbents actively preserve dominance via defense mechanisms (R&D suppression, marketing narrative control, firmware defaults, patent enforcement, training institution support). The sibling reading claims alternatives lapsed due to collective coordination failure unrelated to incumbent action. Empirical resolution: evidence of incumbent awareness of alternatives + deliberate suppression attempts (memos, patent applications, marketing decisions to emphasize QWERTY) vs evidence of alternative failure prior to or independent of incumbent action.',
    'This reading assigns explicit intentionality to beneficiaries (active preservation) and places extraction in the suppression mechanism. Sibling reading assigns no intentionality and locates persistence in passive network effects. The empirical boundary is the presence/absence of documented incumbent decision-making to defend QWERTY.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_boundary_demarcation, empirical, 'Demarcation between incumbent-active-defense and passive-coordination-failure readings').

omega_variable(
    extracted_surplus_quantification,
    'What is the magnitude of consumer surplus loss from QWERTY persistence (foregone efficiency gains + training costs + ergonomic harms) relative to incumbent capital protection?',
    'Estimate: (1) Productivity loss from suboptimal layout (typing speed penalty, error rate increase, retraining time per generation). (2) Health costs from ergonomic suboptimality (RSI prevalence studies comparing QWERTY to alternatives, healthcare costs). (3) Capital protected by manufacturers through standardization lock-in (tooling amortization, supply chain lock-in rent, production standardization value). Compare surplus flows to identify who captures what.',
    'If consumer surplus loss >> incumbent capital protection: extraction is substantial and unjustified by coordination benefits (snare component strengthened). If surplus loss << capital protection OR capital protection is needed for coordination stability: extraction is moderate and partially justified (tangled rope component strengthened).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extracted_surplus_quantification, empirical, 'Quantification of extracted surplus in QWERTY persistence').

omega_variable(
    reading_axiom_empirical_contingency,
    'Is the axiom ''incumbent_defense_necessary_for_persistence'' empirically falsifiable or conceptually fixed?',
    'This axiom (incumbent defense is a necessary component of QWERTY persistence) is grounded in empirical-contingent claims about market dynamics and human behavior. If alternatives were adopted at non-trivial scale despite incumbent defense (proving alternatives can propagate even under suppression), or if QWERTY persisted at niche scale without incumbent defense (proving coordination alone sustains it), the axiom''s empirical grounding would shift but the axiom itself remains revisable. Track for axis_overriding drift if evidence accumulates.',
    'If empirical evidence shows incumbent defense is NOT necessary (or sufficient), this reading''s foundational premise is weakened and the sibling ''lapsed alternatives'' reading gains structural warrant. The constraint might then classify lower on extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_axiom_empirical_contingency, empirical, 'Empirical falsifiability of the incumbent-defense-necessity axiom').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence__incumbent_preservation_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwerty_incumb_theater_t0, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(qwerty_incumb_theater_t30, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 30, 0.48).
narrative_ontology:measurement(qwerty_incumb_theater_t60, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 60, 0.55).

% Extraction over time
narrative_ontology:measurement(qwerty_incumb_extract_t0, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(qwerty_incumb_extract_t30, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 30, 0.48).
narrative_ontology:measurement(qwerty_incumb_extract_t60, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 60, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(qwerty_incumb_suppress_t0, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(qwerty_incumb_suppress_t30, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement(qwerty_incumb_suppress_t60, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 60, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence__incumbent_preservation_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(qwerty_persistence__incumbent_preservation_reading, 0.18).
narrative_ontology:affects_constraint(qwerty_persistence__incumbent_preservation_reading, qwerty_persistence__lapsed_alternatives_reading).

% DUAL FORMULATION NOTE:
% The QWERTY persistence kernel decomposes into two structurally distinct constraint stories with different epsilon values and different mechanism accounts. This story (incumbent_preservation_reading, epsilon=0.52, tangled rope) models active incumbent defense as a necessary structural component. The sibling story (lapsed_alternatives_reading, epsilon=0.28, rope/piton) models alternatives failing on coordination-level problems independent of incumbent action. These are not the same constraint viewed from two angles — they have different extractiveness values and different causal mechanisms. They should be evaluated as separate constraints linked by the shared kernel they interpret. The reading_relations in cs_structure document their logical relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(qwerty_persistence__incumbent_preservation_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
