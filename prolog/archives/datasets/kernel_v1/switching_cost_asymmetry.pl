% ============================================================================
% CONSTRAINT STORY: switching_cost_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_switching_cost_asymmetry, []).

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
 *   constraint_id: switching_cost_asymmetry
 *   human_readable: Switching Cost Asymmetry in Keyboard Layout Standardization
 *   domain: technology_history/industrial_standards/path_dependence
 *
 * SUMMARY:
 *   The QWERTY keyboard layout represents a contested kernel in technology
 *   history: one reading treats it as a neutral outcome of path-dependent
 *   standardization (coordination problem solved, alternatives lapsed without
 *   antagonism), while another treats it as beneficiary-preserved incumbency
 *   (manufacturers and incumbent typists actively maintained dominance
 *   against superior alternatives). This constraint tests whether structural
 *   analysis can distinguish genuine path dependence from engineered lock-in,
 *   and whether identifying beneficiaries necessarily implies D3 ARTIFACT
 *   (the accusation that beneficiary-hunting finds what it looks for by
 *   construction). The constraint exhibits tangled rope characteristics:
 *   genuine coordination function (manufacturing standardization reduces
 *   costs, typists benefit from universal compatibility) combined with
 *   asymmetric extraction (efficient alternatives are foreclosed, switching
 *   costs are borne by those seeking to exit). The measurement trajectory
 *   shows suppression declining slightly from mechanical typewriter era
 *   (0.62) through mechanical typewriter dominance (0.59) into digital era
 *   (0.54), while theater ratio rises (0.35 → 0.48) as the original
 *   mechanical justification becomes obsolete and the constraint persists
 *   through inertia. The extractiveness rises modestly (0.38 → 0.52) as the
 *   beneficiary advantage becomes clearer in the digital era when mechanical
 *   constraints no longer apply.
 *
 * KEY AGENTS:
 *   - Efficiency-Seeking Typist: Primary victim (powerless/trapped) — trapped by retraining costs, muscle memory lock-in, and global QWERTY dominance. Bears full cost of structural inefficiency.
 *   - Dvorak Advocate Community: Secondary victim/organized counterweight (moderate/constrained) — constrained by hardware limitations and social incompatibility but building alternative ecosystem through software, community coordination, and online education.
 *   - Keyboard Manufacturers (Typewriter and Electronic): Primary beneficiary (institutional/arbitrage) — standardization reduces manufacturing complexity, enables interchangeable parts, simplifies training. No pressure to exit.
 *   - Existing Typist Base: Beneficiary with lock-in (moderate/identity_locked) — beneficiaries of universal compatibility and no retraining requirement, but locked into QWERTY through skill investment. Their identity as 'skilled typists' is constituted through QWERTY proficiency.
 *   - Digital Transition Coalition: Organized actors building alternatives (organized/mobile) — open-source software projects (xmodmap, AutoHotkey), keyboard manufacturers experimenting with alternatives (Kinesis Advantage, ergonomic layouts), software developers enabling remapping. Structural position improves in digital era.
 *   - Mechanical Typewriter Industry: Legacy institutional actor (institutional/arbitrage) — constraint originated in mechanical engineering problem (prevent hammer jam). In digital era, this functional justification evaporates but constraint persists through institutional inertia.
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangement as immutable law of economic coordination (false summit risk).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(switching_cost_asymmetry, 0.52).
domain_priors:suppression_score(switching_cost_asymmetry, 0.58).
domain_priors:theater_ratio(switching_cost_asymmetry, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(switching_cost_asymmetry, extractiveness, 0.52).
narrative_ontology:constraint_metric(switching_cost_asymmetry, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(switching_cost_asymmetry, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(switching_cost_asymmetry, tangled_rope).
narrative_ontology:human_readable(switching_cost_asymmetry, "Switching Cost Asymmetry in Keyboard Layout Standardization").
narrative_ontology:topic_domain(switching_cost_asymmetry, "technology_history/industrial_standards/path_dependence").

domain_priors:requires_active_enforcement(switching_cost_asymmetry).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(switching_cost_asymmetry, '81c63827-cd72-457c-8de5-751fe3c3be70').
narrative_ontology:cs_kernel_codification('81c63827-cd72-457c-8de5-751fe3c3be70', distributed).
narrative_ontology:cs_authority_grounding('81c63827-cd72-457c-8de5-751fe3c3be70', distributed).
narrative_ontology:cs_reading_relation('81c63827-cd72-457c-8de5-751fe3c3be70', switching_cost_asymmetry_neutral_reading, coexists_with).
narrative_ontology:cs_reading_relation('81c63827-cd72-457c-8de5-751fe3c3be70', switching_cost_asymmetry_beneficiary_reading, coexists_with).
narrative_ontology:cs_axiom('81c63827-cd72-457c-8de5-751fe3c3be70', foundational, path_dependence_mathematically_inexorable).
narrative_ontology:cs_axiom_status(path_dependence_mathematically_inexorable, holdable).
narrative_ontology:cs_axiom_grounding('81c63827-cd72-457c-8de5-751fe3c3be70', path_dependence_mathematically_inexorable, empirically_contingent).
narrative_ontology:cs_axiom('81c63827-cd72-457c-8de5-751fe3c3be70', foundational, beneficiary_coalition_actively_maintains_standard).
narrative_ontology:cs_axiom_status(beneficiary_coalition_actively_maintains_standard, holdable).
narrative_ontology:cs_axiom_grounding('81c63827-cd72-457c-8de5-751fe3c3be70', beneficiary_coalition_actively_maintains_standard, empirically_contingent).
narrative_ontology:cs_axiom('81c63827-cd72-457c-8de5-751fe3c3be70', secondary, mechanical_necessity_justified_original_adoption).
narrative_ontology:cs_axiom_status(mechanical_necessity_justified_original_adoption, holdable).
narrative_ontology:cs_axiom_grounding('81c63827-cd72-457c-8de5-751fe3c3be70', mechanical_necessity_justified_original_adoption, empirically_contingent).
narrative_ontology:cs_axiom('81c63827-cd72-457c-8de5-751fe3c3be70', secondary, digital_era_sunset_actualizes_alternatives).
narrative_ontology:cs_axiom_status(digital_era_sunset_actualizes_alternatives, holdable).
narrative_ontology:cs_axiom_grounding('81c63827-cd72-457c-8de5-751fe3c3be70', digital_era_sunset_actualizes_alternatives, empirically_contingent).
narrative_ontology:cs_reference_frame('81c63827-cd72-457c-8de5-751fe3c3be70', standardized_keyboard_layout_equilibrium).
narrative_ontology:cs_drift_state('81c63827-cd72-457c-8de5-751fe3c3be70', digital_keyboard_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('81c63827-cd72-457c-8de5-751fe3c3be70', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(switching_cost_asymmetry, qwerty_incumbent_manufacturers).
narrative_ontology:constraint_beneficiary(switching_cost_asymmetry, existing_typist_base).
narrative_ontology:constraint_victim(switching_cost_asymmetry, alternative_layout_designers).
narrative_ontology:constraint_victim(switching_cost_asymmetry, efficiency_seekers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EFFICIENCY-SEEKING TYPIST (SNARE) — Trapped in QWERTY despite superior alternatives (Dvorak, Colemak) because switching costs are catastrophic: retraining time (40+ hours), muscle memory interference, social incompatibility (most machines are QWERTY), employment risk (hiring favors QWERTY proficiency). No meaningful exit despite structural inefficiency. Maximum experienced extraction — the constraint forces suboptimal performance.
constraint_indexing:constraint_classification(switching_cost_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DVORAK ADVOCATE COMMUNITY (TANGLED ROPE) — Constrained by training costs and keyboard hardware availability, but benefits from community coordination around alternative standards (software remapping, online tutorials, Dvorak communities). Some agency and some benefit, but extraction is real: the market coordination standard is externally imposed, making alternatives perpetually disadvantaged. The community must maintain its own ecosystem despite being structurally disadvantaged.
constraint_indexing:constraint_classification(switching_cost_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: KEYBOARD MANUFACTURERS (ROPE) — Experience the constraint as coordination. All manufacturers benefit from standardization: shared QWERTY production reduces unit costs, enables interchangeable parts, simplifies training for assembly line workers. Network effects work in their favor. No pressure to exit — the constraint serves them. Zero or negative experienced extraction; they see pure coordination benefit.
constraint_indexing:constraint_classification(switching_cost_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DIGITAL TRANSITION COALITION (SCAFFOLD) — In the digital era (post-1980s), switching costs decline: software can remap any key, displays are programmable, no hardware retooling needed. Organized actors (open-source projects, keyboard enthusiasts, software developers) build alternative pathways. The constraint has a structural sunset: as hardware becomes generic and software-configurable, the switching cost enforcement weakens. Theater is low — the digital solution is genuine, not performative.
constraint_indexing:constraint_classification(switching_cost_asymmetry, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: TYPEWRITER INDUSTRY LEGACY (PITON) — By the 2010s, the mechanical engineering constraint that originally justified QWERTY (prevent mechanical jam by separating high-frequency key pairs) no longer applies to electronic keyboards. The constraint persists through institutional inertia: computer keyboards still follow QWERTY because they always have, not because the original engineering problem exists. High theater ratio — the constraint's functional justification has atrophied, but the standard persists. This is degraded incumbency maintained by path dependence rather than active enforcement.
constraint_indexing:constraint_classification(switching_cost_asymmetry, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL PATH DEPENDENCE VIEW (MOUNTAIN) — From a universal perspective, path dependence is an immutable property of coordination systems: once a standard is established, switching costs make alternatives impossible to adopt regardless of technical superiority. This view treats QWERTY as a natural law of economic coordination, not a beneficiary-preserved constraint. The constraint appears unchangeable because the math of network effects and switching costs is irreducible. However, the structural data contradicts this — digital technology, open-source remapping, and the emergence of mobile keyboards suggest the constraint is contingent, not necessary. This is a false summit.
constraint_indexing:constraint_classification(switching_cost_asymmetry, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(switching_cost_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(switching_cost_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(switching_cost_asymmetry, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(switching_cost_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(switching_cost_asymmetry, TR),
    TR >= 0.70.

:- end_tests(switching_cost_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.52): Moderate-high. The constraint benefits manufacturers and incumbent typists while imposing retraining costs on those seeking efficient alternatives. The extraction is not maximal (0.7+) because the coordination function is genuine — standardization does reduce manufacturing costs and provide real value. But the extraction is asymmetric: the beneficiary advantage (no retraining, universal compatibility) is passive (they don't have to do anything), while the victim cost (learning new layout, finding QWERTY-incompatible tools) is active and substantial. Suppression (0.58): Moderate-high. Multiple barriers to exit: (1) retraining cost (40+ hours of inefficient typing), (2) muscle memory interference (retraining creates temporary speed loss), (3) social incompatibility (most machines standardized on QWERTY), (4) employment risk (hiring and testing protocols favor QWERTY proficiency). But suppression is declining (0.62 → 0.54) because digital technology removes hardware switching costs — software can remap any key without retooling keyboards. Theater ratio (0.48): Moderate. The constraint has genuine coordination function (manufacturing standardization is real), so theater is not extremely high. But theater is rising (0.35 → 0.48) because the original mechanical justification (preventing hammer jam on mechanical typewriters) no longer applies in digital keyboards. The constraint persists through institutional inertia — the functional justification has atrophied but the standard remains. In the mechanical era, the constraint was higher-function rope with lower theater. In the digital era, it's tangled rope with rising theater as the coordination function becomes less compelling relative to the extraction mechanism.
 *
 * PERSPECTIVAL GAP:
 *   The kernel ambiguity manifests as a gap between the 'neutral path dependence' reading and the 'beneficiary-preserved incumbency' reading. The first reading treats QWERTY as a natural outcome of coordination: once established, switching costs make alternatives uneconomical regardless of technical superiority. The constraint is presented as mathematically inevitable (mountain). The second reading treats QWERTY as actively maintained by beneficiaries: manufacturers benefit from standardization, incumbent typists benefit from universal compatibility, and these groups have incentives to defend the standard against alternatives. The constraint is presented as a beneficiary coalition (tangled rope or snare depending on the power of alternatives). The kernel question: Does the structural data distinguish these readings empirically, or are they interpretive frames on identical facts? If empirically distinguishable (e.g., if manufacturers actively lobbied for standardization while alternatives were spontaneously rejected), the readings decompose into separate constraint stories. If not empirically distinguishable (beneficiary existence is revealed post-hoc by analysis, not by examining historical decisions), the reading difference is interpretive rather than structural, and the constraint is a single story instantiating both readings.
 *
 * DIRECTIONALITY LOGIC:
 *   The chi formula χ = ε × f(d) × σ(S) applies differently across perspectives because directionality (d) varies by agent's structural position. Manufacturers experience low χ (large negative f(d) because d ≈ 0.15) despite moderate ε — the constraint subsidizes them. Efficiency seekers experience high χ (large positive f(d) because d ≈ 0.95) combined with high ε — maximum extracted cost. Organized digital-era actors experience moderate χ (moderate d ≈ 0.35, rising f(d) as alternatives become available) — extraction weakens as exit costs decline. The scope modifier σ(S) is global (1.2) for perspectives involving international technology standards, amplifying χ across all views. No directionality overrides are needed — the structural derivation from beneficiary/victim + exit options captures the true directional asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by recognizing that 'path dependence' and 'beneficiary preservation' are not mutually exclusive — they are compatible aspects of the same mechanism. Path-dependent equilibria naturally create beneficiaries (those locked into the dominant standard) and victims (those locked out of alternatives). The mandatrophy question becomes: are the beneficiaries passive (they benefit from the standard but didn't cause its dominance) or active (they deliberately maintained dominance against alternatives)? The historical record suggests both: manufacturers benefited from standardization and made choices (e.g., keyboard design) that preserved it, but they didn't need to actively suppress alternatives — network effects did that automatically. The constraint is tangled rope: genuine coordination function (manufacturing standardization) + asymmetric extraction (alternatives are foreclosed) + active enforcement (standard preservation through design choices, manufacturing practices). The false summit risk is that analytical observers treat path-dependent equilibria as natural laws (mountain) when they are actually contingent institutional arrangements (tangled rope). The constraint's classification is robustly tangled rope across most perspectives; only the analytical observer at civilizational scale risks the false summit, and the structural data (identifiable beneficiaries, existence of suppression) contradicts this mountainous view.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    qwerty_original_function_genuine,
    'Was QWERTY originally adopted for genuine mechanical necessity (preventing hammer jams on mechanical typewriters) or was mechanical optimization secondary to manufacturing convenience?',
    'Historical analysis of typewriter patent documents, manufacturing cost data, and contemporaneous accounts of design decisions. Comparison with competing mechanical solutions (e.g., Blickensderfer''s key-stagger approach) and their viability.',
    'If mechanical necessity was genuine and primary: constraint began as rope (coordination around a real problem). If manufacturing convenience was primary: constraint began as snare (extraction under the cover of solving a nonexistent problem). This changes whether the constraint is ''natural path dependence'' or ''engineered lock-in''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(qwerty_original_function_genuine, empirical, 'Whether QWERTY''s original adoption was driven by genuine mechanical necessity').

omega_variable(
    switching_cost_empirical_magnitude,
    'What is the actual retraining cost for switching from QWERTY to Dvorak? Is it truly 40+ hours as advocates claim, or substantially lower, or context-dependent?',
    'Controlled retraining studies; longitudinal tracking of professional typists switching layouts; measurement of typing speed recovery curves and accuracy stabilization under different retraining protocols.',
    'If retraining is 10-20 hours: switching cost is moderate, not catastrophic. Many agents could rationally exit. Classification shifts from snare toward tangled_rope for efficiency seekers. If retraining is 60+ hours plus permanent speed loss: switching cost is genuinely catastrophic. Snare classification confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(switching_cost_empirical_magnitude, empirical, 'Actual magnitude of retraining costs for keyboard layout switching').

omega_variable(
    beneficiary_agency_vs_historical_accident,
    'Is QWERTY''s persistence due to active beneficiary maintenance (manufacturers lobbied for standardization, typists coordinated to preserve their training), or pure historical accident (nobody bothered to change it once it was established)?',
    'Historical record of standardization efforts: did manufacturers or professional organizations actively defend QWERTY against alternatives? Evidence of resistance campaigns? Or was QWERTY''s dominance passive (alternatives simply never reached critical mass)? Network analysis of coordination among manufacturers during standardization era (1870s-1920s).',
    'If active beneficiary maintenance: constraint is tangled_rope or snare (extraction with coordination or without). If passive accident: constraint is rope (pure coordination with no beneficiary asymmetry). This determines whether false summit detection should trigger (beneficiaries exist) or not (no intentional enforcement).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_agency_vs_historical_accident, empirical, 'Whether QWERTY persistence reflects active beneficiary maintenance or passive historical accident').

omega_variable(
    kernel_reading_decomposition,
    'Does the QWERTY constraint instantiate one constraint or two: (A) path-dependent coordination problem with switching costs, or (B) beneficiary-preserved incumbency protecting extraction?',
    'Structural analysis: do these two readings entail different ε values, different beneficiary/victim sets, or different measurement observables? If empirically distinguishable (e.g., one reading predicts monopoly pricing by manufacturers, the other predicts competitive equilibrium), the readings decompose into separate constraint stories. If not empirically distinguishable, the reading difference is interpretive rather than structural.',
    'If decomposable: write separate story files (neutral_path_dependence vs beneficiary_preserved_incumbency), linked via network.affects_constraints. If not decomposable: the constraint is a single story with two legitimate readings (kernel reading rather than constraint decomposition). This determines the authoring architecture for the corpus.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_decomposition, conceptual, 'Whether QWERTY is one constraint or two, decomposed by reading').

omega_variable(
    digital_era_sunset_actualization,
    'Why hasn''t the digital-era cost reduction (software remapping, no hardware retooling) produced widespread Dvorak adoption? Is the scaffold sunset real but progressing slowly, or is the constraint stronger than the digital-era analysis suggests?',
    'Market adoption data: percentage of keyboards shipping with Dvorak support, percentage of typists using Dvorak in 2020s, growth trends. Comparative analysis with other standards disruptions (USB replacing PS/2, wireless replacing wired). Survey data on barriers to adoption for those aware of alternatives (habit, social pressure, employment risk, effort).',
    'If adoption is rising but slow (5-10 year sigmoid): scaffold sunset is real, just not yet complete. If adoption is flat or declining: digital-era cost reduction hasn''t overcome social/network barriers. Constraint is stronger than expected. Classification shifts: snare characteristics persist even under reduced technical switching costs.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(digital_era_sunset_actualization, empirical, 'Whether digital-era cost reduction has actualized the predicted scaffold sunset').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(switching_cost_asymmetry, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sca_tr_t0, switching_cost_asymmetry, theater_ratio, 0, 0.35).
narrative_ontology:measurement(sca_tr_t3, switching_cost_asymmetry, theater_ratio, 3, 0.42).
narrative_ontology:measurement(sca_tr_t6, switching_cost_asymmetry, theater_ratio, 6, 0.48).

% Extraction over time
narrative_ontology:measurement(sca_be_t0, switching_cost_asymmetry, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(sca_be_t3, switching_cost_asymmetry, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(sca_be_t6, switching_cost_asymmetry, base_extractiveness, 6, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(sca_su_t0, switching_cost_asymmetry, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(sca_su_t3, switching_cost_asymmetry, suppression_requirement, 3, 0.59).
narrative_ontology:measurement(sca_su_t6, switching_cost_asymmetry, suppression_requirement, 6, 0.54).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(switching_cost_asymmetry, information_standard).
narrative_ontology:affects_constraint(switching_cost_asymmetry, ergonomic_keyboard_adoption_barrier).
narrative_ontology:affects_constraint(switching_cost_asymmetry, ascii_standard_path_dependence).

% DUAL FORMULATION NOTE:
% Switching cost asymmetry operates at multiple scales: the QWERTY layout itself (this story, ε≈0.52), the broader keyboard form factor (mechanical vs wireless, ε≈0.35), and the digital protocol standards (USB, Bluetooth, ε≈0.28). Each has different beneficiaries and different suppression mechanisms. QWERTY is the highest-extraction story because it locks in skill rather than just hardware preference. Ergonomic keyboard alternatives face lower switching costs (retraining + new hardware simultaneously, ε≈0.40) but are affected by QWERTY dominance. ASCII standard path dependence has lower extraction (ε≈0.28) because software remapping and encoding conversion are cheaper than retraining typists. All three are linked through the manufacturing standardization coordination function.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(switching_cost_asymmetry, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
