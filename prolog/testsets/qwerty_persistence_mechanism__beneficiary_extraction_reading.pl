% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_mechanism__beneficiary_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence_mechanism__beneficiary_extraction_reading, []).

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
 *   constraint_id: qwerty_persistence_mechanism__beneficiary_extraction_reading
 *   human_readable: QWERTY Persistence via Beneficiary Extraction (Incumbent Protection Reading)
 *   domain: economic_history/technology_standards/path_dependence
 *
 * SUMMARY:
 *   This constraint story instantiates the BENEFICIARY-EXTRACTION READING of
 *   the QWERTY persistence kernel. The core claim: QWERTY persists because
 *   manufacturers (Remington, Union Typewriter) and typing schools with sunk
 *   costs in QWERTY-specific training actively maintained the standard to
 *   protect their investments and suppress technically superior alternatives
 *   like Dvorak. This reading treats QWERTY not as a natural outcome of
 *   coordination games or fair competition, but as a deliberately enforced
 *   extraction mechanism. Incumbent typewriter manufacturers benefited from
 *   training-specific switching costs; they had every incentive to prevent
 *   alternatives and the market power to enforce exclusivity. Typing schools,
 *   having invested in QWERTY curricula and standardized testing, became
 *   enforcement partners. Alternative keyboard developers were suppressed
 *   through: (1) exclusive manufacturing arrangements that bundled QWERTY by
 *   default; (2) patent strategy and licensing control; (3) deliberate
 *   blockage of alternative keyboard adoption in dominant platforms; (4)
 *   defunding or regulatory obstruction of government standardization efforts
 *   (e.g., the Navy's Dvorak study was buried, GSA procurement favored
 *   incumbent specifications). The constraint exhibits Tangled Rope
 *   structure: genuine coordination function (training standardization
 *   enables the typing labor market) layered with asymmetric extraction
 *   (beneficiaries capture rents by maintaining artificial switching costs).
 *   The theater_ratio increases over the interval (0.32 → 0.65) as the
 *   original enforcement mechanisms become less visible and the constraint
 *   persists through institutional inertia rather than active maintenance —
 *   the theater increases as the original function degrades.
 *
 * KEY AGENTS:
 *   - Incumbent Typewriter Manufacturers (Remington, Union Typewriter, Underwood): Primary beneficiaries (institutional/arbitrage) — capture rents from sunk training investments; actively enforce QWERTY through exclusive manufacturing and licensing
 *   - Typing Schools (YMCA schools, commercial typing academies, early computer training programs): Secondary beneficiary / enforcement partner (institutional/constrained) — sunk costs in QWERTY curricula; collude with manufacturers to maintain standard; also victims of constraint as capital becomes locked into single layout
 *   - Dvorak and Alternative Keyboard Developers: Primary victims (moderate/constrained) — cannot gain traction despite technical merit; face suppression through default-setting, licensing control, and refusal of adoption support
 *   - Individual Typists and Keyboard Users: Secondary victims (powerless/trapped) — must accept suboptimal ergonomics to participate in typing labor market; cannot switch without coordinating mass defection
 *   - Government Standards Bodies (Navy, GSA, federal procurement): Contested institutional actor (powerful/mobile) — could theoretically override incumbent control through standardization mandates but were actively discouraged or blocked; possessed power but not will to enforce alternative
 *   - Analytical Observer: Sees the constraint as natural lock-in (analytical/analytical) — risks naturalizing beneficiary protection as inevitable consequence of coordination
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.58).
domain_priors:suppression_score(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.62).
domain_priors:theater_ratio(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_mechanism__beneficiary_extraction_reading, tangled_rope).
narrative_ontology:human_readable(qwerty_persistence_mechanism__beneficiary_extraction_reading, "QWERTY Persistence via Beneficiary Extraction (Incumbent Protection Reading)").
narrative_ontology:topic_domain(qwerty_persistence_mechanism__beneficiary_extraction_reading, "economic_history/technology_standards/path_dependence").

domain_priors:requires_active_enforcement(qwerty_persistence_mechanism__beneficiary_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_mechanism__beneficiary_extraction_reading, '0ecb3209-1ffc-4efb-858d-3a6977565ec5').
narrative_ontology:cs_kernel_codification('0ecb3209-1ffc-4efb-858d-3a6977565ec5', fixed_text).
narrative_ontology:cs_authority_grounding('0ecb3209-1ffc-4efb-858d-3a6977565ec5', extraction).
narrative_ontology:cs_reading_relation('0ecb3209-1ffc-4efb-858d-3a6977565ec5', qwerty_persistence_mechanism__lock_in_reading, coexists_with).
narrative_ontology:cs_reading_relation('0ecb3209-1ffc-4efb-858d-3a6977565ec5', qwerty_persistence_mechanism__naturalization_reading, coexists_with).
narrative_ontology:cs_axiom('0ecb3209-1ffc-4efb-858d-3a6977565ec5', foundational, incumbents_actively_protected_qwerty).
narrative_ontology:cs_axiom_status(incumbents_actively_protected_qwerty, holdable).
narrative_ontology:cs_axiom_grounding('0ecb3209-1ffc-4efb-858d-3a6977565ec5', incumbents_actively_protected_qwerty, empirically_contingent).
narrative_ontology:cs_axiom('0ecb3209-1ffc-4efb-858d-3a6977565ec5', foundational, switching_costs_deliberately_maintained_via_enforcement).
narrative_ontology:cs_axiom_status(switching_costs_deliberately_maintained_via_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('0ecb3209-1ffc-4efb-858d-3a6977565ec5', switching_costs_deliberately_maintained_via_enforcement, empirically_contingent).
narrative_ontology:cs_reference_frame('0ecb3209-1ffc-4efb-858d-3a6977565ec5', incumbent_manufacturer_protection).
narrative_ontology:cs_drift_state('0ecb3209-1ffc-4efb-858d-3a6977565ec5', post_digital_keyboard_transition, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0ecb3209-1ffc-4efb-858d-3a6977565ec5', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_mechanism__beneficiary_extraction_reading, qwerty_persistence_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__beneficiary_extraction_reading, incumbent_typewriter_manufacturers).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__beneficiary_extraction_reading, typing_schools_capital_invested).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__beneficiary_extraction_reading, alternative_keyboard_developers).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__beneficiary_extraction_reading, typing_efficiency_general_public).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TYPIST / ALTERNATIVE KEYBOARD DEVELOPERS (SNARE) — Trapped by installed base lock-in. Individual users cannot exit QWERTY without coordinating mass defection. Alternative keyboard developers (Dvorak, Colemak) cannot gain traction because switching costs are artificially maintained by incumbent control of training infrastructure and default implementations. Maximum extraction experienced: must accept suboptimal ergonomics to participate in the typing labor market.
constraint_indexing:constraint_classification(qwerty_persistence_mechanism__beneficiary_extraction_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: TYPING SCHOOLS / SMALL EQUIPMENT RETAILERS (TANGLED ROPE) — Constrained by sunk costs in QWERTY-specific curricula and equipment inventory. These actors both benefit from the QWERTY coordination (standardized skill certification, predictable market) and bear extraction costs (curriculum lock-in, inability to adopt superior alternatives without coordinating across the industry). Active enforcement: major manufacturers bundled QWERTY exclusively with standard machines and withheld licensing for alternative layouts.
constraint_indexing:constraint_classification(qwerty_persistence_mechanism__beneficiary_extraction_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT MANUFACTURERS (ROPE) — Primary beneficiaries. Experience QWERTY as a coordination mechanism that benefits them: training investments in QWERTY are specific capital that generates rents by raising switching costs. Can exit through arbitrage: if alternative layouts threatened their market position, incumbents could switch manufacturing and actively promoted alternatives (they did not). Net beneficiary perspective — extraction runs toward these actors via the protection mechanism.
constraint_indexing:constraint_classification(qwerty_persistence_mechanism__beneficiary_extraction_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: TYPEWRITER INDUSTRY POST-DIGITAL TRANSITION (PITON) — After computers displaced typewriters, QWERTY persisted not because incumbents actively protected it but through institutional inertia. The original extraction mechanism (manufacturer control, training school lock-in) atrophied as the industry declined. Yet QWERTY remained the default keyboard layout through path dependency and network effects — the constraint's original enforcement function (active beneficiary protection) became obsolete, but the constraint's structure persisted via theater (software defaults, training inertia, interface convention). Theater ratio high because the mechanism is now self-perpetuating rather than actively maintained.
constraint_indexing:constraint_classification(qwerty_persistence_mechanism__beneficiary_extraction_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LOCK-IN VIEW (MOUNTAIN) — From a universal timescale, once a coordination standard is established, path-dependent switching costs make alternatives unviable regardless of technical merit. The constraint appears as an immutable consequence of coordination game dynamics: the installed base creates a natural barrier to supersession. This perspective risks naturalizing what is actually a choice by beneficiaries to enforce the standard through active mechanisms. The engine will detect this as a false summit.
constraint_indexing:constraint_classification(qwerty_persistence_mechanism__beneficiary_extraction_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_mechanism__beneficiary_extraction_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(qwerty_persistence_mechanism__beneficiary_extraction_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(qwerty_persistence_mechanism__beneficiary_extraction_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(qwerty_persistence_mechanism__beneficiary_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(qwerty_persistence_mechanism__beneficiary_extraction_reading, TR),
    TR >= 0.70.

:- end_tests(qwerty_persistence_mechanism__beneficiary_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The reading quantifies extraction via switching costs maintained by beneficiaries. Typists face real ergonomic losses (~10-15% speed penalty relative to Dvorak, per ergonomic studies) that compound over careers. Alternative keyboard developers are suppressed entirely. However, extractiveness is not maximal (0.70+) because the mechanism relies partly on legitimate coordination benefits (standardized training IS valuable) and because suppression is incomplete (alternatives exist and have advocates, just lack market power). The value reflects asymmetric extraction layered on genuine coordination. Suppression (0.62): Moderate-high. Measured barriers to adopting alternatives: (1) exclusive manufacturing by incumbents; (2) typing school standardization enforced by industry association coordination; (3) default-setting in machines and software; (4) active obstruction of government standardization efforts (Navy Dvorak study suppression is documented). But suppression is not total (0.80+) because alternatives can and do exist, and some adoption occurred despite enforcement (Dvorak had dedicated users). Theater ratio (0.48 → 0.65): Rising over time. Initially (t=0, early typewriter era), the enforcement mechanism is functional and active — manufacturers genuinely prefer QWERTY for technical reasons (mechanical advantages in early typewriter design) AND for commercial advantage (training lock-in). The theater ratio starts low because the mechanism's function is real. By t=20-50 (post-computer transition), the original technical advantage dissolves but QWERTY persists through institutional inertia — software defaults, training tradition, interface convention — rather than active enforcement. Theater rises as the mechanism becomes performative.
 *
 * PERSPECTIVAL GAP:
 *   This reading produces significant perspectival divergence from the sibling readings. The beneficiary-extraction reading sees QWERTY as an actively maintained snare (powerless victims) and tangled rope with enforcement (moderate constrained actors), while the lock-in reading would see pure coordination failure (rope or tangled rope from all perspectives, with no active suppression). The naturalization reading would see legitimate competition (rope from all perspectives, with superior alternatives failing through fair market process). The gaps reflect different empirical claims: does the historical record show active suppression or passive persistence? The reading's own internal structure shows perspectival coherence: the piton perspective (institutional/arbitrage/immediate) correctly identifies the mechanism as having atrophied over time, while the snare perspective (powerless/trapped/biographical) correctly identifies the extraction experienced by users.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations and exit options per the standard pipeline. Incumbents: beneficiary + arbitrage → low d → negative chi (institution benefits). Typing schools: mixed (benefit from standardization + victim of lock-in) + constrained → moderate d. Typists: victim + trapped → high d → high chi (experience maximum extraction). No overrides needed; structural data produces the correct perspectival gap.
 *
 * MANDATROPHY ANALYSIS:
 *   The beneficiary-extraction reading resolves the mandatrophy by making explicit the asymmetric extraction mechanism. The classification is NOT 'QWERTY is a natural lock-in' (which would hide the beneficiary protection) but 'QWERTY is actively maintained extraction with coordination benefits' (Tangled Rope). The mandatrophy is resolved by distinguishing legitimate coordination costs (typing standardization is genuinely valuable) from extractive overhead (the artificial suppression of alternatives). The piton perspective shows the secondary mandatrophy: once enforcement mechanisms atrophy, the constraint persists through theater — institutional inertia maintaining a structure that no longer serves its original function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    active_vs_passive_maintenance,
    'Did incumbents ACTIVELY ENFORCE QWERTY (licensing restrictions, exclusive manufacturing, deliberate suppression of alternative keyboard layouts) or did QWERTY persist passively through coordination lock-in after initial adoption?',
    'Historical record: licensing agreements, patent strategy, manufacturer correspondence, documented alternatives rejected or suppressed. Comparative analysis of typewriter manufacturers'' treatment of QWERTY vs. other technical specifications.',
    'Active enforcement → Tangled Rope / Snare (deliberate extraction). Passive persistence → Rope / Piton (coordination game without beneficiary malice). This omega distinguishes the beneficiary-extraction reading from the lock-in reading at the mechanism level.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(active_vs_passive_maintenance, empirical, 'Whether QWERTY persistence involved active enforcement or passive coordination lock-in').

omega_variable(
    dvorak_suppression_evidence,
    'What evidence exists that Dvorak and other superior alternatives were actively suppressed rather than simply failing to overcome coordination costs?',
    'Analysis of Dvorak adoption attempts: government standardization efforts (Navy study, GSA evaluation), software accessibility, typing school promotion attempts, financial support or lack thereof. Comparison to how successful keyboard alternatives (Colemak, etc.) eventually gained adoption despite similar or worse initial lock-in.',
    'Strong suppression evidence → beneficiary-extraction reading confirmed (ε ≥ 0.58). Weak evidence → lock-in reading more defensible (ε lower, mechanism is passive coordination). This resolution directly determines which reading''s ε value is empirically justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dvorak_suppression_evidence, empirical, 'Evidence of active suppression vs. passive failure of alternatives').

omega_variable(
    typing_school_collusion_scope,
    'Were typing schools part of a coordinated enforcement mechanism with manufacturers, or did they independently adopt QWERTY curricula following market signals?',
    'Historical documents: typing school association records, curriculum adoption timelines, manufacturer influence on school curricula, presence or absence of formal agreements. Comparison to adoption of alternative keyboards in regions where manufacturers had less control over training institutions.',
    'Coordinated enforcement → confirms active suppression mechanism; beneficiary-extraction ε ≥ 0.58. Independent adoption → suggests coordination lock-in rather than conspiracy; lock-in reading more defensible. Mechanism clarity affects classification confidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(typing_school_collusion_scope, empirical, 'Whether typing schools coordinated with manufacturers or independently adopted QWERTY').

omega_variable(
    reading_kernel_disambiguation,
    'The beneficiary-extraction reading instantiates a specific framing of the QWERTY kernel: that identifiable beneficiaries DELIBERATELY PROTECTED their investment through suppressive mechanisms. Do the sibling readings (lock-in and naturalization) logically rule out this framing, or do they coexist as different parties'' legitimate positions?',
    'Epistemic analysis: Can a single institutional actor (e.g., Remington management, the typewriter industry cartel, government standards bodies) simultaneously believe both that QWERTY was actively protected AND that it persisted through natural lock-in dynamics? If yes, coexists_with; if the core premises contradict within any unified framework, forecloses.',
    'If forecloses → one reading must be rejected as internally contradictory (high confidence in beneficiary-extraction). If coexists_with → multiple readings remain live across different actors/epochs. This determines cs_structure.reading_relations for sibling mappings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_disambiguation, conceptual, 'Logical relationship between beneficiary-extraction reading and sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwerty_benef_theater_t0, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(qwerty_benef_theater_t20, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 20, 0.48).
narrative_ontology:measurement(qwerty_benef_theater_t50, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 50, 0.65).

% Extraction over time
narrative_ontology:measurement(qwerty_benef_extract_t0, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(qwerty_benef_extract_t20, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(qwerty_benef_extract_t50, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 50, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(qwerty_benef_suppress_t0, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(qwerty_benef_suppress_t20, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(qwerty_benef_suppress_t50, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 50, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_mechanism__beneficiary_extraction_reading, resource_allocation).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__beneficiary_extraction_reading, qwerty_persistence_mechanism__lock_in_reading).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__beneficiary_extraction_reading, qwerty_persistence_mechanism__naturalization_reading).

% DUAL FORMULATION NOTE:
% The QWERTY persistence kernel decomposes into three structurally distinct constraint stories, each with different ε values and beneficiary/victim structures. This file (beneficiary_extraction_reading) has ε=0.58, active enforcement, and identifiable suppression. The lock-in reading would have ε≈0.35–0.45, no active suppression, and coordination failure as the mechanism. The naturalization reading would have ε≤0.25, no suppression, and fair competition. All three are linked via network.affects_constraints, forming the constraint family that resolves the contested QWERTY claim.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
