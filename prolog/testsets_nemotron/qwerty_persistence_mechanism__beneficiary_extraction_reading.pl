% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_mechanism__beneficiary_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: qwerty_persistence_mechanism__beneficiary_extraction_reading
 *   human_readable: QWERTY Layout Persistence via Incumbent Maintenance
 *   domain: economic/technological
 *
 * SUMMARY:
 *   This constraint story captures the beneficiary_extraction_reading of the
 *   QWERTY persistence kernel: the claim that QWERTY persisted not through
 *   neutral path dependence or genuine adequacy, but because Remington/Union
 *   Typewriter and incumbent typing schools actively maintained it to protect
 *   their training investments and market position. The constraint operated
 *   as a tangled rope — it solved a genuine coordination problem (universal
 *   layout enabling transferable typing skills and interoperable office
 *   workflows) while simultaneously extracting rents through artificial
 *   switching costs (bundled training contracts, patent enforcement,
 *   standards-body influence). The measurement series runs from ~1874 (Sholes
 *   patent) to ~1924 (peak Remington/Union dominance), showing rising
 *   extraction and suppression as the incumbent coalition hardened. The
 *   claim/metric gap is deliberate: the constraint is CLAIMED as tangled_rope
 *   (authoring seat sees coordination + extraction) while the metrics
 *   describe the operational reality the engine will classify per seat.
 *
 * KEY AGENTS:
 *   - remington_union_typewriter: Primary beneficiary (institutional/trapped) — collects machine sales, training contracts, and standardization rents; actively suppresses alternatives
 *   - incumbent_typing_schools: Primary beneficiary (organized/constrained) — curriculum locked to QWERTY; graduation certificates create switching costs for typists
 *   - legacy_office_equipment_makers: Secondary beneficiary (organized/constrained) — installed base and service contracts depend on QWERTY continuity
 *   - typists_forced_to_relearn: Primary victim (powerless/identity_locked) — skill investment fused to QWERTY; exit requires retraining with career interruption
 *   - alternative_layout_innovators: Primary victim (moderate/trapped) — Dvorak and others structurally excluded by patent thickets, standards capture, and installed base
 *   - organizations_paying_switching_costs: Secondary victim (powerful/constrained) — bear retraining costs and productivity loss when standards shift; locked by workforce skills
 *   - standards_bodies: Agenda setter (institutional/constrained) — captured by incumbent coalition; ratify QWERTY as de facto standard
 *   - economic_historians: Observer (analytical/analytical) — analyze path dependence mechanisms; the Liebowitz/Margolis vs. David debate maps to kernel readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.72).
domain_priors:suppression_score(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.78).
domain_priors:theater_ratio(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_mechanism__beneficiary_extraction_reading, tangled_rope).
narrative_ontology:human_readable(qwerty_persistence_mechanism__beneficiary_extraction_reading, "QWERTY Layout Persistence via Incumbent Maintenance").
narrative_ontology:topic_domain(qwerty_persistence_mechanism__beneficiary_extraction_reading, "economic/technological").

domain_priors:requires_active_enforcement(qwerty_persistence_mechanism__beneficiary_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_mechanism__beneficiary_extraction_reading, 'caad9595-a59a-4f1b-94f5-c9f027a450b3').
narrative_ontology:cs_kernel_codification('caad9595-a59a-4f1b-94f5-c9f027a450b3', implicit).
narrative_ontology:cs_authority_grounding('caad9595-a59a-4f1b-94f5-c9f027a450b3', extraction).
narrative_ontology:cs_interpretation_layer_present('caad9595-a59a-4f1b-94f5-c9f027a450b3').
narrative_ontology:cs_reading_relation('caad9595-a59a-4f1b-94f5-c9f027a450b3', qwerty_persistence_mechanism__naturalization_reading, coexists_with).
narrative_ontology:cs_reading_relation('caad9595-a59a-4f1b-94f5-c9f027a450b3', qwerty_persistence_mechanism__lock_in_reading, influences).
narrative_ontology:cs_axiom('caad9595-a59a-4f1b-94f5-c9f027a450b3', foundational, active_maintenance_by_identifiable_beneficiaries).
narrative_ontology:cs_axiom_status(active_maintenance_by_identifiable_beneficiaries, holdable).
narrative_ontology:cs_axiom_grounding('caad9595-a59a-4f1b-94f5-c9f027a450b3', active_maintenance_by_identifiable_beneficiaries, empirically_contingent).
narrative_ontology:cs_axiom('caad9595-a59a-4f1b-94f5-c9f027a450b3', foundational, artificial_switching_costs_as_extraction_mechanism).
narrative_ontology:cs_axiom_status(artificial_switching_costs_as_extraction_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('caad9595-a59a-4f1b-94f5-c9f027a450b3', artificial_switching_costs_as_extraction_mechanism, empirically_contingent).
narrative_ontology:cs_reference_frame('caad9595-a59a-4f1b-94f5-c9f027a450b3', mechanical_jamming_solution_1874).
narrative_ontology:cs_drift_state('caad9595-a59a-4f1b-94f5-c9f027a450b3', peak_incumbent_dominance_1924, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('caad9595-a59a-4f1b-94f5-c9f027a450b3', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_mechanism__beneficiary_extraction_reading, qwerty_persistence_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__beneficiary_extraction_reading, remington_union_typewriter).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__beneficiary_extraction_reading, incumbent_typing_schools).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__beneficiary_extraction_reading, legacy_office_equipment_makers).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__beneficiary_extraction_reading, typists_forced_to_relearn).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__beneficiary_extraction_reading, alternative_layout_innovators).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__beneficiary_extraction_reading, organizations_paying_switching_costs).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Dominant typewriter manufacturer (later merged with Union Typewriter trust) that designed QWERTY to prevent mechanical jamming, then actively maintained it as the universal standard through patent enforcement, bundled training contracts, and standards-body influence. Collected machine sales, ribbon/service revenue, and standardization rents. Exit was trapped — their entire product line, service network, and dealer ecosystem was built on QWERTY; switching would have destroyed the installed base advantage.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, remington_union_typewriter, agenda_setter,
    institutional, generational, trapped, global).

% Commercial typing schools (e.g., Barnes, YMCA programs) that standardized curricula on QWERTY. Their graduation certificates became the hiring credential for clerical work, creating a switching cost for any typist considering an alternative layout. They collected tuition and placement fees. Exit was constrained — retraining faculty and rewriting curricula was possible but costly, and the credential value depended on QWERTY's dominance.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, incumbent_typing_schools, beneficiary,
    organized, biographical, constrained, national).

% Manufacturers of adding machines, calculators, and early office equipment that adopted QWERTY keyboards for compatibility with typing pools. Their installed base and service contracts created inertia. They benefited from a stable standard that reduced design and support costs. Exit was constrained — redesigning keyboards was technically feasible but would break compatibility with the QWERTY-trained workforce their customers employed.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, legacy_office_equipment_makers, beneficiary,
    organized, biographical, constrained, national).

% Clerical workers (overwhelmingly women entering the workforce 1880-1930) whose professional identity and earning capacity were fused to QWERTY typing speed and accuracy. Switching layouts meant months of retraining with zero income, loss of certification, and competitive disadvantage against QWERTY typists. The skill was not just a tool but a core professional identity — 'I am a typist' meant 'I type QWERTY.' Exit was identity-locked: leaving the constraint meant leaving the profession.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, typists_forced_to_relearn, payer,
    powerless, biographical, identity_locked, global).

% Inventors of superior layouts (Dvorak 1936, Blickensderfer, others) who faced structural exclusion: Remington/Union patent thickets blocked mechanical implementations; typing schools refused to teach alternatives; standards bodies (later ANSI) ratified QWERTY as de facto standard. Their market entry was trapped — not by lack of demand but by incumbent control of the complementary assets (training, certification, machine distribution) required to make a layout viable.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, alternative_layout_innovators, payer,
    moderate, biographical, trapped, global).

% Large employers (government agencies, insurance companies, railroads) that built massive clerical workforces on QWERTY. They bore the direct costs of retraining when standards shifted, productivity loss during transition, and the ongoing premium of QWERTY-licensed machines. They had power to demand alternatives but faced constrained exit: their workforce's skills were QWERTY-specific, and retraining thousands of typists was organizationally disruptive and expensive.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, organizations_paying_switching_costs, payer,
    powerful, biographical, constrained, national).

% Early standardization bodies (precursors to ANSI/ISO) that ratified QWERTY as the de facto keyboard standard. They were captured by the incumbent coalition — Remington/Union dominated the committees, and the installed base made any alternative standard practically unimplementable. They set the agenda for interoperability but their exit from QWERTY was constrained: ratifying an alternative would have required coordinating a massive ecosystem transition they lacked the authority to enforce.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, standards_bodies, agenda_setter,
    institutional, generational, constrained, global).

% Scholars analyzing QWERTY as a case study in path dependence (Paul David, 1985) and its critics (Liebowitz & Margolis, 1990). The David/Liebowitz-Margolis debate maps directly to the kernel readings: David emphasizes lock-in and coordination failure (lock_in_reading); Liebowitz & Margolis argue QWERTY was genuinely efficient (naturalization_reading); this reading adds the active-maintenance-by-beneficiaries mechanism (beneficiary_extraction_reading). Their analytical exit is unconstrained — they choose frameworks.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, economic_historians, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence_mechanism__beneficiary_extraction_reading, remington_union_typewriter).
narrative_ontology:fixing_cost_class(qwerty_persistence_mechanism__beneficiary_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establish a universal keyboard layout enabling interoperable typing pools, transferable clerical skills, and compatible office equipment across organizations and geographies — solving the fragmentation of early typewriter designs.
% TRANSFER_FUNCTION: Moves machine sales revenue, training fees, and standardization rents from typists, employers, and alternative innovators to Remington/Union Typewriter and incumbent typing schools, via artificial switching costs (bundled contracts, patent enforcement, credential gatekeeping).
% ABSENT_VOICES: Typists themselves (overwhelmingly women) had no seat in standards bodies or manufacturer decisions; their skill investment was the asset being leveraged. Alternative layout inventors (Dvorak, Blickensderfer) were structurally excluded from the standard-setting process. Early ergonomics researchers who documented QWERTY's inefficiency were ignored by captured standards committees.
% DISAPPEARANCE_RATIONALE: If QWERTY's active maintenance vanished overnight (no patent enforcement, no bundled contracts, no standards capture), Dvorak and other layouts would have competed on merit. Typing schools would have adopted superior layouts to attract students. Organizations would have retrained workforces where efficiency gains justified cost. The office equipment market would have reorganized around open keyboard standards within a decade — as it eventually did for computer keyboards, where QWERTY persists only by inertia, not active enforcement.
% FOUNDING_PROBLEM: Early typewriters (1860s-1870s) jammed when adjacent keys were struck in rapid succession. Sholes/Glidden/QWERTY layout separated common letter pairs (TH, ST, etc.) to reduce mechanical interference — a genuine engineering solution to a physical constraint.
% FOUNDING_PROBLEM_CORROBORATION: Mechanical engineering histories (outside beneficiary set) confirm jamming was solved by improved typebar mechanisms and later electric/electronic actions by 1900. The David (1985) and Liebowitz-Margolis (1990) literatures both accept the founding problem was mechanical jamming; they dispute whether QWERTY remained optimal after jamming was solved. No party outside the Remington/Union coalition claims the founding problem persisted.
narrative_ontology:disappearance_verdict(qwerty_persistence_mechanism__beneficiary_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_mechanism__beneficiary_extraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_mechanism__beneficiary_extraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(qwerty_persistence_mechanism__beneficiary_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_mechanism__beneficiary_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qwerty_persistence_mechanism__beneficiary_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qwerty_persistence_mechanism__beneficiary_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction rises from 0.35 to 0.72 over the interval as Remington/Union shifts from establishing a standard to defending it against superior alternatives (Dvorak 1936). Suppression rises from 0.3 to 0.78 as active enforcement (patent litigation, contractual tying, standards capture) replaces early coordination. Theater ratio rises from 0.15 to 0.42 as the 'efficiency' justification becomes increasingly performative — the coordination function is real but the maintenance activity increasingly serves extraction. Accessibility collapse at 0.65 reflects that alternatives existed (Dvorak, other layouts) but were practically inaccessible due to switching costs. Resistance at 0.55 captures the sustained but ultimately unsuccessful challenges from alternative layouts, typist advocates, and efficiency studies. All measurements share one time grid (0,10,20,30,40,50) per alignment rule.
 *
 * PERSPECTIVAL GAP:
 *   From Remington/Union's seat (institutional, trapped exit), the constraint is genuine coordination they built and must maintain — the standard enables their business model. From typists' seats (powerless, identity_locked), the same structure is enforced extraction — their career capital is fused to QWERTY, making exit professionally catastrophic. From alternative innovators' seats (moderate, trapped), it is a snare — structural exclusion via patent and standards capture. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Remington/Union and incumbent schools are structural beneficiaries: they collect machine revenue, training fees, and standardization rents (d → 0.0-0.2). Typists are targets: skill investment is identity-locked (career capital fused to QWERTY), exit requires retraining with income loss (d → 0.85-0.95). Alternative innovators are targets with trapped exit: patent thickets and standards capture block market entry (d → 0.9). Organizations are payers with constrained exit: they bear switching costs but have some power to resist (d → 0.6-0.7). Standards bodies are agenda setters with constrained exit: captured by incumbents but formally independent (d → 0.4-0.5). The derivation chain from beneficiary/victim + exit produces these directionalities; no overrides needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (early typewriter mechanism jamming solved by QWERTY's anti-clustering layout) died by ~1900 as mechanical improvements eliminated jamming. The arrangement persisted because the beneficiary coalition (Remington/Union, typing schools) extracted sufficient value to fund active maintenance. This is classic mandatrophy: the coordination function (jam prevention) atrophied while the extraction function (switching cost rents) grew. The classification as tangled_rope rather than snare prevents mislabeling — the coordination function was real and valuable for decades; the extraction layered onto it, it did not replace it entirely.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a reading of the contested kernel qwerty_persistence_mechanism, specifically the beneficiary_extraction_reading?',
    'Committee frame declares this reading explicitly; sibling readings are separate constraint files with their own ε and stakeholder structures.',
    'Confirms this story instantiates one specific structural claim about QWERTY persistence — active maintenance by identifiable beneficiaries — rather than the kernel as a whole.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Kernel-reading identity declaration for committer frame').

omega_variable(
    suppression_mechanism_ambiguity,
    'Was the suppression of alternative layouts structural (contractual exclusion, patent enforcement, standards-body capture) or partly internalized (typists believing QWERTY was superior, identity fused with QWERTY skill)?',
    'Historical analysis of Remington/Union contractual practices, patent litigation records, and contemporary typist testimony; post-adoption trajectory of Dvorak adopters to test persistence of internalized suppression.',
    'If internalized, the constraint''s effective suppression exceeded structural measures — typists carried the suppression with them, raising the coordination cost of alternatives beyond what active enforcement alone explains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in QWERTY maintenance').

omega_variable(
    extraction_coordination_boundary,
    'How much of the measured extraction (ε=0.72) is the price of genuine coordination (single standard enabling interoperable typing pools, transferable skills) versus artificial switching costs imposed to protect incumbents?',
    'Counterfactual: estimate coordination value of a universal layout standard in 1890-1930 office economies; compare to the premium Remington/Union extracted via training contracts and machine bundling.',
    'If coordination value is high, the constraint is more tangled_rope; if artificial switching costs dominate, it leans snare. The boundary determines whether the coordination function is genuine cover or substantial.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_coordination_boundary, conceptual, 'Coordination value vs. extractive premium in QWERTY''s persistence').

omega_variable(
    naturalization_reading_framing,
    'Does the naturalization_reading''s claim (QWERTY became genuinely adequate) describe a different constraint with its own ε, or a different observable of the same constraint?',
    'ε-invariance test: if measuring ''adequacy'' yields low ε while measuring ''incumbent maintenance'' yields high ε, they are distinct constraints. The naturalization_reading should author its own story with its own metrics.',
    'If distinct constraints, the sibling reading is a separate file linked via network.affects_constraints. If same constraint, the author has violated ε-invariance and must decompose.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalization_reading_framing, conceptual, 'Whether naturalization and beneficiary_extraction are distinct constraints per ε-invariance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwerty_pme_tr_t0, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(qwerty_pme_tr_t10, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(qwerty_pme_tr_t20, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 20, 0.31).
narrative_ontology:measurement(qwerty_pme_tr_t30, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(qwerty_pme_tr_t40, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement(qwerty_pme_tr_t50, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 50, 0.42).

% Extraction over time
narrative_ontology:measurement(qwerty_pme_be_t0, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(qwerty_pme_be_t10, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(qwerty_pme_be_t20, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(qwerty_pme_be_t30, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 30, 0.72).
narrative_ontology:measurement(qwerty_pme_be_t40, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 40, 0.75).
narrative_ontology:measurement(qwerty_pme_be_t50, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 50, 0.74).

% Suppression requirement over time
narrative_ontology:measurement(qwerty_pme_su_t0, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(qwerty_pme_su_t10, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(qwerty_pme_su_t20, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(qwerty_pme_su_t30, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(qwerty_pme_su_t40, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 40, 0.78).
narrative_ontology:measurement(qwerty_pme_su_t50, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 50, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_mechanism__beneficiary_extraction_reading, information_standard).
narrative_ontology:boltzmann_floor_override(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.02).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__beneficiary_extraction_reading, qwerty_persistence_mechanism__lock_in_reading).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__beneficiary_extraction_reading, qwerty_persistence_mechanism__naturalization_reading).

% DUAL FORMULATION NOTE:
% This kernel decomposes into three structurally distinct constraints with different ε values: (1) beneficiary_extraction_reading — ε=0.72, active maintenance by incumbents; (2) lock_in_reading — ε=0.45, passive coordination failure; (3) naturalization_reading — ε=0.15, genuine adequacy. The ε-invariance principle requires separate stories; they are linked here. The beneficiary_extraction_reading is upstream — the incumbent maintenance created the conditions for both the coordination failure (lock_in) and the adequacy narrative (naturalization).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
