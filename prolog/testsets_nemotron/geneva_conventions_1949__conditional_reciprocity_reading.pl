% ============================================================================
% CONSTRAINT STORY: geneva_conventions_1949__conditional_reciprocity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_1949__conditional_reciprocity_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: geneva_conventions_1949__conditional_reciprocity_reading
 *   human_readable: Geneva Conventions 1949 — Conditional Reciprocity Reading
 *   domain: international_law/armed_conflict/political_philosophy
 *
 * SUMMARY:
 *   This constraint story represents the conditional reciprocity reading of
 *   the 1949 Geneva Conventions. Under this reading, the Conventions function
 *   as reciprocal restraints among organized armed forces: full protections
 *   apply when both parties comply with Article 4 criteria (command
 *   structure, distinctive insignia, open carry of arms). When irregular
 *   forces fail these criteria, the detaining power may proportionally
 *   degrade protections — classifying captured irregulars as unlawful
 *   combatants without full POW status, and narrowing civilian immunity
 *   through proportionality calculations that weigh military advantage
 *   against collateral harm. The reading claims this is coordination
 *   (rope-like) among regular forces with conditional extraction from
 *   irregulars (tangled rope). The kernel is contested: the humanitarian
 *   ceiling reading treats the Conventions as absolute floors regardless of
 *   adversary conduct; the security maximization reading treats them as
 *   peacetime aspirations suspended in asymmetric conflict.
 *
 * KEY AGENTS:
 *   - state_militaries: Primary beneficiary and agenda setter (institutional/biographical/arbitrage) — they write, enforce, and benefit from the reciprocal framework among regular forces
 *   - organized_armed_forces: Beneficiary (powerful/biographical/mobile) — gain predictable status and protections when fighting peer adversaries
 *   - detaining_powers: Agenda setter/beneficiary (institutional/generational/arbitrage) — control classification and treatment of detainees
 *   - irregular_combatants: Victim/payer (powerless/immediate/trapped) — denied full protections when failing Article 4 criteria
 *   - detained_irregulars: Victim (powerless/immediate/trapped) — classified as unlawful combatants, excluded from POW protections
 *   - civilian_populations_in_asymmetric_conflict: Victim/payer (powerless/immediate/constrained) — immunity narrowed by proportionality calculations that discount their protection when irregulars embed among them
 *   - humanitarian_organizations: Observer/excluded (organized/biographical/analytical) — advocate for humanitarian ceiling reading, excluded from classification decisions
 *   - international_courts: Observer (institutional/generational/analytical) — adjudicate boundary disputes between readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_1949__conditional_reciprocity_reading, 0.58).
domain_priors:suppression_score(geneva_conventions_1949__conditional_reciprocity_reading, 0.42).
domain_priors:theater_ratio(geneva_conventions_1949__conditional_reciprocity_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_1949__conditional_reciprocity_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_1949__conditional_reciprocity_reading, "Geneva Conventions 1949 — Conditional Reciprocity Reading").
narrative_ontology:topic_domain(geneva_conventions_1949__conditional_reciprocity_reading, "international_law/armed_conflict/political_philosophy").

domain_priors:requires_active_enforcement(geneva_conventions_1949__conditional_reciprocity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_1949__conditional_reciprocity_reading, '58150c6c-985e-49f7-ba02-edf9efe7d304').
narrative_ontology:cs_kernel_codification('58150c6c-985e-49f7-ba02-edf9efe7d304', formalized).
narrative_ontology:cs_authority_grounding('58150c6c-985e-49f7-ba02-edf9efe7d304', lineage).
narrative_ontology:cs_interpretation_layer_present('58150c6c-985e-49f7-ba02-edf9efe7d304').
narrative_ontology:cs_reading_relation('58150c6c-985e-49f7-ba02-edf9efe7d304', geneva_conventions_1949__humanitarian_ceiling_reading, coexists_with).
narrative_ontology:cs_reading_relation('58150c6c-985e-49f7-ba02-edf9efe7d304', geneva_conventions_1949__security_maximization_reading, influences).
narrative_ontology:cs_axiom('58150c6c-985e-49f7-ba02-edf9efe7d304', foundational, reciprocity_conditions_protections).
narrative_ontology:cs_axiom_status(reciprocity_conditions_protections, holdable).
narrative_ontology:cs_axiom_grounding('58150c6c-985e-49f7-ba02-edf9efe7d304', reciprocity_conditions_protections, conventional).
narrative_ontology:cs_axiom('58150c6c-985e-49f7-ba02-edf9efe7d304', foundational, article_4_as_status_threshold).
narrative_ontology:cs_axiom_status(article_4_as_status_threshold, holdable).
narrative_ontology:cs_axiom_grounding('58150c6c-985e-49f7-ba02-edf9efe7d304', article_4_as_status_threshold, conventional).
narrative_ontology:cs_axiom('58150c6c-985e-49f7-ba02-edf9efe7d304', secondary, proportional_degradation_permissible).
narrative_ontology:cs_axiom_status(proportional_degradation_permissible, holdable).
narrative_ontology:cs_axiom_grounding('58150c6c-985e-49f7-ba02-edf9efe7d304', proportional_degradation_permissible, instrumental).
narrative_ontology:cs_reference_frame('58150c6c-985e-49f7-ba02-edf9efe7d304', post_westphalian_reciprocal_framework).
narrative_ontology:cs_drift_state('58150c6c-985e-49f7-ba02-edf9efe7d304', post_2001_asymmetric_conflict_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('58150c6c-985e-49f7-ba02-edf9efe7d304', '').
narrative_ontology:cs_kernel_id(geneva_conventions_1949__conditional_reciprocity_reading, geneva_conventions_1949).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__conditional_reciprocity_reading, state_militaries).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__conditional_reciprocity_reading, organized_armed_forces).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__conditional_reciprocity_reading, detaining_powers).
narrative_ontology:constraint_victim(geneva_conventions_1949__conditional_reciprocity_reading, irregular_combatants).
narrative_ontology:constraint_victim(geneva_conventions_1949__conditional_reciprocity_reading, detained_irregulars).
narrative_ontology:constraint_victim(geneva_conventions_1949__conditional_reciprocity_reading, civilian_populations_in_asymmetric_conflict).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Write, ratify, and enforce the Conventions through military doctrine and training. Gain predictable reciprocity with peer adversaries: clear POW protections, combatant immunity, and legal frameworks for detention. In asymmetric conflicts, gain operational flexibility to classify adversaries as unlawful combatants and apply proportional degradation. Their exit is arbitrage-grade: they can withdraw from specific treaties (with notice) and develop alternative legal frameworks, but the Conventions' universal ratification makes full exit politically costly.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, state_militaries, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_1949__conditional_reciprocity_reading, state_militaries, beneficiary).

% Formal military forces of state and non-state actors that meet Article 4 criteria (command structure, distinctive insignia, open carry). Gain full combatant privilege and POW protections when captured by compliant adversaries. Their exit is mobile: they can conform to Article 4 criteria to claim protections, or operate as irregulars and accept the risks. They are not structurally locked into either status.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, organized_armed_forces, beneficiary,
    powerful, biographical, mobile, global).

% Control the classification, detention, and treatment of captured persons. Benefit from the authority to designate irregulars as unlawful combatants, denying them POW protections and enabling coercive interrogation, indefinite detention, or military commissions. Their exit is arbitrage-grade: they administer the system and can shape its interpretation through practice, but are bound by the Conventions' text and international scrutiny.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, detaining_powers, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_1949__conditional_reciprocity_reading, detaining_powers, beneficiary).

% Fighters who cannot or do not meet Article 4 criteria (no command structure, no insignia, concealed carry). When captured, they are denied POW status and protections. They bear the extraction: no combatant privilege, no guaranteed humane treatment standards beyond Common Article 3, exposed to prosecution for mere participation. Their exit is trapped: they cannot easily acquire Article 4 compliance (requires state-like organization), and ceasing to fight may mean political death or persecution. The constraint extracts their protections as the price of the reciprocal framework they cannot join.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, irregular_combatants, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_1949__conditional_reciprocity_reading, irregular_combatants, payer).

% Irregular combatants in detention. Classified as unlawful combatants, they receive neither full POW protections (Third Convention) nor full civilian protections (Fourth Convention). Subject to the detaining power's discretionary treatment within the 'proportional degradation' zone. No effective exit: they cannot change their status retroactively, and habeas corpus or judicial review is often denied or delayed. The constraint's extraction from them is total and structural.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, detained_irregulars, payer,
    powerless, immediate, trapped, local).

% Civilians living in areas where irregular forces operate. Their immunity is narrowed by proportionality calculations that treat proximity to irregulars as implicit risk acceptance. Collateral damage thresholds are calibrated higher when adversaries are irregular. They bear extraction through degraded protection without participating in hostilities. Exit is constrained: they may flee (becoming refugees) but cannot easily escape the conflict zone or change the proportionality calculus that discounts their immunity.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, civilian_populations_in_asymmetric_conflict, payer,
    powerless, immediate, constrained, local).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_1949__conditional_reciprocity_reading, civilian_populations_in_asymmetric_conflict, payer).

% ICRC, NGOs, UN agencies that monitor compliance and advocate for the humanitarian ceiling reading. They have analytical access to detention facilities and conflict zones but are excluded from classification decisions and proportionality calculations. Their role is to document extraction and pressure for higher protection floors. They do not collect from the constraint nor pay into it directly.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, humanitarian_organizations, observer,
    organized, biographical, analytical, global).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_1949__conditional_reciprocity_reading, humanitarian_organizations, excluded).

% ICJ, ICC, ICTY/ICTR, regional human rights courts that adjudicate boundary disputes between the readings. They interpret Common Article 3, the Martens Clause, and proportionality standards. Their rulings shape the enforceable boundary of the conditional reciprocity reading but they do not administer the constraint nor bear its extraction. They are the analytical seat that sees the full structure.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, international_courts, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(geneva_conventions_1949__conditional_reciprocity_reading, detaining_powers).
narrative_ontology:fixing_cost_class(geneva_conventions_1949__conditional_reciprocity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates mutual restraint among organized armed forces that meet Article 4 criteria: provides a stable framework for combatant privilege, POW protections, and civilian immunity in inter-state and symmetric non-international conflicts. Solves the assurance problem: 'I will treat your soldiers humanely if you treat mine humanely.'
% TRANSFER_FUNCTION: Transfers protections and legal status from irregular combatants, detained irregulars, and civilians in asymmetric conflict zones to state militaries and detaining powers. The transfer is: Article 4 compliance → full protections; Article 4 non-compliance → proportional degradation. The detaining power gains operational flexibility (interrogation, detention, targeting latitude) at the expense of the irregular's protections and the civilian's immunity density.
% ABSENT_VOICES: Irregular combatants and detained irregulars are structurally excluded from the drafting, interpretation, and enforcement of the Conventions — they have no seat at the diplomatic conferences, no standing in military commissions, and no effective access to courts. Civilians in asymmetric conflict zones are similarly excluded from proportionality calculations that discount their immunity. The humanitarian organizations that would amplify their voices have observer status but no decision authority. The security_maximization reading would further exclude these voices by treating operational necessity as overriding.
% DISAPPEARANCE_RATIONALE: If the conditional reciprocity reading vanished overnight, the reciprocal framework among regular forces would lose its legal basis — POW protections, combatant immunity, and the Article 4 status system would become customary norms without treaty enforcement. Irregular combatants would lose even the degraded protections of Common Article 3 and the 'unlawful combatant' classification (which at least provides a defined status). Detaining powers would lose the legal architecture for classification and proportional degradation. The humanitarian ceiling reading would fill the vacuum for state parties that accept it; the security maximization reading would fill it for others. The world would rearrange into competing frameworks with no common coordinate system.
% FOUNDING_PROBLEM: The 1949 Conventions were built to solve the coordination failure of the 1929 regime: in total war between state armies, reciprocal atrocity spirals (reprisals against POWs, civilian bombing, denial of quarter) escalated because no stable framework guaranteed mutual restraint. The founding problem was creating a self-enforcing reciprocal framework among peer state adversaries where compliance is incentivized by the expectation of reciprocal compliance.
% FOUNDING_PROBLEM_CORROBORATION: The ICRC's official commentary (outside the benefiting state militaries) attests the founding problem was inter-state reciprocity. State military doctrines (the beneficiaries) attest the problem remains live for peer conflict but is contested for asymmetric conflict. The ICTY's Tadić decision (institutional observer) attests that Common Article 3 extended the framework to non-international conflicts but left the reciprocity logic contested. No single authority corroborates that the founding problem is fully live or fully dead — the contest is structural.
narrative_ontology:disappearance_verdict(geneva_conventions_1949__conditional_reciprocity_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_1949__conditional_reciprocity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_1949__conditional_reciprocity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(geneva_conventions_1949__conditional_reciprocity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_1949__conditional_reciprocity_reading, 0.58, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_1949__conditional_reciprocity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_1949__conditional_reciprocity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_1949__conditional_reciprocity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.58 reflects that the constraint extracts significant protections from irregular forces and civilians in asymmetric conflicts while providing genuine coordination among regular forces. The 1949 baseline (0.35) reflected inter-state war expectations; extractiveness rose with the 1977 Additional Protocols (0.42) which recognized irregular forces but created new classification ambiguities, then sharply with the post-2001 'war on terror' framework (0.52-0.58) where unlawful combatant designation became systematic. Theater ratio 0.25 indicates the coordination function (regular force reciprocity) is real but increasingly performed — classification proceedings and proportionality assessments often serve to legitimate predetermined outcomes rather than genuinely coordinate. Suppression 0.42 reflects that the constraint's persistence depends on state military buy-in (they benefit from reciprocity) and the lack of enforcement alternatives for victims (trapped/identity_locked exit). The constraint is not a mountain (emerges_naturally false, beneficiaries/victims declared), not a snare (coordination function is real among regular forces), not a scaffold (no sunset), not a piton (active enforcement, concentrated beneficiaries). Tangled rope fits: genuine coordination among regular forces + asymmetric extraction from irregulars + active enforcement required.
 *
 * PERSPECTIVAL GAP:
 *   From the state military seat (institutional/arbitrage), the constraint is a rope: it solves the coordination problem of mutual restraint among peer adversaries, and the extraction from irregulars is the price of maintaining that coordination. From the irregular combatant seat (powerless/trapped), the constraint is a snare: it denies them protections granted to regulars while claiming the moral authority of humanitarian law. From the civilian seat (powerless/constrained), the constraint is a degraded rope: it provides some protection but the proportionality calculation systematically discounts their immunity when irregulars operate among them. The detaining power seat (institutional/arbitrage) experiences it as agenda-setting authority — they control the classification that determines extraction. The engine computes these divergences from the declared power/exit/spatial_scope data.
 *
 * DIRECTIONALITY LOGIC:
 *   State militaries and organized armed forces are declared beneficiaries: they gain predictable reciprocity, clear status distinctions, and legal cover for operations against irregulars. Their power (institutional/powerful) and exit (arbitrage/mobile) place them at low directionality (d ≈ 0.1-0.2) — the constraint subsidizes them. Detaining powers are agenda setters and beneficiaries: they administer the classification system and collect the operational flexibility it provides. Irregular combatants and detained irregulars are declared victims: they bear the cost of denied protections. Their power (powerless) and exit (trapped) place them at high directionality (d ≈ 0.9-1.0) — the constraint extracts from them. Civilians in asymmetric conflict are victims/payers: they lose immunity density through proportionality calculations that treat their proximity to irregulars as risk acceptance. Their exit is constrained (not fully trapped — they may flee — but not mobile). Humanitarian organizations and international courts are observers: they analyze but do not collect or pay. The beneficiary/victim declarations drive the engine's directionality derivation; no overrides needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (1949) was coordinating restraint among state armies in total war — preventing reciprocal atrocity spirals. That problem is live for inter-state conflict but contested for asymmetric conflict. The constraint has not resolved its mandatrophy: it continues to apply the inter-state coordination framework to asymmetric conflicts where the reciprocity logic breaks down (irregulars cannot/will not meet Article 4 criteria). The humanitarian ceiling reading argues the mandatrophy is resolved by treating the Conventions as absolute floors; the security maximization reading argues the mandatrophy was never resolved because the Conventions were always aspirational. This reading (conditional reciprocity) occupies the contested middle: it claims the framework still coordinates where reciprocity exists, and degrades proportionally where it does not. The extraction from irregulars is not pure mandatrophy — it is the structural consequence of applying a reciprocal framework to non-reciprocal adversaries. But the proportional degradation mechanism has become a standing extraction architecture, not a temporary adaptation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_frame,
    'Is the Geneva Conventions 1949 a single constraint or a kernel contested by multiple readings?',
    'Structural analysis of each reading''s beneficiary/victim structure, extractiveness, and enforcement requirements. If readings produce materially different ε and stakeholder structures, they are distinct constraints linked by network.affects_constraints.',
    'If confirmed as kernel readings, each reading gets its own constraint story with its own claimed_type, metrics, and stakeholders. The conditional_reciprocity_reading claims moderate extraction from irregular forces in exchange for coordination among regular forces. The humanitarian_ceiling_reading would claim near-zero extraction. The security_maximization_reading would claim high extraction from all protected persons.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_frame, conceptual, 'Whether the 1949 Conventions are one constraint or a constraint family of kernel readings').

omega_variable(
    reciprocity_vs_absolutism_boundary,
    'Where does the conditional reciprocity reading draw the line between permissible degradation and prohibited violation?',
    'Analysis of state practice and ICJ/ICTY jurisprudence on proportionality in reprisals, the Martens Clause, and Common Article 3 as a floor. The reading claims proportionality calculations narrow civilian immunity but preserve a core; the humanitarian ceiling reading claims the core is absolute.',
    'If the boundary is legally indeterminate, the reading''s claimed moderate extractiveness (0.58) may understate actual extraction in practice. If the boundary is determinate and observed, the reading is a genuine tangled rope coordinating regular forces while extracting from irregulars.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_vs_absolutism_boundary, conceptual, 'The structural boundary between conditional reciprocity and humanitarian absolutism').

omega_variable(
    unlawful_combatant_classification_extraction,
    'Does the ''unlawful combatant'' classification function as a coordination category or an extraction mechanism?',
    'Compare treatment of detained irregulars under this reading vs. the humanitarian ceiling reading. If the classification primarily enables denial of POW protections (extraction from detainees) while claiming to coordinate status determination, it is extractive coordination — tangled rope. If it purely coordinates status with minimal extraction, it leans toward rope.',
    'Affects claimed_type: if classification is primarily extractive, tangled_rope stands. If primarily coordinative with incidental extraction, rope becomes plausible. If extraction is total and coordination is pretext, snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unlawful_combatant_classification_extraction, empirical, 'Whether unlawful combatant status determination is a genuine coordination function or extraction cover').

omega_variable(
    proportionality_calculation_as_coordination,
    'Do proportionality calculations in asymmetric conflict function as a genuine coordination mechanism between adversaries, or as a unilateral degradation license?',
    'Examine whether adversaries in asymmetric conflicts (state vs. non-state) engage in reciprocal proportionality signaling, or whether the state actor calculates proportionality unilaterally without adversary input. True coordination requires bilateral signaling; unilateral calculation is extraction.',
    'If bilateral, the reading has stronger coordination claims (rope/tangled_rope). If unilateral, the coordination story is cover for extraction (snare/tangled_rope with higher ε).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(proportionality_calculation_as_coordination, empirical, 'Whether proportionality operates as bilateral coordination or unilateral license').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_1949__conditional_reciprocity_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t1949, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 1949, 0.1).
narrative_ontology:measurement(gene_tr_t1977, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 1977, 0.15).
narrative_ontology:measurement(gene_tr_t2001, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 2001, 0.2).
narrative_ontology:measurement(gene_tr_t2006, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 2006, 0.23).
narrative_ontology:measurement(gene_tr_t2014, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 2014, 0.25).
narrative_ontology:measurement(gene_tr_t2024, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 2024, 0.25).

% Extraction over time
narrative_ontology:measurement(gene_be_t1949, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 1949, 0.35).
narrative_ontology:measurement(gene_be_t1977, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 1977, 0.42).
narrative_ontology:measurement(gene_be_t2001, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 2001, 0.52).
narrative_ontology:measurement(gene_be_t2006, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 2006, 0.56).
narrative_ontology:measurement(gene_be_t2014, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 2014, 0.58).
narrative_ontology:measurement(gene_be_t2024, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t1949, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 1949, 0.25).
narrative_ontology:measurement(gene_su_t1977, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 1977, 0.3).
narrative_ontology:measurement(gene_su_t2001, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 2001, 0.38).
narrative_ontology:measurement(gene_su_t2006, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 2006, 0.4).
narrative_ontology:measurement(gene_su_t2014, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 2014, 0.42).
narrative_ontology:measurement(gene_su_t2024, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 2024, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_1949__conditional_reciprocity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(geneva_conventions_1949__conditional_reciprocity_reading, 0.12).
narrative_ontology:affects_constraint(geneva_conventions_1949__conditional_reciprocity_reading, geneva_conventions_1949__humanitarian_ceiling_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__conditional_reciprocity_reading, geneva_conventions_1949__security_maximization_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__conditional_reciprocity_reading, additional_protocols_1977).
narrative_ontology:affects_constraint(geneva_conventions_1949__conditional_reciprocity_reading, unlawful_combatant_designation_post_2001).
narrative_ontology:affects_constraint(geneva_conventions_1949__conditional_reciprocity_reading, targeted_killing_frameworks).

% DUAL FORMULATION NOTE:
% The geneva_conventions_1949 kernel decomposes into three constraint stories reflecting three structural readings. This reading (conditional_reciprocity) claims moderate extraction (ε=0.58) with genuine coordination among regular forces. The humanitarian_ceiling_reading would claim near-zero extraction (ε≈0.1) with absolute protections. The security_maximization_reading would claim high extraction (ε≈0.8) with near-total suspension in asymmetric conflict. They are linked via network.affects_constraints. The conditional_reciprocity reading influences both siblings: it creates the classification architecture (unlawful combatant) that the security_maximization reading extends, and it creates the proportionality framework that the humanitarian_ceiling reading contests as insufficient.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(geneva_conventions_1949__conditional_reciprocity_reading, institutional, 0.15).
constraint_indexing:directionality_override(geneva_conventions_1949__conditional_reciprocity_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
