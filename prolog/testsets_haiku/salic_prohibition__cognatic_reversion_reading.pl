% ============================================================================
% CONSTRAINT STORY: salic_prohibition__cognatic_reversion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_salic_prohibition__cognatic_reversion_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: salic_prohibition__cognatic_reversion_reading
 *   human_readable: Salic Prohibition as Anachronistic Territorial Misapplication
 *   domain: constitutional/dynastic/political
 *
 * SUMMARY:
 *   Salic Law — the prohibition on female succession — originated in
 *   5th-century Frankish kingdoms as a coordination mechanism to prevent
 *   civil war by establishing a single, clear rule of agnatic primogeniture.
 *   As Frankish dominion expanded and non-Frankish territories were annexed
 *   or confederated (Aquitaine, Provence, Burgundy, Aragon), the rule was
 *   imposed on regions with established cognatic succession customs. This
 *   reading frames Salic enforcement in non-Frankish territories as
 *   anachronistic institutional transfer: the founding coordination problem
 *   (Frankish succession instability) does not persist in territories with
 *   different historical institutions, yet the rule remains enforced through
 *   ecclesiastical authority and dynastic claims. The constraint extracts
 *   succession rights from female cognatic heirs and transfers them to male
 *   agnatic relatives, using the fiction that the rule is immutable
 *   natural/divine law rather than revocable positive law of Frankish origin.
 *
 * KEY AGENTS:
 *   - Frankish male lineage preservers (ecclesiastical + dynastic authorities): institutional beneficiaries, arbitrage exit, set and enforce the rule
 *   - Female cognatic claimants in non-Frankish territories: targets of extraction, constrained exit, lose inheritance rights under imposed agnatic rule
 *   - Non-Frankish territorial customs keepers (local nobility, legal guilds): payers, lose interpretive authority, forced to administer alien rule
 *   - Frankish male agnate heirs: beneficiaries, gain succession they would not have under local custom
 *   - Ecclesiastical legitimacy authority (Church councils, bishops, papacy): agenda-setter, codifies rule as divine law, enforces through sanction threat
 *   - Territorial sovereigns asserting override (Aragon, Navarre, England): excluded, assert the right to permit/forbid Salic succession in their domains, face sanction if they permit female succession
 *   - Historical comparative scholar: analytical observer, measures whether the constraint solves coordination or extracts succession rights
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(salic_prohibition__cognatic_reversion_reading, 0.62).
domain_priors:suppression_score(salic_prohibition__cognatic_reversion_reading, 0.71).
domain_priors:theater_ratio(salic_prohibition__cognatic_reversion_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(salic_prohibition__cognatic_reversion_reading, tangled_rope).
narrative_ontology:human_readable(salic_prohibition__cognatic_reversion_reading, "Salic Prohibition as Anachronistic Territorial Misapplication").
narrative_ontology:topic_domain(salic_prohibition__cognatic_reversion_reading, "constitutional/dynastic/political").

domain_priors:requires_active_enforcement(salic_prohibition__cognatic_reversion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(salic_prohibition__cognatic_reversion_reading, '9b310369-cf46-4605-9ec5-76fb8a460adb').
narrative_ontology:cs_kernel_codification('9b310369-cf46-4605-9ec5-76fb8a460adb', fixed_text).
narrative_ontology:cs_authority_grounding('9b310369-cf46-4605-9ec5-76fb8a460adb', extraction).
narrative_ontology:cs_interpretation_layer_present('9b310369-cf46-4605-9ec5-76fb8a460adb').
narrative_ontology:cs_reading_relation('9b310369-cf46-4605-9ec5-76fb8a460adb', salic_prohibition__immutable_mandate_reading, forecloses).
narrative_ontology:cs_reading_relation('9b310369-cf46-4605-9ec5-76fb8a460adb', salic_prohibition__sovereign_override_reading, coexists_with).
narrative_ontology:cs_axiom('9b310369-cf46-4605-9ec5-76fb8a460adb', foundational, salic_law_positive_revocable).
narrative_ontology:cs_axiom_status(salic_law_positive_revocable, holdable).
narrative_ontology:cs_axiom_grounding('9b310369-cf46-4605-9ec5-76fb8a460adb', salic_law_positive_revocable, conventional).
narrative_ontology:cs_axiom('9b310369-cf46-4605-9ec5-76fb8a460adb', foundational, territorial_sovereignty_over_succession).
narrative_ontology:cs_axiom_status(territorial_sovereignty_over_succession, holdable).
narrative_ontology:cs_axiom_grounding('9b310369-cf46-4605-9ec5-76fb8a460adb', territorial_sovereignty_over_succession, deontological).
narrative_ontology:cs_reference_frame('9b310369-cf46-4605-9ec5-76fb8a460adb', cognatic_succession_as_legitimate_inheritance).
narrative_ontology:cs_drift_state('9b310369-cf46-4605-9ec5-76fb8a460adb', post_frankish_fragmentation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9b310369-cf46-4605-9ec5-76fb8a460adb', '').
narrative_ontology:cs_kernel_id(salic_prohibition__cognatic_reversion_reading, salic_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(salic_prohibition__cognatic_reversion_reading, frankish_male_lineage_preservers).
narrative_ontology:constraint_beneficiary(salic_prohibition__cognatic_reversion_reading, dynastic_legitimacy_administrators).
narrative_ontology:constraint_victim(salic_prohibition__cognatic_reversion_reading, cognatic_succession_claimants).
narrative_ontology:constraint_victim(salic_prohibition__cognatic_reversion_reading, non_frankish_territorial_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(salic_prohibition__cognatic_reversion_reading, frankish_male_agnate_heirs).
narrative_ontology:constraint_victim(salic_prohibition__cognatic_reversion_reading, female_cognatic_claimants).
narrative_ontology:constraint_victim(salic_prohibition__cognatic_reversion_reading, non_frankish_territorial_customs_keepers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ecclesiastical and dynastic authorities who benefit from enforcing agnatic-only succession within territories nominally under Salic Law. They argue the law preserves dynastic legitimacy, orderly transition, and theological order (male headship doctrine). Their interest is in excluding female claimants regardless of geographic origin or territorial custom.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, frankish_male_lineage_preservers, beneficiary,
    institutional, generational, arbitrage, continental).

% Daughters, widows, and collateral female heirs in non-Frankish territories (Aquitaine, Provence, Burgundy, Aragon) where cognatic primogeniture (inheritance by eldest child regardless of sex) was the established custom before Salic impositions. They bear the cost of displacement from rightful succession and must either accept disinheritance or litigate against imposed agnatic rules presented as immutable law.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, female_cognatic_claimants, payer,
    powerful, biographical, constrained, continental).

% Local nobility, legal guilds, and municipal authorities in annexed or confederated territories (Aquitaine, Provence, Burgundy) who administered succession under pre-conquest customary law. They lose interpretive authority and administrative control when Salic Law is imposed from above, and must enforce a rule they did not author and that contradicts established practice.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, non_frankish_territorial_customs_keepers, payer,
    organized, generational, constrained, regional).

% Male relatives in the agnatic line who inherit through Salic enforcement even when female cognatic heirs would have inherited under local custom. They gain succession rights that local law would have denied them, directly displacing female cognatic claimants.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, frankish_male_agnate_heirs, beneficiary,
    powerful, biographical, mobile, continental).

% Church councils, bishops, and papal authorities who codify and transmit Salic Law through canon law and theological justification (male headship, orderly succession). They frame the rule as divine law or natural order, not positive law subject to territorial revision, and enforce it through excommunication and interdict threats against sovereigns who violate it.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, ecclesiastical_legitimacy_authority, agenda_setter,
    institutional, civilizational, analytical, universal).

% Kings and dukes in non-Frankish territories (Aragon, Navarre, England after 1453) who assert the right to permit or forbid Salic succession within their own domains. They would claim that dynastic succession rules are matters of positive law subject to sovereign choice, not immutable natural law. Their exclusion from the rule-setting conversation means they must either submit or face ecclesiastical sanction.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, territorial_sovereigns_asserting_override, excluded,
    institutional, generational, trapped, regional).

% Historian or legal analyst who examines whether Salic Law was ever truly binding on non-Frankish territories, whether it served coordination (preventing civil war through clear succession) or extraction (excluding female heirs to concentrate power in male lineages), and whether its persistence after the Frankish kingdom fragmented is institutional inertia or ongoing enforcement.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, historical_comparative_scholar, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(salic_prohibition__cognatic_reversion_reading, frankish_male_lineage_preservers).
narrative_ontology:fixing_cost_class(salic_prohibition__cognatic_reversion_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, transparent succession rule for dynastic transition: no ambiguity about who inherits, reducing civil war risk and legitimacy contests when a ruler dies. The rule coordinates on agnatic primogeniture across jurisdictions nominally under a shared dynastic umbrella.
% TRANSFER_FUNCTION: Transfers succession rights from female cognatic heirs (who would inherit under pre-conquest local custom) to male agnatic heirs. The transfer is enforced by ecclesiastical threat and by reframing local custom as deviation from universal law.
% ABSENT_VOICES: Territorial sovereigns asserting the right to set their own succession rules are excluded from the conversation; female cognatic claimants are present but powerless to override the rule without military challenge; pre-conquest customary-law authorities are superseded rather than consulted.
% DISAPPEARANCE_RATIONALE: If Salic enforcement vanished, non-Frankish territories would revert to cognatic primogeniture within a generation. Female heirs would inherit directly; territorial custom would reassert control over dynastic rules; ecclesiastical leverage over succession would collapse in those domains. The reorganization would be swift and decisive.
% FOUNDING_PROBLEM: Succession ambiguity in early Frankish kingdoms created civil war risk; a clear agnatic rule reduced contests when multiple claimants existed. The rule solved a coordination problem for Frankish domains circa 5th–8th centuries.
% FOUNDING_PROBLEM_CORROBORATION: Historians attest that Salic succession originated as a solution to Frankish succession instability. BUT scholars outside the ecclesiastical legitimacy apparatus (comparative legal historians, secular historians of dynastic succession in Aragon, Castile, and England) attest that the founding problem (Frankish civil war risk) is no longer live in non-Frankish territories — those regions never experienced the succession crises that justified the rule in Francia, and the rule persists as institutional transfer of power from female to male heirs, not as coordination mechanism.
narrative_ontology:disappearance_verdict(salic_prohibition__cognatic_reversion_reading, world_rearranges).
narrative_ontology:founding_problem_status(salic_prohibition__cognatic_reversion_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(salic_prohibition__cognatic_reversion_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(salic_prohibition__cognatic_reversion_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(salic_prohibition__cognatic_reversion_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(salic_prohibition__cognatic_reversion_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(salic_prohibition__cognatic_reversion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.62 at interval end) because the constraint directly displaces female heirs from succession and cannot be justified by ongoing coordination (the founding problem—Frankish civil war—does not recur in stable non-Frankish kingdoms). Suppression is high (0.71) because enforcement depends on ecclesiastical threat (excommunication, interdict) and dynastic narrative control, not on participant consensus or local custom. Theater rises substantially (0.25 to 0.48 over the interval): the rule is initially presented as solving coordination, but by the High Middle Ages, when ecclesiastical justifications become more elaborate and local resistance more persistent, the performative component (theological argument, canonical formula) dominates over coordination function. Extraction itself plateaus slightly (peaks at 1300, recedes modestly by 1500) as some sovereigns (Aragon, Navarre, England) assert exceptions and begin to permit female succession, reducing the rule's binding force in some territories but not eliminating it. Suppression requirement remains high because Church enforcement continues and agnatic expectations persist even where sovereignty weakens.
 *
 * PERSPECTIVAL GAP:
 *   From the ecclesiastical legitimacy seat, the constraint is immutable natural law preserving divine order and dynastic legitimacy—no choice, no extraction, no problem. From the female cognatic claimant seat, it is pure extraction enforced through institutional coercion—displacement from rightful inheritance, constrained exit, no coordination benefit. From the territorial sovereign seat (excluded from rule-setting), it is an illegitimate imposition of Frankish custom on non-Frankish domains. From the cognatic_reversion_reading seat (this constraint), the rule is neither immutable law nor legitimate sovereign choice—it is anachronistic transfer of rights from female to male heirs, justified by invoking a founding problem that no longer exists in non-Frankish territories. The engine computes these divergent types from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Female cognatic claimants are full targets: they lose succession rights directly, have constrained exit (cannot exit the succession system, can only litigate or rebel), and carry no arbitrage position. Male agnate heirs are beneficiaries: they gain succession through Salic enforcement. Ecclesiastical authorities are beneficiaries-cum-enforcers: they benefit from controlling legitimacy narrative and from the institutional leverage Salic enforcement gives them over sovereigns. Territorial sovereigns are excluded from rule-setting, so their directionality is ambiguous—they would be symmetric or beneficiary-side if permitted to override, but forced exclusion means they experience the rule as constraining, pushing them toward victim-side. Non-Frankish customs keepers are payers: they lose interpretive authority and must enforce alien law.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves the mandatrophy ambiguity by asserting that Salic Law's founding mandate (prevent Frankish civil war through clear succession) is dead in non-Frankish territories. The constraint persists because: (1) institutional inertia—ecclesiastical authorities continue enforcing the rule because it preserves their legitimacy-setting power; (2) beneficiary capture—male agnate heirs and institutional beneficiaries resist any revision; (3) theater—the rule is renarrated as divine law or natural order rather than territorial mismatch. Classification as tangled_rope (not pure snare) is justified because genuine coordination existed in the founding context (Frankish succession clarity) and a coordination story is still invoked to justify enforcement, even though the coordination problem is absent in non-Frankish territories. The extraction component (displacement of female heirs) is asymmetric and requires active enforcement (ecclesiastical sanction, dynastic narrative control, legal challenge resistance). This reading does NOT claim the constraint is pure extraction everywhere—it claims that WHERE the founding problem persists (within actual Frankish domains with genuine succession ambiguity), coordination and extraction are entangled; WHERE the founding problem is dead (non-Frankish territories with stable successions), the constraint operates as pure extraction with theatrical coordination justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_extraction_boundary,
    'Did Salic Law ever serve a genuine coordination function (preventing civil war through clear agnatic succession) in non-Frankish territories, or was it purely extractive transfer of succession rights from female to male heirs from the moment of imposition?',
    'Historical comparison of succession disputes and civil wars in non-Frankish territories before and after Salic imposition. If succession disputes dropped after imposition (controlling for other stability factors), coordination is present; if disputes continued and Salic Law was unevenly enforced, extraction dominates.',
    'If Salic Law served no coordination function in non-Frankish territories from the start, the constraint reclassifies from tangled_rope (coordination + extraction) to snare (pure extraction with coordination cover story). If coordination was real but died when territorial stability increased, the temporal record would show theater_ratio rising as functional coordination erodes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, empirical, 'Whether Salic Law ever coordinated succession in non-Frankish territories or was extraction from imposition.').

omega_variable(
    ecclesiastical_legitimacy_dependence,
    'How much of Salic Law''s binding force in non-Frankish territories depends on ecclesiastical sanction (excommunication, interdict) versus on internalized normative acceptance or military enforcement?',
    'Historical record of sovereigns defying Salic Law without ecclesiastical permission (e.g., Aragon''s female successions, England''s shift after 1453, Navarre''s cognatic rules). If sovereigns can overturn Salic Law with impunity once ecclesiastical authority weakens, then ecclesiastical sanction is the primary enforcement mechanism. If the rule persists even against ecclesiastical neutrality, other mechanisms sustain it.',
    'If ecclesiastical sanction is the binding force, weakening Church authority after the Reformation and rise of secular sovereignty should produce observed decline in Salic enforcement (reflected in measurement deflation). If Salic Law persists against ecclesiastical neutrality, some combination of military enforcement, internalized legitimacy, or institutional inertia sustains it—reclassifying the enforcement type.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ecclesiastical_legitimacy_dependence, empirical, 'Whether ecclesiastical authority is the primary enforcer of Salic Law or whether other mechanisms dominate.').

omega_variable(
    frankish_vs_non_frankish_bifurcation,
    'Should Salic Law within Frankish-origin domains (where the founding coordination problem was real) be classified as a separate constraint from Salic enforcement in non-Frankish annexed territories?',
    'Empirical test: author two separate constraint stories—one for Frankish succession coordination (likely rope or tangled_rope with live coordination function), one for non-Frankish imposition (tangled_rope with dead coordination). Compare their ε values and type classifications. If ε differs substantially (Frankish coordination with low extraction, non-Frankish imposition with high extraction), they are distinct constraints per the ε-invariance principle.',
    'If they are distinct constraints, the corpus should carry both stories, linked via network.affects_constraints. This reading (cognatic_reversion) should then be narrowly scoped to non-Frankish misapplication, and a separate immutable_mandate story would represent the Frankish-origin coordination function. The sibling readings (immutable_mandate, sovereign_override) would then map more cleanly to the Frankish-domain and override-sovereign seats respectively.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(frankish_vs_non_frankish_bifurcation, conceptual, 'Whether Salic Law in Frankish and non-Frankish territories are one constraint or two.').

omega_variable(
    gender_essentialism_vs_institutional_capture,
    'Is Salic Law primarily enforced because it embeds a theological or natural conviction that women cannot rule (gender essentialism), or because it functions as institutional capture—ecclesiastical and dynastic authorities benefit from controlling female exclusion regardless of gender beliefs?',
    'Examine whether Salic enforcement is consistent (every female cognatic heir is displaced equally) or selective (some female heirs are permitted when institutional leverage is low, others are blocked when enforcement is strong). If enforcement is consistent despite gender-essentialist doctrine being variable, institutional capture dominates. If enforcement matches theological conviction strength, gender essentialism is the binding force.',
    'If institutional capture is the primary driver, Salic Law is a renewable constraint dependent on ecclesiastical and dynastic institutional power—it weakens as institutional authority erodes (Reformation, rise of secular sovereignty). If gender essentialism is the binding force, the constraint depends on internalized belief rather than institutional coercion—it may persist or strengthen as belief systems change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gender_essentialism_vs_institutional_capture, preference, 'Whether Salic Law''s enforcement is driven by gender-essentialist belief or institutional beneficiary capture.').

omega_variable(
    reading_boundary_immutable_vs_cognatic,
    'Is the boundary between the IMMUTABLE_MANDATE reading and this COGNATIC_REVERSION reading at the claim ''Salic Law is natural/divine law'' (immutable''s core) versus ''Salic Law is revocable positive law but remains binding in non-Frankish territories as institutional inertia'' (this reading''s claim)?',
    'Author clarification: the cognatic_reversion reading asserts that Salic Law is positive law of Frankish origin (revocable, not immutable by nature), territorially bounded to Frankish domains, but remains enforced in non-Frankish territories through institutional inertia and beneficiary capture. It forecloses the immutable_mandate reading''s core claim that Salic Law is natural/divine law binding everywhere by nature.',
    'Confirms the reading_relations entry: immutable_mandate and cognatic_reversion FORECLOSE (their core premises directly contradict). The immutable_mandate reading claims Salic Law is binding by nature; cognatic_reversion claims it is positive law of limited territorial scope, binding in non-Frankish territories only through institutional inertia (revocable, not immutable). No single framework can hold both.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_boundary_immutable_vs_cognatic, conceptual, 'Clarification of the foreclosure relation between immutable_mandate and cognatic_reversion readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(salic_prohibition__cognatic_reversion_reading, 500, 1500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sali_tr_t500, salic_prohibition__cognatic_reversion_reading, theater_ratio, 500, 0.25).
narrative_ontology:measurement_basis(sali_tr_t500, projected).
narrative_ontology:measurement(sali_tr_t700, salic_prohibition__cognatic_reversion_reading, theater_ratio, 700, 0.32).
narrative_ontology:measurement_basis(sali_tr_t700, projected).
narrative_ontology:measurement(sali_tr_t900, salic_prohibition__cognatic_reversion_reading, theater_ratio, 900, 0.41).
narrative_ontology:measurement_basis(sali_tr_t900, observed).
narrative_ontology:measurement(sali_tr_t1100, salic_prohibition__cognatic_reversion_reading, theater_ratio, 1100, 0.48).
narrative_ontology:measurement_basis(sali_tr_t1100, observed).
narrative_ontology:measurement(sali_tr_t1300, salic_prohibition__cognatic_reversion_reading, theater_ratio, 1300, 0.5).
narrative_ontology:measurement_basis(sali_tr_t1300, observed).
narrative_ontology:measurement(sali_tr_t1500, salic_prohibition__cognatic_reversion_reading, theater_ratio, 1500, 0.48).
narrative_ontology:measurement_basis(sali_tr_t1500, observed).

% Extraction over time
narrative_ontology:measurement(sali_be_t500, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 500, 0.35).
narrative_ontology:measurement_basis(sali_be_t500, projected).
narrative_ontology:measurement(sali_be_t700, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 700, 0.48).
narrative_ontology:measurement_basis(sali_be_t700, projected).
narrative_ontology:measurement(sali_be_t900, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 900, 0.58).
narrative_ontology:measurement_basis(sali_be_t900, observed).
narrative_ontology:measurement(sali_be_t1100, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 1100, 0.63).
narrative_ontology:measurement_basis(sali_be_t1100, observed).
narrative_ontology:measurement(sali_be_t1300, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 1300, 0.65).
narrative_ontology:measurement_basis(sali_be_t1300, observed).
narrative_ontology:measurement(sali_be_t1500, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 1500, 0.62).
narrative_ontology:measurement_basis(sali_be_t1500, observed).

% Suppression requirement over time
narrative_ontology:measurement(sali_su_t500, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 500, 0.4).
narrative_ontology:measurement_basis(sali_su_t500, projected).
narrative_ontology:measurement(sali_su_t700, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 700, 0.55).
narrative_ontology:measurement_basis(sali_su_t700, projected).
narrative_ontology:measurement(sali_su_t900, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 900, 0.68).
narrative_ontology:measurement_basis(sali_su_t900, observed).
narrative_ontology:measurement(sali_su_t1100, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 1100, 0.75).
narrative_ontology:measurement_basis(sali_su_t1100, observed).
narrative_ontology:measurement(sali_su_t1300, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 1300, 0.72).
narrative_ontology:measurement_basis(sali_su_t1300, observed).
narrative_ontology:measurement(sali_su_t1500, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 1500, 0.71).
narrative_ontology:measurement_basis(sali_su_t1500, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(salic_prohibition__cognatic_reversion_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(salic_prohibition__cognatic_reversion_reading, 0.12).
narrative_ontology:affects_constraint(salic_prohibition__cognatic_reversion_reading, salic_prohibition__immutable_mandate_reading).
narrative_ontology:affects_constraint(salic_prohibition__cognatic_reversion_reading, salic_prohibition__sovereign_override_reading).

% DUAL FORMULATION NOTE:
% The SALIC_PROHIBITION kernel decomposes into three readings: (1) COGNATIC_REVERSION (this file)—Salic Law as anachronistic Frankish rule, misapplied to non-Frankish territories; cognatic succession is legitimate where local custom established it. (2) IMMUTABLE_MANDATE—Salic Law as irrevocable natural/divine law embedded in dynastic constitution. (3) SOVEREIGN_OVERRIDE—Salic Law as revocable positive law subject to sovereign choice. These are not three angles on one constraint; they are three distinct constraints instantiating one kernel under different readings. The cognatic_reversion reading assigns low ε within its proper territorial scope (Frankish domains with genuine succession ambiguity) but high ε in non-Frankish territories where the founding coordination problem does not exist. The immutable_mandate reading would assign high extraction across all territories because it treats Salic Law as binding by nature. The sovereign_override reading would assign low extraction because it privileges sovereign authority to permit female succession. Each story carries its own beneficiary/victim structure, its own temporal measurements, and its own claims about whether the constraint solves coordination or extracts rights. They are linked via network.affects_constraints to signal that they address the same historical kernel but decompose it into structurally distinct claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(salic_prohibition__cognatic_reversion_reading, institutional, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
