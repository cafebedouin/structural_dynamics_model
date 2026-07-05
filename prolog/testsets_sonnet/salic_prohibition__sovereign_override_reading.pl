% ============================================================================
% CONSTRAINT STORY: salic_prohibition__sovereign_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_salic_prohibition__sovereign_override_reading, []).

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
    narrative_ontology:affects_constraint/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: salic_prohibition__sovereign_override_reading
 *   human_readable: Salic Succession Prohibition as Revocable Positive Law (Sovereign Override Reading)
 *   domain: constitutional_law/dynastic_succession/political_history
 *
 * SUMMARY:
 *   This story instantiates the sovereign-override reading of the Salic
 *   succession kernel: the exclusion of female succession is treated as
 *   ordinary positive law, amendable by the sovereign's legislative act (a
 *   Pragmatic Sanction or equivalent), rather than as an immutable dynastic
 *   constitution (immutable_mandate_reading, sibling) or as a Frankish custom
 *   never binding outside Francia (cognatic_reversion_reading, sibling).
 *   Under this reading, a sovereign without a qualified male heir may
 *   legislate a change admitting a daughter, and any agnatic claimant who
 *   resists is recast as a rebel against legitimate constituted authority
 *   rather than a defender of an unbreakable rule. The reading's coordination
 *   function (dynastic continuity, avoidance of interregnum) is real, but it
 *   is bundled with asymmetric extraction: the war fought to enforce the
 *   override falls on subjects and the displaced claimant, while the
 *   sovereign, the designated heir, and allied powers capture the
 *   settlement's benefits. This is why the story is authored as tangled_rope
 *   rather than a clean rope or a pure snare — genuine succession-continuity
 *   coordination is present, but it rides alongside enforced extraction from
 *   parties who never consented to the legislative act.
 *
 * KEY AGENTS:
 *   - reigning_sovereign_issuing_pragmatic_sanction: agenda-setter who redefines the succession rule as revocable legislation
 *   - designated_female_heir_and_her_line: primary beneficiary whose claim exists only because of the override
 *   - displaced_agnatic_claimant: primary target, reclassified from heir to rebel by the same act
 *   - subjects_conscripted_into_succession_war: diffuse victims bearing the war's material cost
 *   - great_power_allies_underwriting_settlement: inter-institutional actor whose support is conditional and self-interested
 *   - constitutional_historians: analytical observers assessing whether the doctrine is coherent law or ex post legitimation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(salic_prohibition__sovereign_override_reading, 0.58).
domain_priors:suppression_score(salic_prohibition__sovereign_override_reading, 0.62).
domain_priors:theater_ratio(salic_prohibition__sovereign_override_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(salic_prohibition__sovereign_override_reading, tangled_rope).
narrative_ontology:human_readable(salic_prohibition__sovereign_override_reading, "Salic Succession Prohibition as Revocable Positive Law (Sovereign Override Reading)").
narrative_ontology:topic_domain(salic_prohibition__sovereign_override_reading, "constitutional_law/dynastic_succession/political_history").

domain_priors:requires_active_enforcement(salic_prohibition__sovereign_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(salic_prohibition__sovereign_override_reading, 'cbc9d00d-e36a-4184-9e96-71be3b5c8557').
narrative_ontology:cs_kernel_codification('cbc9d00d-e36a-4184-9e96-71be3b5c8557', distributed).
narrative_ontology:cs_authority_grounding('cbc9d00d-e36a-4184-9e96-71be3b5c8557', extraction).
narrative_ontology:cs_interpretation_layer_present('cbc9d00d-e36a-4184-9e96-71be3b5c8557').
narrative_ontology:cs_reading_relation('cbc9d00d-e36a-4184-9e96-71be3b5c8557', salic_prohibition__immutable_mandate_reading, forecloses).
narrative_ontology:cs_reading_relation('cbc9d00d-e36a-4184-9e96-71be3b5c8557', salic_prohibition__cognatic_reversion_reading, coexists_with).
narrative_ontology:cs_axiom('cbc9d00d-e36a-4184-9e96-71be3b5c8557', foundational, succession_rules_are_revisable_positive_law).
narrative_ontology:cs_axiom_status(succession_rules_are_revisable_positive_law, holdable).
narrative_ontology:cs_axiom_grounding('cbc9d00d-e36a-4184-9e96-71be3b5c8557', succession_rules_are_revisable_positive_law, conventional).
narrative_ontology:cs_axiom('cbc9d00d-e36a-4184-9e96-71be3b5c8557', secondary, sovereign_legislative_act_binds_subsequent_claimants).
narrative_ontology:cs_axiom_status(sovereign_legislative_act_binds_subsequent_claimants, holdable).
narrative_ontology:cs_axiom_grounding('cbc9d00d-e36a-4184-9e96-71be3b5c8557', sovereign_legislative_act_binds_subsequent_claimants, conventional).
narrative_ontology:cs_reference_frame('cbc9d00d-e36a-4184-9e96-71be3b5c8557', sovereign_legislative_supremacy_over_succession).
narrative_ontology:cs_drift_state('cbc9d00d-e36a-4184-9e96-71be3b5c8557', post_pragmatic_sanction_war_of_succession, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('cbc9d00d-e36a-4184-9e96-71be3b5c8557', '').
narrative_ontology:cs_kernel_id(salic_prohibition__sovereign_override_reading, salic_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(salic_prohibition__sovereign_override_reading, reigning_sovereign_issuing_pragmatic_sanction).
narrative_ontology:constraint_beneficiary(salic_prohibition__sovereign_override_reading, designated_female_heir_and_her_line).
narrative_ontology:constraint_beneficiary(salic_prohibition__sovereign_override_reading, great_power_allies_underwriting_settlement).
narrative_ontology:constraint_victim(salic_prohibition__sovereign_override_reading, displaced_agnatic_claimant).
narrative_ontology:constraint_victim(salic_prohibition__sovereign_override_reading, subjects_conscripted_into_succession_war).
narrative_ontology:constraint_victim(salic_prohibition__sovereign_override_reading, provincial_estates_forced_to_ratify_under_pressure).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(salic_prohibition__sovereign_override_reading, provincial_estates_forced_to_ratify_under_pressure).
narrative_ontology:constraint_vindicates(salic_prohibition__sovereign_override_reading, sovereign_legislative_supremacy_doctrine).
narrative_ontology:constraint_vindicates(salic_prohibition__sovereign_override_reading, positive_law_revisability_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues a Pragmatic Sanction (or equivalent legislative act) altering the succession rule to admit a daughter or cognatic line, then spends the remainder of the reign securing ratification from estates, courts, and foreign powers. Frames Salic exclusion as a revocable statute of the royal will, not a fixed law of nature, and treats the act as within legitimate sovereign competence.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, reigning_sovereign_issuing_pragmatic_sanction, agenda_setter,
    institutional, generational, arbitrage, national).

% Gains a succession claim that would not exist under strict agnatic reading. Her legitimacy depends entirely on the sovereign act holding — she has no independent claim if the override is repudiated, which ties her fate to continued enforcement of the reading and to war if agnatic rivals contest it.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, designated_female_heir_and_her_line, beneficiary,
    powerful, generational, constrained, national).

% Extends diplomatic recognition and, when necessary, military support to the sovereign-override succession because it serves their own balance-of-power interests. Can withdraw recognition or switch to backing the agnatic claimant if the alliance calculus shifts, making their support conditional rather than principled.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, great_power_allies_underwriting_settlement, beneficiary,
    institutional, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(salic_prohibition__sovereign_override_reading, great_power_allies_underwriting_settlement, agenda_setter).

% Holds what would be the superior claim under strict male-line reading and is recast as a rebel or usurper the moment the sovereign act is issued. Options are submission, exile, or war; the reclassification from rightful heir to rebel is itself an act of the same legislative authority he is contesting.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, displaced_agnatic_claimant, payer,
    powerful, biographical, constrained, national).

% Bear the direct cost of the succession dispute becoming a war of dynastic legitimacy — conscription, taxation, occupation, famine. They have no voice in whether the override is legitimate and cannot exit the territory whose sovereign is contested.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, subjects_conscripted_into_succession_war, payer,
    powerless, immediate, trapped, national).

% Assemblies and diets asked to register or ratify the sovereign's revised succession act, often under considerable pressure and with implicit threats of disfavor for refusal. Ratification is later cited as proof of consensual legislative process, though the estates' room to refuse was narrow.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, provincial_estates_forced_to_ratify_under_pressure, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(salic_prohibition__sovereign_override_reading, provincial_estates_forced_to_ratify_under_pressure, beneficiary).

% Evaluate whether the sovereign-override framing is a coherent legal doctrine or an ex post legitimation of a power grab, comparing outcomes across dynasties where override succeeded, failed, or produced protracted war.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(salic_prohibition__sovereign_override_reading, reigning_sovereign_issuing_pragmatic_sanction).
narrative_ontology:fixing_cost_class(salic_prohibition__sovereign_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal mechanism for continuing a dynasty's line and avoiding a succession vacuum when no qualified agnatic male heir exists, by treating the succession rule as legislation the sovereign can amend rather than an immutable constitutional fact.
% TRANSFER_FUNCTION: Moves the crown, its revenues, and its territorial claims from the agnatic line that strict Salic reading would favor to the cognatic line the sovereign designates; moves the costs of enforcing that transfer (war, taxation, conscription) onto subjects and onto the displaced claimant's faction.
% ABSENT_VOICES: The displaced agnatic claimant's faction and, more diffusely, the peasantry and townspeople who fight and pay for the succession war are structurally absent from the legislative act itself — the Pragmatic Sanction is negotiated among the sovereign, select estates, and foreign courts, not the population bearing the war's costs.
% DISAPPEARANCE_RATIONALE: If sovereign legislative authority over succession were withdrawn overnight, the designated heiress's claim would collapse to nothing, the displaced agnatic claimant's claim would become the uncontested superior claim, foreign guarantees tied to the Sanction would lapse, and any war fought to defend the override would retroactively become indefensible aggression rather than legitimate defense of the throne.
% FOUNDING_PROBLEM: A sovereign lacks a qualified male agnatic heir and faces the prospect of the dynastic line ending, a disputed succession, or absorption by a rival house; the sovereign-override doctrine supplies a legal path to preserve the dynasty's continuity through a female or cognatic heir.
% FOUNDING_PROBLEM_CORROBORATION: The sovereign and the designated heir's court attest the problem (line extinction) is live and the override necessary. Neutral contemporary jurists and later constitutional historians, examining cases where override provoked decades of war, attest that the 'problem' framing often served to legitimate a preferred succession outcome after the fact rather than solve a genuine legal vacuum — no source entirely outside the beneficiary courts treats the override as uncontroversially necessary.
narrative_ontology:disappearance_verdict(salic_prohibition__sovereign_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(salic_prohibition__sovereign_override_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(salic_prohibition__sovereign_override_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(salic_prohibition__sovereign_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(salic_prohibition__sovereign_override_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(salic_prohibition__sovereign_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(salic_prohibition__sovereign_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(salic_prohibition__sovereign_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.30 to 0.58 across the interval as the override moves from a legislative declaration to a contested settlement requiring war to enforce — the doctrine's costs to the displaced claimant and to conscripted subjects accumulate as resistance to the reading hardens. Theater ratio climbs moderately (0.15 to 0.35) as ratification ceremonies, estate assemblies, and foreign recognitions increasingly perform legitimacy that the underlying dispute has not actually resolved. Suppression tracks the war-enforcement dynamic closely, rising sharply in the middle of the interval as military campaigns are required to make the override stick, then plateauing once one side prevails. All three metrics share one time grid as required.
 *
 * PERSPECTIVAL GAP:
 *   From the sovereign's and designated heir's seats, the override is a legitimate exercise of sovereign legislative competence solving a genuine succession problem. From the displaced agnatic claimant's seat and from the conscripted subjects' seat, the same act is an ad hoc legal invention that strips a superior claim and imposes war costs on people with no say in the legislative act. The engine's per-seat computation should register this divergence directly from the beneficiary/victim and exit-option declarations, not from any authored resolution of which reading is 'true.'
 *
 * DIRECTIONALITY LOGIC:
 *   The sovereign and designated heir sit near the beneficiary end of directionality — they set and profit from the legislative act. The displaced claimant and conscripted subjects sit near the target end — they bear the costs of enforcing an act they did not choose and, in the claimant's case, are relabeled as illegitimate for resisting it. Allied great powers are beneficiaries with mobile exit (arbitrage-adjacent): their support is conditional on their own interests and can be withdrawn, which differentiates them from the trapped domestic subjects who cannot exit the war zone.
 *
 * MANDATROPHY ANALYSIS:
 *   The sovereign-override reading resists mandatrophy misclassification in a specific way: because the founding problem (avoiding dynastic extinction or a succession vacuum) can be genuinely live in some instances and merely convenient in others, treating every instance of the doctrine as pure extraction would erase the real coordination function it sometimes serves, while treating every instance as legitimate law would erase the extraction imposed on displaced claimants and subjects. The tangled_rope classification holds both facts simultaneously rather than collapsing to one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereign_competence_boundary,
    'Does the sovereign''s legislative authority genuinely extend to amending the fundamental succession rule, or is succession a constitutional matter beyond ordinary legislative competence even under a positive-law framing?',
    'Comparative analysis of contemporaneous constitutional theory and estate/parliamentary responses: where estates treated the Pragmatic Sanction as requiring their independent ratifying consent (rather than mere registration), that suggests succession was understood as extra-legislative; where sovereigns issued such acts unilaterally and were obeyed without contest, that supports the sovereign-override reading.',
    'If succession is found to lie outside ordinary legislative competence even in this reading''s own tradition, the override loses its legal coherence and collapses toward the immutable_mandate_reading''s framing of any change as usurpation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereign_competence_boundary, conceptual, 'Whether succession law is genuinely within sovereign legislative competence.').

omega_variable(
    ratification_consent_quality,
    'Were provincial estate ratifications of the Pragmatic Sanction genuine consent or coerced formality?',
    'Examine records of dissent, delay, or conditional ratification by individual estates/diets, and whether refusal carried real consequence for the estate in question.',
    'If ratifications were substantially coerced, the reading''s claim to legislative legitimacy (as opposed to raw dynastic power) weakens considerably, shifting the classification''s weight toward the extractive pole.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ratification_consent_quality, empirical, 'Whether estate ratification constituted genuine legislative consent.').

omega_variable(
    beneficiary_favorable_labeling,
    'Is labeling the displaced agnatic claimant a ''rebel'' a neutral legal description or a beneficiary-authored framing that pre-judges the legitimacy question?',
    'Compare how third-party courts, chroniclers, and foreign chancelleries outside both dynastic factions described the claimant during the dispute, before the war''s outcome was known.',
    'If neutral outside observers described the claimant as a rightful claimant rather than a rebel prior to the outcome, the ''rebel'' label is retroactive legitimation rather than a description this reading can claim as structurally accurate at the time.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_favorable_labeling, conceptual, 'Whether the rebel/legitimate-heir framing is neutral or beneficiary-constructed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(salic_prohibition__sovereign_override_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sali_tr_t0, salic_prohibition__sovereign_override_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(sali_tr_t8, salic_prohibition__sovereign_override_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(sali_tr_t16, salic_prohibition__sovereign_override_reading, theater_ratio, 16, 0.28).
narrative_ontology:measurement(sali_tr_t24, salic_prohibition__sovereign_override_reading, theater_ratio, 24, 0.32).
narrative_ontology:measurement(sali_tr_t32, salic_prohibition__sovereign_override_reading, theater_ratio, 32, 0.34).
narrative_ontology:measurement(sali_tr_t40, salic_prohibition__sovereign_override_reading, theater_ratio, 40, 0.35).

% Extraction over time
narrative_ontology:measurement(sali_be_t0, salic_prohibition__sovereign_override_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(sali_be_t8, salic_prohibition__sovereign_override_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(sali_be_t16, salic_prohibition__sovereign_override_reading, base_extractiveness, 16, 0.5).
narrative_ontology:measurement(sali_be_t24, salic_prohibition__sovereign_override_reading, base_extractiveness, 24, 0.55).
narrative_ontology:measurement(sali_be_t32, salic_prohibition__sovereign_override_reading, base_extractiveness, 32, 0.57).
narrative_ontology:measurement(sali_be_t40, salic_prohibition__sovereign_override_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(sali_su_t0, salic_prohibition__sovereign_override_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(sali_su_t8, salic_prohibition__sovereign_override_reading, suppression_requirement, 8, 0.4).
narrative_ontology:measurement(sali_su_t16, salic_prohibition__sovereign_override_reading, suppression_requirement, 16, 0.55).
narrative_ontology:measurement(sali_su_t24, salic_prohibition__sovereign_override_reading, suppression_requirement, 24, 0.62).
narrative_ontology:measurement(sali_su_t32, salic_prohibition__sovereign_override_reading, suppression_requirement, 32, 0.6).
narrative_ontology:measurement(sali_su_t40, salic_prohibition__sovereign_override_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(salic_prohibition__sovereign_override_reading, salic_prohibition__immutable_mandate_reading).
narrative_ontology:affects_constraint(salic_prohibition__sovereign_override_reading, salic_prohibition__cognatic_reversion_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposing the natural-language label 'Salic Law' per the epsilon-invariance principle. salic_prohibition__immutable_mandate_reading treats the exclusion as irrevocable dynastic constitution (near-mountain framing, high accessibility_collapse, low resistance from within its own tradition); salic_prohibition__cognatic_reversion_reading treats the prohibition as never validly binding outside Frankish territory, dissolving the extraction question by denying the prohibition's jurisdiction; this story, sovereign_override_reading, treats the exclusion as ordinary positive law a sovereign may revise, producing a tangled_rope structure where dynastic-continuity coordination is real but is bundled with war costs imposed on the displaced claimant and conscripted subjects. The three share no single epsilon value because they are not the same constraint measured differently — they are three structurally distinct legal claims that colloquial usage of 'Salic Law' conflates.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
