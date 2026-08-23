% ============================================================================
% CONSTRAINT STORY: salic_prohibition__immutable_mandate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_salic_prohibition__immutable_mandate_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: salic_prohibition__immutable_mandate_reading
 *   human_readable: Salic Law as Immutable Natural/Divine Mandate
 *   domain: constitutional/dynastic/political_history
 *
 * SUMMARY:
 *   The Salic Law prohibition on female succession, under the immutable
 *   mandate reading, is treated as an irrevocable divine and natural law
 *   embedded in dynastic constitutions rather than a revocable human
 *   enactment. This reading categorically excludes female heirs and cognatic
 *   descendants from sovereignty, legitimizes agnatic male claimants as the
 *   sole lawful successors, and licenses preventive war to enforce agnatic
 *   priority. The constraint is here claimed as a Mountain because that is
 *   the ontological status asserted by the reading itself; the authored
 *   metrics describe a heavily extractive, actively enforced arrangement with
 *   concentrated beneficiaries and identifiable victims, inviting False
 *   Summit Mountain evaluation.
 *
 * KEY AGENTS:
 *   - agnatic_monarchs: Primary agenda-setter and beneficiary (institutional/identity_locked) â enforces and collects sovereignty
 *   - male_succession_claimants: Primary beneficiary (powerful/mobile) â gains from exclusion of female rivals
 *   - female_heirs: Primary target (powerless/trapped) â categorically excluded by divine/natural law framing
 *   - cognatic_descendants: Secondary target (moderate/constrained) â excluded through female line
 *   - dynastic_jurists: Agenda-setter (institutional/constrained) â maintains the interpretive apparatus
 *   - cognatic_foreign_powers: Target (powerful/constrained) â delegitimized when pressing cognatic claims
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(salic_prohibition__immutable_mandate_reading, 0.85).
domain_priors:suppression_score(salic_prohibition__immutable_mandate_reading, 0.82).
domain_priors:theater_ratio(salic_prohibition__immutable_mandate_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(salic_prohibition__immutable_mandate_reading, mountain).
narrative_ontology:human_readable(salic_prohibition__immutable_mandate_reading, "Salic Law as Immutable Natural/Divine Mandate").
narrative_ontology:topic_domain(salic_prohibition__immutable_mandate_reading, "constitutional/dynastic/political_history").

domain_priors:requires_active_enforcement(salic_prohibition__immutable_mandate_reading).
domain_priors:emerges_naturally(salic_prohibition__immutable_mandate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(salic_prohibition__immutable_mandate_reading, 'b75e2fa5-5af2-4571-b626-2140adeb4912').
narrative_ontology:cs_kernel_codification('b75e2fa5-5af2-4571-b626-2140adeb4912', fixed_text).
narrative_ontology:cs_authority_grounding('b75e2fa5-5af2-4571-b626-2140adeb4912', lineage).
narrative_ontology:cs_interpretation_layer_present('b75e2fa5-5af2-4571-b626-2140adeb4912').
narrative_ontology:cs_reading_relation('b75e2fa5-5af2-4571-b626-2140adeb4912', salic_prohibition__sovereign_override_reading, forecloses).
narrative_ontology:cs_reading_relation('b75e2fa5-5af2-4571-b626-2140adeb4912', salic_prohibition__cognatic_reversion_reading, forecloses).
narrative_ontology:cs_axiom('b75e2fa5-5af2-4571-b626-2140adeb4912', foundational, agnatic_succession_as_divine_mandate).
narrative_ontology:cs_axiom_status(agnatic_succession_as_divine_mandate, holdable).
narrative_ontology:cs_axiom_grounding('b75e2fa5-5af2-4571-b626-2140adeb4912', agnatic_succession_as_divine_mandate, theological).
narrative_ontology:cs_axiom('b75e2fa5-5af2-4571-b626-2140adeb4912', foundational, female_succession_categorically_null).
narrative_ontology:cs_axiom_status(female_succession_categorically_null, holdable).
narrative_ontology:cs_axiom_grounding('b75e2fa5-5af2-4571-b626-2140adeb4912', female_succession_categorically_null, deontological).
narrative_ontology:cs_reference_frame('b75e2fa5-5af2-4571-b626-2140adeb4912', agnatic_divine_succession_order).
narrative_ontology:cs_drift_state('b75e2fa5-5af2-4571-b626-2140adeb4912', early_modern_sovereignty_challenge, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b75e2fa5-5af2-4571-b626-2140adeb4912', '').
narrative_ontology:cs_kernel_id(salic_prohibition__immutable_mandate_reading, salic_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(salic_prohibition__immutable_mandate_reading, agnatic_monarchs).
narrative_ontology:constraint_beneficiary(salic_prohibition__immutable_mandate_reading, male_succession_claimants).
narrative_ontology:constraint_victim(salic_prohibition__immutable_mandate_reading, female_heirs).
narrative_ontology:constraint_victim(salic_prohibition__immutable_mandate_reading, cognatic_descendants).
narrative_ontology:constraint_victim(salic_prohibition__immutable_mandate_reading, cognatic_foreign_powers).
narrative_ontology:constraint_vindicates(salic_prohibition__immutable_mandate_reading, agnatic_priority_doctrine).
narrative_ontology:constraint_vindicates(salic_prohibition__immutable_mandate_reading, divine_right_of_kings).
narrative_ontology:constraint_vindicates(salic_prohibition__immutable_mandate_reading, frankish_legal_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer and enforce the Salic prohibition as divine and natural mandate. Their own legitimacy and dynastic continuity depend on agnatic succession, creating identity-lock: repudiating the rule would undermine their title. They collect sovereignty and territorial integrity maintained by the exclusion of female and cognatic challengers.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, agnatic_monarchs, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(salic_prohibition__immutable_mandate_reading, agnatic_monarchs, beneficiary).

% Inherit thrones and sovereign territories exclusively through male lines. Benefit directly from the categorical exclusion of female heirs and cognatic descendants, facing reduced competition for succession.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, male_succession_claimants, beneficiary,
    powerful, biographical, mobile, national).

% Born into dynastic families but categorically excluded from succession and sovereign inheritance by the immutable mandate framing. Cannot exit the dynastic system into which they were born; their claims are treated as contrary to divine and natural order.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, female_heirs, payer,
    powerless, biographical, trapped, national).

% Descendants through female lines who are barred from sovereign succession claims. May hold subsidiary titles or property but are excluded from the dynastic core; their path to sovereignty is legally and theologically blocked.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, cognatic_descendants, payer,
    moderate, biographical, constrained, regional).

% Interpret and maintain the legal-theological framework that embeds Salic Law as immutable divine mandate. Their authority and positions depend on preserving the interpretive tradition and defending it against cognatic and sovereign-override challenges.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, dynastic_jurists, agenda_setter,
    institutional, generational, constrained, national).

% States or dynastic houses with cognatic succession traditions that are delegitimized when pressing claims through female lines. Their challenges are framed as unlawful aggression against the divine agnatic order, constraining their diplomatic and military options.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, cognatic_foreign_powers, payer,
    powerful, generational, constrained, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(salic_prohibition__immutable_mandate_reading, agnatic_monarchs).
narrative_ontology:fixing_cost_class(salic_prohibition__immutable_mandate_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes an unambiguous rule of dynastic succession to prevent civil war and territorial fragmentation when a ruler dies without a universally accepted heir.
% TRANSFER_FUNCTION: Transfers sovereignty, territorial titles, and dynastic legitimacy exclusively through agnatic male lines, excluding female heirs and cognatic descendants from inheritance; transfers the right of challenge to male claimants while delegitimizing cognatic challengers.
% ABSENT_VOICES: Female heirs and cognatic claimants are structurally excluded from juridical and theological discourse; their objections are treated as contrary to divine and natural order. Cognatic foreign powers are delegitimized when pressing claims through female lines.
% DISAPPEARANCE_RATIONALE: If the Salic prohibition vanished, female heirs and cognatic descendants would gain succession rights, agnatic territorial monopolies would fragment, and the theological-juridical apparatus sustaining agnatic divine right would collapse. Dynastic alliances and war justifications would rearrange around inclusive succession norms.
% FOUNDING_PROBLEM: Succession instability and dynastic civil war following the death of rulers without clear or universally accepted heirs in the early medieval Frankish realm.
% FOUNDING_PROBLEM_CORROBORATION: No corroborating source outside the beneficiary set exists; modern historiography contests the agnatic founding narrative, and cognatic historians explicitly reject it as retroactive justification manufactured to legitimate agnatic consolidation.
narrative_ontology:disappearance_verdict(salic_prohibition__immutable_mandate_reading, world_rearranges).
narrative_ontology:founding_problem_status(salic_prohibition__immutable_mandate_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(salic_prohibition__immutable_mandate_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(salic_prohibition__immutable_mandate_reading, 'none', 1).
narrative_ontology:epsilon_provenance(salic_prohibition__immutable_mandate_reading, 0.85, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(salic_prohibition__immutable_mandate_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(salic_prohibition__immutable_mandate_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, ExtMetricName, E),
    domain_priors:suppression_score(salic_prohibition__immutable_mandate_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(salic_prohibition__immutable_mandate_reading),
    narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(salic_prohibition__immutable_mandate_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The accessibility_collapse (0.92) and resistance (0.30) profile resembles a Mountain because the divine/natural law framing makes alternatives heretical and unthinkable, suppressing open resistance. However, the extractiveness (0.85) and suppression (0.82) metrics describe an actively enforced human construction that extracts sovereignty from female heirs and cognatic lines. The theater_ratio (0.65) reflects the elaborate theological-juridical performance required to maintain a Frankish legal custom as eternal divine mandate. The temporal series show extraction, theater, and enforcement all intensifying between the early medieval customary phase and the early modern absolutist era, as the immutable reading was progressively codified and theologized.
 *
 * PERSPECTIVAL GAP:
 *   From the agnatic monarch and jurist seats, the constraint appears as the natural order of succession, a Mountain-like feature of the political cosmos. From the female heir and cognatic claimant seats, it appears as an arbitrary extraction mechanism enforced by dynastic power and theological threat. The engine computes this divergence from the same structural data: low directionality for beneficiaries, high directionality for trapped and excluded targets.
 *
 * DIRECTIONALITY LOGIC:
 *   Agnatic monarchs and male claimants are declared beneficiaries with low directionality because the constraint subsidizes their succession claims and sovereignty. Female heirs are declared victims/payers with high directionality because they bear the full cost of exclusion and are trapped by birth into the dynastic system. Cognatic descendants and foreign powers sit at moderate-high directionality because they are partly constrained by diplomatic and military options but still pay the cost of exclusion. Jurists are agenda-setters with low-moderate directionality: they enforce and benefit from the interpretive monopoly but are constrained by the doctrinal framework they maintain.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â succession instability following ruler deaths â was arguably live in the Merovingian and Carolingian periods. Under the immutable mandate reading, however, the arrangement persisted and intensified long after it had become a tool for agnatic consolidation rather than a neutral coordination mechanism. The R5 genealogy shows contested corroboration: the benefiting parties claim the problem remains live, while excluded parties and modern historiography treat it as a manufactured rationale. The mismatch between founding_problem_status=contested and disappearance_verdict=world_rearranges signals mandatrophy: the constraint persists because it extracts, not because it coordinates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_or_dynastic_construct,
    'Is the Salic prohibition a genuine divine/natural law or a dynastic construct theologized to appear immutable?',
    'Comparative legal history establishing the textual and theological interpolation timeline; detection of beneficiary concentration inconsistent with natural-law distribution.',
    'If constructed, the Mountain claim is a false summit and the constraint reclassifies as tangled_rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_or_dynastic_construct, conceptual, 'Natural law versus constructed ambiguity at the core of the immutable mandate claim').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the constraint''s persistence due to internalized belief in divine/natural order or structural military-legal enforcement?',
    'Post-exit trajectory: do cognatic claimants who escape the system through marriage to non-Salic territories continue to accept the prohibition?',
    'If internalized, effective suppression exceeds the structural measure; if purely structural, the constraint is a snare rather than a culturally embedded mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(salic_prohibition__immutable_mandate_reading, 0, 500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sali_tr_t0, salic_prohibition__immutable_mandate_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(sali_tr_t100, salic_prohibition__immutable_mandate_reading, theater_ratio, 100, 0.32).
narrative_ontology:measurement(sali_tr_t200, salic_prohibition__immutable_mandate_reading, theater_ratio, 200, 0.42).
narrative_ontology:measurement(sali_tr_t300, salic_prohibition__immutable_mandate_reading, theater_ratio, 300, 0.52).
narrative_ontology:measurement(sali_tr_t400, salic_prohibition__immutable_mandate_reading, theater_ratio, 400, 0.58).
narrative_ontology:measurement(sali_tr_t500, salic_prohibition__immutable_mandate_reading, theater_ratio, 500, 0.65).

% Extraction over time
narrative_ontology:measurement(sali_be_t0, salic_prohibition__immutable_mandate_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(sali_be_t100, salic_prohibition__immutable_mandate_reading, base_extractiveness, 100, 0.52).
narrative_ontology:measurement(sali_be_t200, salic_prohibition__immutable_mandate_reading, base_extractiveness, 200, 0.65).
narrative_ontology:measurement(sali_be_t300, salic_prohibition__immutable_mandate_reading, base_extractiveness, 300, 0.74).
narrative_ontology:measurement(sali_be_t400, salic_prohibition__immutable_mandate_reading, base_extractiveness, 400, 0.8).
narrative_ontology:measurement(sali_be_t500, salic_prohibition__immutable_mandate_reading, base_extractiveness, 500, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(sali_su_t0, salic_prohibition__immutable_mandate_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(sali_su_t100, salic_prohibition__immutable_mandate_reading, suppression_requirement, 100, 0.55).
narrative_ontology:measurement(sali_su_t200, salic_prohibition__immutable_mandate_reading, suppression_requirement, 200, 0.65).
narrative_ontology:measurement(sali_su_t300, salic_prohibition__immutable_mandate_reading, suppression_requirement, 300, 0.72).
narrative_ontology:measurement(sali_su_t400, salic_prohibition__immutable_mandate_reading, suppression_requirement, 400, 0.78).
narrative_ontology:measurement(sali_su_t500, salic_prohibition__immutable_mandate_reading, suppression_requirement, 500, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(salic_prohibition__immutable_mandate_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
