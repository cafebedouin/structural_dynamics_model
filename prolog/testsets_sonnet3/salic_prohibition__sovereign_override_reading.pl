% ============================================================================
% CONSTRAINT STORY: salic_prohibition__sovereign_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Salic Succession Bar as Revocable Positive Law (Sovereign Override Reading)
 *   domain: constitutional_law/dynastic_succession
 *
 * SUMMARY:
 *   This story instantiates the sovereign_override_reading of the
 *   salic_prohibition kernel: the Salic exclusion of women from succession is
 *   treated not as immutable dynastic constitution but as positive law the
 *   sovereign may amend by legislative act (the Pragmatic Sanction model).
 *   Under this reading, a monarch lacking a male heir may decree that a
 *   daughter inherits; the decree is a valid exercise of sovereign
 *   legislative authority, and those who reject it — collateral male
 *   claimants and their foreign backers — are rebels against a legitimately
 *   altered rule rather than defenders of an unbreakable law. Extraction
 *   rises over the interval as contested successions harden into war: initial
 *   promulgation of the override carries modest extraction (subjects are not
 *   yet mobilized), but as the override is challenged militarily, the cost to
 *   subjects and disinherited claimants climbs. This is a distinct constraint
 *   from the immutable_mandate_reading (which denies the sovereign has any
 *   such amending power) and from the cognatic_reversion_reading (which
 *   denies Salic Law ever bound the territory at all) — each reading has its
 *   own beneficiary/victim structure and its own epsilon, per the
 *   epsilon-invariance principle.
 *
 * KEY AGENTS:
 *   - reigning_monarch: agenda_setter (institutional/arbitrage) — issues and enforces the sovereign override
 *   - designated_female_heir: beneficiary (powerful/constrained) — inherits only if the override holds
 *   - collateral_male_claimants: payer (powerful/constrained) — disinherited by the override
 *   - subjects_conscripted_for_succession_war: payer (powerless/trapped) — bears the war cost of contested legitimacy
 *   - constitutional_historians: observer (analytical/analytical) — assesses the coherence of the override doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(salic_prohibition__sovereign_override_reading, 0.58).
domain_priors:suppression_score(salic_prohibition__sovereign_override_reading, 0.62).
domain_priors:theater_ratio(salic_prohibition__sovereign_override_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(salic_prohibition__sovereign_override_reading, tangled_rope).
narrative_ontology:human_readable(salic_prohibition__sovereign_override_reading, "Salic Succession Bar as Revocable Positive Law (Sovereign Override Reading)").
narrative_ontology:topic_domain(salic_prohibition__sovereign_override_reading, "constitutional_law/dynastic_succession").

domain_priors:requires_active_enforcement(salic_prohibition__sovereign_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(salic_prohibition__sovereign_override_reading, '44a7efb6-d5d2-485d-b6df-0663be3384a9').
narrative_ontology:cs_kernel_codification('44a7efb6-d5d2-485d-b6df-0663be3384a9', distributed).
narrative_ontology:cs_authority_grounding('44a7efb6-d5d2-485d-b6df-0663be3384a9', lineage).
narrative_ontology:cs_interpretation_layer_present('44a7efb6-d5d2-485d-b6df-0663be3384a9').
narrative_ontology:cs_reading_relation('44a7efb6-d5d2-485d-b6df-0663be3384a9', salic_prohibition__immutable_mandate_reading, forecloses).
narrative_ontology:cs_reading_relation('44a7efb6-d5d2-485d-b6df-0663be3384a9', salic_prohibition__cognatic_reversion_reading, coexists_with).
narrative_ontology:cs_axiom('44a7efb6-d5d2-485d-b6df-0663be3384a9', foundational, sovereign_legislative_power_reaches_succession).
narrative_ontology:cs_axiom_status(sovereign_legislative_power_reaches_succession, holdable).
narrative_ontology:cs_axiom_grounding('44a7efb6-d5d2-485d-b6df-0663be3384a9', sovereign_legislative_power_reaches_succession, conventional).
narrative_ontology:cs_axiom('44a7efb6-d5d2-485d-b6df-0663be3384a9', secondary, override_validity_survives_contestation).
narrative_ontology:cs_axiom_status(override_validity_survives_contestation, holdable).
narrative_ontology:cs_axiom_grounding('44a7efb6-d5d2-485d-b6df-0663be3384a9', override_validity_survives_contestation, instrumental).
narrative_ontology:cs_reference_frame('44a7efb6-d5d2-485d-b6df-0663be3384a9', sovereign_legislative_supremacy_over_dynastic_rule).
narrative_ontology:cs_drift_state('44a7efb6-d5d2-485d-b6df-0663be3384a9', post_succession_war_settlement, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('44a7efb6-d5d2-485d-b6df-0663be3384a9', '').
narrative_ontology:cs_kernel_id(salic_prohibition__sovereign_override_reading, salic_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(salic_prohibition__sovereign_override_reading, reigning_monarch).
narrative_ontology:constraint_beneficiary(salic_prohibition__sovereign_override_reading, designated_female_heir).
narrative_ontology:constraint_beneficiary(salic_prohibition__sovereign_override_reading, sovereign_legislative_authority).
narrative_ontology:constraint_victim(salic_prohibition__sovereign_override_reading, collateral_male_claimants).
narrative_ontology:constraint_victim(salic_prohibition__sovereign_override_reading, subjects_conscripted_for_succession_war).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds sovereign legislative authority to issue a Pragmatic Sanction altering the succession rule when no male heir exists. Frames the exclusion of female heirs as a positive-law custom the crown itself can amend, not a fixed constitutional bedrock. Secures the succession of a chosen heir (often a daughter) by decree, backed by whatever military and diplomatic assurance can be mustered.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, reigning_monarch, agenda_setter,
    institutional, generational, arbitrage, national).

% Stands to inherit the throne only if the sovereign act altering the succession rule is upheld. Depends entirely on the legitimacy of the override surviving contestation; her claim collapses if collateral male claimants prevail in the resulting conflict.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, designated_female_heir, beneficiary,
    powerful, biographical, constrained, national).

% The doctrine that the crown's legislative power extends to altering succession rules is vindicated every time an override is enforced and survives challenge; it is a legal principle rather than an actor, listed for completeness of the structural picture.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, sovereign_legislative_authority, beneficiary,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(salic_prohibition__sovereign_override_reading, sovereign_legislative_authority, agenda_setter).
narrative_ontology:stakeholder_non_agent(salic_prohibition__sovereign_override_reading, sovereign_legislative_authority).

% Would have inherited under the older Salic exclusion rule absent the sovereign override. Loses claim to the throne when the monarch's decree is upheld; their recourse is armed contestation, alliance-building with foreign powers, or acceptance of disinheritance under the new sovereign act.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, collateral_male_claimants, payer,
    powerful, biographical, constrained, national).

% Bear the material cost — conscription, taxation, destruction — of the wars of succession that follow when collateral claimants reject the sovereign override and take up arms. Have no voice in whether the override is issued or contested and cannot exit the territory whose throne is disputed.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, subjects_conscripted_for_succession_war, payer,
    powerless, immediate, trapped, national).

% Foreign powers who back a collateral male claimant to extend their own influence are not party to the domestic legal question of whether the sovereign override is valid, yet they materially shape whether it survives by force. Their preferred outcome (a male, foreign-aligned claimant) is excluded from the internal legal proceeding entirely.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, rival_claimant_foreign_backers, excluded,
    powerful, biographical, mobile, continental).

% Analyze whether the sovereign override was a coherent exercise of legislative authority or an ad hoc rationalization for a preferred succession outcome, without a stake in which claimant prevails.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(salic_prohibition__sovereign_override_reading, designated_female_heir).
narrative_ontology:fixing_cost_class(salic_prohibition__sovereign_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal mechanism for the polity to avoid a succession vacuum when the male line fails, by locating in the sovereign the authority to amend the rule of succession rather than accept dynastic extinction or an uncontrolled scramble for the throne.
% TRANSFER_FUNCTION: Moves the throne (and the resources, patronage, and territorial claims attached to it) from the collateral male line that would have inherited under the older rule to the sovereign-designated heir, at the cost of war borne by ordinary subjects and disinherited claimants.
% ABSENT_VOICES: Subjects conscripted into the resulting succession wars have no standing in the legal question of whether the override is valid; foreign backers of the disinherited male claimant are structurally outside the domestic legal proceeding even though they will fight over its outcome.
% DISAPPEARANCE_RATIONALE: If the sovereign's power to override the succession rule were denied, the designated heir's claim collapses entirely, the collateral male line inherits by default, and the entire structure of alliances, wars, and successor states built on the override's validity unravels retroactively.
% FOUNDING_PROBLEM: A ruling house faces extinction of the male line and must choose between accepting the crown's passage to a distant or foreign male relative, or asserting that the sovereign's legislative authority extends to altering the rule of succession to keep the crown within the direct line.
% FOUNDING_PROBLEM_CORROBORATION: The sovereign and the designated heir's court attest the override is a valid and necessary exercise of legislative sovereignty. Foreign chancelleries backing the collateral male claimant, and later constitutional historians examining the pragmatic sanctions of this period, dispute the override's legal validity and read it as retroactive rationalization for a preferred succession — corroboration exists on both sides, which is why the status is contested rather than resolved.
narrative_ontology:disappearance_verdict(salic_prohibition__sovereign_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(salic_prohibition__sovereign_override_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(salic_prohibition__sovereign_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate-high (0.58) because the override, once contested, transfers not just the crown but the material costs of defending it onto subjects who have no say in the legal question. Suppression (0.62) reflects the active military and legal enforcement required to make the override stick against rival claims — it is not self-executing positive law, it must be defended. Theater ratio is modest (0.3): most of the apparatus (courts, estates-general ratification, foreign recognition-seeking) performs real legitimation work, though a growing share becomes performative recognition-seeking as the succession war drags on. Accessibility collapse is moderate (0.45) — the override does not eliminate the collateral claimants' legal argument, it out-legislates it, leaving the alternative reading very much alive (hence resistance at 0.68, reflecting genuine sustained contestation, not mere friction).
 *
 * DIRECTIONALITY LOGIC:
 *   The reigning monarch and the designated female heir sit at the beneficiary end: the override exists to secure the heir's claim and the monarch's dynastic project. Collateral male claimants sit at the target end — they lose a throne they would have held under the prior rule, with constrained exit (dynastic claims are not something one can simply walk away from once asserted). Subjects conscripted into the resulting wars are the most trapped: powerless, immediate time horizon, no legal standing in the succession question at all, yet they pay in blood and treasure. Foreign backers of the collateral claimant are excluded from the domestic legal proceeding but not from its consequences — their exclusion is structural rather than a matter of legal standing.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (avoiding dynastic extinction or an uncontrolled succession scramble) is genuinely live at the moment the override is first issued, but its status becomes contested precisely because the override, once it wins, entrenches a legal doctrine (sovereign amending power over succession) whose ongoing use no longer answers a live extinction crisis so much as a recurring assertion of legislative supremacy. This reading resists mislabeling the override as pure extraction: it does solve a real coordination problem (avoiding succession vacuum) even as its persistence, sustained by war rather than consensus, carries a heavy extractive cost onto conscripted subjects and disinherited claimants — hence tangled_rope rather than snare or rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereign_amending_power_scope,
    'Does sovereign legislative authority genuinely extend to amending the fundamental rule of succession, or is succession a constitutional bedrock beyond the reach of ordinary sovereign legislative acts?',
    'Examination of contemporaneous constitutional theory, estates-general or parlement ratification records, and whether comparable overrides were treated as valid precedent in later successions within the same polity.',
    'If sovereign amending power is genuine, the override reading is the structurally correct one and the immutable_mandate_reading is the constructed fiction; if succession is bedrock, this reading''s beneficiary structure rests on an ultra vires act and the immutable_mandate_reading''s victim set (the override''s beneficiaries become its victims) is the accurate one.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereign_amending_power_scope, conceptual, 'Whether sovereign legislative power can validly reach the rule of succession itself.').

omega_variable(
    override_as_rationalization_vs_principle,
    'Was the Pragmatic Sanction model a genuine, generally-applicable legal principle the sovereign could invoke in any male-line failure, or an ad hoc rationalization constructed specifically to secure one preferred heir?',
    'Compare the legal reasoning and procedural formality of this override against other, non-controversial exercises of sovereign legislative authority in the same polity and period; look for consistency of application versus one-off special pleading.',
    'If ad hoc, the tangled_rope coordination function is largely cover for a dynastic preference, pushing the effective classification toward snare; if a genuine general principle, the coordination function is real and the tangled_rope classification is well-grounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(override_as_rationalization_vs_principle, conceptual, 'Whether the override reflects general legal principle or a one-off rationalization.').

omega_variable(
    reading_selection_evidence,
    'What signals in the historical record support selecting the sovereign_override_reading over the immutable_mandate_reading or cognatic_reversion_reading as the operative legal framing at the time?',
    'This story was authored on the basis that the monarch''s own court and allied jurists explicitly framed the Pragmatic Sanction as an act of legislative sovereignty (not as a claim that Salic Law never applied, and not as an admission that it was binding but being violated). Sibling readings would require different founding documents: the cognatic reading would need evidence the exclusion was never locally codified; the immutable reading would need evidence the sovereign''s own court denied having power to alter it.',
    'If the historical record instead shows the monarch''s court denying the existence of any Salic rule in the territory, this constraint should be re-filed as the cognatic_reversion_reading; if the court instead treated the override as merely broken law rather than validly amended law, this constraint collapses into the immutable_mandate_reading''s victim set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_evidence, conceptual, 'Documents the evidentiary basis for choosing this reading among three live framings of the same kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(salic_prohibition__sovereign_override_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sali_tr_t0, salic_prohibition__sovereign_override_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(sali_tr_t8, salic_prohibition__sovereign_override_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(sali_tr_t16, salic_prohibition__sovereign_override_reading, theater_ratio, 16, 0.22).
narrative_ontology:measurement(sali_tr_t24, salic_prohibition__sovereign_override_reading, theater_ratio, 24, 0.26).
narrative_ontology:measurement(sali_tr_t32, salic_prohibition__sovereign_override_reading, theater_ratio, 32, 0.29).
narrative_ontology:measurement(sali_tr_t40, salic_prohibition__sovereign_override_reading, theater_ratio, 40, 0.3).

% Extraction over time
narrative_ontology:measurement(sali_be_t0, salic_prohibition__sovereign_override_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sali_be_t8, salic_prohibition__sovereign_override_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(sali_be_t16, salic_prohibition__sovereign_override_reading, base_extractiveness, 16, 0.5).
narrative_ontology:measurement(sali_be_t24, salic_prohibition__sovereign_override_reading, base_extractiveness, 24, 0.55).
narrative_ontology:measurement(sali_be_t32, salic_prohibition__sovereign_override_reading, base_extractiveness, 32, 0.57).
narrative_ontology:measurement(sali_be_t40, salic_prohibition__sovereign_override_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(sali_su_t0, salic_prohibition__sovereign_override_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(sali_su_t8, salic_prohibition__sovereign_override_reading, suppression_requirement, 8, 0.48).
narrative_ontology:measurement(sali_su_t16, salic_prohibition__sovereign_override_reading, suppression_requirement, 16, 0.55).
narrative_ontology:measurement(sali_su_t24, salic_prohibition__sovereign_override_reading, suppression_requirement, 24, 0.6).
narrative_ontology:measurement(sali_su_t32, salic_prohibition__sovereign_override_reading, suppression_requirement, 32, 0.62).
narrative_ontology:measurement(sali_su_t40, salic_prohibition__sovereign_override_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(salic_prohibition__sovereign_override_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(salic_prohibition__sovereign_override_reading, salic_prohibition__immutable_mandate_reading).
narrative_ontology:affects_constraint(salic_prohibition__sovereign_override_reading, salic_prohibition__cognatic_reversion_reading).

% DUAL FORMULATION NOTE:
% This constraint, salic_prohibition__immutable_mandate_reading, and salic_prohibition__cognatic_reversion_reading are three readings of one contested kernel (salic_prohibition), each with a distinct epsilon and beneficiary/victim structure per the epsilon-invariance principle. The sovereign_override_reading forecloses the immutable_mandate_reading within any single legal framework (a sovereign either can or cannot amend the succession rule — both cannot be simultaneously true within one dynastic constitution), while it coexists_with the cognatic_reversion_reading, since a polity could consistently hold both 'Salic Law binds here but is amendable by sovereign act' and, in a different territory, 'Salic Law never bound here at all' without contradiction — these are claims about different territories' legal inheritance, not mutually exclusive claims about the same one.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(salic_prohibition__sovereign_override_reading, powerful, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
