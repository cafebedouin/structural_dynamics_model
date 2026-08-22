% ============================================================================
% CONSTRAINT STORY: family_law_authority__muslim_shariat_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_law_authority__muslim_shariat_reading, []).

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
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: family_law_authority__muslim_shariat_reading
 *   human_readable: Muslim Shariat Marriage as Civil Contract (Nikah)
 *   domain: legal/religious/political
 *
 * SUMMARY:
 *   In jurisdictions with religious personal law, Muslim marriage is governed
 *   as nikah, a contract sanctified by divine text and hadith. The constraint
 *   coordinates family formation, inheritance, and community boundaries
 *   through fiqh interpretation, while asymmetrically allocating unilateral
 *   dissolution rights to male spouses and permitting polygyny. State
 *   recognition enforces the framework, while reformist and feminist
 *   movements contest its gender asymmetries. The 2019 Indian ban on instant
 *   triple talaq represents a partial state override that the traditionalist
 *   interpretive layer largely does not acknowledge as legitimate.
 *
 * KEY AGENTS:
 *   - male_spouses: Primary beneficiary (moderate/constrained) â receive unilateral legal advantages within the contract.
 *   - female_spouses: Primary target (powerless/identity_locked) â bear asymmetric divorce access and polygyny risk.
 *   - religious_jurists: Agenda-setter (organized/identity_locked) â interpret and enforce the framework through personal law boards.
 *   - state_judiciary: Analytical observer (institutional/analytical) â reviews and occasionally overrides specific practices.
 *   - womens_reform_collectives: Excluded voice (organized/constrained) â advocates for symmetry but excluded from board deliberations.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__muslim_shariat_reading, 0.62).
domain_priors:suppression_score(family_law_authority__muslim_shariat_reading, 0.7).
domain_priors:theater_ratio(family_law_authority__muslim_shariat_reading, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__muslim_shariat_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__muslim_shariat_reading, "Muslim Shariat Marriage as Civil Contract (Nikah)").
narrative_ontology:topic_domain(family_law_authority__muslim_shariat_reading, "legal/religious/political").

domain_priors:requires_active_enforcement(family_law_authority__muslim_shariat_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__muslim_shariat_reading, 'e7620e54-37ce-4af6-815c-7156fa6b435e').
narrative_ontology:cs_kernel_codification('e7620e54-37ce-4af6-815c-7156fa6b435e', fixed_text).
narrative_ontology:cs_authority_grounding('e7620e54-37ce-4af6-815c-7156fa6b435e', lineage).
narrative_ontology:cs_interpretation_layer_present('e7620e54-37ce-4af6-815c-7156fa6b435e').
narrative_ontology:cs_reading_relation('e7620e54-37ce-4af6-815c-7156fa6b435e', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('e7620e54-37ce-4af6-815c-7156fa6b435e', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('e7620e54-37ce-4af6-815c-7156fa6b435e', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_reading_relation('e7620e54-37ce-4af6-815c-7156fa6b435e', family_law_authority__secular_contractual_reading, coexists_with).
narrative_ontology:cs_axiom('e7620e54-37ce-4af6-815c-7156fa6b435e', foundational, marriage_as_divine_contract).
narrative_ontology:cs_axiom_status(marriage_as_divine_contract, holdable).
narrative_ontology:cs_axiom_grounding('e7620e54-37ce-4af6-815c-7156fa6b435e', marriage_as_divine_contract, theological).
narrative_ontology:cs_axiom('e7620e54-37ce-4af6-815c-7156fa6b435e', foundational, instant_triple_talaq_valid).
narrative_ontology:cs_axiom_status(instant_triple_talaq_valid, overridden).
narrative_ontology:cs_axiom_grounding('e7620e54-37ce-4af6-815c-7156fa6b435e', instant_triple_talaq_valid, theological).
narrative_ontology:cs_reference_frame('e7620e54-37ce-4af6-815c-7156fa6b435e', classical_shariat_family_order).
narrative_ontology:cs_drift_state('e7620e54-37ce-4af6-815c-7156fa6b435e', post_2019_triple_talaq_ban, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e7620e54-37ce-4af6-815c-7156fa6b435e', '').
narrative_ontology:cs_kernel_id(family_law_authority__muslim_shariat_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__muslim_shariat_reading, male_spouses).
narrative_ontology:constraint_beneficiary(family_law_authority__muslim_shariat_reading, religious_jurists).
narrative_ontology:constraint_victim(family_law_authority__muslim_shariat_reading, female_spouses).
narrative_ontology:constraint_vindicates(family_law_authority__muslim_shariat_reading, nikah_contract_doctrine).
narrative_ontology:constraint_vindicates(family_law_authority__muslim_shariat_reading, personal_law_pluralism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enter nikah with a mahr obligation but receive unilateral talaq rights and permission for polygyny up to four wives. Their legal advantage is enforced by community norms, personal law boards, and state-recognized adjudication. Social exit from the community is formally possible but carries heavy familial and identity costs.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, male_spouses, beneficiary,
    moderate, biographical, constrained, national).

% Enter nikah with a mahr claim but face unilateral dissolution risk and lack symmetric unilateral exit. Pre-2019, instant triple talaq was a concrete and immediate threat. Post-2019, the broader asymmetry in talaq access and polygyny accommodation persists. Formal alternatives such as civil marriage exist in statute but are socially inaccessible due to identity-bound community costs.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, female_spouses, payer,
    powerless, biographical, identity_locked, national).

% Interpret Quranic verses and hadith to adjudicate nikah, talaq, mahr, and inheritance disputes. Staff personal law boards and community courts. Their institutional authority, social role, and generational influence depend on the continued state and community recognition of Shariat as the governing framework for Muslim family matters.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, religious_jurists, agenda_setter,
    organized, generational, identity_locked, national).

% Reviews personal law cases and occasionally overrides specific practices, as in Shayara Bano (2017) and the 2019 Muslim Women (Protection of Rights on Marriage) Act banning instant triple talaq. Maintains the broader plural framework rather than abolishing the reading, acting as a circumscribing rather than eliminating force.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, state_judiciary, observer,
    institutional, generational, analytical, national).

% Advocate for gender-symmetric divorce rights, abolition of instant talaq, and a uniform civil code. Active in public interest litigation and legislative lobbying, but structurally excluded from official personal law board deliberations where interpretive authority is concentrated.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, womens_reform_collectives, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(family_law_authority__muslim_shariat_reading, male_spouses).
narrative_ontology:fixing_cost_class(family_law_authority__muslim_shariat_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates family formation, inheritance transmission, and community boundary maintenance for Muslims through a codified contractual framework recognized by the state and administered by religious jurists.
% TRANSFER_FUNCTION: Transfers unilateral dissolution rights and polygyny permission to male spouses; imposes dower (mahr) obligation on male spouses; transfers interpretive and adjudicative authority over domestic relations to religious jurists.
% ABSENT_VOICES: Women reformists, secular feminists, and uniform civil code proponents argue for gender-symmetric divorce rights and the abolition of personal law; they are structurally excluded from personal law board deliberations though audible in public interest litigation and parliamentary debate.
% DISAPPEARANCE_RATIONALE: If the Shariat marriage framework vanished, Muslim family formation, inheritance, and dissolution in the jurisdiction would default to secular contract or another personal law; community boundaries would weaken and the authority of religious jurists would collapse.
% FOUNDING_PROBLEM: How to govern marriage, divorce, inheritance, and sexual ethics for Muslims in accordance with Quranic and hadith authority within a plural legal environment.
% FOUNDING_PROBLEM_CORROBORATION: Classical fiqh texts and colonial-era legal codification attest the problem was live at inception. Contemporary reformist scholars and the Indian Supreme Court in Shayara Bano (2017) contest the continued necessity of the asymmetric form; the 2019 Muslim Women (Protection of Rights on Marriage) Act corroborates the contested status from outside the beneficiary set.
narrative_ontology:disappearance_verdict(family_law_authority__muslim_shariat_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__muslim_shariat_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__muslim_shariat_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(family_law_authority__muslim_shariat_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__muslim_shariat_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__muslim_shariat_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(family_law_authority__muslim_shariat_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(family_law_authority__muslim_shariat_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects the persistent gender asymmetry in dissolution rights and polygyny permission, though the 2019 ban on instant triple talaq removed the most extractive edge. Suppression (0.70) is high because the constraint depends on state-recognized personal law and community enforcement to prevent exit to secular alternatives. Accessibility collapse (0.68) is substantial: while formal exit to civil marriage exists in some jurisdictions, identity-locked social costs make it nearly inaccessible for many. Resistance (0.55) reflects sustained feminist litigation and legislative reform. Theater ratio (0.36) captures the growing performative defense of personal law against uniform civil code advocacy, where some enforcement serves community boundary maintenance more than internal dispute resolution.
 *
 * PERSPECTIVAL GAP:
 *   From the religious jurist seat, the constraint is genuine coordination of family life in accordance with divine command; from the female spouse seat, it is enforced extraction of symmetric legal standing. The male spouse seat experiences a hybrid â genuine contractual rights plus social obligations (mahr) that partially offset but do not eliminate the asymmetry. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Male spouses and religious jurists are structural beneficiaries: the former receive unilateral rights, the latter receive adjudicative authority. Their directionality sits near the beneficiary end. Female spouses are structural targets: they bear the costs of asymmetric exit options and polygyny risk, with identity-locked exit amplifying their directionality toward the target end. The state judiciary sits at analytical distance with no direct extraction or payment.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â governing Muslim family life in a plural context â remains live for traditionalists but is contested by reformists who argue the specific asymmetric form is no longer necessary. The 2019 triple talaq ban demonstrates partial mandatrophy: the instant-dissolution practice was overridden by state action, yet the broader framework persists. The constraint avoids full piton classification because it retains a concentrated beneficiary structure (male spouses, jurists) who actively defend it, and because the coordination function (inheritance, community boundaries) remains real. The theater ratio is moderate rather than dominant.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is this constraint a unique natural law or one reading of a contested family law kernel?',
    'Comparison with sibling readings (hindu_dharmashastra, christian_canonical, parsi_zoroastrian, secular_contractual) to determine if the kernel is a single underlying commitment or a family of structurally distinct constraints.',
    'If the kernel is irreducibly plural, this reading''s epsilon is indexed only to the Shariat framework, not to marriage as a universal institution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer frame: this constraint is one reading among many of the family law authority kernel.').

omega_variable(
    divine_command_vs_juridical_accretion,
    'Does the gender asymmetry in divorce access derive from immutable Quranic injunction or from medieval fiqh accretion that could be reinterpreted?',
    'Historical-critical study of Quranic text versus classical fiqh rulings; tracking reformist ijtihad and feminist hermeneutics within the tradition.',
    'If the asymmetry is accretion, extractiveness is lower and the constraint may be reformable from within; if divine command, the extraction is structurally fixed at the kernel level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_command_vs_juridical_accretion, empirical, 'Whether gender asymmetry is textual or interpretive accretion.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (state recognition of personal law bars exit to civil code) or internalized (community identity fusion makes exit unthinkable)?',
    'Post-exit trajectory: if female spouses who opt for civil marriage still face community ostracism and family coercion, the constraint is partially internalized.',
    'If internalized, effective suppression exceeds the structural measure because the target carries the constraint after formal legal exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__muslim_shariat_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(muslim_shariat_tr_t0, family_law_authority__muslim_shariat_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(muslim_shariat_tr_t8, family_law_authority__muslim_shariat_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(muslim_shariat_tr_t16, family_law_authority__muslim_shariat_reading, theater_ratio, 16, 0.25).
narrative_ontology:measurement(muslim_shariat_tr_t24, family_law_authority__muslim_shariat_reading, theater_ratio, 24, 0.28).
narrative_ontology:measurement(muslim_shariat_tr_t32, family_law_authority__muslim_shariat_reading, theater_ratio, 32, 0.33).
narrative_ontology:measurement(muslim_shariat_tr_t40, family_law_authority__muslim_shariat_reading, theater_ratio, 40, 0.36).

% Extraction over time
narrative_ontology:measurement(muslim_shariat_be_t0, family_law_authority__muslim_shariat_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(muslim_shariat_be_t8, family_law_authority__muslim_shariat_reading, base_extractiveness, 8, 0.62).
narrative_ontology:measurement(muslim_shariat_be_t16, family_law_authority__muslim_shariat_reading, base_extractiveness, 16, 0.64).
narrative_ontology:measurement(muslim_shariat_be_t24, family_law_authority__muslim_shariat_reading, base_extractiveness, 24, 0.65).
narrative_ontology:measurement(muslim_shariat_be_t32, family_law_authority__muslim_shariat_reading, base_extractiveness, 32, 0.63).
narrative_ontology:measurement(muslim_shariat_be_t40, family_law_authority__muslim_shariat_reading, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(muslim_shariat_su_t0, family_law_authority__muslim_shariat_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(muslim_shariat_su_t8, family_law_authority__muslim_shariat_reading, suppression_requirement, 8, 0.55).
narrative_ontology:measurement(muslim_shariat_su_t16, family_law_authority__muslim_shariat_reading, suppression_requirement, 16, 0.6).
narrative_ontology:measurement(muslim_shariat_su_t24, family_law_authority__muslim_shariat_reading, suppression_requirement, 24, 0.65).
narrative_ontology:measurement(muslim_shariat_su_t32, family_law_authority__muslim_shariat_reading, suppression_requirement, 32, 0.7).
narrative_ontology:measurement(muslim_shariat_su_t40, family_law_authority__muslim_shariat_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__muslim_shariat_reading, identity_coordination).
narrative_ontology:affects_constraint(family_law_authority__muslim_shariat_reading, family_law_authority__hindu_dharmashastra_reading).
narrative_ontology:affects_constraint(family_law_authority__muslim_shariat_reading, family_law_authority__christian_canonical_reading).
narrative_ontology:affects_constraint(family_law_authority__muslim_shariat_reading, family_law_authority__parsi_zoroastrian_reading).
narrative_ontology:affects_constraint(family_law_authority__muslim_shariat_reading, family_law_authority__secular_contractual_reading).

% DUAL FORMULATION NOTE:
% This story is one member of the family_law_authority kernel, decomposed from the natural-language concept of marriage law into five structurally distinct readings. The Muslim Shariat reading differs from siblings in its contractual (not sacramental) framing, its gender-asymmetric dissolution terms, and its grounding in Quranic/hadith lineage.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
