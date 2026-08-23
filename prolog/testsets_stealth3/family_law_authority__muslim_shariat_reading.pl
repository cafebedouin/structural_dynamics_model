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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: family_law_authority__muslim_shariat_reading
 *   human_readable: Muslim Personal Law Nikah Regime (Quranic-Hadith Reading)
 *   domain: comparative law / political theory / religious governance
 *
 * SUMMARY:
 *   In a religion-specific personal-law system, Muslim marriages are
 *   constituted as nikah — a contractual exchange of offer, acceptance,
 *   witnesses, and dower — administered through fiqh handbooks, qazi and
 *   quasi-judicial board channels, and statutory hooks laid down in 1937,
 *   amended in 1939 and 1986, and restricted in 2019. The standing
 *   arrangement under contest, and the sole referent for epsilon, is this
 *   statutory-administered nikah regime as it actually operates — not any
 *   alternative family-law design this or any other reading would install.
 *   The claim and the metrics are independent authored facts: claimed_type is
 *   tangled_rope because the same structure that standardizes formation,
 *   dower, and dissolution channels also concentrates dissolution initiative
 *   and remarriage capacity on the husband's side; the metrics describe that
 *   mixed operation descriptively, and the engine computes per-seat types
 *   from the structural data without reconciling them to the claim. This file
 *   is one member of a decomposed constraint family (see
 *   network.dual_formulation_note).
 *
 * KEY AGENTS:
 *   - ulama_personal_law_boards: agenda-setter (institutional/identity_locked) — administers nikah, talaq, and khul' through qazi channels; institutional continuity rides on disputes routing through them
 *   - state_legislature_and_courts: co-agenda-setter (institutional/mobile) — codifies, amends, and adjudicates the personal-law application; demonstrated capacity to amend in 1937, 1939, 1986, and 2019
 *   - muslim_male_spouses: principal beneficiary with payer side-flows (moderate/constrained) — holds dissolution initiative and remarriage capacity; owes mahr, maintenance, and equitable-treatment duties
 *   - muslim_wives_under_talaq_asymmetry: primary target (powerless/trapped) — exposed to a dissolution capacity she does not hold
 *   - cowives_in_polygynous_households: secondary target (powerless/trapped) — compete for resources under hard-to-verify equal-treatment duties
 *   - children_of_unilateral_dissolutions: diffuse target (powerless/constrained) — bear custody-default and support-instability costs of one-sided departures
 *   - patrilineal_kinship_networks: secondary beneficiary (organized/mobile) — lineage continuity served by rapid male remarriage
 *   - muslim_womens_rights_campaigns: excluded voice (organized/constrained) — documents harm and litigates but is not seated where rules are fixed
 *   - comparative_family_law_analysts: analytical observer — compiles cross-community statistics with no administrative stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__muslim_shariat_reading, 0.58).
domain_priors:suppression_score(family_law_authority__muslim_shariat_reading, 0.62).
domain_priors:theater_ratio(family_law_authority__muslim_shariat_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__muslim_shariat_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__muslim_shariat_reading, "Muslim Personal Law Nikah Regime (Quranic-Hadith Reading)").
narrative_ontology:topic_domain(family_law_authority__muslim_shariat_reading, "comparative law / political theory / religious governance").

domain_priors:requires_active_enforcement(family_law_authority__muslim_shariat_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__muslim_shariat_reading, 'bf20a073-5764-4bda-bc00-080d637205ee').
narrative_ontology:cs_kernel_codification('bf20a073-5764-4bda-bc00-080d637205ee', fixed_text).
narrative_ontology:cs_authority_grounding('bf20a073-5764-4bda-bc00-080d637205ee', lineage).
narrative_ontology:cs_interpretation_layer_present('bf20a073-5764-4bda-bc00-080d637205ee').
narrative_ontology:cs_reading_relation('bf20a073-5764-4bda-bc00-080d637205ee', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('bf20a073-5764-4bda-bc00-080d637205ee', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('bf20a073-5764-4bda-bc00-080d637205ee', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_reading_relation('bf20a073-5764-4bda-bc00-080d637205ee', family_law_authority__secular_contractual_reading, influences).
narrative_ontology:cs_axiom('bf20a073-5764-4bda-bc00-080d637205ee', foundational, nikah_validity_requires_contractual_form_per_revelation).
narrative_ontology:cs_axiom_status(nikah_validity_requires_contractual_form_per_revelation, holdable).
narrative_ontology:cs_axiom_grounding('bf20a073-5764-4bda-bc00-080d637205ee', nikah_validity_requires_contractual_form_per_revelation, theological).
narrative_ontology:cs_axiom('bf20a073-5764-4bda-bc00-080d637205ee', foundational, dissolution_initiative_vests_in_husband).
narrative_ontology:cs_axiom_status(dissolution_initiative_vests_in_husband, holdable).
narrative_ontology:cs_axiom_grounding('bf20a073-5764-4bda-bc00-080d637205ee', dissolution_initiative_vests_in_husband, theological).
narrative_ontology:cs_axiom('bf20a073-5764-4bda-bc00-080d637205ee', secondary, mahr_secures_brides_exclusive_provision).
narrative_ontology:cs_axiom_status(mahr_secures_brides_exclusive_provision, holdable).
narrative_ontology:cs_axiom_grounding('bf20a073-5764-4bda-bc00-080d637205ee', mahr_secures_brides_exclusive_provision, theological).
narrative_ontology:cs_reference_frame('bf20a073-5764-4bda-bc00-080d637205ee', classical_madhhab_family_regime).
narrative_ontology:cs_drift_state('bf20a073-5764-4bda-bc00-080d637205ee', post_triple_talaq_ban, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('bf20a073-5764-4bda-bc00-080d637205ee', '').
narrative_ontology:cs_kernel_id(family_law_authority__muslim_shariat_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__muslim_shariat_reading, ulama_personal_law_boards).
narrative_ontology:constraint_beneficiary(family_law_authority__muslim_shariat_reading, muslim_male_spouses).
narrative_ontology:constraint_beneficiary(family_law_authority__muslim_shariat_reading, patrilineal_kinship_networks).
narrative_ontology:constraint_victim(family_law_authority__muslim_shariat_reading, muslim_wives_under_talaq_asymmetry).
narrative_ontology:constraint_victim(family_law_authority__muslim_shariat_reading, cowives_in_polygynous_households).
narrative_ontology:constraint_victim(family_law_authority__muslim_shariat_reading, children_of_unilateral_dissolutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(family_law_authority__muslim_shariat_reading, muslim_male_spouses).
narrative_ontology:constraint_vindicates(family_law_authority__muslim_shariat_reading, shariat_application_doctrine).
narrative_ontology:constraint_vindicates(family_law_authority__muslim_shariat_reading, madhab_juristic_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the fiqh handbooks that define valid nikah procedure, staff qazi and dar-ul-qaza channels that register marriages and hear dissolution disputes, publish guidance to congregations, and mobilize politically whenever legislation touches the personal-law settlement. Their scholarly standing, institutional livelihoods, and mediating role between community and state all depend on family disputes continuing to route through them.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, ulama_personal_law_boards, agenda_setter,
    institutional, generational, identity_locked, national).

% Enacts the statutes that determine how the shariat reading applies (the 1937 application act, the 1939 dissolution act, the 1986 maintenance act, the 2019 instant-talaq ban) and family-court judges adjudicate the resulting cases. It can amend the arrangement by ordinary legislation, as the 2019 ban demonstrates, though each attempt draws sustained organized opposition and electoral calculation.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, state_legislature_and_courts, agenda_setter,
    institutional, generational, mobile, national).

% Hold the classical dissolution initiative — able to end the marriage by spoken formula without the spouse's agreement — and the option of contracting additional marriages, alongside household deference norms in their favor. They owe mahr payment, maintenance during marriage and iddat, and equitable-treatment duties they can be pursued over in court or congregation. Switching to civil registration carries real social cost inside their own communities.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, muslim_male_spouses, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(family_law_authority__muslim_shariat_reading, muslim_male_spouses, payer).

% Can lose the marriage by a formula they do not control; khul' requires the husband's consent or a qazi's intervention. Security rests on mahr balances that are frequently deferred for decades and on maintenance that the 1986 statute narrowed to the iddat period. Leaving the personal-law system altogether typically means leaving family, neighborhood, and congregation networks as well.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, muslim_wives_under_talaq_asymmetry, payer,
    powerless, biographical, trapped, national).

% Share a husband and household resources under an equal-treatment injunction that is difficult to observe or enforce in practice. Their economic security depends on the conduct of the husband and the senior wife; their divorce recourse mirrors the same asymmetry as any other wife's.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, cowives_in_polygynous_households, payer,
    powerless, biographical, trapped, national).

% Absorb abrupt paternal departures decided without negotiation: custody defaults toward the father's side after early childhood under classical rules, support follows the mother's often-strained claims, and households reorganize around whichever relatives step in. They choose none of the arrangement and bear its instabilities.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, children_of_unilateral_dissolutions, payer,
    powerless, immediate, constrained, national).

% Elders arrange matches, broker mahr negotiations, and gain lineage-continuity guarantees when sons can remarry quickly after a dissolution. Lineage heads lose leverage chiefly where couples register civilly and relocate away from kin oversight.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, patrilineal_kinship_networks, beneficiary,
    organized, generational, mobile, regional).

% Document post-divorce destitution in field surveys, litigate on behalf of affected women (the 2017 Supreme Court case began with affected petitioners), and circulate codified reform proposals. Boards decline to seat them in deliberations, and legislators negotiate with the boards instead; their proposals circulate outside the room where rules are fixed.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, muslim_womens_rights_campaigns, excluded,
    organized, biographical, constrained, national).

% Compile divorce-access statistics, mahr fulfillment rates, and court and qazi records across communities; publish comparisons that inform litigation and legislation. They hold no administrative stake in the arrangement.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, comparative_family_law_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(family_law_authority__muslim_shariat_reading, ulama_personal_law_boards).
narrative_ontology:fixing_cost_class(family_law_authority__muslim_shariat_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes marriage formation (offer, acceptance, witnesses, dower), defines maintenance obligations during marriage and iddat, and provides recognized dissolution channels across a large, geographically dispersed community without a central civil registry.
% TRANSFER_FUNCTION: Moves mahr from the groom's side to the bride as her exclusive property, ongoing maintenance from husband to wife during marriage and iddat, custody and support allocations after dissolution — and moves dissolution decision-rights and remarriage capacity toward the husband's side.
% ABSENT_VOICES: Women subject to the dissolution asymmetry were effectively absent when the settlement was codified (the 1937 application act passed with minimal organized Muslim women's input) and remain peripheral when boards deliberate reform today; organized women's campaigns stand outside the room where rules are fixed, negotiating instead with legislators who treat the boards as the community's voice.
% DISAPPEARANCE_RATIONALE: Overnight disappearance would force reorganization across the whole family-law surface: dissolution would reroute through civil courts with no settled procedure, pending mahr claims would lose their enforcement channel, qazi offices and board authority would evaporate, and the community's boundary marker of distinctive personal law would vanish — every named seat's arrangements depend on the regime continuing.
% FOUNDING_PROBLEM: Secure orderly marriage formation, dowered provision for brides, and bounded dissolution across a dispersed community — replacing unlimited pre-Islamic repudiation and unprovided widowhood with contract-bound obligations giving women a minimum secured claim.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the beneficiary set: 1930s legislative assembly debates recording deserted wives' petitions that produced the 1939 Dissolution Act; the 2017 Supreme Court petitioners, themselves affected women; Bharatiya Muslim Mahila Andolan field surveys documenting post-talaq destitution; and dissenting parliamentary testimony during the 1986 rollback. The boards' own attestations are excluded here as in-benefit-party sources.
narrative_ontology:disappearance_verdict(family_law_authority__muslim_shariat_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__muslim_shariat_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__muslim_shariat_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(family_law_authority__muslim_shariat_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__muslim_shariat_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is 0.58 at interval end: the dissolution initiative is classically unilateral, polygyny permission runs one way, and mahr's protective value is eroded in practice by deferral — counterweighted by the maintenance duty, the khul' channel, the 1939 judicial-divorce act, and the 2019 instant-talaq ban, which keep the profile short of pure extraction. Suppression (0.62) is a raw structural property, unscaled by power or scope in authoring: enforcement runs through qazi processes, congregation sanction, and — until 2019 — the absence of any competing valid dissolution path. Theater rises from 0.12 to 0.42 as an increasing share of board activity defends the frame rhetorically (mass mobilizations defending a statistically marginal practice, immutability proclamations amid statutory erosion) rather than administering marriages. Accessibility collapse is moderate (0.45): civil registration exits exist legally but carry heavy social cost. Resistance (0.55) is real and organized: litigation, field-documentation campaigns, and repeated legislative contest. All three tracked series run on one shared grid (1937, 1961, 1986, 2000, 2017, 2019, 2025) so no metric row borrows another's end-state; the 1986 uptick in extractiveness dates to the maintenance rollback, and the 2017-2019 decline dates to the constitutional challenge and ban.
 *
 * PERSPECTIVAL GAP:
 *   The engine computes divergent per-seat classifications from the structural data, and the divergence here is wide. From the board seat the arrangement presents as a self-administering sacred order whose defense is stewardship; from the wives' seat the same procedures present as enforced exposure to a formula they cannot pronounce and cannot answer; from the state seat it presents as recurring legislative contention with electoral weight; from the kin-network seat it presents as lineage insurance. Exit options drive much of the gap — identity_locked for the boards, trapped for affected wives, mobile for the state — and no single seat's experience exhausts the structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries sit toward the subsidized end: the boards lowest of all (authority rents, fee income, and political brokerage collected through the arrangement's continuation, fused with institutional identity), patrilineal networks next (remarriage speed converts into lineage continuity), and male spouses net-beneficiaries but tempered by real mahr and maintenance outflows, which the secondary payer role records. Declared victims sit toward the full-target end: wives under the asymmetry highest (trapped exit, community lock-in), co-wives close behind, children high though diffuse. The three seats absent from the beneficiary/victim arrays (state, campaigns, analysts) take canonical fallbacks; no directionality overrides are authored because the structural derivation covers every declared party and the undeclared seats admit no principled per-agent correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — securing women's minimum provision across a decentralized community — is contested rather than dead: the provisioning need is materially live (documented post-divorce destitution), but the classical instruments are partly obsolete (instant talaq banned, mahr unindexed against inflation, the maintenance window narrowed in 1986). The tangled_rope classification prevents two symmetrical errors: reading the arrangement as rope would erase the documented asymmetric harm running through its dissolution leg; reading it as snare would erase the genuine coordination function and the protections that made it an advance at founding. The receipt surface sharpens the picture: gains concentrate at the board seat (authority, fees, brokerage) even though diffuse marital advantages accrue across male spouses — receipt of the arrangement's proceeds is not the same fact as holding a beneficiary role. Fixing cost is prohibitive relative to benefit for any single actor (constitutional protections, electoral weight, community mobilization capacity), which is why decay has run through litigation and attrition rather than repeal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_delta,
    'Which structural features of this arrangement belong to the shariat reading specifically, and which are common furniture of the family_law_authority kernel shared with sibling readings?',
    'Compile the sibling reading stories and diff victim sets, dissolution mechanics, and enforcement loci across readings.',
    'Per-seat classifications and epsilon are reading-indexed; cross-reading comparisons that ignore the delta will misattribute extraction to the kernel rather than to this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_delta, conceptual, 'Separates reading-specific structure (contractual dissolution initiative, polygyny permission, dower leg) from kernel-shared structure.').

omega_variable(
    divine_immutability_status,
    'Does this reading''s authority structure treat the arrangement as divinely fixed (revision illegitimate in principle) or as juristically constructed (open to ijtihad and amendment)?',
    'Madhab-level doctrinal analysis plus observation of whether internal reinterpretive channels opened after the 2019 ban or the change was framed solely as external usurpation.',
    'A divine-fixity posture suppresses measured resistance and supports natural-law immunity claims; a juristic posture keeps the constraint revisable and keeps tangled_rope dynamics dominant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_immutability_status, conceptual, 'Whether immutability is a theological claim or a rhetorical defense of constructed advantage.').

omega_variable(
    mahr_protective_vs_priced,
    'Does mahr operate as protective provision (bride''s exclusive, promptly paid security) or as a priced element legitimating asymmetric exchange (deferred, nominal, contested at dissolution)?',
    'Compare dissolution outcomes across recorded mahr amounts, promptness, and recovery patterns in court and qazi records.',
    'Protective operation lowers the extraction attributable to the dower leg; priced operation raises it and strengthens the target reading of the wives'' seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mahr_protective_vs_priced, empirical, 'Valuation of the dower obligation''s actual function versus its stated function.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression carried by legal and community enforcement structures, or by internalized piety-and-censure expectations that travel with the agent after exit?',
    'Post-exit trajectory study of women who registered civil marriages or obtained khul'': if felt constraint persists after the mechanism is removed, part of the suppression is internalized.',
    'An internalized share raises effective suppression above the structural measure and predicts persistence of asymmetric norms even after further statutory reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized suppression mechanism in the dissolution asymmetry.').

omega_variable(
    coalition_capacity_growth,
    'Can organized wives'' campaigns convert individually powerless seats into class-level bargaining power sufficient to move enforcement and legislation?',
    'Track legislative outcomes, litigation success rates, and board responses across successive campaign waves (1930s petitions, 1980s mobilization, 2017-2019 litigation wave).',
    'Rising coalition power lowers the effective target-position of the wives'' seat over time and accelerates the extractiveness decline already visible in the measurement series.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_capacity_growth, empirical, 'Whether class-level organizing offsets the individual powerlessness of affected women.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__muslim_shariat_reading, 1937, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fla_msr_tr_t1937, family_law_authority__muslim_shariat_reading, theater_ratio, 1937, 0.12).
narrative_ontology:measurement_basis(fla_msr_tr_t1937, observed).
narrative_ontology:measurement(fla_msr_tr_t1961, family_law_authority__muslim_shariat_reading, theater_ratio, 1961, 0.14).
narrative_ontology:measurement_basis(fla_msr_tr_t1961, observed).
narrative_ontology:measurement(fla_msr_tr_t1986, family_law_authority__muslim_shariat_reading, theater_ratio, 1986, 0.3).
narrative_ontology:measurement_basis(fla_msr_tr_t1986, observed).
narrative_ontology:measurement(fla_msr_tr_t2000, family_law_authority__muslim_shariat_reading, theater_ratio, 2000, 0.26).
narrative_ontology:measurement_basis(fla_msr_tr_t2000, observed).
narrative_ontology:measurement(fla_msr_tr_t2017, family_law_authority__muslim_shariat_reading, theater_ratio, 2017, 0.34).
narrative_ontology:measurement_basis(fla_msr_tr_t2017, observed).
narrative_ontology:measurement(fla_msr_tr_t2019, family_law_authority__muslim_shariat_reading, theater_ratio, 2019, 0.38).
narrative_ontology:measurement_basis(fla_msr_tr_t2019, observed).
narrative_ontology:measurement(fla_msr_tr_t2025, family_law_authority__muslim_shariat_reading, theater_ratio, 2025, 0.42).
narrative_ontology:measurement_basis(fla_msr_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(fla_msr_be_t1937, family_law_authority__muslim_shariat_reading, base_extractiveness, 1937, 0.73).
narrative_ontology:measurement_basis(fla_msr_be_t1937, observed).
narrative_ontology:measurement(fla_msr_be_t1961, family_law_authority__muslim_shariat_reading, base_extractiveness, 1961, 0.68).
narrative_ontology:measurement_basis(fla_msr_be_t1961, observed).
narrative_ontology:measurement(fla_msr_be_t1986, family_law_authority__muslim_shariat_reading, base_extractiveness, 1986, 0.72).
narrative_ontology:measurement_basis(fla_msr_be_t1986, observed).
narrative_ontology:measurement(fla_msr_be_t2000, family_law_authority__muslim_shariat_reading, base_extractiveness, 2000, 0.7).
narrative_ontology:measurement_basis(fla_msr_be_t2000, observed).
narrative_ontology:measurement(fla_msr_be_t2017, family_law_authority__muslim_shariat_reading, base_extractiveness, 2017, 0.66).
narrative_ontology:measurement_basis(fla_msr_be_t2017, observed).
narrative_ontology:measurement(fla_msr_be_t2019, family_law_authority__muslim_shariat_reading, base_extractiveness, 2019, 0.61).
narrative_ontology:measurement_basis(fla_msr_be_t2019, observed).
narrative_ontology:measurement(fla_msr_be_t2025, family_law_authority__muslim_shariat_reading, base_extractiveness, 2025, 0.58).
narrative_ontology:measurement_basis(fla_msr_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(fla_msr_su_t1937, family_law_authority__muslim_shariat_reading, suppression_requirement, 1937, 0.55).
narrative_ontology:measurement_basis(fla_msr_su_t1937, observed).
narrative_ontology:measurement(fla_msr_su_t1961, family_law_authority__muslim_shariat_reading, suppression_requirement, 1961, 0.52).
narrative_ontology:measurement_basis(fla_msr_su_t1961, observed).
narrative_ontology:measurement(fla_msr_su_t1986, family_law_authority__muslim_shariat_reading, suppression_requirement, 1986, 0.62).
narrative_ontology:measurement_basis(fla_msr_su_t1986, observed).
narrative_ontology:measurement(fla_msr_su_t2000, family_law_authority__muslim_shariat_reading, suppression_requirement, 2000, 0.59).
narrative_ontology:measurement_basis(fla_msr_su_t2000, observed).
narrative_ontology:measurement(fla_msr_su_t2017, family_law_authority__muslim_shariat_reading, suppression_requirement, 2017, 0.61).
narrative_ontology:measurement_basis(fla_msr_su_t2017, observed).
narrative_ontology:measurement(fla_msr_su_t2019, family_law_authority__muslim_shariat_reading, suppression_requirement, 2019, 0.64).
narrative_ontology:measurement_basis(fla_msr_su_t2019, observed).
narrative_ontology:measurement(fla_msr_su_t2025, family_law_authority__muslim_shariat_reading, suppression_requirement, 2025, 0.62).
narrative_ontology:measurement_basis(fla_msr_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__muslim_shariat_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(family_law_authority__muslim_shariat_reading, family_law_authority__hindu_dharmashastra_reading).
narrative_ontology:affects_constraint(family_law_authority__muslim_shariat_reading, family_law_authority__christian_canonical_reading).
narrative_ontology:affects_constraint(family_law_authority__muslim_shariat_reading, family_law_authority__parsi_zoroastrian_reading).
narrative_ontology:affects_constraint(family_law_authority__muslim_shariat_reading, family_law_authority__secular_contractual_reading).

% DUAL FORMULATION NOTE:
% 'Religious family law authority' is a colloquial label spanning structurally distinct arrangements; per the epsilon-invariance principle it decomposes into one story per reading of the family_law_authority kernel. This file authors the muslim_shariat_reading; sibling files instantiate the dharmashastra, canonical, Zoroastrian, and secular-contractual readings, each with its own epsilon, beneficiary/victim structure, and classification. Jurisdictionally the secular-contractual reading sits downstream: its universalization is blocked by the personal-law settlements this reading helps hold, so this story's edges to it are structural-pressure edges rather than logical ones. Cross-family comparisons are valid only through the kernel_reading_delta omega, never by treating the readings as one constraint viewed from angles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
