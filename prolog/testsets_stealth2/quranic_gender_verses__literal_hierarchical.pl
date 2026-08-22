% ============================================================================
% CONSTRAINT STORY: quranic_gender_verses__literal_hierarchical
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quranic_gender_verses__literal_hierarchical, []).

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
 *   constraint_id: quranic_gender_verses__literal_hierarchical
 *   human_readable: Qur'anic Gender Provisions (4:11, 2:282, 4:34) — Literal-Hierarchical Reading as Timeless Divine Ordinance
 *   domain: legal/hermeneutic/gender
 *
 * SUMMARY:
 *   This file instantiates ONE reading — literal_hierarchical — of the
 *   contested kernel quranic_gender_verses. The standing arrangement under
 *   contest, which is this story's referent, is the application of Qur'an
 *   4:11 (fixed estate shares at a 2:1 male-to-female ratio), 2:282
 *   (documentation and witness procedure with halved testimonial weight in
 *   specified credit contexts), and 4:34 (male qiwamah with graded
 *   disciplinary authority) as direct, timeless, divinely legislated rule in
 *   personal-status law. Per the epsilon-referent rule, epsilon here is
 *   authored by THIS reading's own lights over that fixed referent: the frame
 *   assesses the schedule as divine apportionment paired with countervailing
 *   male obligations, hence low reading-indexed extractiveness. Structural
 *   data — beneficiaries, victims, exit costs, enforcement — are authored
 *   descriptively and independently of that assessment. The UKE_SCOPE
 *   manifest seeded an expectation of high extraction with women in the
 *   victim set and high exit costs; that expectation is honored in the
 *   structural data and should surface in the engine's computed effective
 *   extraction (directionality and scope amplification on trapped,
 *   identity-locked payers at global scope), while the authored claim remains
 *   the reading's own. Sibling readings (contextual_egalitarian,
 *   progressive_abrogation) are separate files sharing this referent and
 *   authoring their own epsilon. KEY AGENTS (by structural relationship): -
 *   male_household_heads: Primary beneficiary (organized/constrained) —
 *   collect differentiated estate shares, hold guardianship and disciplinary
 *   authority; bear maintenance and blood-money duties -
 *   religious_jurist_class: Agenda setter and collector
 *   (institutional/identity_locked) — interpret, adjudicate, certify; office
 *   and doctrine fused - sharia_administering_states: Agenda setter and
 *   legitimacy beneficiary (institutional/constrained) — codify and enforce
 *   personal-status law - women_governed_by_personal_status_codes: Primary
 *   target (powerless/identity_locked) — differentiated shares, halved
 *   testimony, guardian-mediated procedure; exit via apostasy or family
 *   rupture - devout_women_affirming_ordainment: Framework-fused beneficiary
 *   with payer costs (moderate/identity_locked) - muslim_feminist_hermeneuts:
 *   Excluded voice (organized/mobile) — egalitarian exegesis without
 *   certifying standing - comparative_law_scholars: Analytical observer
 *   (analytical/analytical)
 *
 * KEY AGENTS:
 *   - male_household_heads — primary beneficiary (organized/constrained): estate-share recipients and holders of qiwamah authority
 *   - religious_jurist_class — agenda setter (institutional/identity_locked): interpretive authority fused with the doctrine it administers
 *   - sharia_administering_states — agenda setter with beneficiary secondary role (institutional/constrained): codify and enforce; harvest legitimacy
 *   - women_governed_by_personal_status_codes — primary payer (powerless/identity_locked): differentiated shares, testimony weight, guardian mediation; apostasy/family-rupture exit
 *   - devout_women_affirming_ordainment — beneficiary with payer secondary role (moderate/identity_locked): consenting insiders whose identity is framework-fused
 *   - muslim_feminist_hermeneuts — excluded (organized/mobile): contest interpretation without standing in certifying bodies
 *   - comparative_law_scholars — observer (analytical/analytical): document doctrine-practice divergence across jurisdictions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quranic_gender_verses__literal_hierarchical, 0.18).
domain_priors:suppression_score(quranic_gender_verses__literal_hierarchical, 0.55).
domain_priors:theater_ratio(quranic_gender_verses__literal_hierarchical, 0.13).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, extractiveness, 0.18).
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, theater_ratio, 0.13).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quranic_gender_verses__literal_hierarchical, mountain).
narrative_ontology:human_readable(quranic_gender_verses__literal_hierarchical, "Qur'anic Gender Provisions (4:11, 2:282, 4:34) — Literal-Hierarchical Reading as Timeless Divine Ordinance").
narrative_ontology:topic_domain(quranic_gender_verses__literal_hierarchical, "legal/hermeneutic/gender").

domain_priors:requires_active_enforcement(quranic_gender_verses__literal_hierarchical).
domain_priors:emerges_naturally(quranic_gender_verses__literal_hierarchical).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quranic_gender_verses__literal_hierarchical, 'e5f091cc-959a-4385-a55a-c3c06dc48e30').
narrative_ontology:cs_kernel_codification('e5f091cc-959a-4385-a55a-c3c06dc48e30', fixed_text).
narrative_ontology:cs_authority_grounding('e5f091cc-959a-4385-a55a-c3c06dc48e30', lineage).
narrative_ontology:cs_interpretation_layer_present('e5f091cc-959a-4385-a55a-c3c06dc48e30').
narrative_ontology:cs_reading_relation('e5f091cc-959a-4385-a55a-c3c06dc48e30', quranic_gender_verses__progressive_abrogation, forecloses).
narrative_ontology:cs_reading_relation('e5f091cc-959a-4385-a55a-c3c06dc48e30', quranic_gender_verses__contextual_egalitarian, coexists_with).
narrative_ontology:cs_axiom('e5f091cc-959a-4385-a55a-c3c06dc48e30', foundational, gender_verses_timeless_direct_legislation).
narrative_ontology:cs_axiom_status(gender_verses_timeless_direct_legislation, holdable).
narrative_ontology:cs_axiom_grounding('e5f091cc-959a-4385-a55a-c3c06dc48e30', gender_verses_timeless_direct_legislation, theological).
narrative_ontology:cs_axiom('e5f091cc-959a-4385-a55a-c3c06dc48e30', foundational, male_qiwamah_divine_apportionment).
narrative_ontology:cs_axiom_status(male_qiwamah_divine_apportionment, holdable).
narrative_ontology:cs_axiom_grounding('e5f091cc-959a-4385-a55a-c3c06dc48e30', male_qiwamah_divine_apportionment, theological).
narrative_ontology:cs_reference_frame('e5f091cc-959a-4385-a55a-c3c06dc48e30', timeless_divine_ordainment).
narrative_ontology:cs_drift_state('e5f091cc-959a-4385-a55a-c3c06dc48e30', contemporary_human_rights_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('e5f091cc-959a-4385-a55a-c3c06dc48e30', '').
narrative_ontology:cs_kernel_id(quranic_gender_verses__literal_hierarchical, quranic_gender_verses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quranic_gender_verses__literal_hierarchical, male_household_heads).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__literal_hierarchical, religious_jurist_class).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__literal_hierarchical, sharia_administering_states).
narrative_ontology:constraint_victim(quranic_gender_verses__literal_hierarchical, women_governed_by_personal_status_codes).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__literal_hierarchical, devout_women_affirming_ordainment).
narrative_ontology:constraint_victim(quranic_gender_verses__literal_hierarchical, male_household_heads).
narrative_ontology:constraint_victim(quranic_gender_verses__literal_hierarchical, devout_women_affirming_ordainment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercise household headship (qiwamah) over wives and dependent kin, direct family decisions, and take the larger shares of estates as sons, brothers, and husbands under the fixed fractional schedule. They bear court-assigned maintenance duties (nafaqa) and blood-money liability for dependents, which the tradition presents as the countervailing obligation. Leaving the arrangement would mean severing kin standing and faith community membership, so even dissatisfied men rarely exit.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, male_household_heads, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(quranic_gender_verses__literal_hierarchical, male_household_heads, payer).

% Interpret the verses, staff courts and seminaries, certify marriages, divorces, and estate divisions, and train successors in transmission chains anchored to the revealed text. Their professional office, adjudication income, and social deference rest on the texts remaining final and directly legislative; abandoning that position would dissolve the ground of their own authority, so the office and the doctrine have become the same thing.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, religious_jurist_class, agenda_setter,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(quranic_gender_verses__literal_hierarchical, religious_jurist_class, beneficiary).

% Codify personal-status law (marriage, divorce, custody, inheritance) from these provisions and enforce it through state courts, gaining legitimacy with constituencies who read enforcement as fidelity to revelation. Attempts to substitute civil-code provisions meet clerical opposition, constitutional entrenchment of sharia clauses, and in some cases street mobilization, making unilateral replacement costly.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, sharia_administering_states, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(quranic_gender_verses__literal_hierarchical, sharia_administering_states, beneficiary).

% Marry, divorce, inherit, and give testimony under the differentiated schedule: daughters receive half a son's share of the estate, women's testimony is weighted at half a man's in specified contractual contexts, husbands hold disciplinary latitude under 4:34 as classically applied, and many jurisdictions route first marriage and divorce through male-guardian consent or court gatekeeping. Departure from the framework entails apostasy consequences in several states and family and community rupture everywhere, and individual economic dependence narrows realistic alternatives.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, women_governed_by_personal_status_codes, payer,
    powerless, biographical, identity_locked, global).

% Affirm the schedule as divine apportionment paired with male maintenance duty, and receive guaranteed support, retained independent title to personal property, and spiritual-equal status within the frame. They carry costs in testimonial weighting and guardian-mediated procedure, but their self-concept and belonging are constituted through the framework, so departure is not a live option they entertain.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, devout_women_affirming_ordainment, beneficiary,
    moderate, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(quranic_gender_verses__literal_hierarchical, devout_women_affirming_ordainment, payer).

% Produce egalitarian exegeses that reread the verses through contextual and purposive (maqasid) methods and argue the provisions were situated steps rather than final hierarchy. They publish through universities and presses but hold no certifying standing in orthodox courts or seminaries; the interpretive establishments that control application of the rules do not admit them to the conversation that decides meaning.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, muslim_feminist_hermeneuts, excluded,
    organized, generational, mobile, global).

% Document how different jurisdictions operationalize the provisions, tracking reform waves (restrictions on unilateral divorce, conditioning of polygyny, minimum-age rules) and mapping divergence between classical doctrine and actual state practice. They collect and analyze but neither administer nor bear the arrangement.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, comparative_law_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quranic_gender_verses__literal_hierarchical, male_household_heads).
narrative_ontology:fixing_cost_class(quranic_gender_verses__literal_hierarchical, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fixes fractional estate shares that close inheritance disputes without open-ended negotiation among heirs; standardizes evidentiary weighting and documentation in credit transactions (2:282 answered recurring debt-record conflicts); locates household decision authority in one member to end authority contests; pairs spousal support duty with household management to allocate marital obligations.
% TRANSFER_FUNCTION: Moves inheritance wealth from daughters, widows, and female collaterals toward sons and male kin at the fixed 2:1 ratio; discounts women's testimonial standing to half a man's in designated contract contexts; places domestic labor and marital obedience under the husband's direction in exchange for maintenance; delivers interpretive office, adjudication income, and social deference to the scholar class, and legitimacy rents to administering states.
% ABSENT_VOICES: Women ruled by these schedules historically lacked standing to contest interpretation, and in many contemporary settings still lack a formal seat in the bodies that certify meaning; Muslim egalitarian exegetes are barred from official interpretive authority; non-Muslim subjects under confessional family-court systems in several states had no voice in rules governing their marriages and estates.
% DISAPPEARANCE_RATIONALE: Personal-status law across dozens of jurisdictions, inheritance-division machinery, judicial training pipelines, marriage-contracting procedure, and the entire professional structure of religious courts are built on these provisions. Overnight removal would force wholesale recodification of family law in dozens of states, redistribute estate flows toward female heirs, unsettle the jurist class's institutional footing, and strip administering states of a major legitimacy instrument.
% FOUNDING_PROBLEM: Seventh-century Medina: recurrent inheritance quarrels as property concentrated in the new polity; unresolved debt-recording disputes damaging commerce; contested household authority in a tribal society transitioning to urban governance. The verses fixed estate shares, mandated written documentation with witnesses, and assigned household headship.
% FOUNDING_PROBLEM_CORROBORATION: Historical-critical scholarship on pre-Islamic Arabian succession practice and early Islamic estate administration corroborates the situational origin from outside the benefiting parties. Modern civil registry and probate systems attest that the original record-keeping and dispute-pacification functions are otherwise served. The jurist class attests continued liveness as divine ordinance, but no disinterested party attests that the gender-specific allocations remain necessary to the founding problems.
narrative_ontology:disappearance_verdict(quranic_gender_verses__literal_hierarchical, world_rearranges).
narrative_ontology:founding_problem_status(quranic_gender_verses__literal_hierarchical, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quranic_gender_verses__literal_hierarchical, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quranic_gender_verses__literal_hierarchical, 'none', 1).
narrative_ontology:epsilon_provenance(quranic_gender_verses__literal_hierarchical, 0.18, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quranic_gender_verses__literal_hierarchical_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, ExtMetricName, E),
    domain_priors:suppression_score(quranic_gender_verses__literal_hierarchical, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(quranic_gender_verses__literal_hierarchical),
    narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(quranic_gender_verses__literal_hierarchical_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.18 from this reading's own lights: the frame judges the schedule as divinely apportioned and offset by male maintenance and blood-money liability, leaving only the residual asymmetry the frame itself concedes when it speaks of 'differentiated rights.' That value is NOT tuned toward any predicted engine output; the engine scales effective extraction upward for targets (women: full-target directionality amplified by identity-locked exit) and by global scope, so the manifest's expected high extraction should appear in computed chi rather than authored epsilon — that divergence is the datum. Suppression (0.55) is authored as the raw structural coercive substrate: religious courts, guardian-consent requirements, apostasy sanction in several states, and pervasive communal sanction; the frame narrates these as implementation of revealed law, but the enforcement machinery is observable regardless of narration. Theater ratio is low (0.13): courts issue consequential rulings; the modest theatrical component appears in modern ceremonial assertions of fidelity that outrun actual doctrinal function. Accessibility collapse is high (0.90): within the framework, alternative allocations are not merely imprudent but illegitimate — divine legislation forecloses them — with the small remainder reflecting observable flight to civil-code systems in some jurisdictions. Resistance (0.35): sustained egalitarian hermeneutics, reform legislation in several states, and quiet noncompliance; the frame experiences these as error, but they are observably persistent and organized. Claimed type is mountain because the reading's own axiom is direct, timeless divine ordinance — an asserted reality-level constraint emerging outside human choice (emerges_naturally: true). Beneficiaries are declared intentionally to trigger false-summit evaluation: a 'mountain' with a jurist class, administering states, and household-head collectors collecting from it is exactly the signature the FSM machinery exists to test. The measurement series runs on one shared grid (632/900/1300/1700/1950/2026) across all three tracked metrics; base extractiveness rises monotonically from 0.06 to 0.18 as surrounding societies' norms and legal systems advance while the fixed ordinance stands still — accumulation by contextual outpacing, which should engage the T17 abductive trigger on the mountain claim. Suppression requirement rises with the maturation of enforcement infrastructure from community-scale sanction to imperial judiciary to codifying modern state.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute radically different constraints from one text. From the jurist seat, the arrangement is a completed divine legal order that the office exists to transmit; exit is identity-locked because abandoning timelessness abandons the office. From the administering-state seat, it is a legitimacy asset too entrenched to trade away cheaply. From the male-household-head seat, it is a fair exchange — authority assumed, maintenance owed — and the ledger nets positive. From the devout-affirming-women seat, it is received protection and spiritual equality, with costs accepted as apportioned. From the governed-women seat — trapped and identity-locked, with coalition power presently latent and fragmented — the same provisions are enforced asymmetry in shares, voice, and bodily-disciplinary latitude. The excluded hermeneut seat sees the interpretive closure itself as the operative injury. Nothing in the text changes across these seats; the structural relationship does. The engine derives this divergence from the authored power, exit, and directional data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (male_household_heads, religious_jurist_class, sharia_administering_states) drive low directionality for those seats: they receive shares, office, and legitimacy, and none faces exit pressure from the arrangement. The victim declaration (women_governed_by_personal_status_codes) drives near-full-target directionality, amplified by identity_locked exit — apostasy consequence and family rupture remove arbitrage-grade escape — and further scaled by global scope, which raises verification difficulty and effective extraction. Devout affirming women sit near the beneficiary pole despite bearing payer costs: consent fused with framework membership places them structurally with the subsidized, and their identity_lock reinforces rather than opposes the arrangement. Male heads carry a payer secondary role (maintenance, blood-money), but as a class they remain net receivers, so no directionality override is warranted — the derivation chain from declared roles plus exit options suffices for every seat, and no override array is authored.
 *
 * MANDATROPHY ANALYSIS:
 *   The R5 genealogy shows the founding problems — debt-record standardization and estate-dispute pacification — are today served by civil registries, probate courts, and standardized contracting that operate independently of the gendered schedule; the gender-allocation overlay persists on eternity grounds that render obsolescence conceptually inadmissible within the frame. Founding-problem status is therefore contested, and the mismatch consumer should watch the dead-times-world_rearranges cell as jurisdictions continue secularizing commercial law while retaining gendered family law. Mandatrophy_resolved is deliberately NOT declared: the reading itself denies the mandate can age. The classification apparatus guards against two symmetrical mislabels: flattening the arrangement into pure extraction would erase the genuine coordination the fixed shares performed and still perform where civil probate is absent; accepting the ordainment frame uncritically would launder the asymmetric incidence the structural data plainly record. Per-seat computation keeps both visible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_ordinance_vs_constructed_arrangement,
    'Is this arrangement a genuine natural-law order (binding regardless of enforcement, as the reading asserts) or a constructed arrangement whose persistence depends on courts, sanctions, and identifiable beneficiaries?',
    'Enforcement-withdrawal analysis: compare compliance and persistence across jurisdictions and periods where state enforcement intensity varied. An arrangement held to be divinely self-authenticating should persist where enforcement lapses; a constructed one decays or requires escalating enforcement (visible in the rising suppression_requirement series).',
    'Resolves the false-summit ambiguity carried by the mountain claim: persistence without enforcement supports the reading''s own classification; decay or enforcement-dependence supports reclassification toward tangled_rope or snare with jurist-class and household-head capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_ordinance_vs_constructed_arrangement, empirical, 'Natural-law versus constructed status of the ordinance, the FSM-required ambiguity.').

omega_variable(
    sibling_reading_epsilon_spread,
    'How large is the epsilon spread across the three readings of this kernel over the identical referent, and does this file''s low reading-indexed value reflect the literal frame''s own assessment rather than external evaluation smuggled in?',
    'Compare authored extractiveness in the contextual_egalitarian and progressive_abrogation files, which share this referent (the standing literal-hierarchical arrangement) but assess it from their own lights.',
    'A large spread confirms reading-indexing and makes cross-reading classification divergence interpretable; convergence would indicate referent slippage in one family member and corrupt the family comparison.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_epsilon_spread, conceptual, 'Cross-reading epsilon divergence over a fixed referent.').

omega_variable(
    kernel_disagreement_location,
    'Is the kernel contest located in the verses'' temporal status (timeless versus superseded), in interpretive method (plain legislative sense versus contextual purpose), or in the justice assessment of the resulting allocation?',
    'Structural comparison of the three readings'' axioms, reference frames, and drift vectors: which structural element each sibling''s foundational claims actually target.',
    'Determines which network edges propagate contamination and which sibling relations are foreclosure-grade versus rivalry-grade; mislocating the disagreement would miswire the family''s influence structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_disagreement_location, conceptual, 'Locating the axis along which sibling readings genuinely differ.').

omega_variable(
    nafaqa_compensation_symmetry,
    'Does the male maintenance obligation and blood-money liability offset the differentiated shares and testimonial discount so that lifetime transfers are roughly symmetric — the reading''s central justice defense?',
    'Household-level longitudinal economic accounting across income strata and jurisdictions; actuarial comparison of lifetime inflows and outflows by sex under the administered schedule.',
    'If transfers are uncompensated, extractiveness rises sharply even by the reading''s own lights and pulls the whole family toward tangled_rope or snare; if compensated, the low reading-indexed value stands and the contest shifts wholly to the justice-of-divine-apportionment axis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nafaqa_compensation_symmetry, empirical, 'Whether the compensating-obligation defense survives accounting.').

omega_variable(
    exit_suppression_attribution,
    'Are the high exit costs intrinsic to the ordinance (apostasy as a revealed category with sanctioned consequence) or artifacts of particular state codifications and communal enforcement?',
    'Comparative jurisdiction study correlating exit-cost variation with codification choices while holding doctrinal content constant; identify states where apostasy is uncodified and measure realized exit rates and sanction severity.',
    'If exit costs are jurisdictional rather than doctrinal, the suppression component is contingent on state adoption, lowering structural suppression and weakening any pull toward snare classification; if intrinsic, the reading''s own framework generates the trap.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_suppression_attribution, empirical, 'Attribution of exit-cost suppression between doctrine and jurisdiction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quranic_gender_verses__literal_hierarchical, 632, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t632, quranic_gender_verses__literal_hierarchical, theater_ratio, 632, 0.04).
narrative_ontology:measurement(qura_tr_t900, quranic_gender_verses__literal_hierarchical, theater_ratio, 900, 0.06).
narrative_ontology:measurement(qura_tr_t1300, quranic_gender_verses__literal_hierarchical, theater_ratio, 1300, 0.08).
narrative_ontology:measurement(qura_tr_t1700, quranic_gender_verses__literal_hierarchical, theater_ratio, 1700, 0.09).
narrative_ontology:measurement(qura_tr_t1950, quranic_gender_verses__literal_hierarchical, theater_ratio, 1950, 0.11).
narrative_ontology:measurement(qura_tr_t2026, quranic_gender_verses__literal_hierarchical, theater_ratio, 2026, 0.13).

% Extraction over time
narrative_ontology:measurement(qura_be_t632, quranic_gender_verses__literal_hierarchical, base_extractiveness, 632, 0.06).
narrative_ontology:measurement(qura_be_t900, quranic_gender_verses__literal_hierarchical, base_extractiveness, 900, 0.09).
narrative_ontology:measurement(qura_be_t1300, quranic_gender_verses__literal_hierarchical, base_extractiveness, 1300, 0.11).
narrative_ontology:measurement(qura_be_t1700, quranic_gender_verses__literal_hierarchical, base_extractiveness, 1700, 0.13).
narrative_ontology:measurement(qura_be_t1950, quranic_gender_verses__literal_hierarchical, base_extractiveness, 1950, 0.16).
narrative_ontology:measurement(qura_be_t2026, quranic_gender_verses__literal_hierarchical, base_extractiveness, 2026, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t632, quranic_gender_verses__literal_hierarchical, suppression_requirement, 632, 0.28).
narrative_ontology:measurement(qura_su_t900, quranic_gender_verses__literal_hierarchical, suppression_requirement, 900, 0.36).
narrative_ontology:measurement(qura_su_t1300, quranic_gender_verses__literal_hierarchical, suppression_requirement, 1300, 0.44).
narrative_ontology:measurement(qura_su_t1700, quranic_gender_verses__literal_hierarchical, suppression_requirement, 1700, 0.47).
narrative_ontology:measurement(qura_su_t1950, quranic_gender_verses__literal_hierarchical, suppression_requirement, 1950, 0.52).
narrative_ontology:measurement(qura_su_t2026, quranic_gender_verses__literal_hierarchical, suppression_requirement, 2026, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quranic_gender_verses__literal_hierarchical, resource_allocation).
narrative_ontology:affects_constraint(quranic_gender_verses__literal_hierarchical, quranic_gender_verses__contextual_egalitarian).
narrative_ontology:affects_constraint(quranic_gender_verses__literal_hierarchical, quranic_gender_verses__progressive_abrogation).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition note: the colloquial label 'what the Qur'an says about gender' covers three structurally distinct claims with distinct epsilons, beneficiary sets, and failure modes — (1) this file: the verses as timeless directly-binding ordinance (mountain claim, reading-indexed low epsilon over the standing literal-hierarchical referent); (2) quranic_gender_verses__contextual_egalitarian: the verses as situated progressive steps whose binding force is interpretive rather than schedular; (3) quranic_gender_verses__progressive_abrogation: the verses as a superseded stage. All three share one referent — the standing literal-hierarchical arrangement as applied — and differ in authored epsilon by reading lights, per the epsilon-invariance discipline: one reading, one constraint, one epsilon. This file is the referent-fixing member; the siblings cite it as the arrangement they contest. Edges here link the family; contamination propagates along interpretive-authority channels (seminary curricula, court appointment, codification precedent).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
