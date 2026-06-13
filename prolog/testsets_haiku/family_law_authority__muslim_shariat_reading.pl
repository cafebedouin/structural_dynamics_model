% ============================================================================
% CONSTRAINT STORY: family_law_authority__muslim_shariat_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: family_law_authority__muslim_shariat_reading
 *   human_readable: Marriage Contract Authority (Muslim Shariat Reading)
 *   domain: religious_governance/comparative_law
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the family-law-authority
 *   kernel: how marriage is defined, who controls its dissolution, what
 *   rights and obligations it creates, and under what authority. The Muslim
 *   shariat reading treats marriage as a civil contract (nikah) grounded in
 *   Quranic directives and hadith precedent. Key structural features: husband
 *   holds unilateral talaq (repudiation) right; mahr (dower) is a contractual
 *   obligation from groom to bride; polygyny (up to four wives) is permitted;
 *   women seeking divorce must petition for khul or faskh with unequal access
 *   to grounds. This reading is CLAIMED as tangled_rope: it coordinates
 *   household formation and inheritance legitimacy (coordination function)
 *   while extracting unequal dissolution rights and custody authority
 *   (asymmetric extraction toward male household heads). The foundational
 *   axiom distinguishing this reading from siblings is the contractual nature
 *   grounded in Quranic textual authority—not sacrament, not secular state
 *   contract, not dharmic samskara.
 *
 * KEY AGENTS:
 *   - male_household_heads: beneficiaries of talaq unilaterality and polygyny rights; institutional authority for household structure
 *   - women_in_marriage_contract: payers through constrained exit and identity-lock; beneficiaries through mahr and inheritance recognition
 *   - religious_scholars_and_institutions: agenda-setters interpreting shariat; control the reading and its modifications
 *   - modern_reform_movements: excluded from classical shariat adjudication; contest the reading from outside
 *   - secular_legal_systems: observers applying/modifying the constraint within state law; cannot rewrite foundational texts
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__muslim_shariat_reading, 0.58).
domain_priors:suppression_score(family_law_authority__muslim_shariat_reading, 0.62).
domain_priors:theater_ratio(family_law_authority__muslim_shariat_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__muslim_shariat_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__muslim_shariat_reading, "Marriage Contract Authority (Muslim Shariat Reading)").
narrative_ontology:topic_domain(family_law_authority__muslim_shariat_reading, "religious_governance/comparative_law").

domain_priors:requires_active_enforcement(family_law_authority__muslim_shariat_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__muslim_shariat_reading, 'd51dd69c-9444-4eea-ba99-6fd028af2ee0').
narrative_ontology:cs_kernel_codification('d51dd69c-9444-4eea-ba99-6fd028af2ee0', fixed_text).
narrative_ontology:cs_authority_grounding('d51dd69c-9444-4eea-ba99-6fd028af2ee0', lineage).
narrative_ontology:cs_interpretation_layer_present('d51dd69c-9444-4eea-ba99-6fd028af2ee0').
narrative_ontology:cs_reading_relation('d51dd69c-9444-4eea-ba99-6fd028af2ee0', family_law_authority__christian_canonical_reading, forecloses).
narrative_ontology:cs_reading_relation('d51dd69c-9444-4eea-ba99-6fd028af2ee0', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('d51dd69c-9444-4eea-ba99-6fd028af2ee0', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_reading_relation('d51dd69c-9444-4eea-ba99-6fd028af2ee0', family_law_authority__secular_contractual_reading, influences).
narrative_ontology:cs_axiom('d51dd69c-9444-4eea-ba99-6fd028af2ee0', foundational, marriage_quranic_civil_contract).
narrative_ontology:cs_axiom_status(marriage_quranic_civil_contract, holdable).
narrative_ontology:cs_axiom_grounding('d51dd69c-9444-4eea-ba99-6fd028af2ee0', marriage_quranic_civil_contract, deontological).
narrative_ontology:cs_axiom('d51dd69c-9444-4eea-ba99-6fd028af2ee0', foundational, husband_talaq_unilateral_authority).
narrative_ontology:cs_axiom_status(husband_talaq_unilateral_authority, holdable).
narrative_ontology:cs_axiom_grounding('d51dd69c-9444-4eea-ba99-6fd028af2ee0', husband_talaq_unilateral_authority, empirically_contingent).
narrative_ontology:cs_axiom('d51dd69c-9444-4eea-ba99-6fd028af2ee0', secondary, mahr_mandatory_dower_obligation).
narrative_ontology:cs_axiom_status(mahr_mandatory_dower_obligation, holdable).
narrative_ontology:cs_axiom_grounding('d51dd69c-9444-4eea-ba99-6fd028af2ee0', mahr_mandatory_dower_obligation, deontological).
narrative_ontology:cs_reference_frame('d51dd69c-9444-4eea-ba99-6fd028af2ee0', quranic_marriage_authority).
narrative_ontology:cs_drift_state('d51dd69c-9444-4eea-ba99-6fd028af2ee0', contemporary_reform_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('d51dd69c-9444-4eea-ba99-6fd028af2ee0', '').
narrative_ontology:cs_kernel_id(family_law_authority__muslim_shariat_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__muslim_shariat_reading, male_household_heads).
narrative_ontology:constraint_beneficiary(family_law_authority__muslim_shariat_reading, religious_scholars_and_institutions).
narrative_ontology:constraint_beneficiary(family_law_authority__muslim_shariat_reading, patrilineal_inheritance_beneficiaries).
narrative_ontology:constraint_victim(family_law_authority__muslim_shariat_reading, divorced_women_without_mahr_enforcement).
narrative_ontology:constraint_victim(family_law_authority__muslim_shariat_reading, women_seeking_divorce_under_khul).
narrative_ontology:constraint_victim(family_law_authority__muslim_shariat_reading, children_of_dissolved_unions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(family_law_authority__muslim_shariat_reading, women_in_marriage_contract).
narrative_ontology:constraint_victim(family_law_authority__muslim_shariat_reading, women_in_marriage_contract).
narrative_ontology:constraint_vindicates(family_law_authority__muslim_shariat_reading, quranic_authority_on_marriage).
narrative_ontology:constraint_vindicates(family_law_authority__muslim_shariat_reading, hadith_binding_precedent).
narrative_ontology:constraint_vindicates(family_law_authority__muslim_shariat_reading, shariat_household_structure).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold unilateral talaq (divorce-by-repudiation) rights recognized by classical shariat, enabling exit from marriage at will. Permitted to take up to four wives simultaneously under the framework. Control mahr negotiation as giver rather than receiver. Benefit from patrilineal inheritance recognition and household authority structures encoded in classical interpretations. Exit is available through talaq; remarriage under the same framework is permitted without restriction.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, male_household_heads, beneficiary,
    powerful, generational, arbitrage, global).

% Receive mahr (dower/gift) at marriage as a contractual right. Gain legal recognition of marriage contract, inheritance rights for children, and residence claims. Constrained in unilateral divorce access under classical shariat: must seek khul (dissolution by mutual agreement or judicial decree) or faskh (annulment for grounds like non-support). Identity-locked: self-concept and community standing are constituted through marital status within this framework. Cannot easily exit without bearing reputation cost or abandoning children under traditional custody rules.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, women_in_marriage_contract, payer,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(family_law_authority__muslim_shariat_reading, women_in_marriage_contract, beneficiary).

% Interpret and adjudicate marriage law under shariat authority. Determine what counts as valid marriage contract, conditions for talaq validity, mahr amounts, and grounds for khul or faskh. Control the interpretive tradition; newer rulings (like Saudi/Egyptian bans on triple talaq) operate within this institutional frame. Derive authority from textual tradition (Quran, hadith) and scholarly consensus (ijma). Can modify interpretations while maintaining textual fidelity.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, religious_scholars_and_institutions, agenda_setter,
    institutional, civilizational, analytical, global).

% Benefit from inheritance structures tied to marriage legitimacy and paternity recognition under shariat law. Sons inherit double the share of daughters; the marital framework determines who is recognized as legitimate heir. Cannot exit without losing inheritance rights entirely; trapped by economic dependence on family property and social structures built on inheritance.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, patrilineal_inheritance_beneficiaries, beneficiary,
    moderate, generational, trapped, global).

% Must negotiate with husband or petition courts for khul (dissolution by mutual agreement or judicial grounds). Often forced to forfeit mahr or return gifts to secure exit. Courts vary widely in granting khul; some require proof of harm (domestic violence, non-support), others require husband consent (effectively unilateral). Carry reputational cost of divorce and often lose custody of children in traditional practice. Identity is fractured by the status shift from married to divorced woman in conservative communities.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, women_seeking_divorce_under_khul, payer,
    powerless, biographical, identity_locked, global).

% Custody defaults to father after mother's remarriage or at age of discernment (~7 years for sons, older for daughters) under classical shariat. Inheritance rights depend on legitimate paternity recognized at marriage. Non-support by father is a harm but remedies are weak; mother's maintenance obligation is limited. Trapped by dependency on parental resources and legal status tied to legitimate marriage.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, children_of_dissolved_unions, payer,
    powerless, biographical, trapped, global).

% Seek equal divorce rights, gender-symmetric mahr, and mutual consent requirements. Excluded from classical shariat adjudication within traditional institutional structures; operate through parallel state courts, liberal religious reinterpretation, or secular family law alternatives. Some jurisdictions (Tunisia, Morocco post-2004) adopt reform positions while maintaining Islamic law frame. Cannot modify the shariat reading from within its own authority structure without challenging textual interpretation itself.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, modern_reform_movements, excluded,
    organized, generational, constrained, global).

% Enforce family law in majority-Muslim states; some adopt shariat family law directly, others blend shariat and civil law codes. National constitutions may guarantee equality while family law retains gender-asymmetric provisions. Can impose formal reforms (triple talaq bans) while maintaining shariat frame. Analytical position: apply and interpret; cannot rewrite foundational texts without losing religious law identity.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, secular_legal_systems, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(family_law_authority__muslim_shariat_reading, male_household_heads).
narrative_ontology:fixing_cost_class(family_law_authority__muslim_shariat_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes binding marriage contract with recognized legal consequences: mahr obligation, inheritance rights, legitimate progeny status, household residence rights, dower protections. Coordinates household formation, property transmission, and child legitimacy through a single authoritative framework drawing on Quranic and hadith sources.
% TRANSFER_FUNCTION: Transfers dower (mahr) from groom to bride as contractual obligation; transfers household authority and unilateral dissolution rights to husband under classical interpretation; transfers inheritance rights through legitimate marriage-recognized lines. For women, the contract provides legal recognition and rights; for men, it provides exit rights (talaq) and household authority. For children, it determines legitimacy, inheritance entitlement, and custody assignment (to father after age of discernment).
% ABSENT_VOICES: Divorced women whose voices on mahr enforcement are weak; women in khul disputes without judicial representation; children in custody transitions; modern reform movements within Islam who read the texts differently but lack institutional platform in classical shariat adjudication; secular female jurists advocating gender-symmetric divorce rights.
% DISAPPEARANCE_RATIONALE: If this constraint disappeared, Muslim-majority societies would not instantly reorganize—but inheritance patterns, marriage recognition, custody default rules, and property transmission tied to legitimate marriage would require radical restructuring. Alternative frameworks (secular family law, Hindu dharma law in mixed communities, civil contracts) would fill the void. The absence would force immediate legal substitution and generate succession disputes in families built on the mahr-inheritance-custody nexus.
% FOUNDING_PROBLEM: Codification of marriage as binding contract in early Islamic society: establish conditions for legitimate sexual union, property rights of spouse, inheritance of progeny, and household governance under divine guidance. Solved the problem of marital stability and property transmission in a newly formed religious community lacking centralized state enforcement.
% FOUNDING_PROBLEM_CORROBORATION: Classical Islamic jurists attest the founding problem is permanently live (marriage is divine contract, eternally binding in principle). Modern reformers attest the founding problem is partially solved and the framework's enforcement mechanisms are outdated for contemporary gender relations. Secular legal scholars and comparative law authorities attest the founding problem is historically specific and no longer operative in state-law contexts; the constraint persists as institutional inertia and religious authority preservation.
narrative_ontology:disappearance_verdict(family_law_authority__muslim_shariat_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__muslim_shariat_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__muslim_shariat_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(family_law_authority__muslim_shariat_reading, 'none', 1).

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
 *   Extractiveness rises from 0.42 to 0.58 over the interval, reflecting growing documentation of unequal divorce access, weak mahr enforcement, and custody asymmetries. The trajectory models a constraint whose coordination function (marriage recognition, inheritance legitimacy) was genuine at founding but whose extraction component is increasingly visible as alternatives (secular family law, international human-rights norms) emerge. Suppression requirement rises from 0.48 to 0.62, modeling rising enforcement cost: classical shariat family law increasingly requires active institutional defense against reform movements and secular alternatives, not mere normative acceptance. Theater ratio rises from 0.12 to 0.28, capturing the performative maintenance of classical interpretations (scholarly invocations of Quranic immutability) even as modern jurisdictions (Saudi 2019 ban on triple talaq, Egypt's khul reforms) modify enforcement rules while preserving the shariat frame. Accessibility of alternatives (secular divorce, family law courts) collapses incompletely (0.71): women can petition courts, but reputational cost and identity fusion with marital status create internalized barriers, making formal alternatives psychologically unavailable.
 *
 * PERSPECTIVAL GAP:
 *   Male household heads compute this constraint near the rope end: genuine coordination (household formation, inheritance legitimacy) with benefits they collect uncontested. Women in marriage compute it near the tangled_rope/snare boundary: coordination function (legal marriage recognition) paired with extraction (constrained exit, custody loss, mahr enforcement gaps). This seat divergence emerges directly from the structural asymmetry: unilateral talaq rights and patrilineal custody assignment are the constraint's defining features, and they operate completely differently for agents at each power level. The engine computes per-seat classification from this asymmetry; the authored claim remains independent.
 *
 * DIRECTIONALITY LOGIC:
 *   Male household heads: d near 0.0 (full beneficiary). Unilateral talaq exit, polygyny, household authority, inheritance preference—all flow to them without extraction cost within this constraint. Women: d near 0.8 (predominantly target). Mahr provides some counterweight (benefit), but constrained exit (identity-locked), khul negotiation burden, custody asymmetry, and reputation cost create net extraction. Children: d near 0.85 (target). Depend on paternity recognition and father's maintenance; custody defaults to father; mothers lose custody on remarriage. Religious scholars: d near 0.5 (symmetric). Maintain authority and interpretive privilege; also constrained by textual tradition and must defend against reform challenges. Modern reformers: excluded, not targets—they resist the constraint from outside rather than bearing its costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint satisfies tangled_rope gates: requires_active_enforcement=true (shariat courts, scholar consensus on interpretations, institutional defense against secular alternatives); beneficiaries={male_household_heads, scholars, inheritance beneficiaries} (clear coordination and extraction beneficiaries); victims={women without khul access, divorced women in mahr disputes, children in custody transitions} (identifiable targets). The founding problem (codifying marriage as binding contract in early Islamic society) is contested: classical scholars attest it remains foundationally live; modern reformers attest it was historically specific and is now institutionally inert—the constraint persists by authority preservation, not problem solving. The measured theater ratio (0.28) reflects this: performative scholarly invocations of Quranic immutability accompany practical modifications (triple talaq bans, khul reforms). This pattern—rising extraction, rising theater, rising suppression, foundation problem declared dead by external authority—is diagnostic of mandatrophy: the constraint was built for a real coordination problem, solves it still, but now extracts asymmetrically and requires active defense to persist. The constraint is NOT piton (beneficiaries do capture the extraction and maintain it deliberately); it IS tangled_rope with clear mandatrophy signal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_immutability_vs_interpretive_revision,
    'Are Quranic directives on marriage (Q4:4 on mahr, Q2:226-232 on talaq, Q33:49 on three divorces) fixed in meaning, or do they permit reinterpretation to accommodate contemporary conditions without losing shariat identity?',
    'Historical evidence: Saudi Arabia banned triple talaq (2019) and Egypt reformed khul (2000) while maintaining shariat law frame. This demonstrates textual immutability is pragmatically negotiable. Theologico-legal evidence: classical jurisprudence (Maliki school allows more khul grounds than Hanafi) shows schools differed on the same texts—meaning is not fixed. What counts as reinterpretation vs. textual violation remains theologically contested.',
    'If Quranic directives are flexible, the constraint''s extraction component (unequal talaq, weak khul) could be reformed without losing religious law identity, potentially reclassifying to rope. If immutable, the constraint remains tangled_rope with extraction locked by textual authority. This distinction maps onto the contested founding_problem_status: is the problem still live (immutability view) or is the constraint now inert (flexibility view)?',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_immutability_vs_interpretive_revision, conceptual, 'Whether Quranic family-law directives are textually fixed or interpretively flexible.').

omega_variable(
    mahr_enforcement_mechanism_collapse,
    'Is the measured mahr benefit to women real or performative? Many women in traditional practice forfeit mahr in khul negotiations or receive nominal amounts; courts often do not enforce unpaid mahr against fathers-in-law.',
    'Empirical: audit court records on mahr enforcement rates in Morocco, Egypt, Saudi Arabia, and Pakistan. Interview divorced women on actual mahr receipt. Compare written law to enforcement practice.',
    'If mahr enforcement is weak in practice (< 40% of eligible women collect awarded mahr), the constraint is more extractive than base_extractiveness=0.58 suggests—women''s counterweight benefit is illusory. This would elevate snare classification likelihood. If enforcement is strong (> 70%), the mahr mechanism genuinely coordinates property protection, supporting tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mahr_enforcement_mechanism_collapse, empirical, 'Whether mahr obligation is functionally enforceable or theatrically maintained.').

omega_variable(
    identity_lock_internalization_mechanism,
    'Women''s exit_options=identity_locked. Is this lock structural (legal barriers preventing exit without child/property loss) or internalized (women believe they SHOULD NOT exit even when legal barriers are removed)?',
    'Natural experiment: compare exit behavior (divorce rates, khul petitions) in jurisdictions with reformed family law (Tunisia''s 1956 equality; Morocco post-2004) vs. classical shariat enforcement. If divorce rates spike post-reform, lock is structural. If rates remain low, lock is internalized identity fusion with marital status.',
    'If internalized, suppression is higher than measured—the constraint carries its enforcement apparatus inside targets'' self-concepts, not merely in courts and enforcement. Post-exit suppression persistence (continued identity as ''divorced woman'' with lowered remarriage prospects) would confirm internalization. This elevates extraction severity on the target seats and suggests snare characteristics despite tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_internalization_mechanism, empirical, 'Whether women''s constrained exit is structural or internalized.').

omega_variable(
    reform_movements_as_excluded_or_internal_schism,
    'Are modern reform movements (advocating equal talaq, gender-symmetric mahr, mutual consent requirement) excluded from shariat authority, or are they an internal schism within Islamic jurisprudential tradition?',
    'Theological evidence: do reformers claim shariat authority (reinterpreting hadith, citing minority school precedents, invoking maqasid al-sharia—objectives of sharia) or reject shariat frame entirely? Institutional evidence: are reformers seated in traditional madrasas/scholar councils or operating through secular courts and international human-rights bodies?',
    'If excluded-from-outside, the constraint''s institutional architecture is uncontested within its own frame—resistance is exogenous. If internal-schism, the constraint faces legitimacy pressure from within the tradition, which may accelerate future interpretive drift and reduce theater requirement. Classification remains tangled_rope either way, but the path to mandatrophy resolution diverges.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reform_movements_as_excluded_or_internal_schism, conceptual, 'Whether family-law reform movements are excluded from shariat authority or represent internal jurisprudential dissent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__muslim_shariat_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t0, family_law_authority__muslim_shariat_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(fami_tr_t5, family_law_authority__muslim_shariat_reading, theater_ratio, 5, 0.16).
narrative_ontology:measurement(fami_tr_t10, family_law_authority__muslim_shariat_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(fami_tr_t15, family_law_authority__muslim_shariat_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement(fami_tr_t20, family_law_authority__muslim_shariat_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement(fami_tr_t25, family_law_authority__muslim_shariat_reading, theater_ratio, 25, 0.28).

% Extraction over time
narrative_ontology:measurement(fami_be_t0, family_law_authority__muslim_shariat_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(fami_be_t5, family_law_authority__muslim_shariat_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(fami_be_t10, family_law_authority__muslim_shariat_reading, base_extractiveness, 10, 0.53).
narrative_ontology:measurement(fami_be_t15, family_law_authority__muslim_shariat_reading, base_extractiveness, 15, 0.56).
narrative_ontology:measurement(fami_be_t20, family_law_authority__muslim_shariat_reading, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(fami_be_t25, family_law_authority__muslim_shariat_reading, base_extractiveness, 25, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t0, family_law_authority__muslim_shariat_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(fami_su_t5, family_law_authority__muslim_shariat_reading, suppression_requirement, 5, 0.53).
narrative_ontology:measurement(fami_su_t10, family_law_authority__muslim_shariat_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(fami_su_t15, family_law_authority__muslim_shariat_reading, suppression_requirement, 15, 0.61).
narrative_ontology:measurement(fami_su_t20, family_law_authority__muslim_shariat_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(fami_su_t25, family_law_authority__muslim_shariat_reading, suppression_requirement, 25, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__muslim_shariat_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(family_law_authority__muslim_shariat_reading, 0.12).
narrative_ontology:affects_constraint(family_law_authority__muslim_shariat_reading, family_law_authority__christian_canonical_reading).
narrative_ontology:affects_constraint(family_law_authority__muslim_shariat_reading, family_law_authority__hindu_dharmashastra_reading).
narrative_ontology:affects_constraint(family_law_authority__muslim_shariat_reading, family_law_authority__parsi_zoroastrian_reading).
narrative_ontology:affects_constraint(family_law_authority__muslim_shariat_reading, family_law_authority__secular_contractual_reading).
narrative_ontology:affects_constraint(family_law_authority__muslim_shariat_reading, gender_equity_norm__islamic_societies).
narrative_ontology:affects_constraint(family_law_authority__muslim_shariat_reading, inheritance_legitimacy__patrilineal_system).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the family-law-authority kernel. All five readings (Christian canonical, Hindu dharmashastra, Muslim shariat, Parsi Zoroastrian, secular contractual) are distinct constraints with different ε values, beneficiaries, victims, and enforcement mechanisms. They coexist and compete in multi-religious societies. The ε-invariance principle requires separate stories because measuring 'marriage' under different authority frames yields incompatible extraction values: Christian sacrament reading has near-zero unilateral dissolution (ε very low); Muslim shariat reading has unequal talaq (ε moderate-high); secular contractual reading has symmetric divorce (ε very low). The readings are not observations of one constraint—they are structurally distinct constraints instantiated by the same kernel under different interpretive lenses. Each reading's sibling links are documented in the reading_relations array of its cs_structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(family_law_authority__muslim_shariat_reading, institutional, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
