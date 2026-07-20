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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Muslim Shariat Nikah Reading of Family Law Authority
 *   domain: comparative_law/religious_governance
 *
 * SUMMARY:
 *   This constraint story instantiates the muslim_shariat_reading of the
 *   contested family_law_authority kernel. It treats marriage as a civil
 *   contract (nikah) governed by Quranic injunctions and hadith, distinct
 *   from sacramental or purely secular readings. The constraint operates
 *   through state-recognized personal law systems (notably in India) where
 *   shariat-derived rules on nikah, mahr, talaq, and polygyny are enforced
 *   for Muslim citizens. The reading is characterized by contractual
 *   dissolution mechanisms, permitted polygyny, the mahr obligation, and
 *   historically gender-asymmetric divorce access (including the now-banned
 *   instant triple talaq). It coordinates genuine community
 *   needsâlegitimacy, inheritance, lineageâwhile structurally extracting
 *   from female contracting parties through asymmetric exit rights and
 *   polygyny exposure.
 *
 * KEY AGENTS:
 *   - shariat_interpreters: Agenda-setter (institutional/constrained) â derive authority from textual lineage and administer the nikah framework through personal law boards.
 *   - male_contracting_parties: Primary beneficiary (moderate/constrained) â receive asymmetric divorce access and polygyny permission.
 *   - female_contracting_parties: Primary target (powerless/identity_locked) â bear asymmetric barriers to dissolution and polygyny risk.
 *   - secular_state_judiciary: Analytical observer (institutional/analytical) â enforces constitutional limits on personal law.
 *   - muslim_womens_rights_collectives: Excluded voice (organized/mobile) â demand reform but are structurally excluded from shariat interpretive bodies.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__muslim_shariat_reading, 0.63).
domain_priors:suppression_score(family_law_authority__muslim_shariat_reading, 0.58).
domain_priors:theater_ratio(family_law_authority__muslim_shariat_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, extractiveness, 0.63).
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__muslim_shariat_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__muslim_shariat_reading, "Muslim Shariat Nikah Reading of Family Law Authority").
narrative_ontology:topic_domain(family_law_authority__muslim_shariat_reading, "comparative_law/religious_governance").

domain_priors:requires_active_enforcement(family_law_authority__muslim_shariat_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__muslim_shariat_reading, '8f4a1c31-1890-4820-bce4-abd3b6daee1d').
narrative_ontology:cs_kernel_codification('8f4a1c31-1890-4820-bce4-abd3b6daee1d', fixed_text).
narrative_ontology:cs_authority_grounding('8f4a1c31-1890-4820-bce4-abd3b6daee1d', lineage).
narrative_ontology:cs_interpretation_layer_present('8f4a1c31-1890-4820-bce4-abd3b6daee1d').
narrative_ontology:cs_reading_relation('8f4a1c31-1890-4820-bce4-abd3b6daee1d', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('8f4a1c31-1890-4820-bce4-abd3b6daee1d', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('8f4a1c31-1890-4820-bce4-abd3b6daee1d', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_reading_relation('8f4a1c31-1890-4820-bce4-abd3b6daee1d', family_law_authority__secular_contractual_reading, influences).
narrative_ontology:cs_axiom('8f4a1c31-1890-4820-bce4-abd3b6daee1d', foundational, nikah_as_quranic_contract).
narrative_ontology:cs_axiom_status(nikah_as_quranic_contract, holdable).
narrative_ontology:cs_axiom_grounding('8f4a1c31-1890-4820-bce4-abd3b6daee1d', nikah_as_quranic_contract, theological).
narrative_ontology:cs_axiom('8f4a1c31-1890-4820-bce4-abd3b6daee1d', foundational, male_unilateral_dissolution_right).
narrative_ontology:cs_axiom_status(male_unilateral_dissolution_right, holdable).
narrative_ontology:cs_axiom_grounding('8f4a1c31-1890-4820-bce4-abd3b6daee1d', male_unilateral_dissolution_right, theological).
narrative_ontology:cs_reference_frame('8f4a1c31-1890-4820-bce4-abd3b6daee1d', classical_fiqh_nikah_framework).
narrative_ontology:cs_drift_state('8f4a1c31-1890-4820-bce4-abd3b6daee1d', post_statutory_reform_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8f4a1c31-1890-4820-bce4-abd3b6daee1d', '').
narrative_ontology:cs_kernel_id(family_law_authority__muslim_shariat_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__muslim_shariat_reading, male_contracting_parties).
narrative_ontology:constraint_beneficiary(family_law_authority__muslim_shariat_reading, shariat_interpreters).
narrative_ontology:constraint_victim(family_law_authority__muslim_shariat_reading, female_contracting_parties).
narrative_ontology:constraint_vindicates(family_law_authority__muslim_shariat_reading, quranic_supremacy_in_family_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Derive and administer rules of nikah, talaq, and mahr from Quranic text and hadith; staff personal law boards and qazi courts; their institutional authority depends on maintaining the interpretive monopoly over Muslim family law.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, shariat_interpreters, agenda_setter,
    institutional, generational, constrained, national).

% Enter nikah with structural advantages including permitted polygyny, lower barriers to dissolution through various forms of talaq, and patriarchal contractual authority; pay mahr but retain superior bargaining position in marital exit.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, male_contracting_parties, beneficiary,
    moderate, biographical, constrained, national).

% Enter nikah with right to mahr but face asymmetric barriers to initiating divorce, exposure to unilateral dissolution and polygyny, and heavy social cost for exiting through secular channels; religious identity fuses with familial honor making exit cognitively costly.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, female_contracting_parties, payer,
    powerless, biographical, identity_locked, national).

% Recognize nikah under personal law statutes and adjudicate conflicts with constitutional rights; occasionally override specific practices (e.g., triple talaq ban) while preserving the broader shariat-governed framework.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, secular_state_judiciary, observer,
    institutional, generational, analytical, national).

% Advocate for gender-symmetric reform from within Islamic feminist frameworks; structurally excluded from official personal law board deliberations which are male-dominated; their reformist readings are treated as illegitimate by traditionalist agenda-setters.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, muslim_womens_rights_collectives, excluded,
    organized, generational, mobile, national).

narrative_ontology:fixing_cost_class(family_law_authority__muslim_shariat_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a religio-legal framework for legitimate marriage, inheritance, and lineage within the Muslim community, resolving coordination problems around property transfer, child legitimacy, and communal boundaries through a contractual model grounded in Quranic text and Prophetic practice.
% TRANSFER_FUNCTION: Transfers authority over marriage formation and dissolution to male contracting parties and shariat interpreters; transfers obligation of mahr from male to female party; transfers structural vulnerability in divorce access and polygyny risk from male to female contracting parties.
% ABSENT_VOICES: Muslim women's collectives and gender-equality advocates are structurally underrepresented in shariat interpretation bodies; secular feminists and constitutional lawyers are often framed as external interfering parties rather than legitimate stakeholders.
% DISAPPEARANCE_RATIONALE: If the shariat-governed nikah framework vanished, Muslim family formation would lose its recognized religio-legal structure; inheritance, legitimacy, and community boundary mechanisms would require alternative adjudication, and the authority of ulema and personal law boards would collapse.
% FOUNDING_PROBLEM: How to organize marriage, lineage, and property transmission for Muslims in accordance with Quranic revelation and Prophetic practice in the absence of a centralized church or modern state codification.
% FOUNDING_PROBLEM_CORROBORATION: Shariat interpreters attest the problem is live. Muslim women's rights collectives and the secular judiciary attest that the founding problem has been substantially modified by modern constitutional and human rights frameworks; the Supreme Court of India and the Law Commission of India have documented this tension from outside the benefiting parties.
narrative_ontology:disappearance_verdict(family_law_authority__muslim_shariat_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__muslim_shariat_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__muslim_shariat_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(family_law_authority__muslim_shariat_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__muslim_shariat_reading, 0.63, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.63) is moderately high because the constraint systematically transfers dissolution authority and polygyny privilege to male parties, though bounded by mahr and some procedural protections. Suppression (0.58) reflects state enforcement of personal law plus community identity pressure, tempered by the existence of secular marital exit routes (Special Marriage Act) that are socially costly. Theater ratio (0.45) has risen as traditionalist authorities performatively defend the shariat framework against constitutional reform pressures, particularly after the triple talaq ban. Accessibility collapse (0.48) is moderate: secular alternatives exist but are identity-costly. Resistance (0.62) is substantial and growing, driven by Muslim women's movements and periodic judicial intervention.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (shariat interpreters) experiences the constraint as a sacred coordination mechanism preserving communal identity; the payer seat (female contracting parties) experiences it as a structurally asymmetric arrangement that constrains exit and exposes them to unilateral dissolution. The secular judiciary sits analytically outside both, registering the tension but rarely dissolving the broader framework. The male beneficiary seat experiences moderate subsidy in divorce rights but does not perceive extraction because the cost is borne asymmetrically by the female party.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low directionality: male_contracting_parties receive structural subsidies in dissolution and polygyny (low d), while shariat_interpreters collect institutional authority (low d). Victim declaration maps to high directionality: female_contracting_parties bear the extraction through identity_locked exit and asymmetric divorce barriers (high d). The secular judiciary and excluded collectives occupy analytical or excluded positions outside the primary directionality derivation chain.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled rope prevents mislabeling the constraint as either a pure coordination rope (which would ignore the gender-asymmetric extraction) or a pure snare (which would deny the genuine community-coordinating function of nikah in organizing lineage, inheritance, and legitimate membership). The mandate has partially atrophiedâstate reforms have overridden instant triple talaqâyet the broader asymmetric framework persists, indicating it is not yet a piton because concentrated beneficiaries (male parties and interpreters) still actively maintain it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the shariat-governed nikah reading of family law authority a genuinely distinct constraint from its secular contractual sibling, or do they converge on the same civil-contract structure with different justification layers?',
    'Comparative analysis of dissolution mechanics, polygyny rules, and mahr enforcement across jurisdictions that recognize both frames.',
    'If the structures converge, the extraction profile is driven by the state-enforcement layer rather than the theological kernel; if they remain distinct, the Quranic grounding independently shapes the beneficiary-victim structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Structural distinctness of shariat reading within the family law authority kernel.').

omega_variable(
    suppression_internalized_vs_structural,
    'Is the constraint on female contracting parties'' exit structural (state-enforced personal law, economic dependency) or internalized (religious identity fusion making secular exit unthinkable)?',
    'Post-exit trajectory study: do women who opt for secular marriage under the Special Marriage Act report persistent suppression or identity conflict?',
    'If internalized, effective suppression exceeds structural measure; if purely structural, reform of state recognition alone may suffice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalized_vs_structural, empirical, 'Internalized vs structural suppression mechanism for female parties.').

omega_variable(
    post_ban_extraction_trajectory,
    'Has the 2019 statutory ban on instant triple talaq substantively reduced the constraint''s extraction, or has asymmetric dissolution power migrated into informal or community-level channels?',
    'Empirical monitoring of talaq pronouncement patterns, khula negotiation outcomes, and domestic dispute mediation records in Muslim communities post-2019.',
    'If extraction migrated rather than declined, the statutory reform addressed theater without altering the underlying tangled rope; if declined, the reform successfully reduced base extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_ban_extraction_trajectory, empirical, 'Whether triple talaq ban reduced extraction or displaced it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__muslim_shariat_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(muslim_shariat_tr_t0, family_law_authority__muslim_shariat_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(muslim_shariat_tr_t15, family_law_authority__muslim_shariat_reading, theater_ratio, 15, 0.28).
narrative_ontology:measurement(muslim_shariat_tr_t30, family_law_authority__muslim_shariat_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement(muslim_shariat_tr_t45, family_law_authority__muslim_shariat_reading, theater_ratio, 45, 0.35).
narrative_ontology:measurement(muslim_shariat_tr_t60, family_law_authority__muslim_shariat_reading, theater_ratio, 60, 0.42).
narrative_ontology:measurement(muslim_shariat_tr_t70, family_law_authority__muslim_shariat_reading, theater_ratio, 70, 0.45).

% Extraction over time
narrative_ontology:measurement(muslim_shariat_be_t0, family_law_authority__muslim_shariat_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(muslim_shariat_be_t15, family_law_authority__muslim_shariat_reading, base_extractiveness, 15, 0.6).
narrative_ontology:measurement(muslim_shariat_be_t30, family_law_authority__muslim_shariat_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(muslim_shariat_be_t45, family_law_authority__muslim_shariat_reading, base_extractiveness, 45, 0.64).
narrative_ontology:measurement(muslim_shariat_be_t60, family_law_authority__muslim_shariat_reading, base_extractiveness, 60, 0.65).
narrative_ontology:measurement(muslim_shariat_be_t70, family_law_authority__muslim_shariat_reading, base_extractiveness, 70, 0.63).

% Suppression requirement over time
narrative_ontology:measurement(muslim_shariat_su_t0, family_law_authority__muslim_shariat_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(muslim_shariat_su_t15, family_law_authority__muslim_shariat_reading, suppression_requirement, 15, 0.5).
narrative_ontology:measurement(muslim_shariat_su_t30, family_law_authority__muslim_shariat_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement(muslim_shariat_su_t45, family_law_authority__muslim_shariat_reading, suppression_requirement, 45, 0.55).
narrative_ontology:measurement(muslim_shariat_su_t60, family_law_authority__muslim_shariat_reading, suppression_requirement, 60, 0.58).
narrative_ontology:measurement(muslim_shariat_su_t70, family_law_authority__muslim_shariat_reading, suppression_requirement, 70, 0.56).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__muslim_shariat_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
