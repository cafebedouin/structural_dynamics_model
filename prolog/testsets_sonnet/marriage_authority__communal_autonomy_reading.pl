% ============================================================================
% CONSTRAINT STORY: marriage_authority__communal_autonomy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__communal_autonomy_reading, []).

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
 *   constraint_id: marriage_authority__communal_autonomy_reading
 *   human_readable: Communal Autonomy Reading of Marriage Authority (Personal Law Pluralism)
 *   domain: legal_pluralism/constitutional_law/family_law
 *
 * SUMMARY:
 *   This story instantiates the communal autonomy reading of the marriage
 *   authority kernel: the claim that legitimate authority over marriage,
 *   divorce, and inheritance rules belongs to each religious community's own
 *   tradition, with the state's role limited to enforcing (registering,
 *   adjudicating, executing) community-determined outcomes rather than
 *   authoring family-law content itself. Under this reading, legislative
 *   amendment to a community's family code is treated as illegitimate — or at
 *   least politically untenable — without the consent of that community's
 *   religious leadership. This produces genuine coordination value
 *   (communities avoid majoritarian imposition of an alien family code) but
 *   also produces asymmetric extraction: intra-community dissenters, and
 *   especially women disadvantaged by unequal divorce/maintenance norms, bear
 *   costs that persist precisely because reform requires the consent of the
 *   same leadership that benefits from the status quo. This is a Tangled
 *   Rope, not a Rope: the coordination function is real, but so is the
 *   asymmetric extraction, and it requires active state enforcement
 *   (recognizing religious tribunal rulings, registering
 *   religiously-sanctioned marriages as legally binding) to persist.
 *
 * KEY AGENTS:
 *   - religious_leadership: primary agenda-setter and beneficiary — administers norms and gatekeeps amendment
 *   - intra_community_dissenters and women_seeking_exit_from_personal_law_marriages: primary targets — bear costs of norms they cannot unilaterally change
 *   - the_state: enforces outcomes without authoring content — structurally distinct from religious leadership despite sharing agenda_setter role
 *   - gender_equality_litigants: excluded voice — treated as external interference under this reading's own logic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__communal_autonomy_reading, 0.42).
domain_priors:suppression_score(marriage_authority__communal_autonomy_reading, 0.55).
domain_priors:theater_ratio(marriage_authority__communal_autonomy_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__communal_autonomy_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority__communal_autonomy_reading, "Communal Autonomy Reading of Marriage Authority (Personal Law Pluralism)").
narrative_ontology:topic_domain(marriage_authority__communal_autonomy_reading, "legal_pluralism/constitutional_law/family_law").

domain_priors:requires_active_enforcement(marriage_authority__communal_autonomy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__communal_autonomy_reading, '483e0ffb-7539-4212-8cc1-0daade66bd61').
narrative_ontology:cs_kernel_codification('483e0ffb-7539-4212-8cc1-0daade66bd61', distributed).
narrative_ontology:cs_authority_grounding('483e0ffb-7539-4212-8cc1-0daade66bd61', lineage).
narrative_ontology:cs_interpretation_layer_present('483e0ffb-7539-4212-8cc1-0daade66bd61').
narrative_ontology:cs_reading_relation('483e0ffb-7539-4212-8cc1-0daade66bd61', marriage_authority__secularist_reading, forecloses).
narrative_ontology:cs_reading_relation('483e0ffb-7539-4212-8cc1-0daade66bd61', marriage_authority__gender_rights_reading, influences).
narrative_ontology:cs_reading_relation('483e0ffb-7539-4212-8cc1-0daade66bd61', marriage_authority__federalist_millet_reading, coexists_with).
narrative_ontology:cs_reading_relation('483e0ffb-7539-4212-8cc1-0daade66bd61', marriage_authority__judicial_harmonization_reading, influences).
narrative_ontology:cs_axiom('483e0ffb-7539-4212-8cc1-0daade66bd61', foundational, communal_religious_tradition_is_legitimate_family_law_source).
narrative_ontology:cs_axiom_status(communal_religious_tradition_is_legitimate_family_law_source, holdable).
narrative_ontology:cs_axiom_grounding('483e0ffb-7539-4212-8cc1-0daade66bd61', communal_religious_tradition_is_legitimate_family_law_source, conventional).
narrative_ontology:cs_axiom('483e0ffb-7539-4212-8cc1-0daade66bd61', secondary, amendment_requires_community_consent).
narrative_ontology:cs_axiom_status(amendment_requires_community_consent, holdable).
narrative_ontology:cs_axiom_grounding('483e0ffb-7539-4212-8cc1-0daade66bd61', amendment_requires_community_consent, conventional).
narrative_ontology:cs_reference_frame('483e0ffb-7539-4212-8cc1-0daade66bd61', communal_religious_self_governance).
narrative_ontology:cs_drift_state('483e0ffb-7539-4212-8cc1-0daade66bd61', contemporary_constitutional_equality_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('483e0ffb-7539-4212-8cc1-0daade66bd61', '').
narrative_ontology:cs_kernel_id(marriage_authority__communal_autonomy_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__communal_autonomy_reading, religious_leadership).
narrative_ontology:constraint_beneficiary(marriage_authority__communal_autonomy_reading, community_institutional_continuity).
narrative_ontology:constraint_victim(marriage_authority__communal_autonomy_reading, intra_community_dissenters).
narrative_ontology:constraint_victim(marriage_authority__communal_autonomy_reading, women_seeking_exit_from_personal_law_marriages).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_authority__communal_autonomy_reading, community_majority_members).
narrative_ontology:constraint_vindicates(marriage_authority__communal_autonomy_reading, communal_self_governance_doctrine).
narrative_ontology:constraint_vindicates(marriage_authority__communal_autonomy_reading, religious_freedom_of_association).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and administers the community's marriage, divorce, and inheritance norms; certifies which unions and dissolutions are religiously valid. Negotiates directly with the state over the boundary of personal law jurisdiction, and any legislative amendment to the community's family code requires its sign-off before it will be treated as legitimate within the community. Retains authority and social standing precisely because the state defers to it rather than legislating family law itself.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, religious_leadership, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__communal_autonomy_reading, religious_leadership, beneficiary).

% The abstract interest in the community's family-law tradition persisting intact across generations; not an actor itself but the value religious leadership and orthodox community members invoke to justify resisting external amendment.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, community_institutional_continuity, beneficiary,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(marriage_authority__communal_autonomy_reading, community_institutional_continuity).

% Community members who want a different marriage, divorce, or inheritance arrangement than the community's religious authority recognizes. Exiting the personal law system means exiting recognized communal identity — losing standing at ceremonies, inheritance rights, and family relationships — so most stay and comply even where the norms harm them. Legislative reform is blocked unless religious leadership consents, which routes their grievance through the very authority they are dissenting from.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, intra_community_dissenters, payer,
    moderate, biographical, constrained, regional).

% Women whose divorce, maintenance, or custody outcomes are governed by the community's religious family code rather than a uniform civil standard. Where that code disadvantages them relative to civil law (e.g., unilateral divorce provisions, unequal maintenance), their only avenues are religious tribunals controlled by the same leadership that authored the disadvantaging norm, or years-long constitutional litigation with uncertain and partial results.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, women_seeking_exit_from_personal_law_marriages, payer,
    powerless, biographical, trapped, local).

% Enforces personal law rulings through its courts and civil registries — recognizing religious marriages and divorces for legal purposes — but has historically declined to legislate a uniform family code, treating communal religious authority as the legitimate source of family-law content. Retains formal power to override but exercises it only rarely, given the political cost of appearing to intrude on religious self-governance.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, the_state, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__communal_autonomy_reading, the_state, observer).

% Community members whose marriages and family arrangements align comfortably with the traditional norms. They experience the arrangement as functioning coordination — a stable, familiar, community-endorsed framework for family life that does not require them to interact with an unfamiliar civil system.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, community_majority_members, beneficiary,
    moderate, generational, mobile, regional).

% Constitutional lawyers and activists who argue this reading's deference to communal consent for amendment insulates gender-discriminatory norms from correction. Under the communal autonomy reading their claims are treated as external interference in religious self-governance rather than legitimate objections from within, and their preferred forum (judicial constitutional review) is precisely what this reading is structured to minimize.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, gender_equality_litigants, excluded,
    moderate, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority__communal_autonomy_reading, religious_leadership).
narrative_ontology:fixing_cost_class(marriage_authority__communal_autonomy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows each religious community to maintain a coherent, self-administered family-law tradition — marriage, divorce, inheritance rules consistent with its own doctrine — without a single legislature imposing one uniform code on communities with genuinely different theological commitments about marriage.
% TRANSFER_FUNCTION: Moves interpretive and amendment authority over family law from the state legislature to community religious leadership; moves the cost of norm-conformity from the community's institutional continuity onto individual dissenters and disadvantaged members (especially women) within each community, who bear the disadvantaging norms until the community itself consents to change them.
% ABSENT_VOICES: Intra-community reformers and gender-equality litigants would object that requiring community consent for amendment makes internal reform structurally impossible when the beneficiaries of the status quo are the same body whose consent is required. They are formally free to litigate but are treated, within this reading's own logic, as raising a matter outside legitimate state jurisdiction.
% DISAPPEARANCE_RATIONALE: If communal consent authority disappeared overnight, the state would either legislate a uniform civil family code directly or hand full adjudicative authority to ordinary courts applying general equality law — either way, communities would lose the ability to maintain distinct marriage/divorce norms, religious leadership would lose a major source of institutional relevance, and disadvantaged intra-community members would gain a route to reform that does not require the consent of those benefiting from the current norms.
% FOUNDING_PROBLEM: In a religiously plural polity, a single legislated marriage code risks either majoritarian imposition of one religion's family norms on others, or protracted conflict over whose theology the state should encode; deferring family-law content to each community's own tradition was adopted to avoid both a legitimacy crisis over state secularism and a communal backlash against perceived cultural erasure.
% FOUNDING_PROBLEM_CORROBORATION: Religious leadership and community-continuity advocates attest the founding problem remains live — a uniform code would still be experienced as majoritarian imposition. Independent constitutional scholars, gender-rights litigators, and dissenting community members attest the arrangement now functions primarily to insulate discriminatory internal norms from reform, citing decades of stalled amendment where community leadership never consents to changes it would itself be required to relinquish authority under.
narrative_ontology:disappearance_verdict(marriage_authority__communal_autonomy_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__communal_autonomy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__communal_autonomy_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_authority__communal_autonomy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__communal_autonomy_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__communal_autonomy_reading_tests).
:- end_tests(marriage_authority__communal_autonomy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) is moderate rather than severe: most community members experience the arrangement as functioning coordination, and the extraction is concentrated on a subset (dissenters, disadvantaged women) rather than diffused across the whole community. Suppression (0.55) is meaningfully above the extraction level because exit from the personal law system carries an identity cost (loss of communal standing, inheritance rights, family relationships) independent of whether any specific norm is unjust — this is the suppression mechanism that keeps dissenters compliant even when they object. Theater ratio is low (0.2) because the coordination function is genuinely operative, not merely performed — communities really do maintain distinct, functioning family-law traditions, this is not a vestigial arrangement.
 *
 * PERSPECTIVAL GAP:
 *   From religious leadership's seat, this is functioning coordination — a community maintaining doctrinal integrity against external imposition. From the seat of a woman seeking exit from a maintenance-disadvantaged marriage, the same structure is an enforced barrier: the state lends its coercive machinery (civil registration, court recognition) to outcomes she cannot appeal without either abandoning her communal identity or awaiting a consent that structurally will not come from the party asked to give it up. The engine should compute these divergently from the same structural facts — that divergence is the finding, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious leadership sits near the beneficiary end: it authors and interprets the norms, gatekeeps amendment, and derives institutional standing from being the deferred-to authority. Community majority members are moderate beneficiaries — genuine coordination value, mobile in the sense that the arrangement suits their actual family situation. Intra-community dissenters and disadvantaged women sit near the target end: trapped or constrained exit (leaving means losing communal identity and material entitlements), and the amendment-consent requirement means their grievance is structurally routed back through the party benefiting from the status quo. The state occupies an unusual dual position — institutional power, but its exit options are themselves constrained by the political cost of appearing to override religious self-governance, which is why it enforces rather than authors.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (avoiding majoritarian imposition of one community's family norms on others) remains partially live — genuine religious plurality still exists and a single legislated code would still raise legitimate concern. But the amendment-consent mechanism has drifted from 'protecting communities from external imposition' to 'protecting incumbent religious leadership from internal reform,' since the consent gate blocks correction of internally-contested norms as readily as it blocks externally-imposed ones. This is not full mandatrophy (the coordination function is not dead) but it is a live site of the tangled-rope structure: coordination and extraction riding the same mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    communal_consent_authenticity,
    'When ''religious leadership'' consents or withholds consent to a family-law amendment, does that consent authentically represent the community''s considered view, or does it represent the preferences of an unelected, self-perpetuating leadership stratum insulated from the community members most affected by the norm in question?',
    'Compare amendment outcomes reached through community leadership consent processes against outcomes from internal community polling or representative consultation mechanisms where they exist; track whether leadership positions on contested norms diverge systematically from the preferences of affected subgroups (especially women).',
    'If consent is authentically representative, the coordination framing is stronger and closer to a genuine Rope; if consent tracks leadership self-interest rather than community preference, the tangled_rope classification understates the extraction and a snare classification becomes more defensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(communal_consent_authenticity, empirical, 'Whether religious leadership consent authentically represents community preference or leadership self-interest.').

omega_variable(
    kernel_reading_selection_basis,
    'Is the communal_autonomy_reading the historically dominant framing because it best describes how personal law actually functions, or because it is the framing most favorable to the institutional actors (religious leadership, state actors avoiding political cost) who have had the most influence over how the kernel is publicly characterized?',
    'Compare the communal_autonomy_reading''s account of state deference against the judicial_harmonization_reading''s account of accumulating case-by-case constitutional intervention; if courts have in fact been imposing a constitutional floor more aggressively than the communal_autonomy_reading acknowledges, that reading may be descriptively stale rather than currently accurate.',
    'If the judicial_harmonization_reading better describes current practice, this story''s premise (state enforces but does not author) may already be obsolete, and the tangled_rope classification here describes a fading rather than a stable equilibrium.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_basis, conceptual, 'Whether this reading''s framing of pure state deference remains descriptively accurate given judicial trends.').

omega_variable(
    exit_cost_measurement,
    'How severe, in practice, is the identity/material cost of exiting the personal law system for a dissenting community member — is ''trapped''/''constrained'' an accurate characterization or an overstatement given increasing intercommunity mobility and civil-law alternatives in some jurisdictions?',
    'Empirical tracking of actual exit rates, and the material/social outcomes for those who do exit, across communities and over time.',
    'If exit costs are lower than assumed, suppression is overstated and the classification shifts toward rope; if exit costs are as severe as characterized or worse, the tangled_rope/snare boundary should be revisited.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exit_cost_measurement, empirical, 'Whether the trapped/constrained exit characterization for dissenters is empirically accurate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__communal_autonomy_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority__communal_autonomy_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(marr_tr_t14, marriage_authority__communal_autonomy_reading, theater_ratio, 14, 0.14).
narrative_ontology:measurement(marr_tr_t28, marriage_authority__communal_autonomy_reading, theater_ratio, 28, 0.16).
narrative_ontology:measurement(marr_tr_t42, marriage_authority__communal_autonomy_reading, theater_ratio, 42, 0.18).
narrative_ontology:measurement(marr_tr_t56, marriage_authority__communal_autonomy_reading, theater_ratio, 56, 0.19).
narrative_ontology:measurement(marr_tr_t70, marriage_authority__communal_autonomy_reading, theater_ratio, 70, 0.2).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority__communal_autonomy_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(marr_be_t14, marriage_authority__communal_autonomy_reading, base_extractiveness, 14, 0.33).
narrative_ontology:measurement(marr_be_t28, marriage_authority__communal_autonomy_reading, base_extractiveness, 28, 0.36).
narrative_ontology:measurement(marr_be_t42, marriage_authority__communal_autonomy_reading, base_extractiveness, 42, 0.39).
narrative_ontology:measurement(marr_be_t56, marriage_authority__communal_autonomy_reading, base_extractiveness, 56, 0.41).
narrative_ontology:measurement(marr_be_t70, marriage_authority__communal_autonomy_reading, base_extractiveness, 70, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority__communal_autonomy_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(marr_su_t14, marriage_authority__communal_autonomy_reading, suppression_requirement, 14, 0.44).
narrative_ontology:measurement(marr_su_t28, marriage_authority__communal_autonomy_reading, suppression_requirement, 28, 0.47).
narrative_ontology:measurement(marr_su_t42, marriage_authority__communal_autonomy_reading, suppression_requirement, 42, 0.5).
narrative_ontology:measurement(marr_su_t56, marriage_authority__communal_autonomy_reading, suppression_requirement, 56, 0.53).
narrative_ontology:measurement(marr_su_t70, marriage_authority__communal_autonomy_reading, suppression_requirement, 70, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__communal_autonomy_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_authority__communal_autonomy_reading, 0.1).
narrative_ontology:affects_constraint(marriage_authority__communal_autonomy_reading, secularist_reading).
narrative_ontology:affects_constraint(marriage_authority__communal_autonomy_reading, gender_rights_reading).
narrative_ontology:affects_constraint(marriage_authority__communal_autonomy_reading, federalist_millet_reading).
narrative_ontology:affects_constraint(marriage_authority__communal_autonomy_reading, judicial_harmonization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of five sibling readings of the marriage_authority kernel, each instantiating a structurally distinct claim about where legitimate authority over family law resides and how it should evolve. communal_autonomy_reading (this story) treats communal religious tradition as the legitimate source with state-enforcement-only; secularist_reading treats the arrangement as a transitional anomaly awaiting legislative unification; gender_rights_reading treats internal gender inequality as grounds for judicial equality-based reform; federalist_millet_reading treats the same fragmentation as a deliberate anti-majoritarian mechanism; judicial_harmonization_reading treats a constitutional floor as already emerging case-by-case. Each carries its own ε, beneficiary/victim structure, and classification — they are linked here as siblings in the same kernel contest, not as one constraint measured differently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
