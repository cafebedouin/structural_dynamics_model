% ============================================================================
% CONSTRAINT STORY: constitutional_secularism__strict_neutrality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_secularism__strict_neutrality_reading, []).

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
 *   constraint_id: constitutional_secularism__strict_neutrality_reading
 *   human_readable: Constitutional Secularism — Strict Neutrality Reading
 *   domain: constitutional_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   This constraint is the strict-neutrality reading of constitutional
 *   secularism: the state maintains equal legal distance from all religions,
 *   provides no preferential treatment, and does not intervene in religious
 *   affairs. The reading frames secularism as a coordination mechanism that
 *   prevents majoritarian capture and protects religious minorities through
 *   uniform law. This is one of three contested readings of the secularism
 *   kernel. The strict-neutrality reading instantiates a specific
 *   configuration of beneficiaries (minorities, secular institutions),
 *   victims (majority establishments seeking privilege, oppressed subgroups
 *   seeking intervention), and outcome structure: equal treatment in law but
 *   foreclosure of state support for internal reform.
 *
 * KEY AGENTS:
 *   - constitutional_court: Interprets and enforces the equal-distance principle; sets the operational boundary of permissible state conduct toward religious affairs
 *   - religious_minorities: Protected from majoritarian preference by neutrality; constrained from appealing for intervention against intra-community oppression
 *   - majority_religion_establishment: Loses formal establishment privileges and preferential access to state resources
 *   - state_secular_institutions: Operate uniformly without religious constraint or carve-outs
 *   - intra_community_reform_advocates: Locked out of state support for liberalizing change within their own communities
 *   - oppressed_community_subgroups: Trapped between community authority and state non-intervention; most vulnerable to the constraint's foreclosure of intervention
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_secularism__strict_neutrality_reading, 0.38).
domain_priors:suppression_score(constitutional_secularism__strict_neutrality_reading, 0.41).
domain_priors:theater_ratio(constitutional_secularism__strict_neutrality_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_secularism__strict_neutrality_reading, rope).
narrative_ontology:human_readable(constitutional_secularism__strict_neutrality_reading, "Constitutional Secularism — Strict Neutrality Reading").
narrative_ontology:topic_domain(constitutional_secularism__strict_neutrality_reading, "constitutional_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(constitutional_secularism__strict_neutrality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_secularism__strict_neutrality_reading, '63c74477-da99-470d-ae24-8018fe0a5120').
narrative_ontology:cs_kernel_codification('63c74477-da99-470d-ae24-8018fe0a5120', formalized).
narrative_ontology:cs_authority_grounding('63c74477-da99-470d-ae24-8018fe0a5120', lineage).
narrative_ontology:cs_interpretation_layer_present('63c74477-da99-470d-ae24-8018fe0a5120').
narrative_ontology:cs_reading_relation('63c74477-da99-470d-ae24-8018fe0a5120', constitutional_secularism__principled_intervention_reading, coexists_with).
narrative_ontology:cs_reading_relation('63c74477-da99-470d-ae24-8018fe0a5120', constitutional_secularism__reformist_reading, coexists_with).
narrative_ontology:cs_axiom('63c74477-da99-470d-ae24-8018fe0a5120', foundational, state_must_maintain_equal_distance).
narrative_ontology:cs_axiom_status(state_must_maintain_equal_distance, holdable).
narrative_ontology:cs_axiom_grounding('63c74477-da99-470d-ae24-8018fe0a5120', state_must_maintain_equal_distance, deontological).
narrative_ontology:cs_axiom('63c74477-da99-470d-ae24-8018fe0a5120', foundational, state_intervention_in_religion_impermissible).
narrative_ontology:cs_axiom_status(state_intervention_in_religion_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('63c74477-da99-470d-ae24-8018fe0a5120', state_intervention_in_religion_impermissible, deontological).
narrative_ontology:cs_reference_frame('63c74477-da99-470d-ae24-8018fe0a5120', equal_legal_distance_from_all_faiths).
narrative_ontology:cs_drift_state('63c74477-da99-470d-ae24-8018fe0a5120', contemporary_pluralist_challenge, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('63c74477-da99-470d-ae24-8018fe0a5120', '').
narrative_ontology:cs_kernel_id(constitutional_secularism__strict_neutrality_reading, constitutional_secularism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_secularism__strict_neutrality_reading, religious_minorities).
narrative_ontology:constraint_beneficiary(constitutional_secularism__strict_neutrality_reading, secular_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_secularism__strict_neutrality_reading, state_secular_institutions).
narrative_ontology:constraint_victim(constitutional_secularism__strict_neutrality_reading, majority_religion_establishment).
narrative_ontology:constraint_victim(constitutional_secularism__strict_neutrality_reading, intra_community_reform_advocates).
narrative_ontology:constraint_victim(constitutional_secularism__strict_neutrality_reading, oppressed_community_subgroups).
narrative_ontology:constraint_vindicates(constitutional_secularism__strict_neutrality_reading, liberal_neutrality_principle).
narrative_ontology:constraint_vindicates(constitutional_secularism__strict_neutrality_reading, equal_protection_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces the equal-distance principle through constitutional adjudication. Issues rulings on state interference in religious affairs and on preferential treatment of majority religions. Decides which state actions constitute impermissible preference versus legitimate secular regulation. Their decisions set the operational boundary between permissible and impermissible state conduct.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, constitutional_court, agenda_setter,
    institutional, generational, analytical, national).

% Protected by the equal-distance principle: the state cannot privilege majority religions or restrict minority practice through preferential legislation. They receive legal immunity from majoritarian religious preference. However, they remain vulnerable to the majority's use of secular regulation as a proxy for religious disadvantage, and the strict neutrality frame prevents state intervention to protect them from intra-community oppression by conservative factions.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, religious_minorities, beneficiary,
    organized, generational, constrained, national).

% Loses formal establishment privileges and access to preferential state support (funding, regulatory carve-outs, legislative deference to religious law). Must accept equal legal treatment with minority faiths. Cannot use state machinery to advance their doctrines or enforce their norms on adherents who exit or dissent. Constrained by the neutrality principle from capturing state resources for sectarian purposes.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, majority_religion_establishment, payer,
    powerful, generational, constrained, national).

% Operate free from religious constraint or preferential deference to religious authority. Schools, courts, legislatures, and bureaucracies apply uniform secular law without exception for religious doctrine. They benefit from a unified legal regime uncomplicated by sectarian carve-outs or competing authorities claiming religious jurisdiction.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, state_secular_institutions, beneficiary,
    institutional, generational, analytical, national).

% Seek state support for liberalizing reforms within their own communities (e.g., women's education, child marriage prohibition, gender-egalitarian interpretation). The strict neutrality reading denies them this: the state cannot intervene in religious affairs, even to support reformers against conservative authorities. They remain subject to intra-community pressure while barred from appealing to state power for protection or enforcement of reform norms.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, intra_community_reform_advocates, payer,
    moderate, biographical, identity_locked, national).

% Vulnerable to religious authorities within their own communities (e.g., women under patriarchal family law, sexual minorities under doctrine-based exclusion, ritual-practice dissenters). The strict neutrality principle denies them state intervention: the state cannot override religious authority or custom to protect them, framing such intervention as impermissible interference. They are trapped between community authority and state non-intervention.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, oppressed_community_subgroups, payer,
    powerless, biographical, trapped, national).

% Model different readings of secularism: strict neutrality, principled intervention, and reformist approaches. Each reading produces different classifications and different distributional outcomes. Their experience provides comparative evidence for whether neutrality sustains pluralism or entrenches intra-community oppression.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, comparative_democracies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_secularism__strict_neutrality_reading, majority_religion_establishment).
narrative_ontology:fixing_cost_class(constitutional_secularism__strict_neutrality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents sectarian conflict by establishing a uniform legal regime that does not prefer any religious tradition over others. Solves the problem of how a pluralist state can adjudicate claims from competing faith communities without triggering majoritarian capture of state machinery. Creates a stable framework: all religions are treated equally under law, removing incentive for zero-sum competition for state favor.
% TRANSFER_FUNCTION: Transfers from majority religious establishments the privilege of state preference, enforcement, and resource allocation. Transfers from all religious communities the state's power to override religious authority in the name of secular law. Transfers to secular institutions and to religious minorities a claim on equal treatment. The net transfer is asymmetric: majority establishments lose more in formal privilege than they gain in legal equality; minorities gain legal protection from discrimination but lose state support for internal reform.
% ABSENT_VOICES: Oppressed subgroups within religious communities (women, sexual minorities, reform advocates) are not seated as parties because the strict neutrality reading defines them as outside the scope of state concern — their claims for intervention are the very interference the constraint forbids. Reformist advocates and interventionalist theorists (the principled_intervention_reading and reformist_reading) are alternative readings, not present seats. Their absence is structural to this reading: they would argue for precisely what this reading prohibits.
% DISAPPEARANCE_RATIONALE: If strict neutrality vanished — if the state reasserted authority to prefer majority traditions or to intervene in religious affairs — the constitutional structure would shift dramatically: majority religions would recover establishment privileges, religious minorities would lose legal immunity from preferential discrimination, intra-community authority would face renewed state intervention (some for reform, some for majority advantage), and secular institutions would operate under differential deference to religious doctrine. The political economy of religion-state relations would reorganize around competing claims for state favor rather than equal treatment.
% FOUNDING_PROBLEM: Post-colonial and post-sectarian states needed a framework to adjudicate competing religious claims without triggering majoritarian capture of state power or institutionalizing minority subordination. The founding problem: how to prevent the state from becoming the instrument of majority religious interest while also preventing it from intervening in religious affairs in ways that could empower authoritarian factions.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars and secular-governance theorists attest the founding problem remains live. Comparative democracy advocates cite evidence that neutrality prevents majoritarian capture. Reformist activists and oppressed-group advocates attest the founding problem is unresolved or reframed: they argue the constraint now enables intra-community oppression by foreclosing state intervention on behalf of vulnerable subgroups. Religious minorities in pluralist states attest neutrality protects them; oppressed minorities within religious communities attest neutrality abandons them.
narrative_ontology:disappearance_verdict(constitutional_secularism__strict_neutrality_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_secularism__strict_neutrality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_secularism__strict_neutrality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_secularism__strict_neutrality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_secularism__strict_neutrality_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_secularism__strict_neutrality_reading_tests).
:- end_tests(constitutional_secularism__strict_neutrality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38) because the constraint produces genuine coordination benefit (preventing majoritarian capture, enabling pluralism) while also systematically disadvantaging two groups: reform advocates and oppressed minorities. The cost is not extraction in the classical sense—it is foreclosure of a capacity. Suppression is moderate-low (0.41) because the constraint is sustained by legal interpretation and institutional habit rather than active coercion; courts enforce it, but the principle itself is genuinely endorsed by secular-governance theorists and minority-protection advocates. Theater is low (0.22) because the constraint's operation is largely transparent: court decisions openly apply the equal-distance principle, there is little performative cover. The measurement series shows slight drift upward in suppression (as the court consolidates the principle through rulings) and stable extractiveness (the cost structure remains constant). Accessibility collapse is moderate (0.52): alternatives exist (principled intervention, reformist approaches) but operating within them requires constitutional amendment or different judicial interpretation, both costly at the national level.
 *
 * PERSPECTIVAL GAP:
 *   The constitutional court and secular institutions view this as genuine coordination protecting pluralism. Religious minorities view it as valuable protection from majoritarian discrimination. Majority religious establishments view it as illegitimate loss of privilege. Reform advocates view it as abandonment. Oppressed subgroups view it as structural entrapment. The engine should compute different effective-extraction values from each seat: the court and minorities sit near low/negative extraction (coordination benefit); the majority establishment sits near high extraction (loss of privilege); reform advocates and oppressed groups sit near high extraction (foreclosure of remedy). The structural asymmetry is the reading's defining feature.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary set (religious minorities, secular institutions) derives directionality near 0.2-0.3 because they benefit from the constraint's coordination function and face low direct cost. The payer set (majority establishment, oppressed subgroups, reform advocates) derives directionality near 0.7-0.85 because they bear costs—either loss of privilege or foreclosure of remedy—without equivalent coordination benefit. Intra-community reform advocates are particularly asymmetric: they face identity-locked exit (they cannot leave their community without abandoning their reform mission), moderate power (they have intellectual and grassroots capacity but no state authority), and a directionality near 0.8 because the constraint actively prevents their strategy while others' strategies remain available. The court sits at directionality near 0.5 (symmetric): it enforces the constraint but also benefits from clear institutional authority under it.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not mandatrophy-resolved. The founding problem (how to prevent majoritarian capture while enabling pluralism) remains live, though contested. Reformist and interventionalist readings dispute whether the founding problem is still live or has been reframed by the constraint's operation. The measurement series shows suppression increasing slightly over the interval (courts consolidating the principle), not decreasing—so there is no drift toward abandonment. Theater is low and stable, indicating the constraint is not primarily performative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    neutrality_mask_asymmetry,
    'Does equal legal distance actually distribute costs equally, or does it mask asymmetric costs to oppressed minorities and reform advocates by treating them as external to the state''s scope of concern?',
    'Comparative analysis of outcomes: if oppressed minorities (women under patriarchal family law, sexual minorities, reform advocates) consistently lack remedies while majority establishments retain non-legal power, the neutrality frame is masking asymmetry. Longitudinal tracking of reform success rates and exit options across religious communities.',
    'If asymmetry is confirmed, the constraint reclassifies from rope (genuine coordination) toward snare (extraction masked as neutrality): the beneficiaries would be shown to be narrower than declared, the real victims broader. The classification would shift if the constraint is shown to systematically enable intra-community oppression by foreclosing intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(neutrality_mask_asymmetry, empirical, 'Whether neutrality distributes costs symmetrically or masks oppression of internal minorities.').

omega_variable(
    intervention_boundary_under_determination,
    'What counts as impermissible state interference in religion versus permissible secular regulation? Is that boundary determinate or permanently contestable?',
    'Doctrinal analysis: if courts consistently apply clear criteria (e.g., ''religious motivation vs. secular purpose''), the boundary is determinate. If courts disagree on boundary placement or shift the boundary over time, it is under-determined. Natural experiments where courts in otherwise-similar jurisdictions rule differently on the same regulatory question.',
    'If the boundary is under-determined, the constraint''s stability depends on institutional path-dependence rather than principle. Courts may migrate between strict-neutrality, principled-intervention, and reformist readings as membership changes, making the classification unstable. If determinate, the constraint is more robust.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intervention_boundary_under_determination, conceptual, 'Whether the permissible-interference boundary is principled and stable or contested and path-dependent.').

omega_variable(
    oppressed_minority_structural_vulnerability,
    'Are oppressed subgroups (women, sexual minorities, reform advocates) genuinely trapped by the constraint, or do they have effective remedies outside the state—through exit, community mobilization, or transnational support?',
    'Exit-option ethnography: track cases where reform advocates or oppressed individuals exit communities, seek remedies, or mobilize internally. If exit is actually available and successful, the constraint is more modest. If exit results in loss of livelihood, social annihilation, or vulnerability to violence, exit is not real.',
    'If exit is effectively unavailable, the constraint is more extractive for oppressed minorities than the moderate extractiveness score suggests. The classification might shift toward snare if the constraint systematically prevents remedy for the most vulnerable. The theater ratio might also increase if the constraint performatively claims neutrality while operationally enabling oppression.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(oppressed_minority_structural_vulnerability, empirical, 'Whether oppressed minorities genuinely lack exit, rendering the constraint''s foreclosure of intervention exploitative.').

omega_variable(
    reading_coexistence_vs_foreclosure,
    'Do the strict-neutrality, principled-intervention, and reformist readings coexist as live positions held by different parties, or does one reading logically foreclose the others within a single coherent framework?',
    'Jurisprudential analysis: if a constitutional court or theoretical tradition endorses one reading and explicitly rejects the others as incoherent, there is foreclosure. If different democracies and different constitutional traditions hold different readings without logical contradiction, they coexist. Comparative case analysis across India, Turkey, France, United States, and other secular-constitutional states.',
    'If the readings foreclose one another, the constraint is structurally contested in ways that classification as ''rope'' may not capture. The engine would compute different types for different institutional seats reading the same kernel differently. If readings coexist, the classification is more stable but the kernel remains contested.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_coexistence_vs_foreclosure, conceptual, 'Whether sibling readings are logically foreclosed or structurally coexistent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_secularism__strict_neutrality_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_secularism__strict_neutrality_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(cons_tr_t5, constitutional_secularism__strict_neutrality_reading, theater_ratio, 5, 0.19).
narrative_ontology:measurement(cons_tr_t10, constitutional_secularism__strict_neutrality_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(cons_tr_t15, constitutional_secularism__strict_neutrality_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement(cons_tr_t20, constitutional_secularism__strict_neutrality_reading, theater_ratio, 20, 0.23).
narrative_ontology:measurement(cons_tr_t25, constitutional_secularism__strict_neutrality_reading, theater_ratio, 25, 0.22).
narrative_ontology:measurement(cons_tr_t30, constitutional_secularism__strict_neutrality_reading, theater_ratio, 30, 0.22).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(cons_be_t5, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(cons_be_t10, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 10, 0.37).
narrative_ontology:measurement(cons_be_t15, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 15, 0.39).
narrative_ontology:measurement(cons_be_t20, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(cons_be_t25, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 25, 0.38).
narrative_ontology:measurement(cons_be_t30, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 30, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(cons_su_t5, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 5, 0.37).
narrative_ontology:measurement(cons_su_t10, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 10, 0.39).
narrative_ontology:measurement(cons_su_t15, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 15, 0.41).
narrative_ontology:measurement(cons_su_t20, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(cons_su_t25, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 25, 0.41).
narrative_ontology:measurement(cons_su_t30, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 30, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_secularism__strict_neutrality_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_secularism__strict_neutrality_reading, 0.12).
narrative_ontology:affects_constraint(constitutional_secularism__strict_neutrality_reading, constitutional_secularism__principled_intervention_reading).
narrative_ontology:affects_constraint(constitutional_secularism__strict_neutrality_reading, constitutional_secularism__reformist_reading).

% DUAL FORMULATION NOTE:
% The constitutional-secularism kernel instantiates three structurally distinct constraints, each with different ε values, different beneficiary/victim structures, and different types. Strict neutrality emphasizes equal treatment and plural protection; principled intervention emphasizes state capacity for reform within communities; reformist reading emphasizes state duty to eliminate oppression. Each reading produces a different distribution of costs and benefits. Linked by network.affects_constraints: strict neutrality influences both siblings by setting what counts as interference; siblings influence strict neutrality by contesting its boundary.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_secularism__strict_neutrality_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
