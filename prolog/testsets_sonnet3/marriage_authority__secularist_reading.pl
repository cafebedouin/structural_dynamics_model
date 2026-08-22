% ============================================================================
% CONSTRAINT STORY: marriage_authority__secularist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__secularist_reading, []).

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
 *   constraint_id: marriage_authority__secularist_reading
 *   human_readable: Legislative Supremacy Over Marriage Law — Secularist/UCC Reading
 *   domain: legal/constitutional/social
 *
 * SUMMARY:
 *   This story authors the secularist reading of the marriage-authority
 *   kernel: the claim that democratic legislature holds proper authority over
 *   marriage law, and that personal law pluralism is a transitional
 *   constitutional anomaly the state is obligated to eliminate through a
 *   Uniform Civil Code. On this reading, the constitutional directive toward
 *   uniform civil law is a promissory note overdue for payment, and communal
 *   legal autonomy is a historical accommodation rather than a permanent
 *   right. The reading treats intra-community gender inequities as evidence
 *   for wholesale replacement rather than for community-internal reform,
 *   distinguishing it sharply from the gender_rights_reading and
 *   communal_autonomy_reading, which are separate constraints (see
 *   kernel_context). ε here describes the standing arrangement — legislative
 *   claim-making over marriage authority pressing against entrenched personal
 *   law pluralism — assessed by this reading's own lights: extractive because
 *   the reform program concentrates authorship of family-law identity in a
 *   legislative-secular coalition while treating minority communities and
 *   their internal reformers as objects rather than co-authors of change.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__secularist_reading, 0.71).
domain_priors:suppression_score(marriage_authority__secularist_reading, 0.62).
domain_priors:theater_ratio(marriage_authority__secularist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__secularist_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority__secularist_reading, "Legislative Supremacy Over Marriage Law — Secularist/UCC Reading").
narrative_ontology:topic_domain(marriage_authority__secularist_reading, "legal/constitutional/social").

domain_priors:requires_active_enforcement(marriage_authority__secularist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__secularist_reading, 'd60b7395-8337-4b30-bfce-91b1b7282dce').
narrative_ontology:cs_kernel_codification('d60b7395-8337-4b30-bfce-91b1b7282dce', distributed).
narrative_ontology:cs_authority_grounding('d60b7395-8337-4b30-bfce-91b1b7282dce', distributed).
narrative_ontology:cs_reading_relation('d60b7395-8337-4b30-bfce-91b1b7282dce', marriage_authority__communal_autonomy_reading, forecloses).
narrative_ontology:cs_reading_relation('d60b7395-8337-4b30-bfce-91b1b7282dce', marriage_authority__federalist_millet_reading, forecloses).
narrative_ontology:cs_reading_relation('d60b7395-8337-4b30-bfce-91b1b7282dce', marriage_authority__gender_rights_reading, influences).
narrative_ontology:cs_reading_relation('d60b7395-8337-4b30-bfce-91b1b7282dce', marriage_authority__judicial_harmonization_reading, influences).
narrative_ontology:cs_axiom('d60b7395-8337-4b30-bfce-91b1b7282dce', foundational, legislative_supremacy_over_family_law).
narrative_ontology:cs_axiom_status(legislative_supremacy_over_family_law, holdable).
narrative_ontology:cs_axiom_grounding('d60b7395-8337-4b30-bfce-91b1b7282dce', legislative_supremacy_over_family_law, conventional).
narrative_ontology:cs_axiom('d60b7395-8337-4b30-bfce-91b1b7282dce', foundational, pluralism_as_transitional_defect).
narrative_ontology:cs_axiom_status(pluralism_as_transitional_defect, holdable).
narrative_ontology:cs_axiom_grounding('d60b7395-8337-4b30-bfce-91b1b7282dce', pluralism_as_transitional_defect, instrumental).
narrative_ontology:cs_reference_frame('d60b7395-8337-4b30-bfce-91b1b7282dce', constituent_assembly_directive_principle).
narrative_ontology:cs_drift_state('d60b7395-8337-4b30-bfce-91b1b7282dce', contemporary_stalled_ucc_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('d60b7395-8337-4b30-bfce-91b1b7282dce', '').
narrative_ontology:cs_kernel_id(marriage_authority__secularist_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__secularist_reading, secular_modernist_coalition).
narrative_ontology:constraint_beneficiary(marriage_authority__secularist_reading, national_legal_uniformity_institutions).
narrative_ontology:constraint_victim(marriage_authority__secularist_reading, minority_religious_communities).
narrative_ontology:constraint_victim(marriage_authority__secularist_reading, personal_law_dependent_women).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_authority__secularist_reading, personal_law_dependent_women).
narrative_ontology:constraint_vindicates(marriage_authority__secularist_reading, civic_nationalism_doctrine).
narrative_ontology:constraint_vindicates(marriage_authority__secularist_reading, formal_legal_equality_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A coalition of legislators, urban professional classes, and civic-nationalist advocacy groups that campaigns for a Uniform Civil Code as the completion of constitutional secularism. They frame personal law pluralism as a colonial-era holdover and a permanent obstacle to equal citizenship. Because they control the legislative and media agenda around this issue in national politics, they can shape the terms of debate even when they lack the votes to pass a UCC outright, and they bear little personal cost if reform stalls.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, secular_modernist_coalition, beneficiary,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__secularist_reading, secular_modernist_coalition, agenda_setter).

% Law commissions, drafting committees, and central government ministries tasked with producing model uniform codes. They administer the ongoing project of harmonization, issue draft bills, and hold hearings. Their institutional survival and mandate are tied to the eventual displacement of personal law systems, giving them a structural stake in framing pluralism as transitional rather than permanent.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, national_legal_uniformity_institutions, agenda_setter,
    institutional, civilizational, analytical, national).

% Religious minority populations whose marriage, divorce, inheritance, and custody arrangements are currently governed by community-specific personal law. Under the secularist reading, their family law arrangements are treated as a temporary deviation from the norm that the state is obligated to eliminate. They experience UCC advocacy as an assimilationist threat to communal self-governance, and their exit options are constrained by political minority status and by the cost of contesting reform through litigation or protest, which invites majoritarian backlash.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, minority_religious_communities, payer,
    moderate, generational, constrained, national).

% Women whose marital, maintenance, and inheritance rights are determined by personal law codes that in some cases fall below constitutional equality guarantees. Some stand to gain individual rights protections under a uniform code, but they are simultaneously used as the moral justification for a reform program authored and controlled by the secular coalition rather than by women within the affected communities. Their situation is invoked rhetorically far more than it is consulted structurally, and exit from their communal legal status is often practically unavailable without loss of social standing or custody risk.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, personal_law_dependent_women, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__secularist_reading, personal_law_dependent_women, beneficiary).

% Clerical bodies and community legal authorities who administer personal law within their communities and would object strongly to legislative override, arguing constitutional protection for religious freedom and communal autonomy. Their position is treated by the secularist reading as an obstacle to be legislated past rather than a party to negotiate with; they appear mainly as objects of the reform debate, not participants in drafting it.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, religious_community_leadership, excluded,
    organized, generational, constrained, national).

% Courts adjudicate individual challenges to personal law provisions on constitutional equality grounds without themselves authoring a uniform code. They observe and sometimes accelerate the secularist reading's momentum through incremental rulings, but their institutional posture is case-by-case rather than legislative, distinguishing their role from the coalition's wholesale replacement agenda.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, constitutional_courts, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority__secularist_reading, secular_modernist_coalition).
narrative_ontology:fixing_cost_class(marriage_authority__secularist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, legislatively authored civil marriage and family law regime applicable to all citizens regardless of religion, replacing a fragmented set of community-administered personal law codes with one national standard — genuinely solving the coordination problem of forum-shopping, code conflicts in interfaith unions, and unequal treatment of similarly situated citizens under different personal laws.
% TRANSFER_FUNCTION: Moves interpretive and administrative authority over marriage, divorce, inheritance, and custody from religious community institutions to the national legislature and its drafting bodies, and moves the normative default from communal self-governance to state-defined civic uniformity — a transfer of jurisdiction and identity-defining authority from minority communities to the national legislative-secular coalition.
% ABSENT_VOICES: Religious community leadership and, more sharply, ordinary community members who value their personal law system for reasons other than gender subordination (cultural continuity, distrust of a majoritarian legislature, preference for community-internal reform) are largely absent from the drafting process, which is dominated by legislators and law commissions; women within these communities who might want reform on their own community's terms rather than through wholesale replacement are represented by advocates who do not answer to them directly.
% DISAPPEARANCE_RATIONALE: If legislative claim to marriage authority and the UCC project disappeared overnight, personal law pluralism would lose its 'transitional' framing and would need to be re-justified as a permanent constitutional settlement rather than an anomaly awaiting correction; national law commissions built around harmonization would lose their core mandate; minority community institutions would regain uncontested administrative authority over marriage matters; and reform of communal family law would have to proceed, if at all, through intra-community or judicial routes rather than wholesale legislative replacement.
% FOUNDING_PROBLEM: Independence-era constitution-makers left personal law pluralism in place as a compromise with religious minorities and princely-state successor communities, with a constitutional directive (not a binding mandate) toward eventual uniform civil law, intended to address forum inconsistency, unequal treatment of women across codes, and the unfinished project of a unified national civic identity.
% FOUNDING_PROBLEM_CORROBORATION: The secular coalition and national law commissions attest the founding problem (fragmented, unequal family law) remains live and the sunset toward uniformity is overdue. Constitutional law scholars outside both the coalition and the communities note the directive provision was drafted as aspirational and non-binding, and that decades of non-implementation without judicial or political majority for it suggest the 'transitional anomaly' framing is itself a majoritarian political program rather than a completion of an agreed constitutional design; several comparative federalism scholars and minority-rights litigators corroborate that comparable plural societies have sustained personal law diversity as a permanent settlement, undercutting the claim that pluralism must be transitional as a matter of constitutional logic rather than contested policy.
narrative_ontology:disappearance_verdict(marriage_authority__secularist_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__secularist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__secularist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_authority__secularist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__secularist_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__secularist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority__secularist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority__secularist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises across the interval (0.42 to 0.71) because the secularist coalition's institutional footing — law commissions, drafting bodies, media framing — hardens over time even where legislative passage stalls; the accumulating asymmetry is between who authors the reform narrative and who bears its consequences. Suppression (0.62 at endpoint) reflects the structural pressure minority communities face to accept the transitional framing or be cast as obstructing constitutional completion, not formal legal coercion alone. Theater ratio is moderate-low (0.28) because law commission activity, hearings, and draft bills are substantively real institutional work, not merely performative, even though the transitional framing itself functions partly as legitimating narrative for a program that has not achieved democratic majority support for decades.
 *
 * PERSPECTIVAL GAP:
 *   From the secular coalition's seat, this is coordination completing an unfinished constitutional commitment to equal citizenship — a rope narrative. From the minority community seat, the same legislative claim to marriage authority operates as an enforced narrowing of communal self-governance riding on a promise of gender equality it does not deliver through community-controlled means — a tangled or snare-adjacent narrative. The engine computes these divergent seat classifications from the declared structural data; this story's claimed_type (tangled_rope) already registers that the coordination function (uniformity, equal treatment across codes) is real but travels with asymmetric extraction (loss of communal authorship, instrumentalized invocation of women's interests).
 *
 * DIRECTIONALITY LOGIC:
 *   The secular modernist coalition and national uniformity institutions sit near the beneficiary end: they set the terms of the reform debate, accrue political and institutional capital from advancing it, and bear minimal cost if reform stalls or if communities resist. Minority religious communities and personal-law-dependent women sit near the target end but for different reasons — communities because their governing authority is the explicit object of planned elimination, and women because their situation is invoked as justification for a program that transfers authority to a coalition they did not select and that treats their voice as secondary to the uniformity goal.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem interview surfaces the central mandatrophy risk directly: the constitutional directive toward uniform civil law was drafted as aspirational, not binding, and decades of non-implementation without a legislative majority suggest the 'transitional anomaly, awaiting completion' framing may itself be a live political program mislabeled as constitutional housekeeping. Corroboration from outside the coalition and communities (comparative federalism scholarship, minority-rights litigators) supports treating founding_problem_status as contested rather than settled — precisely the flag structure this six-questions battery is built to catch: status=contested plus disappearance_verdict=world_rearranges signals that the arrangement is neither obviously alive nor obviously dead, but actively fought over by parties with unequal capacity to shape the fight.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transitional_anomaly_or_permanent_settlement,
    'Is personal law pluralism genuinely a transitional constitutional anomaly awaiting UCC completion, or was it authored and has it functioned as a permanent constitutional settlement that the secularist reading mislabels as transitional to justify its elimination?',
    'Comparative constitutional history: examine whether founding-era drafters treated the uniform civil code directive as binding policy or aspirational guidance, and whether comparable plural constitutional orders have sustained personal law diversity for multiple generations without treating it as an unresolved defect.',
    'If the anomaly framing is accurate, the secularist reading''s claim to legislative authority reads closer to overdue coordination correcting a known defect. If the framing is a political program mislabeled as constitutional completion, the reading''s extraction is better understood as majoritarian assertion of authority rather than fulfillment of a settled design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transitional_anomaly_or_permanent_settlement, conceptual, 'Whether pluralism is a real anomaly or a mislabeled permanent settlement.').

omega_variable(
    womens_interests_instrumentalization,
    'Do women within minority communities substantively benefit from and endorse the UCC program as designed, or are their interests being invoked by the secular coalition without their meaningful participation in drafting?',
    'Survey and qualitative research directly sampling women within affected communities on preferred reform pathways (community-internal reform vs. wholesale legislative replacement), compared against the coalition''s public framing of consensus.',
    'If substantive endorsement exists, the beneficiary/victim split for personal_law_dependent_women should shift toward beneficiary; if instrumentalization is confirmed, the payer classification and the extraction reading are reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(womens_interests_instrumentalization, empirical, 'Whether invoked beneficiary status for women reflects genuine consultation or instrumentalized justification.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Given that the marriage_authority kernel supports at least five structurally distinct readings (secularist, communal_autonomy, federalist_millet, gender_rights, judicial_harmonization), is the secularist framing — legislative authorship as the proper locus of marriage authority — itself contestable as the natural or default reading, or is it one political program among structurally coequal alternatives?',
    'Cross-reading comparison of cs_structure.axioms and reading_relations across all five sibling constraints to determine whether any reading has stronger constitutional-textual grounding than the others, or whether the kernel is genuinely underdetermined by the constitutional text itself.',
    'If the secularist reading has no privileged textual claim over its siblings, its ε and extraction profile should be read as one contested political program''s account rather than a constitutionally mandated trajectory — reinforcing the tangled_rope classification over any mountain-adjacent naturalization of legislative supremacy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether the secularist reading has privileged constitutional standing among the kernel''s readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__secularist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority__secularist_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(marr_tr_t8, marriage_authority__secularist_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(marr_tr_t16, marriage_authority__secularist_reading, theater_ratio, 16, 0.21).
narrative_ontology:measurement(marr_tr_t24, marriage_authority__secularist_reading, theater_ratio, 24, 0.24).
narrative_ontology:measurement(marr_tr_t32, marriage_authority__secularist_reading, theater_ratio, 32, 0.26).
narrative_ontology:measurement(marr_tr_t40, marriage_authority__secularist_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority__secularist_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(marr_be_t8, marriage_authority__secularist_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(marr_be_t16, marriage_authority__secularist_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(marr_be_t24, marriage_authority__secularist_reading, base_extractiveness, 24, 0.62).
narrative_ontology:measurement(marr_be_t32, marriage_authority__secularist_reading, base_extractiveness, 32, 0.68).
narrative_ontology:measurement(marr_be_t40, marriage_authority__secularist_reading, base_extractiveness, 40, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority__secularist_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(marr_su_t8, marriage_authority__secularist_reading, suppression_requirement, 8, 0.46).
narrative_ontology:measurement(marr_su_t16, marriage_authority__secularist_reading, suppression_requirement, 16, 0.51).
narrative_ontology:measurement(marr_su_t24, marriage_authority__secularist_reading, suppression_requirement, 24, 0.56).
narrative_ontology:measurement(marr_su_t32, marriage_authority__secularist_reading, suppression_requirement, 32, 0.6).
narrative_ontology:measurement(marr_su_t40, marriage_authority__secularist_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__secularist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, marriage_authority__communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, marriage_authority__gender_rights_reading).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, marriage_authority__federalist_millet_reading).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, marriage_authority__judicial_harmonization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of five sibling readings of the marriage_authority kernel, each authored as a separate constraint story per the ε-invariance principle: secularist_reading (this story, tangled_rope, high ε), communal_autonomy_reading, federalist_millet_reading, gender_rights_reading, and judicial_harmonization_reading. Each reading has its own stakeholders, beneficiary/victim structure, and claimed_type; they share only the kernel identity and are linked here via affects_constraints and via cs_structure.reading_relations, not merged into one measurement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_authority__secularist_reading, powerless, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
