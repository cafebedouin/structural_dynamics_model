% ============================================================================
% CONSTRAINT STORY: marriage_authority__secularist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: marriage_authority__secularist_reading
 *   human_readable: Secularist Legislative Monopoly on Marriage Authority (UCC Reading)
 *   domain: legal/constitutional/family_law
 *
 * SUMMARY:
 *   This constraint instantiates the secularist reading of the marriage
 *   authority kernel: the claim that marriage and family law are the
 *   exclusive domain of a democratic legislature, and that religious personal
 *   law pluralism is a transitional anomaly to be eliminated by a Uniform
 *   Civil Code. In the Indian constitutional context, this reading treats
 *   Article 44 (UCC Directive Principle) as the operative commitment and
 *   regards community-specific marriage laws as relics of colonial
 *   accommodation incompatible with modern citizenship. The constraint is
 *   actively enforced through legislative debate, draft UCC bills, and
 *   political campaigns that frame personal law as backward. It generates
 *   coordination (uniform legal standards, national integration narrative)
 *   alongside asymmetric extraction (stripping minority religious communities
 *   of normative autonomy). The claim/metric independence is maintained: the
 *   reading is claimed as tangled_rope â coordination with extraction â
 *   while the metrics describe high extraction and rising theater as the
 *   project has become more majoritarian and performative over time.
 *
 * KEY AGENTS:
 *   - union_parliament: Agenda-setter (institutional/constrained) â claims and exercises constitutional authority to codify uniform marriage law.
 *   - secular_modernist_coalition: Primary beneficiary (organized/constrained) â draws ideological and political capital from the UCC project and national-uniformity narrative.
 *   - minority_religious_communities: Primary target (moderate/identity_locked) â bear the loss of personal law autonomy; exit is blocked because marriage norms are fused with religious identity.
 *   - supreme_court: Analytical observer (institutional/analytical) â reviews personal law constitutionally but defers to the legislature on UCC codification in this reading.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__secularist_reading, 0.72).
domain_priors:suppression_score(marriage_authority__secularist_reading, 0.68).
domain_priors:theater_ratio(marriage_authority__secularist_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__secularist_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority__secularist_reading, "Secularist Legislative Monopoly on Marriage Authority (UCC Reading)").
narrative_ontology:topic_domain(marriage_authority__secularist_reading, "legal/constitutional/family_law").

domain_priors:requires_active_enforcement(marriage_authority__secularist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__secularist_reading, '9092b5c1-18d2-49ab-9b21-027cf423c162').
narrative_ontology:cs_kernel_codification('9092b5c1-18d2-49ab-9b21-027cf423c162', formalized).
narrative_ontology:cs_authority_grounding('9092b5c1-18d2-49ab-9b21-027cf423c162', lineage).
narrative_ontology:cs_interpretation_layer_present('9092b5c1-18d2-49ab-9b21-027cf423c162').
narrative_ontology:cs_reading_relation('9092b5c1-18d2-49ab-9b21-027cf423c162', marriage_authority__communal_autonomy_reading, forecloses).
narrative_ontology:cs_reading_relation('9092b5c1-18d2-49ab-9b21-027cf423c162', marriage_authority__federalist_millet_reading, influences).
narrative_ontology:cs_reading_relation('9092b5c1-18d2-49ab-9b21-027cf423c162', marriage_authority__gender_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('9092b5c1-18d2-49ab-9b21-027cf423c162', marriage_authority__judicial_harmonization_reading, influences).
narrative_ontology:cs_axiom('9092b5c1-18d2-49ab-9b21-027cf423c162', foundational, individual_secular_citizenship_supersedes_communal_status).
narrative_ontology:cs_axiom_status(individual_secular_citizenship_supersedes_communal_status, holdable).
narrative_ontology:cs_axiom_grounding('9092b5c1-18d2-49ab-9b21-027cf423c162', individual_secular_citizenship_supersedes_communal_status, deontological).
narrative_ontology:cs_axiom('9092b5c1-18d2-49ab-9b21-027cf423c162', foundational, legislative_uniformization_imperative).
narrative_ontology:cs_axiom_status(legislative_uniformization_imperative, holdable).
narrative_ontology:cs_axiom_grounding('9092b5c1-18d2-49ab-9b21-027cf423c162', legislative_uniformization_imperative, conventional).
narrative_ontology:cs_reference_frame('9092b5c1-18d2-49ab-9b21-027cf423c162', democratic_legislative_supremacy).
narrative_ontology:cs_drift_state('9092b5c1-18d2-49ab-9b21-027cf423c162', contemporary_political_gridlock, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('9092b5c1-18d2-49ab-9b21-027cf423c162', '').
narrative_ontology:cs_kernel_id(marriage_authority__secularist_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__secularist_reading, secular_modernist_coalition).
narrative_ontology:constraint_victim(marriage_authority__secularist_reading, minority_religious_communities).
narrative_ontology:constraint_vindicates(marriage_authority__secularist_reading, parliamentary_sovereignty_family_law).
narrative_ontology:constraint_vindicates(marriage_authority__secularist_reading, national_integrative_legal_unification).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claims sole constitutional authority to codify marriage and family law through uniform civil legislation. Introduces bills and debates the Uniform Civil Code as a directive principle obligation, overriding personal law jurisdictions. Its exit is constrained by the constitutional text it inherits and the political costs of alienating religious constituencies.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, union_parliament, agenda_setter,
    institutional, generational, constrained, national).

% Draws political capital and ideological coherence from the promise of a uniform modern legal order replacing personal law fragmentation. Benefits from the framing of national progress and gender justice that the UCC project provides, consolidating a constituency around secular state identity.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, secular_modernist_coalition, beneficiary,
    organized, generational, constrained, national).

% Bear the loss of normative autonomy over marriage, inheritance, and family matters as the state moves to replace their personal laws with a uniform code. Their identity is fused with religious family law; state-imposed uniformity threatens communal boundaries and self-governance. Political exit is limited because opposition to UCC is framed as anti-national or backward.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, minority_religious_communities, payer,
    moderate, generational, identity_locked, national).

% Reviews personal law norms against constitutional fundamental rights, occasionally striking down discriminatory practices but deferring to the legislature on uniform codification. In this reading it is an observer of legislative authority rather than the primary harmonizing agent; it waits for Parliament to act on the UCC directive.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, supreme_court, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, territorially uniform legislative framework for marriage and family law across religious communities, eliminating jurisdictional fragmentation and creating consistent legal rights and obligations for citizens regardless of communal affiliation.
% TRANSFER_FUNCTION: Moves marriage authority from religious communities and their personal law institutions to the central democratic legislature and secular state administration; transfers normative autonomy over family matters from minority religious communities to majoritarian lawmaking processes.
% ABSENT_VOICES: Religious personal law authorities and minority community leaders who treat marriage law as intrinsic to communal self-definition are heard in parliamentary committee debates but structurally overruled; the communal autonomy reading itself is delegitimized as a transitional anachronism rather than a coordinate constitutional value.
% DISAPPEARANCE_RATIONALE: If the secularist legislative monopoly vanished, personal law pluralism would revert to being the default constitutional arrangement, the UCC project would lose its institutional anchor, and marriage authority would disperse back to communities, courts, or regional bodies â the national-secular legal architecture would fragment along communal lines.
% FOUNDING_PROBLEM: Post-colonial state-building confronted a fragmented legal inheritance where religious personal laws governed family matters, producing jurisdictional inconsistency, gender inequality, and a legal structure that impeded the construction of a unified national citizenship.
% FOUNDING_PROBLEM_CORROBORATION: Secular feminist legal historians and constitutional scholars attest that personal law fragmentation entrenched gender injustice and required uniform legislative remedy. Minority rights advocates and comparative law scholars outside the secular modernist coalition contest this, arguing that the founding problem was better addressed through intra-community reform and consociational accommodation rather than majoritarian uniformization.
narrative_ontology:disappearance_verdict(marriage_authority__secularist_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__secularist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__secularist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_authority__secularist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__secularist_reading, 0.72, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is high (0.72) because the constraint transfers substantial normative autonomy from minority communities to the center with no compensatory mechanism. Suppression is high (0.68) because the project requires active state enforcement to override centuries-old personal law systems and communal resistance. Theater ratio is above 0.5 (0.55) because an increasing share of UCC advocacy performs national-majoritarian identity rather than delivering legislative text or gender-justice outcomes. Accessibility collapse is moderate-high (0.62): alternatives (personal law, communal autonomy) are legally available but increasingly stigmatized as illegitimate in national discourse. Resistance is moderate (0.58): minority communities and federalist scholars actively contest the project but are structurally outvoted.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seat (secular modernist coalition) experiences the constraint as necessary nation-building coordination and gender-justice scaffolding; the payer seat (minority religious communities) experiences the same structure as majoritarian extraction of communal identity. The union parliament experiences it as constitutional duty; the Supreme Court experiences it as a deferential boundary on judicial power. The engine computes these divergences from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The secular_modernist_coalition and union_parliament sit near the beneficiary end: they collect ideological coherence, authority, and political consolidation from the constraint. Minority_religious_communities sit near the full-target end: they pay through lost autonomy, identity-locked exit, and cultural erosion. The supreme_court is analytical and near neutral. Spatial scope is national, amplifying extraction for the identity-locked community.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â colonial legal fragmentation and gender injustice â is contested in status. If the problem is genuinely live and best solved by uniform legislation, the constraint is a scaffold or tangled rope. If the problem has been partially solved by other means (judicial review, intra-community reform) and the UCC persists primarily as a majoritarian wedge, the constraint approaches snare or piton. The temporal measurements show rising extraction and theater over decades, suggesting the coordination story has atrophied while the extraction has intensified â a mandatrophy drift toward piton. However, because the coalition still genuinely believes in the coordination narrative and the UCC is not yet fully enacted, the structurally true type remains tangled_rope: both coordination and extraction are present and enforced.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_ambiguity,
    'Does the secularist reading of marriage authority foreclose the communal autonomy reading within a single constitutional framework, or can they be held as coexisting constitutional values?',
    'Comparative constitutional analysis of jurisdictions that maintain both uniform civil codes and opt-out personal law arrangements; examination of whether the Indian Constitution''s fundamental rights and directive principles can structurally accommodate both readings simultaneously.',
    'If foreclosed, the constraint operates as a zero-sum displacement of communal authority, strengthening the extraction profile. If coexistent, the constraint is one reading among several and its extractiveness is moderated by constitutional pluralism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_ambiguity, conceptual, 'Whether secularist legislative supremacy logically excludes communal marriage autonomy').

omega_variable(
    ucc_gender_justice_delivery,
    'Does the secularist UCC project actually advance gender equality, or does it primarily repackage patriarchal control under state authority while dismantling minority women''s communal support structures?',
    'Empirical outcome studies following any UCC implementation, comparing gender property, divorce, and inheritance outcomes under uniform state law versus reformed personal law and special-marriage-act regimes.',
    'If gender equality outcomes do not improve, the coordination narrative (national integration through gender justice) becomes theatrical cover for majoritarian homogenization, pushing the computed classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ucc_gender_justice_delivery, empirical, 'Whether the UCC produces the gender-justice coordination it claims').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__secularist_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority__secularist_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(marr_tr_t14, marriage_authority__secularist_reading, theater_ratio, 14, 0.25).
narrative_ontology:measurement(marr_tr_t28, marriage_authority__secularist_reading, theater_ratio, 28, 0.35).
narrative_ontology:measurement(marr_tr_t42, marriage_authority__secularist_reading, theater_ratio, 42, 0.45).
narrative_ontology:measurement(marr_tr_t56, marriage_authority__secularist_reading, theater_ratio, 56, 0.5).
narrative_ontology:measurement(marr_tr_t70, marriage_authority__secularist_reading, theater_ratio, 70, 0.55).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority__secularist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(marr_be_t14, marriage_authority__secularist_reading, base_extractiveness, 14, 0.4).
narrative_ontology:measurement(marr_be_t28, marriage_authority__secularist_reading, base_extractiveness, 28, 0.52).
narrative_ontology:measurement(marr_be_t42, marriage_authority__secularist_reading, base_extractiveness, 42, 0.63).
narrative_ontology:measurement(marr_be_t56, marriage_authority__secularist_reading, base_extractiveness, 56, 0.68).
narrative_ontology:measurement(marr_be_t70, marriage_authority__secularist_reading, base_extractiveness, 70, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority__secularist_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(marr_su_t14, marriage_authority__secularist_reading, suppression_requirement, 14, 0.35).
narrative_ontology:measurement(marr_su_t28, marriage_authority__secularist_reading, suppression_requirement, 28, 0.48).
narrative_ontology:measurement(marr_su_t42, marriage_authority__secularist_reading, suppression_requirement, 42, 0.58).
narrative_ontology:measurement(marr_su_t56, marriage_authority__secularist_reading, suppression_requirement, 56, 0.64).
narrative_ontology:measurement(marr_su_t70, marriage_authority__secularist_reading, suppression_requirement, 70, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__secularist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, marriage_authority__communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, marriage_authority__federalist_millet_reading).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, marriage_authority__gender_rights_reading).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, marriage_authority__judicial_harmonization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of five sibling readings of the marriage_authority kernel. It is linked to the other four as a constraint family under the epsilon-invariance principle: each reading instantiates a structurally distinct claim with its own epsilon, beneficiaries, and victims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
