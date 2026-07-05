% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__hindu_codified_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__hindu_codified_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: marriage_authority_kernel__hindu_codified_reading
 *   human_readable: Codified Hindu Marriage Law as Adjudicated by Civil Courts
 *   domain: comparative_law/religious_governance/family_law
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested
 *   marriage_authority_kernel: the claim that marriage and family law
 *   authority for persons statutorily classified as Hindu derives from the
 *   codified Hindu Marriage Act 1955, as interpreted and enforced by civil
 *   courts rather than by religious functionaries. This is structurally
 *   distinct from the muslim_shariat_reading (authority via personal law
 *   boards/qazis, uncodified in comparable form), the
 *   christian_canonical_reading (authority via an 1872 colonial-era canonical
 *   codification), the parsi_communal_reading (authority via a much smaller
 *   1936 communal statute), and the secular_civil_reading (authority via a
 *   religion-neutral 1954 civil code grounded directly in constitutional
 *   individual rights rather than in a religiously-labeled community). Each
 *   reading has a distinct victim set, distinct gender-equity profile, and
 *   distinct persistence dynamic — they are not the same constraint viewed
 *   from different angles; they are five separate constraints that happen to
 *   share the label 'personal law' and occupy the same jurisdictional space,
 *   hence the ε values differ and each gets its own file per the ε-invariance
 *   principle.
 *
 * KEY AGENTS:
 *   - state_judiciary: administers/interprets the Act (institutional/analytical) — agenda_setter
 *   - hindu_male_householders: net beneficiary of residual asymmetric provisions (moderate/constrained)
 *   - hindu_women_seeking_divorce: primary payer, bears procedural and substantive cost (powerless/constrained)
 *   - inter_caste_couples: nominal legal permission defeated by informal enforcement gap (powerless/trapped)
 *   - religious_minorities_misclassified_as_hindu: swept into scope without consent (powerless/trapped)
 *   - hindu_law_reform_lobby: institutional beneficiary of codification-as-reform-lever (organized/mobile)
 *   - constitutional_courts: analytical observer with periodic constitutional review power (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__hindu_codified_reading, 0.42).
domain_priors:suppression_score(marriage_authority_kernel__hindu_codified_reading, 0.48).
domain_priors:theater_ratio(marriage_authority_kernel__hindu_codified_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__hindu_codified_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__hindu_codified_reading, "Codified Hindu Marriage Law as Adjudicated by Civil Courts").
narrative_ontology:topic_domain(marriage_authority_kernel__hindu_codified_reading, "comparative_law/religious_governance/family_law").

domain_priors:requires_active_enforcement(marriage_authority_kernel__hindu_codified_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__hindu_codified_reading, 'd64ddfd1-419f-4290-9ef5-b0be6d387936').
narrative_ontology:cs_kernel_codification('d64ddfd1-419f-4290-9ef5-b0be6d387936', formalized).
narrative_ontology:cs_authority_grounding('d64ddfd1-419f-4290-9ef5-b0be6d387936', lineage).
narrative_ontology:cs_interpretation_layer_present('d64ddfd1-419f-4290-9ef5-b0be6d387936').
narrative_ontology:cs_reading_relation('d64ddfd1-419f-4290-9ef5-b0be6d387936', marriage_authority_kernel__muslim_shariat_reading, influences).
narrative_ontology:cs_reading_relation('d64ddfd1-419f-4290-9ef5-b0be6d387936', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('d64ddfd1-419f-4290-9ef5-b0be6d387936', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_reading_relation('d64ddfd1-419f-4290-9ef5-b0be6d387936', marriage_authority_kernel__secular_civil_reading, influences).
narrative_ontology:cs_axiom('d64ddfd1-419f-4290-9ef5-b0be6d387936', foundational, state_codification_legitimately_supersedes_community_custom).
narrative_ontology:cs_axiom_status(state_codification_legitimately_supersedes_community_custom, holdable).
narrative_ontology:cs_axiom_grounding('d64ddfd1-419f-4290-9ef5-b0be6d387936', state_codification_legitimately_supersedes_community_custom, conventional).
narrative_ontology:cs_axiom('d64ddfd1-419f-4290-9ef5-b0be6d387936', foundational, civil_courts_are_proper_final_interpreters_of_religious_family_law).
narrative_ontology:cs_axiom_status(civil_courts_are_proper_final_interpreters_of_religious_family_law, holdable).
narrative_ontology:cs_axiom_grounding('d64ddfd1-419f-4290-9ef5-b0be6d387936', civil_courts_are_proper_final_interpreters_of_religious_family_law, conventional).
narrative_ontology:cs_axiom('d64ddfd1-419f-4290-9ef5-b0be6d387936', secondary, codified_reform_can_proceed_within_a_religiously_labeled_framework).
narrative_ontology:cs_axiom_status(codified_reform_can_proceed_within_a_religiously_labeled_framework, holdable).
narrative_ontology:cs_axiom_grounding('d64ddfd1-419f-4290-9ef5-b0be6d387936', codified_reform_can_proceed_within_a_religiously_labeled_framework, instrumental).
narrative_ontology:cs_reference_frame('d64ddfd1-419f-4290-9ef5-b0be6d387936', post_independence_codification_settlement).
narrative_ontology:cs_drift_state('d64ddfd1-419f-4290-9ef5-b0be6d387936', contemporary_uniform_civil_code_debate, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('d64ddfd1-419f-4290-9ef5-b0be6d387936', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__hindu_codified_reading, hindu_male_householders).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__hindu_codified_reading, state_judiciary).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__hindu_codified_reading, hindu_law_reform_lobby).
narrative_ontology:constraint_victim(marriage_authority_kernel__hindu_codified_reading, hindu_women_seeking_divorce).
narrative_ontology:constraint_victim(marriage_authority_kernel__hindu_codified_reading, inter_caste_couples).
narrative_ontology:constraint_victim(marriage_authority_kernel__hindu_codified_reading, religious_minorities_misclassified_as_hindu).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__hindu_codified_reading, parliamentary_supremacy_over_personal_law).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__hindu_codified_reading, codification_produces_uniformity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Civil courts (not religious authorities) interpret and apply the Hindu Marriage Act 1955 — deciding validity, divorce grounds, maintenance, custody. Courts have progressively read gender-equity provisions expansively (e.g. cruelty, irretrievable breakdown) but remain bound by the statute's structure, which still privileges certain caste/community definitions of who counts as 'Hindu' for the Act's purposes. The judiciary administers the kernel and could, through interpretation or referral to Parliament, reshape it, but has strong institutional interest in maintaining codified personal law as a going concern rather than escalating toward a uniform civil code.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, state_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Benefit from a codified, predictable legal framework for marriage, inheritance, and divorce that historically encoded asymmetric bargaining power (e.g. restitution of conjugal rights, historically weaker maintenance defaults). Reform has eroded some of this asymmetry but residual advantages in property and custody proceedings persist. Exit from the framework requires either interfaith/inter-caste marriage under the Special Marriage Act or litigation to reclassify status — both costly.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, hindu_male_householders, beneficiary,
    moderate, biographical, constrained, national).

% Bear the cost of a framework that, despite 1955/1976 amendments, still channels divorce, maintenance, and custody disputes through a statute drafted with substantial input from conservative reform-era legislators balancing modernization against community backlash. Many rely on courts' expansive interpretation of 'cruelty' rather than statutory clarity. Their practical exit options are litigation (slow, costly, socially stigmatizing) or informal separation without legal protection.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, hindu_women_seeking_divorce, payer,
    powerless, biographical, constrained, national).

% The Act nominally permits inter-caste Hindu marriage, but registration processes, family court practice, and social enforcement (including honor-based violence in some regions) mean the codified permission is frequently defeated in practice by non-legal suppression that the statute does not address. They are trapped between a technically permissive law and an enforcement gap the state does not close.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, inter_caste_couples, payer,
    powerless, biographical, trapped, national).

% The Hindu Marriage Act's definitional scope sweeps in Buddhists, Jains, and Sikhs by statutory fiat, whether or not those communities consider themselves governed by Hindu personal law. This assigns them into the Hindu codified regime by default rather than by consent, and reclassification requires affirmative, often unfamiliar, legal action.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, religious_minorities_misclassified_as_hindu, payer,
    powerless, biographical, trapped, national).

% Legal reform organizations, women's rights litigators, and legislators who have driven amendments (1976 Marriage Laws Amendment, subsequent case law) benefit from the existence of a codified, amendable statute as a lever for incremental gender-equity reform — a lever that would not exist under uncodified custom. They have institutional standing to litigate and lobby, which distinguishes them from the powerless payer seats.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, hindu_law_reform_lobby, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__hindu_codified_reading, hindu_law_reform_lobby, agenda_setter).

% Advocates for Muslim personal law and secular uniform civil code advocates are not parties to Hindu Marriage Act adjudication, but the Hindu codified reading is frequently cited in political and judicial discourse as the template ('if Hindus can be codified, so can others' / 'Hindu codification proves uniform code is unnecessary') — a debate in which this reading's existence has stakes for their positions without their being present in the courtroom.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, muslim_shariat_board_advocates, excluded,
    organized, generational, analytical, national).

% The Supreme Court periodically reviews whether personal law codes, including this one, comport with constitutional equality guarantees (Articles 14, 15, 21), producing landmark rulings that reshape interpretation without formally repealing the statute.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, constitutional_courts, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority_kernel__hindu_codified_reading, hindu_male_householders).
narrative_ontology:fixing_cost_class(marriage_authority_kernel__hindu_codified_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides Hindus (as statutorily defined) with a single, predictable, judicially enforceable set of rules for marriage validity, divorce, maintenance, adoption, and succession, replacing a patchwork of regional customary practices that varied by caste, sect, and locality and were difficult for courts to adjudicate consistently.
% TRANSFER_FUNCTION: Moves adjudicatory authority from decentralized community/caste custom and religious functionaries to the state judiciary; within marriages, moves bargaining leverage disproportionately toward historically advantaged spouses (typically husbands) in unreformed provisions, while amendments have shifted some leverage back toward economically dependent spouses (typically wives) through maintenance and cruelty provisions.
% ABSENT_VOICES: Buddhist, Jain, and Sikh communities swept into the Act's definition of 'Hindu' without separate consent are not consulted as a distinct constituency in most litigation. Inter-caste couples facing informal social enforcement (rather than formal legal barriers) rarely appear before the courts that could address the enforcement gap, because the harm occurs outside the courtroom.
% DISAPPEARANCE_RATIONALE: If the Hindu Marriage Act's codified authority vanished overnight, an estimated 800 million+ people's marriage validity, divorce grounds, and inheritance rights would revert to uncodified customary and case-law patchwork, family courts would lose their primary statutory basis for the majority of matrimonial litigation in India, and the political question of a Uniform Civil Code would be forced to the fore immediately rather than gradually.
% FOUNDING_PROBLEM: Pre-1955, Hindu personal law was fragmented across regional schools (Mitakshara, Dayabhaga), caste custom, and colonial-era case law, producing inconsistent, often regressive outcomes (child marriage, lack of divorce rights, restricted female inheritance) that reformist legislators sought to standardize and modernize through a single Act.
% FOUNDING_PROBLEM_CORROBORATION: Feminist legal historians and Law Commission reports (external to the state judiciary and to the reform lobby that benefits from continued codification) attest the founding problem of fragmentation was substantially solved by 1955 codification, but argue the residual gender-inequitable provisions and the definitional overreach into non-Hindu communities represent a persistence of the arrangement past its original justification. The reform lobby itself argues the founding problem — full gender equity within Hindu personal law — remains live and unfinished, which is a self-interested framing given their institutional stake in continued reform litigation.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__hindu_codified_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__hindu_codified_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__hindu_codified_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_authority_kernel__hindu_codified_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__hindu_codified_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__hindu_codified_reading_tests).
:- end_tests(marriage_authority_kernel__hindu_codified_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) is moderate: real coordination benefit exists (uniform rules replacing fragmented custom) but genuine asymmetric extraction persists in unreformed corners of the statute and in the definitional overreach into non-consenting communities. Suppression (0.48) reflects the combination of formal state enforcement (moderate) plus informal social enforcement of caste/community boundaries that the statute does not counteract. Theater ratio is low-moderate (0.28) and rising: the amendments (1976 and after) represent real functional change, but an increasing share of the apparatus (public reform announcements, symbolic litigation) now performs progress that trails the underlying gender-equity gap. Both accessibility_collapse and resistance are mid-range (0.55/0.45), consistent with a codified system where alternatives exist in principle (special marriage act, migration to a different personal law via conversion or civil marriage) but are costly and socially penalized, and where resistance (feminist litigation, minority reclassification suits, constitutional challenges) has grown substantially since 1955 as documented in the coercion grid, where resistance climbs across all four levels while accessibility_collapse and suppression fall — reform is real, not merely theatrical, even as theater_ratio itself rises.
 *
 * PERSPECTIVAL GAP:
 *   From the state judiciary's seat, codification is straightforward coordination: a single interpretable text replacing chaotic custom. From the seat of hindu_women_seeking_divorce or religious_minorities_misclassified_as_hindu, the same text is an imposed framework whose exit costs are high and whose definitional scope was set without their consent. The engine should compute these seats to different types from the same structural facts — that divergence is the point, not an error.
 *
 * DIRECTIONALITY LOGIC:
 *   hindu_male_householders and the reform lobby sit toward the beneficiary end: the former through residual asymmetric provisions, the latter through institutional standing gained from the codification project's ongoing existence as a reform target. hindu_women_seeking_divorce, inter_caste_couples, and religious_minorities_misclassified_as_hindu sit toward the target end: they bear the statute's costs (procedural, definitional, or enforcement-gap costs) with constrained or trapped exit. The state judiciary is treated as agenda_setter/analytical rather than beneficiary or victim — it administers the arrangement without directly collecting rents from it, though its institutional continuity depends on personal law codes remaining a going judicial concern.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (fragmented, inconsistent, often regressive customary Hindu law) was substantially solved by 1955 codification and subsequent amendment — this prevents mislabeling the entire arrangement as pure extraction. But the tangled_rope classification is warranted because genuine coordination (uniform, judicially enforceable rules) coexists with genuine asymmetric extraction (definitional overreach into non-consenting minority communities, residual gender-inequitable provisions, and an enforcement gap on inter-caste marriage that the state does not close). Neither a pure rope reading nor a pure snare reading fits; both a coordination function and identifiable victims must be named, which the schema's tangled_rope gate correctly requires.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    codification_as_genuine_reform_vs_containment,
    'Was 1955 codification a genuine emancipatory reform that improved on pre-existing custom, or a containment strategy that froze reform at a politically negotiated midpoint to forestall more radical uniform civil code demands?',
    'Comparative analysis of legislative debate records (constituent assembly and parliamentary debates 1948-1955) against contemporaneous feminist and Dalit organizing demands, to establish whether the final statute represented the maximum reform available or a deliberate compromise below what was achievable.',
    'If containment, the coordination function is partly cover for preserving caste/patriarchal structures under a modernized label, pushing the classification further toward snare; if genuine reform ceiling, the tangled_rope reading with declining extraction over time is well-supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(codification_as_genuine_reform_vs_containment, conceptual, 'Whether codification was reform or negotiated containment of more radical demands.').

omega_variable(
    definitional_scope_consent_ambiguity,
    'Does the statutory inclusion of Buddhists, Jains, and Sikhs under ''Hindu'' for the Act''s purposes reflect a genuine historical/legal continuity these communities largely accept in practice, or an imposed classification against their self-understanding?',
    'Survey of actual reclassification litigation rates and community-level self-identification surveys across the three groups; compare against Sikh personal law reform movements'' explicit demands for separate codification.',
    'High rates of self-identification as distinct combined with high reclassification-suit friction would support treating these communities more strongly as victims of definitional overreach; low friction and general acceptance would soften that reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(definitional_scope_consent_ambiguity, empirical, 'Whether definitional inclusion of allied religions is accepted continuity or imposed classification.').

omega_variable(
    sibling_reading_gender_equity_ranking_stability,
    'Is the claimed structural delta (moderate gender equity — better than Muslim, worse than secular) stable across doctrinal areas, or does it invert for specific provisions (e.g. maintenance under CrPC 125 applies more uniformly across religions than divorce grounds)?',
    'Provision-by-provision comparison across the five sibling readings'' statutes and case law on divorce grounds, maintenance, custody, and inheritance.',
    'If the ranking inverts for specific provisions, the single scalar gender-equity comparison implied by the kernel contest oversimplifies; each reading''s ε may need finer within-provision decomposition in future stories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_gender_equity_ranking_stability, empirical, 'Whether the cross-reading gender-equity ranking holds uniformly or varies by legal provision.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__hindu_codified_reading, 1955, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1955, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 1955, 0.15).
narrative_ontology:measurement(marr_tr_t1970, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 1970, 0.18).
narrative_ontology:measurement(marr_tr_t1985, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 1985, 0.22).
narrative_ontology:measurement(marr_tr_t2000, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 2000, 0.24).
narrative_ontology:measurement(marr_tr_t2015, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 2015, 0.26).
narrative_ontology:measurement(marr_tr_t2025, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(marr_be_t1955, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 1955, 0.58).
narrative_ontology:measurement(marr_be_t1970, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 1970, 0.52).
narrative_ontology:measurement(marr_be_t1985, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 1985, 0.47).
narrative_ontology:measurement(marr_be_t2000, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 2000, 0.45).
narrative_ontology:measurement(marr_be_t2015, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 2015, 0.43).
narrative_ontology:measurement(marr_be_t2025, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1955, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 1955, 0.6).
narrative_ontology:measurement(marr_su_t1970, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 1970, 0.55).
narrative_ontology:measurement(marr_su_t1985, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 1985, 0.52).
narrative_ontology:measurement(marr_su_t2000, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 2000, 0.5).
narrative_ontology:measurement(marr_su_t2015, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 2015, 0.49).
narrative_ontology:measurement(marr_su_t2025, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 2025, 0.48).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1955, tn=2025
narrative_ontology:measurement(marr_grid_01, marriage_authority_kernel__hindu_codified_reading, accessibility_collapse(class), 1955, 0.7).
narrative_ontology:measurement(marr_grid_02, marriage_authority_kernel__hindu_codified_reading, accessibility_collapse(class), 2025, 0.58).
narrative_ontology:measurement(marr_grid_03, marriage_authority_kernel__hindu_codified_reading, accessibility_collapse(individual), 1955, 0.75).
narrative_ontology:measurement(marr_grid_04, marriage_authority_kernel__hindu_codified_reading, accessibility_collapse(individual), 2025, 0.6).
narrative_ontology:measurement(marr_grid_05, marriage_authority_kernel__hindu_codified_reading, accessibility_collapse(organizational), 1955, 0.6).
narrative_ontology:measurement(marr_grid_06, marriage_authority_kernel__hindu_codified_reading, accessibility_collapse(organizational), 2025, 0.45).
narrative_ontology:measurement(marr_grid_07, marriage_authority_kernel__hindu_codified_reading, accessibility_collapse(structural), 1955, 0.65).
narrative_ontology:measurement(marr_grid_08, marriage_authority_kernel__hindu_codified_reading, accessibility_collapse(structural), 2025, 0.55).
narrative_ontology:measurement(marr_grid_09, marriage_authority_kernel__hindu_codified_reading, resistance(class), 1955, 0.25).
narrative_ontology:measurement(marr_grid_10, marriage_authority_kernel__hindu_codified_reading, resistance(class), 2025, 0.45).
narrative_ontology:measurement(marr_grid_11, marriage_authority_kernel__hindu_codified_reading, resistance(individual), 1955, 0.15).
narrative_ontology:measurement(marr_grid_12, marriage_authority_kernel__hindu_codified_reading, resistance(individual), 2025, 0.35).
narrative_ontology:measurement(marr_grid_13, marriage_authority_kernel__hindu_codified_reading, resistance(organizational), 1955, 0.3).
narrative_ontology:measurement(marr_grid_14, marriage_authority_kernel__hindu_codified_reading, resistance(organizational), 2025, 0.55).
narrative_ontology:measurement(marr_grid_15, marriage_authority_kernel__hindu_codified_reading, resistance(structural), 1955, 0.2).
narrative_ontology:measurement(marr_grid_16, marriage_authority_kernel__hindu_codified_reading, resistance(structural), 2025, 0.4).
narrative_ontology:measurement(marr_grid_17, marriage_authority_kernel__hindu_codified_reading, stakes_inflation(class), 1955, 0.5).
narrative_ontology:measurement(marr_grid_18, marriage_authority_kernel__hindu_codified_reading, stakes_inflation(class), 2025, 0.4).
narrative_ontology:measurement(marr_grid_19, marriage_authority_kernel__hindu_codified_reading, stakes_inflation(individual), 1955, 0.65).
narrative_ontology:measurement(marr_grid_20, marriage_authority_kernel__hindu_codified_reading, stakes_inflation(individual), 2025, 0.5).
narrative_ontology:measurement(marr_grid_21, marriage_authority_kernel__hindu_codified_reading, stakes_inflation(organizational), 1955, 0.25).
narrative_ontology:measurement(marr_grid_22, marriage_authority_kernel__hindu_codified_reading, stakes_inflation(organizational), 2025, 0.3).
narrative_ontology:measurement(marr_grid_23, marriage_authority_kernel__hindu_codified_reading, stakes_inflation(structural), 1955, 0.3).
narrative_ontology:measurement(marr_grid_24, marriage_authority_kernel__hindu_codified_reading, stakes_inflation(structural), 2025, 0.35).
narrative_ontology:measurement(marr_grid_25, marriage_authority_kernel__hindu_codified_reading, suppression(class), 1955, 0.6).
narrative_ontology:measurement(marr_grid_26, marriage_authority_kernel__hindu_codified_reading, suppression(class), 2025, 0.5).
narrative_ontology:measurement(marr_grid_27, marriage_authority_kernel__hindu_codified_reading, suppression(individual), 1955, 0.7).
narrative_ontology:measurement(marr_grid_28, marriage_authority_kernel__hindu_codified_reading, suppression(individual), 2025, 0.55).
narrative_ontology:measurement(marr_grid_29, marriage_authority_kernel__hindu_codified_reading, suppression(organizational), 1955, 0.45).
narrative_ontology:measurement(marr_grid_30, marriage_authority_kernel__hindu_codified_reading, suppression(organizational), 2025, 0.4).
narrative_ontology:measurement(marr_grid_31, marriage_authority_kernel__hindu_codified_reading, suppression(structural), 1955, 0.55).
narrative_ontology:measurement(marr_grid_32, marriage_authority_kernel__hindu_codified_reading, suppression(structural), 2025, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__hindu_codified_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(marriage_authority_kernel__hindu_codified_reading, 0.12).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel__muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel__christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel__parsi_communal_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel__secular_civil_reading).

% DUAL FORMULATION NOTE:
% This story is one of five sibling readings of marriage_authority_kernel, each authored as a separate ε-invariant constraint per the decomposition principle. hindu_codified_reading is frequently cited in Indian political and judicial discourse as evidence either for the feasibility of a Uniform Civil Code (secular_civil_reading direction) or as sufficient reform without further uniformity (a containment argument used against secular_civil_reading). Its existence and reform trajectory create downstream legitimacy pressure on muslim_shariat_reading (used as a comparative benchmark in Uniform Civil Code debates) without logically foreclosing any sibling reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_authority_kernel__hindu_codified_reading, organized, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
