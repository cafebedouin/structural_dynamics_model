% ============================================================================
% CONSTRAINT STORY: marriage_authority__federalist_millet_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__federalist_millet_reading, []).

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
 *   constraint_id: marriage_authority__federalist_millet_reading
 *   human_readable: Consociational Fragmentation of Marriage Authority (Federalist/Millet Reading)
 *   domain: legal/constitutional/comparative family law
 *
 * SUMMARY:
 *   In multi-communal polities descended from the millet model, authority
 *   over marriage and divorce is deliberately fragmented: each recognized
 *   community's personal law governs its members' family formation,
 *   administered by communal institutions and enforced by state courts, while
 *   the constitutional order entrenches the fragmentation so that no
 *   majoritarian coalition can legislate a single code over dissenting
 *   segments. This story authors that standing arrangement from the
 *   federalist/millet reading's seat: the fragmentation is a consociational
 *   anti-tyranny design — an elite bargain among communal leaderships that
 *   trades mutual non-imposition for communal self-rule, with legislative
 *   paralysis over a uniform code functioning as the design's stability
 *   feature rather than its failure. The claim and the metrics are authored
 *   independently: the reading claims rope (protective coordination with
 *   net-benefiting participants), while the metrics record what the
 *   arrangement's operation shows under this reading's own lights — low but
 *   non-zero extraction concentrated on individuals inside communal
 *   jurisdiction, structural rather than enforcement-driven suppression, and
 *   a real, low-theater coordination function.
 *
 * KEY AGENTS:
 *   - minority_communities: primary beneficiary (organized / identity_locked) — protected by the settlement from majoritarian marriage-law imposition; exit from community membership is not a live option
 *   - communal_religious_leaderships: agenda-setting elite bargainer (organized / identity_locked) — administers communal marriage jurisdiction, controls the validity gates, collects adjudicative authority and status
 *   - majority_religious_establishment: secondary beneficiary (powerful / identity_locked) — retains jurisdiction over the majority's members and is shielded from both secular unification and internal statutory reform
 *   - women_under_personal_law: primary payer (powerless / constrained) — governed by birth-community law without individual consent; bear unequal divorce, maintenance, and remarriage terms
 *   - interfaith_marriage_couples: payer (moderate / mobile) — no communal law covers both partners; bear procedural friction and social pressure, with cross-border marriage as a costly partial exit
 *   - secular_individual_rights_advocates: excluded voice (moderate / mobile) — individual-consent and equality claims have no seat in the elite bargain
 *   - apex_constitutional_court: analytical observer (institutional / analytical) — reshapes terms at the margins case by case while leaving the architecture standing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__federalist_millet_reading, 0.22).
domain_priors:suppression_score(marriage_authority__federalist_millet_reading, 0.3).
domain_priors:theater_ratio(marriage_authority__federalist_millet_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__federalist_millet_reading, rope).
narrative_ontology:human_readable(marriage_authority__federalist_millet_reading, "Consociational Fragmentation of Marriage Authority (Federalist/Millet Reading)").
narrative_ontology:topic_domain(marriage_authority__federalist_millet_reading, "legal/constitutional/comparative family law").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__federalist_millet_reading, 'f6e845fa-b47d-44e8-bc37-f1d2e6c83d5d').
narrative_ontology:cs_kernel_codification('f6e845fa-b47d-44e8-bc37-f1d2e6c83d5d', formalized).
narrative_ontology:cs_authority_grounding('f6e845fa-b47d-44e8-bc37-f1d2e6c83d5d', lineage).
narrative_ontology:cs_interpretation_layer_present('f6e845fa-b47d-44e8-bc37-f1d2e6c83d5d').
narrative_ontology:cs_reading_relation('f6e845fa-b47d-44e8-bc37-f1d2e6c83d5d', marriage_authority__communal_autonomy_reading, coexists_with).
narrative_ontology:cs_reading_relation('f6e845fa-b47d-44e8-bc37-f1d2e6c83d5d', marriage_authority__secularist_reading, influences).
narrative_ontology:cs_reading_relation('f6e845fa-b47d-44e8-bc37-f1d2e6c83d5d', marriage_authority__gender_rights_reading, influences).
narrative_ontology:cs_reading_relation('f6e845fa-b47d-44e8-bc37-f1d2e6c83d5d', marriage_authority__judicial_harmonization_reading, influences).
narrative_ontology:cs_axiom('f6e845fa-b47d-44e8-bc37-f1d2e6c83d5d', foundational, segmental_veto_prevents_majoritarian_domination).
narrative_ontology:cs_axiom_status(segmental_veto_prevents_majoritarian_domination, holdable).
narrative_ontology:cs_axiom_grounding('f6e845fa-b47d-44e8-bc37-f1d2e6c83d5d', segmental_veto_prevents_majoritarian_domination, instrumental).
narrative_ontology:cs_axiom('f6e845fa-b47d-44e8-bc37-f1d2e6c83d5d', foundational, plural_marriage_jurisdiction_is_final_settlement).
narrative_ontology:cs_axiom_status(plural_marriage_jurisdiction_is_final_settlement, holdable).
narrative_ontology:cs_axiom_grounding('f6e845fa-b47d-44e8-bc37-f1d2e6c83d5d', plural_marriage_jurisdiction_is_final_settlement, conventional).
narrative_ontology:cs_reference_frame('f6e845fa-b47d-44e8-bc37-f1d2e6c83d5d', consociational_segmental_autonomy).
narrative_ontology:cs_drift_state('f6e845fa-b47d-44e8-bc37-f1d2e6c83d5d', contemporary_reform_crisis_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('f6e845fa-b47d-44e8-bc37-f1d2e6c83d5d', '').
narrative_ontology:cs_kernel_id(marriage_authority__federalist_millet_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__federalist_millet_reading, minority_communities).
narrative_ontology:constraint_beneficiary(marriage_authority__federalist_millet_reading, communal_religious_leaderships).
narrative_ontology:constraint_beneficiary(marriage_authority__federalist_millet_reading, majority_religious_establishment).
narrative_ontology:constraint_victim(marriage_authority__federalist_millet_reading, women_under_personal_law).
narrative_ontology:constraint_victim(marriage_authority__federalist_millet_reading, interfaith_marriage_couples).
narrative_ontology:constraint_vindicates(marriage_authority__federalist_millet_reading, consociational_stability_doctrine).
narrative_ontology:constraint_vindicates(marriage_authority__federalist_millet_reading, segmental_autonomy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live as recognized segments of the polity whose marriage and divorce are governed by their own communal law, administered by their own institutions and enforced by state courts. The settlement guarantees that no majority coalition can replace their law with the majority's norms. Leaving the community is not a practical option — membership is ascriptive and bound to family, worship, and status — so their protection depends on the settlement holding.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, minority_communities, beneficiary,
    organized, generational, identity_locked, national).

% Bargain with the state and with each other over the terms of communal jurisdiction, administer marriage and divorce under their respective laws, and control the gates: whose marriage is valid, whose divorce is recognized, who remains in good standing. The settlement flows adjudicative authority and status to them, and they mobilize their communities whenever the terms are contested.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, communal_religious_leaderships, agenda_setter,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__federalist_millet_reading, communal_religious_leaderships, beneficiary).

% Administers the majority community's marriage law over its much larger membership and benefits from the same entrenchment that protects minority establishments: a uniform secular code would dissolve its jurisdiction as surely as it would dissolve the minorities'. The settlement also shields it from internal reformers who would modernize majority family law by statute.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, majority_religious_establishment, beneficiary,
    powerful, generational, identity_locked, national).

% Marry and divorce under their community's law by birth, without having consented to it as individuals. Where communal provisions are unequal — in divorce grounds, maintenance, remarriage, custody — they bear the difference personally. Opting out means invoking civil-marriage routes that carry social cost, family rupture, and in some settlements near-total unavailability.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, women_under_personal_law, payer,
    powerless, biographical, constrained, national).

% Seek to marry across community lines, where no single communal law covers both partners. They navigate civil-marriage procedures, waiting periods, registration hurdles, and family or community pressure; some marry abroad and seek recognition at home, with outcomes that vary by jurisdiction.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, interfaith_marriage_couples, payer,
    moderate, biographical, mobile, national).

% Argue that marriage rules should follow individual consent and constitutional equality rather than birth community. They are not a segment in the bargain — the settlement's table seats communal leaderships, not individual-rights claimants — so their program advances, when it advances at all, through courts and electoral politics rather than through the bargain itself.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, secular_individual_rights_advocates, excluded,
    moderate, biographical, mobile, national).

% Hears challenges to communal provisions, applies constitutional guarantees case by case, and declines or defers wholesale restructuring absent legislation. Its incremental rulings reshape the terms of communal jurisdiction at the margins while leaving the settlement's architecture standing.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, apex_constitutional_court, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority__federalist_millet_reading, communal_religious_leaderships).
narrative_ontology:fixing_cost_class(marriage_authority__federalist_millet_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the inter-communal recognition problem: marriage and divorce rules are settled within each community under its own law, so no segment must live under another segment's norms and no cross-segment majority can legislate a single code over dissenting segments. Jurisdiction follows community membership, and an elite bargain among communal leaderships maintains mutual non-imposition.
% TRANSFER_FUNCTION: Moves adjudicative authority and gatekeeping over marriage and divorce from a centralized legislature to communal leaderships; moves compliance with communal norms from individuals — disproportionately women — to their birth community's jurisdiction; moves protection from majoritarian imposition to minority communities.
% ABSENT_VOICES: Individual community members — disproportionately women — have no seat: the bargain is struck leadership-to-leadership, and individual-rights claimants (secular reformers, would-be civil spouses) stand outside the consociational table. They would object that jurisdiction should follow individual consent and constitutional equality rather than birth community; their objection currently travels only through courts and electoral politics, not through the bargain itself.
% DISAPPEARANCE_RATIONALE: If the fragmentation and its entrenchment vanished overnight — a single statutory marriage code imposed — minority communities would wake under the majority's family law, communal leaderships would lose jurisdiction and gatekeeping, recognition disputes would erupt around every mixed marriage, and the elite bargain that channels inter-communal conflict over family law would be replaced by direct majoritarian-minoritarian contest.
% FOUNDING_PROBLEM: At the founding settlements (the Ottoman millet arrangement and its modern constitutional descendants), the problem was how a multi-communal polity could share a state without the majority's identity-laden family law absorbing the minorities: marriage law touches worship, status, and community survival, so a unified code read as domination. Fragmentation with mutual non-imposition was the negotiated answer.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the beneficiary set: founding-era constitutional debates and negotiation records document the anti-imposition design intent; recurring majoritarian uniform-code campaigns attest that the imposition risk is real (the arrangement's opponents confirm the problem it solves); and state courts' repeated refusal to restructure personal law absent legislation attests the settlement's continuing operative function. Minority community organizations attest it from the beneficiary side; the external attestation comes from the reform campaigns and the judicial record.
narrative_ontology:disappearance_verdict(marriage_authority__federalist_millet_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__federalist_millet_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__federalist_millet_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_authority__federalist_millet_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__federalist_millet_reading, 0.22, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__federalist_millet_reading_tests).
:- end_tests(marriage_authority__federalist_millet_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored low (0.22) because under this reading's lights the arrangement's dominant flow is protective — minorities keep their law, no segment imposes on another — with real but bounded costs concentrated on individuals governed by communal jurisdiction without individual consent. Suppression (0.30) is structural, not enforcement-driven: the settlement requires no standing coercive machinery (communities self-administer; state courts act as backstop, hence requires_active_enforcement is false), but communal jurisdiction is compulsory for members and opt-out routes are socially costly. Theater is low (0.12): the coordination function — inter-communal non-imposition — is performed, not performed-at. Accessibility_collapse is low (0.25): civil-marriage acts, judicially recognized foreign marriages, and migration keep alternatives partly open, though unevenly across settlements. Resistance (0.30) is real — women's-rights and secular-reform campaigns contest the terms — while communal mobilization defends the architecture. All three tracked series share one time grid (t=0,10,20,30,40,50,60,70). Base_extractiveness and theater_ratio run nearly flat, while suppression_requirement oscillates with reform crises: an equality ruling or uniform-code push spikes the coercive overhead needed to hold the bargain, communal mobilization follows, the crisis settles (often by legislative reversal or a narrowed ruling), and overhead decays. The cycle is a side effect of external rights-claim pressure rather than an engineered reinforcement schedule, though elites do harvest each crisis to re-entrench their gatekeeping — a dynamic the omegas track.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/agenda seats should compute differently. From the communal leaderships' seat the arrangement is the peace treaty they administer: jurisdiction, status, and veto flow to them, and the anti-imposition guarantee is its own reward. From women_under_personal_law the same architecture is compulsory jurisdiction with unequal terms and no individual seat at the bargain. From minority_communities as corporate segments the settlement is protection; from their own individual members it can be subjection with a protective wrapper. The majority establishment's seat adds a second asymmetry: it benefits from the settlement not only against external unification but against internal reformers, so its stake in paralysis exceeds the minorities' stake in protection. Same-level differentiation: minority and majority leaderships hold structurally similar offices but different dependence — if the bargain fails, the minority establishment's members face majoritarian norms while the majority establishment's members face only their own reformers. The engine computes these divergences from power, exit, and role data; the story's rope claim is this reading's aggregate assessment and does not adjudicate the seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real flows: minority_communities receive the anti-imposition guarantee (d near the beneficiary pole); communal_religious_leaderships receive jurisdiction, gatekeeping, and status (also near the beneficiary pole, with the agenda_setter role capturing that they administer what they collect); majority_religious_establishment receives jurisdictional preservation against both external and internal unification. Victim declarations map to concentrated costs: women_under_personal_law bear unequal communal terms with constrained exit (near the target pole); interfaith_marriage_couples bear friction with mobile partial exit (damped toward symmetry by that exit). No directionality_overrides are declared: the derivation from beneficiary/victim data, power atoms, and exit options captures these relationships without correction. National spatial scope applies the engine's modest verification amplification; suppression is authored as a raw structural property and is not scaled by power or scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — holding a multi-communal polity together without letting the majority's family law absorb the minorities — remains live: majoritarian uniform-code campaigns recur, and communal mobilization against them recurs in step, which is the design operating rather than a mandate outliving its function; no sunset clause is authored because the settlement is not transitional by its own design. The classification discipline matters in both directions: reading the arrangement as pure extraction erases the genuine coordination that holds the polity's family law together; reading it as pure inherited tradition erases the elite bargain that actually maintains it and the individuals who pay inside it. The rope claim, with payer seats left to the engine's per-seat computation, keeps both errors visible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This story instantiates only the federalist_millet_reading of the marriage_authority kernel; which structural elements do the sibling readings relocate, and where exactly is the disagreement located?',
    'Compile the four sibling stories and compare, over the same standing arrangement: beneficiary/victim sets, epsilon, and per-seat classifications. The disagreement is located in (a) what grounds communal jurisdiction — the elite bargain (this reading) versus communal tradition itself; (b) whether pluralism is a final settlement or a transitional anomaly; (c) whether the unit of assessment is the segment or the individual member; (d) whether revision runs through legislation or case-by-case adjudication.',
    'Adopting a sibling''s grounding relocates beneficiaries and victims: tradition-grounding removes the elite-bargain seat; the transitional reading converts minority_communities from protected beneficiaries to holdouts awaiting unification; the equality-centered reading promotes women_under_personal_law from priced cost-bearers to primary victims and raises epsilon sharply over the same referent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: this constraint is one reading of the marriage_authority kernel; sibling readings change the beneficiary/victim structure and epsilon over the same referent.').

omega_variable(
    anti_tyranny_function_vs_elite_rent,
    'Is the anti-tyranny justification the arrangement''s operative function, or the rhetorical cover under which communal elites retain gatekeeping authority over their members'' marriages?',
    'Founding-debate records plus elite behavior when reform is proposed: if communal leaderships accept equality-enhancing reforms that leave jurisdictional gates intact but resist any dilution of the gates themselves, rent retention is weight-bearing rather than incidental.',
    'If rent retention dominates, the arrangement slides toward a hybrid profile from the payer seats even under this reading''s lights — the coordination is real, but extraction is layered onto it through the same gatekeeping structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(anti_tyranny_function_vs_elite_rent, empirical, 'Whether the consociational justification or elite gatekeeping rent is the operative function.').

omega_variable(
    legislative_paralysis_valence,
    'Is legislative paralysis — the persistent failure to enact a uniform marriage code — a stability feature of the anti-tyranny design, or evidence that communal elites hold a veto that blocks majority-community reformers and internal dissent alike?',
    'Analyze the reform-attempt record: whether paralysis tracks minority-protection needs or elite gatekeeping interests — specifically, whether reforms that would reduce elite authority fail while functionally equivalent reforms leaving elite gates intact proceed.',
    'The feature reading supports the rope claim; the veto reading supports a hybrid classification with the leaderships as extractive agenda-setters, since the same paralysis would then serve concentrated elite interests against diffuse individual ones.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legislative_paralysis_valence, empirical, 'Whether non-legislation is the anti-tyranny mechanism working or an elite veto at work.').

omega_variable(
    unit_of_benefit_community_vs_member,
    'Does the anti-imposition benefit accrue to communities as corporate segments while the arrangement''s costs concentrate on intra-community individuals, disproportionately women?',
    'Distributional analysis within communities: who is seated when the bargain is renegotiated, and who bears the costs and benefits of each reform episode.',
    'If benefits accrue corporately while costs concentrate on members, the beneficiary declarations understate payer-seat extraction and per-seat classifications will diverge sharply from the story-level assessment — the engine''s seat-level output becomes the decisive measurement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(unit_of_benefit_community_vs_member, conceptual, 'Whether the unit of benefit is the segment or the individual, and where costs land.').

omega_variable(
    civil_exit_discipline_effect,
    'Do civil-marriage opt-out routes and cross-border marriage arbitrage discipline communal establishments into more consensual jurisdiction, or do they hollow the settlement by draining the most mobile while leaving the least mobile in place?',
    'Compare communities and periods with and without accessible opt-outs: endogamy rates, elite responsiveness to internal reform demands, and the socioeconomic profile of who actually uses the exits.',
    'If exit is regressive, the aggregate suppression scalar understates the effective constraint on those who cannot use the exits — payer-seat suppression is higher than the story-level 0.30 while beneficiary-side pressure to reform weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civil_exit_discipline_effect, empirical, 'Whether exit routes discipline the bargain or stratify it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__federalist_millet_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority__federalist_millet_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(marr_tr_t0, observed).
narrative_ontology:measurement(marr_tr_t10, marriage_authority__federalist_millet_reading, theater_ratio, 10, 0.09).
narrative_ontology:measurement_basis(marr_tr_t10, observed).
narrative_ontology:measurement(marr_tr_t20, marriage_authority__federalist_millet_reading, theater_ratio, 20, 0.09).
narrative_ontology:measurement_basis(marr_tr_t20, observed).
narrative_ontology:measurement(marr_tr_t30, marriage_authority__federalist_millet_reading, theater_ratio, 30, 0.12).
narrative_ontology:measurement_basis(marr_tr_t30, observed).
narrative_ontology:measurement(marr_tr_t40, marriage_authority__federalist_millet_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement_basis(marr_tr_t40, observed).
narrative_ontology:measurement(marr_tr_t50, marriage_authority__federalist_millet_reading, theater_ratio, 50, 0.1).
narrative_ontology:measurement_basis(marr_tr_t50, observed).
narrative_ontology:measurement(marr_tr_t60, marriage_authority__federalist_millet_reading, theater_ratio, 60, 0.12).
narrative_ontology:measurement_basis(marr_tr_t60, observed).
narrative_ontology:measurement(marr_tr_t70, marriage_authority__federalist_millet_reading, theater_ratio, 70, 0.12).
narrative_ontology:measurement_basis(marr_tr_t70, observed).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority__federalist_millet_reading, base_extractiveness, 0, 0.19).
narrative_ontology:measurement_basis(marr_be_t0, observed).
narrative_ontology:measurement(marr_be_t10, marriage_authority__federalist_millet_reading, base_extractiveness, 10, 0.2).
narrative_ontology:measurement_basis(marr_be_t10, observed).
narrative_ontology:measurement(marr_be_t20, marriage_authority__federalist_millet_reading, base_extractiveness, 20, 0.2).
narrative_ontology:measurement_basis(marr_be_t20, observed).
narrative_ontology:measurement(marr_be_t30, marriage_authority__federalist_millet_reading, base_extractiveness, 30, 0.22).
narrative_ontology:measurement_basis(marr_be_t30, observed).
narrative_ontology:measurement(marr_be_t40, marriage_authority__federalist_millet_reading, base_extractiveness, 40, 0.2).
narrative_ontology:measurement_basis(marr_be_t40, observed).
narrative_ontology:measurement(marr_be_t50, marriage_authority__federalist_millet_reading, base_extractiveness, 50, 0.21).
narrative_ontology:measurement_basis(marr_be_t50, observed).
narrative_ontology:measurement(marr_be_t60, marriage_authority__federalist_millet_reading, base_extractiveness, 60, 0.22).
narrative_ontology:measurement_basis(marr_be_t60, observed).
narrative_ontology:measurement(marr_be_t70, marriage_authority__federalist_millet_reading, base_extractiveness, 70, 0.22).
narrative_ontology:measurement_basis(marr_be_t70, observed).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority__federalist_millet_reading, suppression_requirement, 0, 0.24).
narrative_ontology:measurement_basis(marr_su_t0, observed).
narrative_ontology:measurement(marr_su_t10, marriage_authority__federalist_millet_reading, suppression_requirement, 10, 0.27).
narrative_ontology:measurement_basis(marr_su_t10, observed).
narrative_ontology:measurement(marr_su_t20, marriage_authority__federalist_millet_reading, suppression_requirement, 20, 0.3).
narrative_ontology:measurement_basis(marr_su_t20, observed).
narrative_ontology:measurement(marr_su_t30, marriage_authority__federalist_millet_reading, suppression_requirement, 30, 0.38).
narrative_ontology:measurement_basis(marr_su_t30, observed).
narrative_ontology:measurement(marr_su_t40, marriage_authority__federalist_millet_reading, suppression_requirement, 40, 0.29).
narrative_ontology:measurement_basis(marr_su_t40, observed).
narrative_ontology:measurement(marr_su_t50, marriage_authority__federalist_millet_reading, suppression_requirement, 50, 0.26).
narrative_ontology:measurement_basis(marr_su_t50, observed).
narrative_ontology:measurement(marr_su_t60, marriage_authority__federalist_millet_reading, suppression_requirement, 60, 0.34).
narrative_ontology:measurement_basis(marr_su_t60, observed).
narrative_ontology:measurement(marr_su_t70, marriage_authority__federalist_millet_reading, suppression_requirement, 70, 0.3).
narrative_ontology:measurement_basis(marr_su_t70, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__federalist_millet_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, marriage_authority__communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, marriage_authority__secularist_reading).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, marriage_authority__gender_rights_reading).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, marriage_authority__judicial_harmonization_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'personal law / marriage-authority pluralism' decomposes into five structurally distinct claims per the epsilon-invariance principle: tradition-grounded communal autonomy; deliberate consociational fragmentation (this file); transitional pluralism awaiting unification; intra-community equality contest; and case-by-case judicial harmonization. Each carries its own epsilon, beneficiary/victim structure, and claimed type. This file links all four siblings: the communal_autonomy reading is the upstream ally whose tradition-grounding this reading's bargain entrenches; the secularist and gender_rights readings are downstream programs whose operating conditions the entrenched bargain reshapes (raising unification's cost; channeling equality claims into courts); the judicial_harmonization reading is the incremental mechanism operating inside the space this reading's legislative paralysis leaves open.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
