% ============================================================================
% CONSTRAINT STORY: woman_category__sex_biology_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_category__sex_biology_reading, []).

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
 *   constraint_id: woman_category__sex_biology_reading
 *   human_readable: Sex-Biology Determination of the Woman Category (Typical-Case Reading)
 *   domain: political philosophy / law / social policy / bioethics
 *
 * SUMMARY:
 *   This story authors ONE reading of a contested kernel. The kernel is the
 *   boundary definition of the category 'woman' as it operates in law, sport,
 *   services, and statistics. The reading instantiated here holds that
 *   membership is determined by chromosomal, anatomical, and reproductive
 *   biology — 'woman' = adult human female, typical case (XX chromosomes,
 *   female reproductive anatomy). The standing arrangement under contest is
 *   the institutional regime that gates women's categories, single-sex
 *   protections, and sex-disaggregated instruments on this biological
 *   criterion and maintains it with testing, adjudication, and arbitration.
 *   The reading delivers genuine protection to a class with sexed
 *   vulnerabilities, and the same enforcement machinery imposes severe,
 *   asymmetric costs on boundary populations: transgender women are excluded
 *   outright, and DSD/intersex women are tested, medicated, and sometimes
 *   barred. Sibling readings (gender_identity, intersex_accommodation)
 *   instantiate different constraints with different victim sets; they are
 *   NOT described inside this constraint. Claim and metrics are independent
 *   authored facts: the type is claimed as tangled_rope because both a
 *   coordination function and asymmetric extraction run through the same
 *   enforced structure; the metrics are authored as descriptively true of how
 *   the arrangement has actually operated, including its enforcement ratchet.
 *   KEY AGENTS (by structural relationship): - transgender_women: Primary
 *   target (moderate/trapped) — excluded from the women's category across
 *   sport, services, and legal recognition; bears the transfer -
 *   dsd_intersex_women: Concentrated target (powerless/trapped) — tested,
 *   medicated, or barred by the eligibility apparatus despite living as women
 *   - cisgender_female_population: Primary beneficiary
 *   (organized/constrained) — receives the protected categories; also absorbs
 *   testing spillover - sports_governing_bodies: Agenda setter and
 *   institutional beneficiary (institutional/arbitrage) — writes and rewrites
 *   the criterion, collects administrability and authority -
 *   sex_based_statistics_agencies: Secondary beneficiary
 *   (institutional/mobile) — decades of comparable sex-disaggregated data
 *   rest on the stable binary - single_sex_service_regulators: National
 *   agenda setter (institutional/arbitrage) — decides admission to refuges,
 *   prison wards, hospital wards - gender_identity_advocacy_organizations:
 *   Excluded challenger (organized/arbitrage) — campaigns from outside the
 *   rule-setting rooms - human_rights_treaty_bodies: Analytical observer
 *   (institutional/analytical) — adjudicates rights compliance, compels
 *   rewrites without setting rules
 *
 * KEY AGENTS:
 *   - transgender_women: primary target (moderate/trapped) — bears the category exclusion across all enforcement sites
 *   - dsd_intersex_women: concentrated target (powerless/trapped) — bears testing, mandated medication, and disqualification
 *   - cisgender_female_population: primary beneficiary (organized/constrained) — receives protected categories; carries testing spillover
 *   - sports_governing_bodies: agenda setter with secondary beneficiary position (institutional/arbitrage) — owns and rewrites the criterion
 *   - sex_based_statistics_agencies: secondary beneficiary (institutional/mobile) — longitudinal comparability depends on the binary
 *   - single_sex_service_regulators: agenda setter (institutional/arbitrage) — national admission rules for single-sex services
 *   - gender_identity_advocacy_organizations: excluded challenger (organized/arbitrage) — no seat in rule-setting, presses via courts and legislatures
 *   - human_rights_treaty_bodies: analytical observer (institutional/analytical) — external rights adjudication
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_category__sex_biology_reading, 0.78).
domain_priors:suppression_score(woman_category__sex_biology_reading, 0.75).
domain_priors:theater_ratio(woman_category__sex_biology_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_category__sex_biology_reading, tangled_rope).
narrative_ontology:human_readable(woman_category__sex_biology_reading, "Sex-Biology Determination of the Woman Category (Typical-Case Reading)").
narrative_ontology:topic_domain(woman_category__sex_biology_reading, "political philosophy / law / social policy / bioethics").

domain_priors:requires_active_enforcement(woman_category__sex_biology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_category__sex_biology_reading, '04b4eb94-a3f4-4728-8e65-2e5c28d867b7').
narrative_ontology:cs_kernel_codification('04b4eb94-a3f4-4728-8e65-2e5c28d867b7', formalized).
narrative_ontology:cs_authority_grounding('04b4eb94-a3f4-4728-8e65-2e5c28d867b7', expertise).
narrative_ontology:cs_interpretation_layer_present('04b4eb94-a3f4-4728-8e65-2e5c28d867b7').
narrative_ontology:cs_reading_relation('04b4eb94-a3f4-4728-8e65-2e5c28d867b7', woman_category__gender_identity_reading, forecloses).
narrative_ontology:cs_reading_relation('04b4eb94-a3f4-4728-8e65-2e5c28d867b7', woman_category__intersex_accommodation_reading, influences).
narrative_ontology:cs_axiom('04b4eb94-a3f4-4728-8e65-2e5c28d867b7', foundational, membership_determined_by_biological_sex).
narrative_ontology:cs_axiom_status(membership_determined_by_biological_sex, holdable).
narrative_ontology:cs_axiom_grounding('04b4eb94-a3f4-4728-8e65-2e5c28d867b7', membership_determined_by_biological_sex, empirically_contingent).
narrative_ontology:cs_axiom('04b4eb94-a3f4-4728-8e65-2e5c28d867b7', secondary, bright_line_binary_required_for_protective_governance).
narrative_ontology:cs_axiom_status(bright_line_binary_required_for_protective_governance, holdable).
narrative_ontology:cs_axiom_grounding('04b4eb94-a3f4-4728-8e65-2e5c28d867b7', bright_line_binary_required_for_protective_governance, instrumental).
narrative_ontology:cs_reference_frame('04b4eb94-a3f4-4728-8e65-2e5c28d867b7', natural_binary_sex_classification).
narrative_ontology:cs_drift_state('04b4eb94-a3f4-4728-8e65-2e5c28d867b7', contemporary_dsd_litigation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('04b4eb94-a3f4-4728-8e65-2e5c28d867b7', '').
narrative_ontology:cs_kernel_id(woman_category__sex_biology_reading, woman_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_category__sex_biology_reading, cisgender_female_population).
narrative_ontology:constraint_beneficiary(woman_category__sex_biology_reading, sports_governing_bodies).
narrative_ontology:constraint_beneficiary(woman_category__sex_biology_reading, sex_based_statistics_agencies).
narrative_ontology:constraint_victim(woman_category__sex_biology_reading, transgender_women).
narrative_ontology:constraint_victim(woman_category__sex_biology_reading, dsd_intersex_women).
narrative_ontology:constraint_vindicates(woman_category__sex_biology_reading, sexual_dimorphism_doctrine).
narrative_ontology:constraint_vindicates(woman_category__sex_biology_reading, male_puberty_performance_advantage).
narrative_ontology:constraint_vindicates(woman_category__sex_biology_reading, sex_disaggregated_data_necessity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Are classified female at birth and remain so under the criterion without applying or declaring anything. They receive the protected categories: women's races structured so female-bodied athletes remain competitive, single-sex refuges and prison wards, and decades of sex-disaggregated health data. Elite women additionally carry the spillover of eligibility testing — the apparatus sweeps all female-category athletes, and the historical record includes cisgender champions disqualified by the very tests built to protect the category. They cannot opt out of sex classification and mostly would not want to.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, cisgender_female_population, beneficiary,
    organized, generational, constrained, global).

% Live as women and seek access to women's sports categories, single-sex refuges and prison placement, and legal recognition tied to the category. Under the governing criterion their applications are refused regardless of transition stage, hormone profile, or documentation; the available paths are competing in male or open categories, using mixed services, or abandoning the sought setting altogether. Their advocacy runs through courts, legislatures, and media because the rule-setting bodies offer no seat.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, transgender_women, payer,
    moderate, biographical, trapped, global).

% Women with 46,XY differences of sex development and related variations who were raised and live as women, usually hold female legal documents, and compete in women's events. Eligibility frameworks single them out for testing; those above testosterone thresholds must take suppressive medication to keep competing or change events; some have been barred outright. Test notifications arrive during competition season, results carry public-disclosure risk, and many come from federations with little leverage to contest global rules.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, dsd_intersex_women, payer,
    powerless, biographical, trapped, global).

% Set and rewrite eligibility rules — hyperandrogenism limits, DSD regulations, transgender eligibility policies — commission the medical panels that adjudicate borderline athletes, and defend the rules in arbitration. The criterion gives them administrable rules and institutional authority; arbitration losses and public pressure have already forced multiple rewrites, and each rewrite trades one controversy for another. They can exit any particular formulation of the rule by rewriting it, which they have repeatedly done.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, sports_governing_bodies, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(woman_category__sex_biology_reading, sports_governing_bodies, beneficiary).

% Run censuses, health surveys, and longitudinal cohorts keyed to binary sex recorded at birth. The stable binary lets them compare across decades and detect sex-specific disease and outcome patterns. Adding gender-identity instruments is feasible and some agencies have done it, at the cost of breaking time-series comparability and drawing political attack from one side or the other; the choice remains open to them.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, sex_based_statistics_agencies, beneficiary,
    institutional, generational, mobile, national).

% Issue statutory guidance deciding who may be admitted to women's refuges, prison wings, and hospital wards. Their guidance follows the biological criterion in several jurisdictions; they face litigation and political risk whichever line they draw, and they periodically reverse course under court pressure or change of government.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, single_sex_service_regulators, agenda_setter,
    institutional, generational, arbitrage, national).

% Campaign for identity-based category membership through litigation, treaty-body submissions, and electoral lobbying. They are consulted during public-comment windows but hold no vote in federation councils, arbitration panels, or the drafting rooms where eligibility rules are written; their wins arrive as external overrides from courts and governments rather than as changes made inside the rule-setting process.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, gender_identity_advocacy_organizations, excluded,
    organized, generational, arbitrage, global).

% Review states' and federations' practices against rights treaties, publish findings that DSD regulations and blanket exclusions discriminate, and recommend that the criterion be reopened. They compel responses and their findings have preceded federation rewrites, but they set no rules themselves.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, human_rights_treaty_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(woman_category__sex_biology_reading, diffuse).
narrative_ontology:fixing_cost_class(woman_category__sex_biology_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides an administrable, hard-to-fabricate criterion for gating protected categories and spaces built around sexed vulnerability: women's competitive classes, single-sex refuges and prison placement, and sex-disaggregated medicine and statistics. It solves the collective-action problem of defining a protected class without relying on self-report, which the reading holds gameable in competitive and resource-allocation settings.
% TRANSFER_FUNCTION: Moves category access and its attached resources — competition eligibility, shelter beds, prison ward placement, quota slots, statistical visibility — exclusively to members meeting the biological criterion; moves enforcement costs (testing, disclosure, mandated medication, documentation) onto boundary populations; and moves competitive opportunity away from transgender and DSD women into male or open categories.
% ABSENT_VOICES: Transgender women and DSD athletes hold no seat in the bodies that set the criterion — federation councils, arbitration panels, statute drafting rooms. Affected athletes from low-resource federations are furthest from those tables, and trans service users were absent from several single-sex guidance processes. They object from litigation, treaty submissions, and media rather than from rule-setting seats, which is why their influence arrives as external override rather than internal revision.
% DISAPPEARANCE_RATIONALE: Women's competitive categories, refuge admission rules, prison placement policy, and decades of sex-disaggregated datasets are organized around the criterion. Overnight removal forces immediate reorganization of eligibility frameworks, service admission, and statistical instruments — and simultaneous counter-reorganization by actors holding the identity-based reading, since the category's boundary would not vanish but be redrawn by whoever moves first.
% FOUNDING_PROBLEM: Protect a class defined by female reproductive biology from displacement and predation in contexts where male-puberty physiology confers decisive advantage (elite sport) and where sex is material to safety and service design (refuges, prisons), using a criterion that cannot be gamed by declaration. The mid-century sex-testing regime and the 1970s women's refuge movement were both built on this problem statement.
% FOUNDING_PROBLEM_CORROBORATION: Independent sports-science literature on puberty-derived performance gaps and archival records of the refuge movement corroborate that the founding problem was real and partly remains so. Human-rights treaty bodies and the arbitration adjudication record attest from outside the beneficiary set that current enforcement extends past the founding problem into rights-restricting territory. No single external attestation settles the status question — both sides are corroborated, which is why the status is contested rather than live or dead.
narrative_ontology:disappearance_verdict(woman_category__sex_biology_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_category__sex_biology_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_category__sex_biology_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(woman_category__sex_biology_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_category__sex_biology_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_category__sex_biology_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_category__sex_biology_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(woman_category__sex_biology_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78 at interval end) because the criterion's costs concentrate on identifiable boundary populations: transgender women lose the entire protected category, and DSD women face testing, mandated testosterone suppression, and occasional disqualification — costs decoupled from any service rendered to them. Suppression (0.75) is authored as a raw structural property, unscaled by power or scope: the arrangement's persistence depends on closing alternatives (self-report categories, open divisions, additional protected classes) through arbitration rulings, eligibility bans, and statutory guidance, not on participant preference. Theater is moderate (0.45): the protective function is real, but a growing share of activity is performative boundary defense — contested thresholds defended as settled science, certificate politics, and policy announcements aimed at constituencies rather than athletes. Accessibility collapse is moderate-low (0.48) because alternatives visibly persist — several jurisdictions run identity-based or expanded classifications, and some federations operate open categories. Resistance is substantial (0.62): athlete litigation, treaty-body findings, and advocacy campaigns have twice forced rule rewrites. The measurement series share one grid (eight points, 1992–2024) so every tracked metric is authored at every examined time point. The 2015 dip in suppression_requirement marks the IOC's relaxation of surgical prerequisites for transgender athletes while the DSD testing machinery persisted — enforcement capacity is not strictly monotonic, but the interval's overall shape is an enforcement ratchet: chromosome testing abandoned in 1992, hyperandrogenism machinery built in 2011, compulsory suppression mandated in 2018, blanket exclusion consolidated by 2024. That trajectory is why suppression_requirement is traced temporally rather than left static. Boltzmann coupling note: the declared identity_coordination type carries a complexity offset for genuine boundary-maintenance complexity, but the observed Power x Scope coupling concentrates extraction on powerless agents at global scope (DSD athletes drawn disproportionately from low-resource federations); the offset accommodates the complexity, it does not launder that asymmetry.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seats should compute differently from the same structural data. From a transgender woman's or a DSD athlete's position the arrangement operates as enforced closure — no compliant path exists except bodily modification or exit from the category's prizes — a snare-flavored experience. From the federation's position the same structure is a legitimately administered fairness rule it has twice redesigned under external pressure; from the protected class's position it is a hard-won safeguard. The engine computes this divergence per seat; the authored claim does not adjudicate it. Coalition potential deserves note: the payer classes are not doomed to isolation — the Martinez-Patino precedent (a cisgender champion disqualified by chromosome testing who allied with intersex advocates) shows cisgender, DSD, and transgender interests converging whenever the testing apparatus sweeps inside the protected class, and that convergence is the main mechanism by which payer-seat power could rise. Identity-lock dynamics bind both sides: defending organizations have fused institutional identity with guardianship of the category (the organization has become the category's protector, making retreat read as betrayal), while for transgender women the identity claim itself is the stake the criterion adjudicates. If the defenders' identity frame broke — if a federation reframed its role as fairness management rather than category guardianship — the arrangement would soften toward transitional, negotiable form.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive derivation, so no directionality overrides are authored. cisgender_female_population sits near the beneficiary end (receives protected categories; the involuntary attachment cuts both ways but nets as subsidy — the historical testing spillover is real but episodic and does not flip the sign). sports_governing_bodies derive low d as declared beneficiaries and administrators: they collect administrability and authority, and their repeated ability to rewrite the rules (arbitrage-grade exit from any particular formulation) keeps them far from the target end. sex_based_statistics_agencies sit near zero engagement: incidental beneficiaries whose instrument choice is reversible. transgender_women derive high d — full exclusion from the category's benefits with no compliant path, trapped exit. dsd_intersex_women derive the highest d: individually powerless against global federations, trapped by national-team ties and career horizons, and singled out by name in the regulations. gender_identity_advocacy_organizations are excluded rather than coordinated — they stand outside the enforcement perimeter and press inward; human_rights_treaty_bodies hold the analytical seat. An override was considered for the protected class (derivation might read them as pure beneficiaries while elite women carry testing spillover), but the override mechanism keys on power atom rather than agent, and the only other organized-power agent is the advocacy sector, which would be wrongly pulled toward beneficiary — so the derivation is left untouched and the nuance is recorded here instead.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is partially live and partially transformed, so mandatrophy is not resolved: the physiological substrate the category protects against (puberty-derived performance gaps) and the service-design needs of refuges and prisons persist, while the original chromosome-testing mandate died in 1992 and was replaced by successive machinery serving a shifting mandate. The tangled_rope classification prevents symmetric mislabelings: calling the arrangement a pure snare erases documented protection delivered to a large class (women's sport remained winnable, refuges stayed single-sex, epidemiology stayed disaggregated); calling it a pure rope erases the measured extraction from boundary populations that the enforcement record makes undeniable. The Goodhart vector to watch is mandate substitution inside the enforcement apparatus: as the founding fairness problem becomes administrable, enforcement effort migrates toward purity-of-category maintenance (documentation checks, definitional legislation) whose output is theater rather than protection — the theater_ratio series is the early-warning instrument for that migration, and the founding_problem_status x disappearance_verdict pair (contested x world_rearranges) correctly declines to flag the whole arrangement as zombie while its protective half remains demanded.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates one reading (sex_biology) of the woman_category kernel. How would the sibling readings (gender_identity_reading, intersex_accommodation_reading) re-partition the beneficiary and victim sets and re-index epsilon over the shared standing arrangement?',
    'Comparative classification across the sibling constraint files; no dataset resolves this — it is framing-dependent by construction.',
    'Under the gender_identity reading, transgender women move out of the victim set into membership and the exclusion burden relocates to cisgender-defined gatekeeping; under the intersex_accommodation reading the DSD victim set contracts sharply. Effective extraction and possibly per-seat type change materially with the reading adopted.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: this constraint is one reading of a contested kernel; sibling readings are separate constraints, not parameters of this one.').

omega_variable(
    typical_case_edge_share,
    'How large is the governed population that fails the typical-case criterion (46,XY DSD athletes, transgender women seeking category access), and does the enforcement burden scale with that population?',
    'Clinical prevalence data for differences of sex development combined with federation eligibility caseloads and appeal volumes over the interval.',
    'A large edge population pushes effective extraction upward and tilts the classification toward snare; a negligible edge population supports the reading''s coordination account, since boundary costs would then be rare exceptions rather than a standing levy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(typical_case_edge_share, empirical, 'Whether the typical-case definition leaves a small residue or a substantial governed edge population.').

omega_variable(
    retained_advantage_effect_size,
    'What performance advantage persists after testosterone suppression, and is it large enough to justify exclusion under the reading''s own fairness standard?',
    'Longitudinal performance studies of transgender and DSD athletes across pre- and post-suppression periods, independent of federation-commissioned analysis.',
    'A large retained advantage validates the protective function and weights the rope side of the classification; a small or absent advantage exposes the exclusion as extraction without a coordination payoff.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retained_advantage_effect_size, empirical, 'Empirical foundation of the performance-advantage framework the reading relies on.').

omega_variable(
    enforcement_scope_creep,
    'Does enforcement extend beyond competitive eligibility into documentation and surveillance of everyday sex classification (facility admission checks, certificate audits, reporting requirements)?',
    'Compare enforcement incidents and guidance revisions across jurisdictions over the interval, separating sport-internal enforcement from civic-document enforcement.',
    'Scope creep raises true suppression above the authored scalar and broadens the victim set beyond competitive exclusion; confinement to sport keeps suppression bounded and site-specific.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_scope_creep, empirical, 'Whether the enforcement machinery stays within its founding site or migrates into general civic classification.').

omega_variable(
    intersex_dual_status_extraction,
    'Is ambiguous intersex inclusion itself an extraction mechanism — recognition granted as a general matter but withdrawn exactly where material stakes attach (prize money, records, selection)?',
    'Track DSD athletes'' recognition status across career stages and legal contexts; analyze the trigger conditions under which recognition is suspended.',
    'If confirmed, the victim set widens beyond competitive exclusion and the tangled-rope balance tilts extractive; if recognition is stable outside competition, ambiguity is a definitional artifact rather than a levy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intersex_dual_status_extraction, conceptual, 'Whether conditional recognition constitutes a standing cost borne by the boundary population.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_category__sex_biology_reading, 1992, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wc_sexbio_tr_t1992, woman_category__sex_biology_reading, theater_ratio, 1992, 0.3).
narrative_ontology:measurement(wc_sexbio_tr_t1997, woman_category__sex_biology_reading, theater_ratio, 1997, 0.28).
narrative_ontology:measurement(wc_sexbio_tr_t2004, woman_category__sex_biology_reading, theater_ratio, 2004, 0.26).
narrative_ontology:measurement(wc_sexbio_tr_t2011, woman_category__sex_biology_reading, theater_ratio, 2011, 0.34).
narrative_ontology:measurement(wc_sexbio_tr_t2015, woman_category__sex_biology_reading, theater_ratio, 2015, 0.32).
narrative_ontology:measurement(wc_sexbio_tr_t2018, woman_category__sex_biology_reading, theater_ratio, 2018, 0.38).
narrative_ontology:measurement(wc_sexbio_tr_t2021, woman_category__sex_biology_reading, theater_ratio, 2021, 0.42).
narrative_ontology:measurement(wc_sexbio_tr_t2024, woman_category__sex_biology_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(wc_sexbio_be_t1992, woman_category__sex_biology_reading, base_extractiveness, 1992, 0.55).
narrative_ontology:measurement(wc_sexbio_be_t1997, woman_category__sex_biology_reading, base_extractiveness, 1997, 0.55).
narrative_ontology:measurement(wc_sexbio_be_t2004, woman_category__sex_biology_reading, base_extractiveness, 2004, 0.58).
narrative_ontology:measurement(wc_sexbio_be_t2011, woman_category__sex_biology_reading, base_extractiveness, 2011, 0.64).
narrative_ontology:measurement(wc_sexbio_be_t2015, woman_category__sex_biology_reading, base_extractiveness, 2015, 0.66).
narrative_ontology:measurement(wc_sexbio_be_t2018, woman_category__sex_biology_reading, base_extractiveness, 2018, 0.72).
narrative_ontology:measurement(wc_sexbio_be_t2021, woman_category__sex_biology_reading, base_extractiveness, 2021, 0.74).
narrative_ontology:measurement(wc_sexbio_be_t2024, woman_category__sex_biology_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(wc_sexbio_su_t1992, woman_category__sex_biology_reading, suppression_requirement, 1992, 0.35).
narrative_ontology:measurement(wc_sexbio_su_t1997, woman_category__sex_biology_reading, suppression_requirement, 1997, 0.36).
narrative_ontology:measurement(wc_sexbio_su_t2004, woman_category__sex_biology_reading, suppression_requirement, 2004, 0.42).
narrative_ontology:measurement(wc_sexbio_su_t2011, woman_category__sex_biology_reading, suppression_requirement, 2011, 0.55).
narrative_ontology:measurement(wc_sexbio_su_t2015, woman_category__sex_biology_reading, suppression_requirement, 2015, 0.52).
narrative_ontology:measurement(wc_sexbio_su_t2018, woman_category__sex_biology_reading, suppression_requirement, 2018, 0.66).
narrative_ontology:measurement(wc_sexbio_su_t2021, woman_category__sex_biology_reading, suppression_requirement, 2021, 0.68).
narrative_ontology:measurement(wc_sexbio_su_t2024, woman_category__sex_biology_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_category__sex_biology_reading, identity_coordination).
narrative_ontology:affects_constraint(woman_category__sex_biology_reading, woman_category__gender_identity_reading).
narrative_ontology:affects_constraint(woman_category__sex_biology_reading, woman_category__intersex_accommodation_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'definition of woman' conflates three structurally distinct constraints sharing one kernel. This file instantiates the sex_biology reading (membership fixed by chromosomal/anatomical/reproductive biology, typical case; victim set = transgender women excluded outright plus DSD women ambiguously swept in; enforcement-heavy). The gender_identity reading (membership by internal identity) is a separate constraint with a different victim set and a different epsilon over the shared standing arrangement. The intersex_accommodation reading (biology-grounded but variation-inclusive) is a refinement constraint whose victim set contracts to those excluded under stricter variants. The readings are linked via affects_constraints; each carries its own stable epsilon, beneficiaries, and victims per the epsilon-invariance principle — no observable parameter mediates between them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
