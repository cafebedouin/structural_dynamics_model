% ============================================================================
% CONSTRAINT STORY: equal_protection_commitment__diversity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_commitment__diversity_reading, []).

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
 *   constraint_id: equal_protection_commitment__diversity_reading
 *   human_readable: Equal Protection Diversity Consideration in Educational Admissions
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   The diversity_reading of equal protection doctrine permits universities
 *   to consider race as one factor among many in admissions to achieve
 *   educational diversity as a compelling state interest. This reading frames
 *   equal protection not as a prohibition on ALL racial classification (the
 *   colorblind_reading) nor as a mandate to dismantle subordination through
 *   race-conscious remedy (the remedial_reading), but as a permission for
 *   institutional discretion within bounds. Universities gain the right to
 *   pursue diversity; applicants lose transparency about how race-weighting
 *   affected individual outcomes; underrepresented students gain increased
 *   access; courts must police the boundary (holistic review, genuine
 *   one-factor, not mechanical point-system). The extractiveness is
 *   low-to-moderate (0.28) because the constraint is procedural and
 *   discretionary rather than coercive — it permits rather than requires;
 *   suppression is moderate (0.42) because enforcement depends on court
 *   oversight and institutional choice. Theater rises over time as the
 *   constraint's discretion invites expanding admissions bureaucracy and
 *   justificatory infrastructure.
 *
 * KEY AGENTS:
 *   - Universities with diversity mission — institutional agenda-setter, constrained exit (litigation risk), collect discretion
 *   - Applicants (race-unspecified) — moderate power, scattered, mobile exit, bear opacity cost
 *   - Underrepresented racial groups — powerless, constrained exit (depend on institutional discretion), benefit from access increase
 *   - Courts — institutional agenda-setter/observer, maintain boundary enforcement
 *   - Legislative bodies — excluded from primary constraint but retain statutory override
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_commitment__diversity_reading, 0.28).
domain_priors:suppression_score(equal_protection_commitment__diversity_reading, 0.42).
domain_priors:theater_ratio(equal_protection_commitment__diversity_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_commitment__diversity_reading, rope).
narrative_ontology:human_readable(equal_protection_commitment__diversity_reading, "Equal Protection Diversity Consideration in Educational Admissions").
narrative_ontology:topic_domain(equal_protection_commitment__diversity_reading, "constitutional/political").

domain_priors:requires_active_enforcement(equal_protection_commitment__diversity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_commitment__diversity_reading, '49ed1006-1ba7-4537-b839-bacfea2b7940').
narrative_ontology:cs_kernel_codification('49ed1006-1ba7-4537-b839-bacfea2b7940', fixed_text).
narrative_ontology:cs_authority_grounding('49ed1006-1ba7-4537-b839-bacfea2b7940', lineage).
narrative_ontology:cs_interpretation_layer_present('49ed1006-1ba7-4537-b839-bacfea2b7940').
narrative_ontology:cs_reading_relation('49ed1006-1ba7-4537-b839-bacfea2b7940', equal_protection_commitment__colorblind_reading, coexists_with).
narrative_ontology:cs_reading_relation('49ed1006-1ba7-4537-b839-bacfea2b7940', equal_protection_commitment__remedial_reading, influences).
narrative_ontology:cs_axiom('49ed1006-1ba7-4537-b839-bacfea2b7940', foundational, race_consciousness_permits_diversity_pursuit).
narrative_ontology:cs_axiom_status(race_consciousness_permits_diversity_pursuit, holdable).
narrative_ontology:cs_axiom_grounding('49ed1006-1ba7-4537-b839-bacfea2b7940', race_consciousness_permits_diversity_pursuit, deontological).
narrative_ontology:cs_axiom('49ed1006-1ba7-4537-b839-bacfea2b7940', secondary, compelling_state_interest_justifies_narrow_race_tailoring).
narrative_ontology:cs_axiom_status(compelling_state_interest_justifies_narrow_race_tailoring, holdable).
narrative_ontology:cs_axiom_grounding('49ed1006-1ba7-4537-b839-bacfea2b7940', compelling_state_interest_justifies_narrow_race_tailoring, instrumental).
narrative_ontology:cs_reference_frame('49ed1006-1ba7-4537-b839-bacfea2b7940', constitutional_equal_protection_permission).
narrative_ontology:cs_drift_state('49ed1006-1ba7-4537-b839-bacfea2b7940', contemporary_post_fisher_v_university_of_texas, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('49ed1006-1ba7-4537-b839-bacfea2b7940', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(equal_protection_commitment__diversity_reading, equal_protection_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_commitment__diversity_reading, universities_with_diversity_mission).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__diversity_reading, underrepresented_racial_groups).
narrative_ontology:constraint_victim(equal_protection_commitment__diversity_reading, applicants_not_considered_via_holistic_race_factor).
narrative_ontology:constraint_victim(equal_protection_commitment__diversity_reading, individual_applicants_denied_transparency).
narrative_ontology:constraint_vindicates(equal_protection_commitment__diversity_reading, equal_protection_permits_remedial_classification).
narrative_ontology:constraint_vindicates(equal_protection_commitment__diversity_reading, compelling_state_interest_in_diversity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and defend admissions policies that consider race as one factor among many to achieve student-body diversity. Argue this discretion serves the educational mission of cross-racial understanding and institutional excellence. Must navigate the legal boundary: the constraint permits race-consideration within bounds (holistic review, not mechanical quota or point-adder), but enforcement pressure and litigation costs mount as courts narrow the permission.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, universities_with_diversity_mission, agenda_setter,
    institutional, generational, constrained, national).

% Individual applicants — particularly Asian American applicants under affirmative-action regimes, and white applicants in regions with strong diversity commitment — bear a potential cost: their individual merit or circumstances may be shadowed by holistic review that weights racial diversity as an institutional good. The constraint obscures the magnitude of that cost (no transparency on how race-consideration affected individual outcomes), and exit is available (apply to other schools) but entails accepting a less-preferred institution or departing the preference set entirely.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, applicants_not_considered_via_holistic_race_factor, payer,
    moderate, biographical, mobile, national).

% Historically underrepresented students gain increased access to elite educational pathways when race is a permissible admissions factor. The constraint enables universities to reach beyond the applicant pool that would accumulate under race-neutral admissions (itself a product of structural inequality in earlier schooling). Their exit options are constrained: they depend on institutional discretion to implement the permission; if universities abandon race-consideration, underrepresented groups face a tighter admissions barrier.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, underrepresented_racial_groups, beneficiary,
    powerless, generational, constrained, national).

% All applicants — regardless of race or outcome — are systematically denied individualized explanation of how race-consideration affected their outcome. Holistic review is intentionally opaque (to shield against mechanical-quota claims and to allow institutional discretion). Applicants cannot know whether they were rejected because of race-weighting, merit insufficiency, or other factors. This opacity is structurally built into the constraint's permission: once race is 'one factor among many,' disaggregating its contribution becomes legally and administratively fraught.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, individual_applicants_denied_transparency, payer,
    powerless, immediate, trapped, national).

% Race-neutral mechanisms (socioeconomic affirmative action, geographic diversity, first-generation status) would produce some diversity without explicit racial classification, but under the diversity_reading they sit BELOW the hierarchy of permissible tools — universities retain discretion to use them, but the constraint explicitly validates race-consideration as a preferable instrument for achieving diversity. These alternatives are structurally diminished by the constraint's validation of race-direct methods.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, alternative_diversity_mechanisms, excluded,
    moderate, biographical, constrained, national).

% Courts (Supreme Court especially) interpret and defend the boundary of when race-consideration is permissible. They are both agenda-setters (they draw the line) and observers (they must respond to evolving doctrine and institutional practice). Under the diversity_reading, they validate the permission and set the enforcement bounds (holistic review, no mechanical point-system, genuine consideration of race as one factor, not primary driver).
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, equal_protection_doctrine_custodians, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_commitment__diversity_reading, equal_protection_doctrine_custodians, observer).

% State legislatures and Congress can restrict race-consideration by statute (as some states have done via ballot initiative), but the diversity_reading permits universities to proceed under the constitutional permission even if a legislature disfavors it — at least until legislative restriction is enacted. Legislatures are excluded from the primary constraint operation but retain veto power via restriction.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, legislative_bodies, excluded,
    institutional, generational, constrained, national).

% Organizations and scholars defending the diversity_reading argue that race-neutral admissions cannot undo the effects of systemic racism in K-12 schooling and that diversity serves compelling institutional and democratic interests. They testify in litigation, brief courts, and shape public discourse. They neither set the constraint nor directly pay or benefit, but their advocacy sustains the reading's legitimacy.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, civil_rights_advocates, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_commitment__diversity_reading, universities_with_diversity_mission).
narrative_ontology:fixing_cost_class(equal_protection_commitment__diversity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reconciles the competing values of equal individual protection (no state classification on race) and collective diversity outcomes (institutions need discretion to ensure cross-racial contact and representation). It coordinates institutional mission autonomy with constitutional constraint by permitting race-consideration within bounds (holistic review, one factor, not mechanical) — a procedural compromise that lets universities pursue diversity while courts retain oversight.
% TRANSFER_FUNCTION: Transfers institutional discretion to universities (permission to weigh race) while transferring opacity cost to applicants (individual outcomes remain unexplained). It transfers admission access to underrepresented students and transfers reduced certainty/transparency to applicants not favored by race-weighting. The constraint moves power from race-neutral algorithmic rules toward human judgment, and moves transparency away from individual applicants.
% ABSENT_VOICES: Asian American applicants organized against affirmative action (now gaining court attention post-Students for Fair Admissions v. Harvard); white applicants and their advocates who claim individual merit is subordinated to diversity mathematics (marginalized from the diversity_reading's legitimacy frame); legislators who wish to restrict race-consideration (excluded from the primary constraint but retain statutory override). Class-position applicants (low-income, rural, working-class regardless of race) who might benefit from socioeconomic rather than race-based diversity (their voice suggests an alternative tool that is structurally diminished by race-explicit permission).
% DISAPPEARANCE_RATIONALE: If the diversity_reading vanished and courts prohibited race-consideration entirely, universities would shift to socioeconomic, geographic, and holistic race-neutral criteria; underrepresented racial groups would likely face tighter admissions barriers; institutions would lose one tool for achieving demographic diversity; applicants would gain slightly more procedural clarity (no race-weighting to hide) but less institutional discretion; the racial composition of elite student bodies would shift. The admissions ecosystem is organized around whether and how race-consideration is permitted; removal of the permission would require rapid reorganization.
% FOUNDING_PROBLEM: Elite educational institutions have historically been segregated by race and class; race-neutral admissions rules inherited this stratification (applicant pools from segregated K-12 schools produce segregated college cohorts). The diversity_reading was designed to permit universities to break that historical lock-in: to treat the past segregation as a fact universities must actively counter, and to validate race-conscious admissions as a legitimate mechanism for that counter-action. The problem is: how can an institution satisfy equal protection (which forbids racial classification) while also undoing the legacy effects of racism that race-neutral rules perpetuate?
% FOUNDING_PROBLEM_CORROBORATION: Defenders of the diversity_reading (civil rights scholars, institutional leaders, some legal scholars) attest the founding problem is live: segregation effects persist in applicant pools and elementary outcomes. Opponents (colorblind reading advocates, some economists) attest the problem has been substantially redressed by decades of civil rights law and that race-conscious remediation is now over-reach. Outside the advocacy echo: empirical fact is that racial segregation in K-12 education remains substantial (documented by demographers like Gary Orfield) and that college admissions patterns show correlation between race-consideration and representation — both parties concede the facts but read them differently. The founding problem's reality is not in doubt; its ongoing severity and the adequacy of the diversity_reading's remedy are contested.
narrative_ontology:disappearance_verdict(equal_protection_commitment__diversity_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_commitment__diversity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_commitment__diversity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(equal_protection_commitment__diversity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_commitment__diversity_reading, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_commitment__diversity_reading_tests).
:- end_tests(equal_protection_commitment__diversity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured as the cost imposed on applicants and the discretionary power transferred to universities. The constraint does not mandate outcomes (no quota) and does not require universities to use race (discretionary), so extractiveness is low. However, the opacity imposed on applicants — the systematic denial of individual-level explanation — carries a real cost: applicants in disfavored groups cannot know their exact rejection reason, which blocks their ability to contest or appeal on individuation grounds. This opacity is STRUCTURAL to the constraint's operation: once race is 'one factor among many' in holistic review, itemized disclosure becomes legally fraught (triggers Fourteenth Amendment scrutiny of the school's race-consideration logic). Suppression is moderate because the constraint's persistence depends on court enforcement (judges must police the one-factor boundary and strike down mechanical systems) and on institutional buy-in (universities must choose to implement the permission). Resistance is high (0.68) because the constraint faces sustained legal challenge and organized opposition; many applicants and legislators resist it. Theater rises over time as universities develop increasingly elaborate diversity justifications and affirmative-action compliance bureaucracies, suggesting the constraint's function is partly to manage legitimacy rather than achieve outcomes. The measurement series use one shared time grid: every metric is authored at each time point.
 *
 * PERSPECTIVAL GAP:
 *   The diversity_reading should compute differently by seat. From the university's perspective (agenda_setter), the constraint is genuine coordination: it permits them to pursue institutional missions (educational excellence, cross-racial contact) while satisfying constitutional constraint — the cost is litigation defense, but the benefit is mission autonomy. From the individual applicant's perspective (payer seat, particularly denied-transparency applicants), the constraint operates as enforced opacity: their costs are borne silently, their exit is expensive (transfer to less-preferred school), and they have no remedy if race-weighting was the determining factor. From the underrepresented student's perspective (beneficiary), the constraint is access-enhancing: it removes a structural barrier that race-neutral rules perpetuate. The engine should compute these divergences from the structural data: universities get low d (beneficiary, powerful, arbitrage exit to alternative admissions models); applicants get high d (payer, moderate-to-powerless, constrained/mobile exit); underrepresented students get very low d (beneficiary, powerless, identity-locked exit — their group membership is the precise mechanism by which they benefit).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: universities_with_diversity_mission (they collect institutional discretion and mission autonomy; exit to race-neutral admissions would constrain their judgment); underrepresented_racial_groups (they collect increased access; exit means the permission is abandoned and their applicant pool tightens). Victims (payers): applicants_not_considered_via_holistic_race_factor (they pay opacity cost and potential rejection-cost if race-weighting is applied to them), and individual_applicants_denied_transparency (all applicants pay the transparency cost; some pay outcome-cost). The directionality follows: universities are near 0.1 (beneficiary, powerful, can adapt), underrepresented students near 0.05 (beneficiary, powerless, fully dependent on permission), and applicants near 0.7–0.8 (payer, powerless-to-moderate, trapped by the opacity + identity-locked if race-weighting operates on their racial category). No override needed; the structural data produce the right d values.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy resolution via the founding problem: does the diversity_reading solve the problem it was designed to address? The founding problem is 'how can universities satisfy equal protection (forbids racial classification) while undoing segregation effects that race-neutral rules perpetuate?' The diversity_reading answers: by interpreting equal protection to permit race-consideration within bounds. The FOUNDING PROBLEM STATUS is contested. Defenders say the problem is live: K-12 segregation remains substantial, and race-neutral admissions perpetuate it. Opponents say the problem is substantially solved (decades of civil rights law have reduced outright discrimination; remaining racial sorting is largely caused by family resources/geography, not race-targeting). If the founding problem is no longer live — if K-12 segregation is not a pressing barrier and family-resource sorting is the real mechanism — then the diversity_reading persists on outdated premises. If the founding problem remains live, then the diversity_reading solves it as advertised. The constraint's persistence is NOT contingent on the founding problem's status (courts will enforce the permission regardless); the constraint's LEGITIMACY is contested. Mandatrophy is avoided by the reading's honest acknowledgment that the founding problem's severity is in dispute. The constraint does not claim the problem is solved; it claims it remains live. That claim is empirically contestable (census data on school segregation, family-wealth sorting, admissions-outcome correlations can test it), but the claim itself is honest — the reading does not deny the problem or pivot to an unstated goal. Therefore: no clear mandatrophy, but a high-stakes empirical dispute about whether the founding problem justifies the means.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_persistence,
    'Is the founding problem (K-12 segregation perpetuated by race-neutral admissions) still a pressing barrier to equal opportunity, or has it been substantially remedied by decades of civil rights law and integrated K-12 schooling?',
    'Longitudinal data on racial composition of K-12 schools by region; comparison of family wealth and home values by race; admissions outcome analysis (do race-neutral admissions produce segregated cohorts in practice); testimony from underrepresented students about barriers they face.',
    'If the founding problem persists, the diversity_reading solves a real problem and mandatrophy is avoided. If the problem is substantially solved, the constraint persists without matching its justifying function — mandatrophy risk rises, and the competing readings gain force.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_persistence, empirical, 'Whether the structural segregation the diversity_reading addresses remains live.').

omega_variable(
    holistic_review_mechanicity,
    'Is race-consideration in holistic review genuinely ONE FACTOR among many, or does race systematically dominate outcomes in practice?',
    'Disclosure of admissions statistics: what % of applicants are rejected despite meeting merit thresholds, broken down by race; analysis of weight distribution in institutional admissions models; comparison of acceptance rates across schools with/without explicit race-weighting.',
    'If race is truly one factor and decisions would be similar without it, the constraint is low-extractive and respectful of individual applicants'' decisions. If race dominates outcomes (e.g., high-scoring applicants of some races are rejected while lower-scoring applicants of other races are admitted), the constraint is more extractive and the opacity cost is higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(holistic_review_mechanicity, empirical, 'Whether race-weighting in holistic review operates as intended (one factor) or as a hidden quota.').

omega_variable(
    alternative_diversity_mechanism_adequacy,
    'Could race-neutral mechanisms (socioeconomic affirmative action, geographic diversity, first-generation status, legacy-removal) achieve comparable diversity without explicit racial classification?',
    'Natural experiment: jurisdictions that ban race-conscious admissions but adopt robust socioeconomic affirmative action and see what diversity outcomes result; modeling of admissions outcomes under alternative criteria.',
    'If race-neutral mechanisms produce comparable diversity, the diversity_reading is not the only way to solve the founding problem; courts might uphold it as one permissible method but not as uniquely necessary. If race-neutral mechanisms produce significantly lower diversity (especially for traditionally underrepresented racial groups), the diversity_reading becomes more strongly justified as the only viable tool.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_diversity_mechanism_adequacy, empirical, 'Whether alternative, race-neutral diversity mechanisms can achieve comparable outcomes.').

omega_variable(
    reading_versus_colorblind_foreclosure,
    'Does the diversity_reading logically foreclose the colorblind_reading, or can both coexist within different parties'' commitments to equal protection?',
    'Conceptual analysis: examine whether a party committed to the diversity_reading MUST deny the colorblind_reading''s core premise (that equal protection forbids racial classification), or whether both can be held as live constitutional interpretations by different judicial/legislative seats.',
    'If the readings foreclose each other (one is true, the other false, no middle ground), the constraint family should be modeled as a zero-sum dispute. If both coexist as live readings held by different parties, they influence each other but neither rules out the other — the corpus should model multiple constraint stories linked by `coexists_with` relations rather than as foreclosure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_versus_colorblind_foreclosure, conceptual, 'Whether the diversity_reading and colorblind_reading have a logical foreclosure or coexistence relationship.').

omega_variable(
    transparency_cost_internalization,
    'Is the opacity cost imposed on applicants (denied individual explanation for race-weighting) a necessary component of the constraint, or could universities adopt stronger transparency protocols while retaining race-consideration?',
    'Analysis of whether detailed applicant-level disclosure (explaining how race-weighting affected specific decisions) would violate Fourteenth Amendment scrutiny of the university''s race-consideration logic, or whether such disclosure is compatible with constitutional permissibility.',
    'If transparency and race-consideration can coexist, the constraint''s extractiveness could drop significantly (less opacity cost) while the diversity function is retained. If transparency would undermine the constraint''s legal permissibility (detailed disclosure triggers strict scrutiny that strikes down the program), then opacity is structurally necessary, and the constraint is more extractive than its beneficiaries acknowledge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transparency_cost_internalization, empirical, 'Whether opacity is structurally necessary to the constraint or could be decoupled.').

omega_variable(
    institutionally_nested_reading_contest,
    'Is the diversity_reading a genuine commitment of the courts (endorsed in binding doctrine), or is it a contested reading that lacks stable institutional support?',
    'Trajectory of Supreme Court doctrine over time (has the diversity_reading''s permissibility been affirmed, narrowed, or overturned in recent terms?); track whether lower courts treat the diversity_reading as settled or as open for reconsideration.',
    'If the diversity_reading has stable doctrinal support, it remains a live constraint with predictable boundaries. If Supreme Court doctrine has shifted to favor colorblind_reading (as suggested by recent opinions narrowing affirmative action), the diversity_reading''s institutional grounding is eroding — it may persist in some institutions but lose constitutional permission, shifting it toward piton (maintained by inertia, not doctrine) or toward snare (extraction without permission).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutionally_nested_reading_contest, empirical, 'Whether the diversity_reading retains stable institutional/doctrinal support or is being displaced.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_commitment__diversity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t0, equal_protection_commitment__diversity_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(equa_tr_t0, observed).
narrative_ontology:measurement(equa_tr_t5, equal_protection_commitment__diversity_reading, theater_ratio, 5, 0.21).
narrative_ontology:measurement_basis(equa_tr_t5, observed).
narrative_ontology:measurement(equa_tr_t10, equal_protection_commitment__diversity_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement_basis(equa_tr_t10, observed).
narrative_ontology:measurement(equa_tr_t15, equal_protection_commitment__diversity_reading, theater_ratio, 15, 0.27).
narrative_ontology:measurement_basis(equa_tr_t15, observed).
narrative_ontology:measurement(equa_tr_t20, equal_protection_commitment__diversity_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement_basis(equa_tr_t20, observed).
narrative_ontology:measurement(equa_tr_t25, equal_protection_commitment__diversity_reading, theater_ratio, 25, 0.32).
narrative_ontology:measurement_basis(equa_tr_t25, observed).
narrative_ontology:measurement(equa_tr_t30, equal_protection_commitment__diversity_reading, theater_ratio, 30, 0.31).
narrative_ontology:measurement_basis(equa_tr_t30, observed).
narrative_ontology:measurement(equa_tr_t40, equal_protection_commitment__diversity_reading, theater_ratio, 40, 0.31).
narrative_ontology:measurement_basis(equa_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(equa_be_t0, equal_protection_commitment__diversity_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement_basis(equa_be_t0, observed).
narrative_ontology:measurement(equa_be_t5, equal_protection_commitment__diversity_reading, base_extractiveness, 5, 0.24).
narrative_ontology:measurement_basis(equa_be_t5, observed).
narrative_ontology:measurement(equa_be_t10, equal_protection_commitment__diversity_reading, base_extractiveness, 10, 0.26).
narrative_ontology:measurement_basis(equa_be_t10, observed).
narrative_ontology:measurement(equa_be_t15, equal_protection_commitment__diversity_reading, base_extractiveness, 15, 0.27).
narrative_ontology:measurement_basis(equa_be_t15, observed).
narrative_ontology:measurement(equa_be_t20, equal_protection_commitment__diversity_reading, base_extractiveness, 20, 0.28).
narrative_ontology:measurement_basis(equa_be_t20, observed).
narrative_ontology:measurement(equa_be_t25, equal_protection_commitment__diversity_reading, base_extractiveness, 25, 0.28).
narrative_ontology:measurement_basis(equa_be_t25, observed).
narrative_ontology:measurement(equa_be_t30, equal_protection_commitment__diversity_reading, base_extractiveness, 30, 0.27).
narrative_ontology:measurement_basis(equa_be_t30, observed).
narrative_ontology:measurement(equa_be_t40, equal_protection_commitment__diversity_reading, base_extractiveness, 40, 0.28).
narrative_ontology:measurement_basis(equa_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t0, equal_protection_commitment__diversity_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(equa_su_t0, observed).
narrative_ontology:measurement(equa_su_t5, equal_protection_commitment__diversity_reading, suppression_requirement, 5, 0.37).
narrative_ontology:measurement_basis(equa_su_t5, observed).
narrative_ontology:measurement(equa_su_t10, equal_protection_commitment__diversity_reading, suppression_requirement, 10, 0.39).
narrative_ontology:measurement_basis(equa_su_t10, observed).
narrative_ontology:measurement(equa_su_t15, equal_protection_commitment__diversity_reading, suppression_requirement, 15, 0.4).
narrative_ontology:measurement_basis(equa_su_t15, observed).
narrative_ontology:measurement(equa_su_t20, equal_protection_commitment__diversity_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement_basis(equa_su_t20, observed).
narrative_ontology:measurement(equa_su_t25, equal_protection_commitment__diversity_reading, suppression_requirement, 25, 0.43).
narrative_ontology:measurement_basis(equa_su_t25, observed).
narrative_ontology:measurement(equa_su_t30, equal_protection_commitment__diversity_reading, suppression_requirement, 30, 0.42).
narrative_ontology:measurement_basis(equa_su_t30, observed).
narrative_ontology:measurement(equa_su_t40, equal_protection_commitment__diversity_reading, suppression_requirement, 40, 0.42).
narrative_ontology:measurement_basis(equa_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_commitment__diversity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(equal_protection_commitment__diversity_reading, 0.1).
narrative_ontology:affects_constraint(equal_protection_commitment__diversity_reading, equal_protection_commitment__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_commitment__diversity_reading, equal_protection_commitment__remedial_reading).

% DUAL FORMULATION NOTE:
% The equal_protection_commitment is a contested kernel with three distinct readings, each instantiating a different constraint. The diversity_reading permits race-consideration for educational diversity (low-moderate extractiveness, procedural). The colorblind_reading forbids all racial classification (near-zero extractiveness because it removes discretion entirely, though enforcement costs rise). The remedial_reading permits race-conscious remedy to dismantle subordination (higher extractiveness from those in non-subordinated groups, focused on dismantling caste systems). These are not three angles on one constraint — they are three structurally distinct constraints with different beneficiary/victim sets, different ε values, and different types. They are linked here because all three interpret the same legal text (the Fourteenth Amendment) and operate in the same institutional domain (American constitutional law). The diversity_reading coexists with the colorblind_reading (live positions in different judicial/political constituencies) and influences the remedial_reading (by validating race-consciousness in principle, it opens space for stronger remedial readings, though it also constrains them by insisting on narrow tailoring and compelling interest).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(equal_protection_commitment__diversity_reading, powerless, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
