% ============================================================================
% CONSTRAINT STORY: equal_protection_kernel__colorblind_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_kernel__colorblind_reading, []).

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
 *   constraint_id: equal_protection_kernel__colorblind_reading
 *   human_readable: Equal Protection Clause — Colorblind Reading (Per Se Bar on State Racial Classifications)
 *   domain: constitutional law / education policy / civil rights
 *
 * SUMMARY:
 *   The Fourteenth Amendment's Equal Protection Clause is a contested kernel;
 *   this file instantiates exactly one reading of it — the colorblind
 *   reading, under which the clause categorically forbids state use of racial
 *   classifications regardless of purpose ('Our Constitution is
 *   color-blind'). The reading's modern arc runs from Harlan's Plessy dissent
 *   through Croson and Adarand (strict scrutiny hardening), the temporary
 *   check of Grutter, Parents Involved's extension to voluntary integration,
 *   and SFFA's conversion of the rule into a per se bar on race-conscious
 *   admissions. The colorblind rule is now the standing arrangement in this
 *   domain, and it is the ε referent here: assessed by the reading's own
 *   lights, the rule protects rather than takes, so the reading-indexed ε is
 *   low. The structural data (beneficiaries, victims, enforcement) are
 *   authored independently so the engine can compute per-seat
 *   classifications, which diverge sharply from the reading's
 *   self-assessment. The sibling readings (remedial, antisubordination) are
 *   separate constraint files with their own ε values and victim sets; they
 *   are linked, not described, here. KEY AGENTS (by structural relationship):
 *   - federal_judiciary: Agenda setter (institutional/mobile) — administers
 *   the rule, strikes race-conscious state action, accumulates final
 *   interpretive authority - disfavored_race_applicants: Primary beneficiary
 *   (organized/mobile) — compete without racial penalty nationwide -
 *   originalist_legal_movement: Secondary beneficiary (organized/mobile) —
 *   doctrinal project vindicated; identity fused with the reading -
 *   race_preference_opposing_public: Diffuse beneficiary (moderate/mobile) —
 *   polling majorities whose preference becomes constitutional command -
 *   historically_excluded_minority_applicants: Primary target
 *   (powerless/trapped) — lose the remedial pathway; bear inherited disparity
 *   costs with no collective remedy channel -
 *   diversity_minded_public_universities: Dual-positioned payer
 *   (institutional/constrained) — lose the instrument, gain compliance
 *   certainty - voluntary_integration_districts: Target
 *   (moderate/constrained) — integration instruments foreclosed -
 *   civil_rights_advocacy_organizations: Organized target
 *   (organized/constrained) — litigate from permanent doctrinal disadvantage
 *   - race_conscious_policy_voters: Excluded voice (moderate/trapped) —
 *   retain the vote but not the option - constitutional_law_academy:
 *   Analytical observer (analytical/analytical) — maps the structure, split
 *   across readings
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_kernel__colorblind_reading, 0.15).
domain_priors:suppression_score(equal_protection_kernel__colorblind_reading, 0.78).
domain_priors:theater_ratio(equal_protection_kernel__colorblind_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_kernel__colorblind_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_kernel__colorblind_reading, "Equal Protection Clause — Colorblind Reading (Per Se Bar on State Racial Classifications)").
narrative_ontology:topic_domain(equal_protection_kernel__colorblind_reading, "constitutional law / education policy / civil rights").

domain_priors:requires_active_enforcement(equal_protection_kernel__colorblind_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_kernel__colorblind_reading, '9883dfb6-2203-45a5-abaf-9d4b3d15c92d').
narrative_ontology:cs_kernel_codification('9883dfb6-2203-45a5-abaf-9d4b3d15c92d', fixed_text).
narrative_ontology:cs_authority_grounding('9883dfb6-2203-45a5-abaf-9d4b3d15c92d', lineage).
narrative_ontology:cs_interpretation_layer_present('9883dfb6-2203-45a5-abaf-9d4b3d15c92d').
narrative_ontology:cs_reading_relation('9883dfb6-2203-45a5-abaf-9d4b3d15c92d', equal_protection_kernel__remedial_reading, forecloses).
narrative_ontology:cs_reading_relation('9883dfb6-2203-45a5-abaf-9d4b3d15c92d', equal_protection_kernel__antisubordination_reading, forecloses).
narrative_ontology:cs_axiom('9883dfb6-2203-45a5-abaf-9d4b3d15c92d', foundational, state_may_never_allocate_benefits_or_burdens_by_race).
narrative_ontology:cs_axiom_status(state_may_never_allocate_benefits_or_burdens_by_race, holdable).
narrative_ontology:cs_axiom_grounding('9883dfb6-2203-45a5-abaf-9d4b3d15c92d', state_may_never_allocate_benefits_or_burdens_by_race, deontological).
narrative_ontology:cs_axiom('9883dfb6-2203-45a5-abaf-9d4b3d15c92d', secondary, identical_formal_treatment_satisfies_equal_protection).
narrative_ontology:cs_axiom_status(identical_formal_treatment_satisfies_equal_protection, holdable).
narrative_ontology:cs_axiom_grounding('9883dfb6-2203-45a5-abaf-9d4b3d15c92d', identical_formal_treatment_satisfies_equal_protection, conventional).
narrative_ontology:cs_reference_frame('9883dfb6-2203-45a5-abaf-9d4b3d15c92d', original_public_meaning_colorblind_text).
narrative_ontology:cs_drift_state('9883dfb6-2203-45a5-abaf-9d4b3d15c92d', contemporary_post_sffa_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('9883dfb6-2203-45a5-abaf-9d4b3d15c92d', '').
narrative_ontology:cs_kernel_id(equal_protection_kernel__colorblind_reading, equal_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_kernel__colorblind_reading, disfavored_race_applicants).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__colorblind_reading, originalist_legal_movement).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__colorblind_reading, race_preference_opposing_public).
narrative_ontology:constraint_victim(equal_protection_kernel__colorblind_reading, historically_excluded_minority_applicants).
narrative_ontology:constraint_victim(equal_protection_kernel__colorblind_reading, diversity_minded_public_universities).
narrative_ontology:constraint_victim(equal_protection_kernel__colorblind_reading, voluntary_integration_districts).
narrative_ontology:constraint_victim(equal_protection_kernel__colorblind_reading, civil_rights_advocacy_organizations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__colorblind_reading, diversity_minded_public_universities).
narrative_ontology:constraint_vindicates(equal_protection_kernel__colorblind_reading, formal_equality_doctrine).
narrative_ontology:constraint_vindicates(equal_protection_kernel__colorblind_reading, harlan_colorblind_dissent).
narrative_ontology:constraint_vindicates(equal_protection_kernel__colorblind_reading, strict_scrutiny_for_racial_classifications).
narrative_ontology:constraint_vindicates(equal_protection_kernel__colorblind_reading, original_public_meaning_jurisprudence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decides every case turning on whether a state actor may use a racial classification, and in the modern line (Croson, Adarand, Parents Involved, SFFA) has converted a balancing inquiry into a categorical prohibition. Each extension of the rule adds to the Court's final interpretive authority over race policy; the Court recently demonstrated it can revise the doctrine wholesale when its composition shifts.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, federal_judiciary, agenda_setter,
    institutional, generational, mobile, national).

% Applicants to selective institutions whom race-conscious schemes previously disadvantaged — principally Asian American and white candidates. Under the rule they compete without racial penalty at every covered institution nationwide; their litigation coalition supplied the vehicle that produced the categorical bar, and they can pursue any institution without encountering a race-preference term.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, disfavored_race_applicants, beneficiary,
    organized, biographical, mobile, national).

% Judges, academics, and advocacy networks whose long project was to restore the clause's supposed original colorblind meaning. The SFFA outcome vindicates decades of their scholarship and staffing strategy; their professional identities are built around the reading, and abandoning it would dissolve the movement's central claim.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, originalist_legal_movement, beneficiary,
    organized, generational, mobile, national).

% Polling majorities that consistently tell surveyors they oppose race as an admissions factor. They pay nothing and organize little; the rule entrenches their stated preference as constitutional command rather than leaving it to jurisdiction-by-jurisdiction politics.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, race_preference_opposing_public, beneficiary,
    moderate, biographical, mobile, national).

% Black, Latino, and Native American applicants who inherit the downstream effects of centuries of exclusion — wealth gaps, school-quality gaps, network gaps — and who lost the one mechanism that weighed that history in individual decisions. They cannot exit the admissions system, cannot vote the ruling away, and have no alternative channel through which their group's history reaches the decision.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, historically_excluded_minority_applicants, payer,
    powerless, generational, trapped, national).

% Public institutions that used race-conscious admissions to compose classes and now must pursue demographic goals through race-neutral means that they judge less effective and that draw their own litigation risk. On the other side of the ledger they received a bright-line rule that ends decades of balancing uncertainty and shields them from reverse-discrimination claims.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, diversity_minded_public_universities, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_kernel__colorblind_reading, diversity_minded_public_universities, beneficiary).

% School districts that adopted race-conscious assignment or magnet plans to prevent resegregation after unitary-status declarations. The Parents Involved line foreclosed those plans; the districts retain the segregation problem but not the instrument, and cannot relocate their student populations.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, voluntary_integration_districts, payer,
    moderate, generational, constrained, regional).

% Organizations built around the remedial and antisubordination traditions that won Brown. They now litigate from permanent doctrinal disadvantage: their preferred theories are outside the operative rule, their amicus and direct representation repeatedly loses, and each defense consumes resources that once built affirmative victories.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, civil_rights_advocacy_organizations, payer,
    organized, biographical, constrained, national).

% State and local electorates that might choose race-conscious integration or preference policies through ordinary politics. After the categorical bar their choice does not survive review in any forum; they retain the vote but not the option, and no ballot can amend the judicial rule short of constitutional amendment.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, race_conscious_policy_voters, excluded,
    moderate, biographical, trapped, national).

% Scholars who map the clause's competing readings, their textual and historical warrants, and their distributive consequences. The field is split across the three readings; its analyses inform all camps but bind none.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, constitutional_law_academy, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_kernel__colorblind_reading, federal_judiciary).
narrative_ontology:fixing_cost_class(equal_protection_kernel__colorblind_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides one uniform, administrable federal rule telling every state actor when racial classifications may be used — never — replacing case-by-case strict-scrutiny balancing with a bright line, and solving the collective-action problems of racial favoritism, spoils allocation, and inter-jurisdiction arbitrage over preferential policies.
% TRANSFER_FUNCTION: Moves decision rights over racial remediation from state and local democratic institutions to the federal judiciary; reallocates selective-admission and program access from applicants formerly preferred by race-conscious schemes toward those formerly disfavored; and leaves the costs of historical discrimination sitting where they currently fall by removing the collective transfer mechanisms that weighed it.
% ABSENT_VOICES: Historically excluded communities and their advocacy organizations argued the remedial and antisubordination readings in briefs, dissents, and scholarship; they remain outside the operative doctrine. State and local majorities that might choose race-conscious integration have no forum in which their choice survives.
% DISAPPEARANCE_RATIONALE: Race-conscious admissions, targeted outreach, integration assignments, and set-aside programs would revive within a single admissions or budget cycle; universities and districts would re-instrument immediately; the judiciary would lose its final-word position over race policy; and the formal-equality settlement across education, contracting, and employment would unravel into jurisdiction-by-jurisdiction renegotiation.
% FOUNDING_PROBLEM: State-enforced racial caste: slavery, the Black Codes, and Jim Crow segregation. The Fourteenth Amendment was ratified to make the freed population full citizens and to strip states of the power to subordinate them by law.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the colorblind coalition: Reconstruction-era congressional debates and Freedmen's Bureau records attest that the Thirty-Ninth Congress targeted the subordination of the freed population rather than classification-neutrality as such; professional Reconstruction historiography concurs; and the SFFA dissenting opinions, joined by decades of civil-rights scholarship, attest that subordination's mechanisms persisted after formal caste fell. The colorblind coalition's own attestation — Harlan's dissent read as categorical neutrality — is precisely what that external record contests.
narrative_ontology:disappearance_verdict(equal_protection_kernel__colorblind_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_kernel__colorblind_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_kernel__colorblind_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equal_protection_kernel__colorblind_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_kernel__colorblind_reading, 0.15, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_kernel__colorblind_reading_tests).
:- end_tests(equal_protection_kernel__colorblind_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.15 because ε here is reading-indexed over a fixed referent: the standing arrangement is the colorblind rule itself, and by this reading's own lights the rule takes nothing — identical treatment is what it owes everyone, and the closure of the remedial pathway is the absence of an entitlement, not a taking. The small residual reflects costs even sympathetic formal-equality theorists concede (lost diversity benefits, lost institutional flexibility). Suppression is a raw structural property, unscaled by power or scope: 0.78 reflects a categorical, purpose-blind prohibition enforced by judicial review, funding leverage, and a spreading chill that reaches programs the holdings do not formally touch. Theater_ratio 0.45 reflects the widening gap between neutrality rhetoric and operational reality — legacy and donor preferences untouched while essay-proxies are eyed suspiciously, and institutions performing visible compliance. Accessibility_collapse 0.65: race-conscious instruments are totally foreclosed, but race-neutral substitutes (percentage plans, socioeconomic preferences) remain lawful though chilled. Resistance 0.65: four-Justice dissents, a large scholarly opposition, state-level preservation movements, and ongoing proxy litigation. The measurement series run on one shared seven-point grid (0=1989 Croson, 6=1995 Adarand, 12=2003 Grutter, 18=2007 Parents Involved, 24=2016 Fisher II, 30=2023 SFFA, 36=2025), with every tracked metric authored at every point. The suppression series is the story's tracked dynamic — enforcement intensification from balancing to per se bar to proxy-chilling, with the 2003 dip marking Grutter's temporary check. The ε series is deliberately near-flat: the reading never regarded its own rule as extractive, and honesty forbids manufacturing drift that the reading's lights do not show. Claim and metrics are independent: claimed_type tangled_rope is my structural judgment; the metrics are my descriptive judgments; the engine computes the seats.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute very differently. From the judiciary's seat the rule is the neutral administration of a fixed text — near-beneficiary position, and the seat that collects the arrangement's principal product (final authority over race policy). From the disfavored-applicant seat the rule is a subsidy: it removes a penalty they bore. From the historically-excluded-applicant seat the same rule operates as full-target extraction amplified by trapped exit — they bear inherited costs and hold no exit, no forum, and no entitlement-recognition. Two organized actors at identical nominal power — the originalist_legal_movement and civil_rights_advocacy_organizations — sit on opposite sides of the categorical line, so their computed positions diverge entirely on constraint-specific structure, not global power. Identity-lock dynamics: the originalist movement's fusion is ideological (the reading IS the movement's warrant; exit is unthinkable without dissolving the movement), and the judiciary's is institutional ('we apply the text' — adjudicative identity constituted by the fixed-text posture). If the originalist identity frame broke, the beneficiary coalition would fragment and the rule's enforcement constituency would shrink to the bench alone.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations (disfavored_race_applicants, originalist_legal_movement, race_preference_opposing_public) drive low d for those seats, with mobile exit pulling the applicant seat nearest the subsidy end. The victim declarations (historically_excluded_minority_applicants, diversity_minded_public_universities, voluntary_integration_districts, civil_rights_advocacy_organizations) drive high d, with trapped exit pushing the minority-applicant seat toward the full-target end and the universities' secondary beneficiary role moderating their d below a pure payer's. Coalition check: the primary victim seat is individually powerless, and although coalition vehicles exist (the advocacy organizations), judicial insulation routes coalition power through appointment politics on a generational lag — so coalition potential does not rescue that seat's computed position. The judiciary carries no beneficiary/victim declaration; its authority-capture position is recorded here and in gain_flow rather than forced through the arrays. I omitted directionality_overrides deliberately: overrides key on power atoms, and the institutional tier contains both the judiciary and the universities with opposed structural positions — a single institutional-tier override would mislabel one of them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — state-enforced racial caste — is half-dead and half-live, hence founding_problem_status: contested. Formally dead: legal caste is abolished, and the uniform nondiscrimination rule is a genuine coordination achievement that outlives its original emergency. Live: the mechanisms that maintain racial disparity persist, and the rule forecloses the collective instruments that addressed them. The mismatch consumer reads status x verdict: contested + world_rearranges yields no zombie flag, correctly — this is not a mandate-outlived shell but an actively expanding arrangement. The classification discipline prevents two opposite mislabels: calling this a rope ignores the asymmetric closure of the remedial pathway (real victims, real enforcement); calling it a snare ignores the genuine uniform-rule coordination function and the broad, sincere beneficiary base that is not a cover story. Tangled_rope holds both horns. The piton test fails on both prongs: there is a concentrated receiver of the arrangement's principal product (the judiciary's authority capture), and fixing is prohibitive for every actor positioned to attempt it — not the neglect-and-theater profile of an atrophied leftover.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the equal_protection_kernel; would instantiating the remedial or antisubordination reading instead change the constraint''s beneficiary/victim structure and classification outright?',
    'Compare the sibling constraint files directly: the remedial reading declares a different victim set (documented-discrimination victims denied tailored remedies) and the antisubordination reading declares hierarchy-entrenching state action as its target; the engine''s per-seat classifications over each file make the divergence computable rather than arguable.',
    'If a sibling reading were the operative doctrine, the victim set, directionality map, and likely the computed type all shift; this file''s classification is valid only for the colorblind instantiation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer-frame omega: which reading of the Equal Protection kernel is instantiated, and what the siblings would change structurally.').

omega_variable(
    discovered_vs_constructed_rule,
    'Is the colorblind rule a discovered feature of the Constitution''s fixed meaning (presented by its adherents as inevitable, mountain-like), or a constructed doctrinal choice maintained by an identifiable enforcement coalition with identifiable beneficiaries?',
    'Originalist methodological audit: if competent original-meaning analysis converges on categorical prohibition independently of outcome preference, the discovered reading strengthens; if the historical record (Reconstruction debates, early enforcement practice, upheld race-conscious Reconstruction statutes) shows the founders themselves used racial classifications for remedial ends, the constructed reading prevails.',
    'If constructed, the rule''s presentation-as-nature is rhetorical armor over an enforceable choice, and its classification should weight the enforcement and beneficiary structure heavily; if discovered, the rule approaches a fixed-text mountain and the extraction analysis re-referents to what the rule strikes down rather than the rule itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discovered_vs_constructed_rule, conceptual, 'Whether the constraint is naturalized fact or maintained construction — the ambiguity its own rhetoric trades on.').

omega_variable(
    remedial_entitlement_status,
    'Does the closure of the remedial pathway constitute extraction from historically excluded groups, or the mere absence of an entitlement they never possessed?',
    'Not resolvable by data alone: it turns on whether historical injustice generates present-day corrective claims — a question of political morality. Track how successive courts and polities answer it, and whether compensatory institutions emerge outside constitutional law (legislative, private, international).',
    'If corrective claims are recognized, the rule''s victim seat carries genuine extraction and the tangled_rope asymmetry hardens toward snare at that seat; if not, the reading''s low-ε self-assessment stands and the rule looks closer to rope-plus-chill.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedial_entitlement_status, preference, 'The value question underneath the reading-indexed epsilon divergence between this reading and its siblings.').

omega_variable(
    proxy_substitution_effect,
    'Will race-neutral proxies (income preferences, percentage plans, adversity factors) reproduce meaningful access for historically excluded groups, or fail and entrench the disparity the struck-down instruments addressed?',
    'Longitudinal cohort studies of post-SFFA admissions at comparable institutions: enrollment shares, yield, and graduation outcomes for the affected groups under race-neutral regimes versus the pre-SFFA baseline.',
    'If proxies substantially reproduce access, the rule''s effective victim set shrinks and its extraction profile softens; if they fail, the asymmetry between who the rule protects and who it burdens hardens, and pressure for a sibling reading''s revival grows.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proxy_substitution_effect, empirical, 'Whether the rule''s distributive burden is as large in practice as its foreclosure is in form.').

omega_variable(
    enforcement_chill_scope,
    'How far does the rule''s suppressive force extend beyond its formal holdings — into race-neutral DEI programming, scholarships, faculty hiring, and private institutions anticipating liability?',
    'Audit institutional behavior post-SFFA: program closures, counsel memos, state enforcement letters, and litigation filings targeting conduct the holdings do not squarely prohibit; compare formal holding scope against observed compliance retreat.',
    'If the chill is wide, the authored suppression understates the rule''s operative force and the enforcement-intensification trajectory continues past the interval end; if narrow, suppression is bounded by doctrine and the current value is calibrated correctly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_chill_scope, empirical, 'The gap between what the rule prohibits and what it effectively suppresses.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_kernel__colorblind_reading, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epk_colorblind_reading_tr_t0, equal_protection_kernel__colorblind_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(epk_colorblind_reading_tr_t6, equal_protection_kernel__colorblind_reading, theater_ratio, 6, 0.12).
narrative_ontology:measurement(epk_colorblind_reading_tr_t12, equal_protection_kernel__colorblind_reading, theater_ratio, 12, 0.15).
narrative_ontology:measurement(epk_colorblind_reading_tr_t18, equal_protection_kernel__colorblind_reading, theater_ratio, 18, 0.22).
narrative_ontology:measurement(epk_colorblind_reading_tr_t24, equal_protection_kernel__colorblind_reading, theater_ratio, 24, 0.28).
narrative_ontology:measurement(epk_colorblind_reading_tr_t30, equal_protection_kernel__colorblind_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(epk_colorblind_reading_tr_t36, equal_protection_kernel__colorblind_reading, theater_ratio, 36, 0.45).

% Extraction over time
narrative_ontology:measurement(epk_colorblind_reading_be_t0, equal_protection_kernel__colorblind_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(epk_colorblind_reading_be_t6, equal_protection_kernel__colorblind_reading, base_extractiveness, 6, 0.1).
narrative_ontology:measurement(epk_colorblind_reading_be_t12, equal_protection_kernel__colorblind_reading, base_extractiveness, 12, 0.11).
narrative_ontology:measurement(epk_colorblind_reading_be_t18, equal_protection_kernel__colorblind_reading, base_extractiveness, 18, 0.13).
narrative_ontology:measurement(epk_colorblind_reading_be_t24, equal_protection_kernel__colorblind_reading, base_extractiveness, 24, 0.14).
narrative_ontology:measurement(epk_colorblind_reading_be_t30, equal_protection_kernel__colorblind_reading, base_extractiveness, 30, 0.15).
narrative_ontology:measurement(epk_colorblind_reading_be_t36, equal_protection_kernel__colorblind_reading, base_extractiveness, 36, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(epk_colorblind_reading_su_t0, equal_protection_kernel__colorblind_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(epk_colorblind_reading_su_t6, equal_protection_kernel__colorblind_reading, suppression_requirement, 6, 0.42).
narrative_ontology:measurement(epk_colorblind_reading_su_t12, equal_protection_kernel__colorblind_reading, suppression_requirement, 12, 0.38).
narrative_ontology:measurement(epk_colorblind_reading_su_t18, equal_protection_kernel__colorblind_reading, suppression_requirement, 18, 0.52).
narrative_ontology:measurement(epk_colorblind_reading_su_t24, equal_protection_kernel__colorblind_reading, suppression_requirement, 24, 0.56).
narrative_ontology:measurement(epk_colorblind_reading_su_t30, equal_protection_kernel__colorblind_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(epk_colorblind_reading_su_t36, equal_protection_kernel__colorblind_reading, suppression_requirement, 36, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_kernel__colorblind_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_protection_kernel__colorblind_reading, equal_protection_kernel__remedial_reading).
narrative_ontology:affects_constraint(equal_protection_kernel__colorblind_reading, equal_protection_kernel__antisubordination_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the equal_protection_kernel per the ε-invariance principle: the colloquial label 'what the Equal Protection Clause requires' conflates three structurally distinct constraints — the colorblind reading (this file: categorical prohibition, beneficiaries among formerly disfavored applicants, victims among the historically excluded), the remedial reading (conditional permission under narrow tailoring; victims are documented-discrimination claimants denied tailored remedies), and the antisubordination reading (target is hierarchy-entrenching state action; victims are groups facing entrenched subordination). Each file carries its own ε, its own beneficiary/victim structure, and its own cs_structure axioms; the upstream/downstream pressure between them runs through appointment politics and doctrinal legitimacy rather than logical entailment within any single framework. This file links to both siblings; each sibling links back.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
