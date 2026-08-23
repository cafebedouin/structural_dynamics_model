% ============================================================================
% CONSTRAINT STORY: equal_protection_kernel__colorblind_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-09-01
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: equal_protection_kernel__colorblind_reading
 *   human_readable: Colorblind Reading of Equal Protection: Categorical Bar on State Racial Classifications
 *   domain: legal/political
 *
 * SUMMARY:
 *   A categorical rule, authored by a winning coalition within the Supreme
 *   Court and consolidated through four decades of narrowing decisions,
 *   forbids state institutions from sorting people by race for any purpose
 *   whatever. Where the clause once licensed remedial designs, the rule now
 *   refuses every classification request outright: selective-university
 *   admissions, school-integration plans, and contracting set-asides must
 *   operate through formally unmarked criteria. The rule performs a real
 *   coordination function — one uniform, administrable standard that ends
 *   per-case motive testing and guarantees every applicant identical formal
 *   treatment — while transferring opportunity and discretion along an
 *   asymmetric gradient: the designated pathway closes for groups still
 *   carrying the accumulated effects of past exclusion, and the freed
 *   capacity accrues to unmarked competitors and to the interpreting
 *   coalition's authority. KEY AGENTS (by structural relationship): -
 *   supreme_court_colorblind_coalition: agenda setter
 *   (institutional/constrained) — writes, revises, and enforces the rule;
 *   collects interpretive authority; bound by its own published reasoning -
 *   historically_excluded_applicants: primary target (powerless/constrained)
 *   — lose the designated pathway; race-neutral substitutes underdeliver -
 *   formally_advantaged_applicants: primary beneficiary (moderate/mobile) —
 *   receive the redistributed odds under unmarked competition -
 *   colorblind_legal_movement: secondary beneficiary (powerful/mobile) —
 *   four-decade litigation network collecting prestige, fees, and agenda
 *   power from the win - civil_rights_advocacy_organizations: payer
 *   (organized/identity_locked) — principal legal instrument removed;
 *   identity fused to the remedial project -
 *   selective_university_administrators: payer (powerful/constrained) —
 *   redesign around proxies; cannot exit funding conditions or review -
 *   majority_minority_school_districts: payer (moderate/constrained) —
 *   integration designs invalidated; absorb redesign costs -
 *   federally_recognized_tribal_nations: excluded voice
 *   (organized/constrained) — never seated in the adjudication reshaping
 *   their programs - constitutional_scholars: analytical observer — maps the
 *   rule's reach and coherence Family note: this story is the colorblind
 *   emission of the equal_protection_kernel. The remedial and
 *   antisubordination emissions are separate stories with their own epsilon
 *   values, beneficiary sets, and types, linked through
 *   network.affects_constraints; the kernel contest itself lives in the omega
 *   variables and commentary.kernel_context, not inside this constraint's
 *   classification. Claim/metric independence: the type claim below is
 *   authored from the structure just described; the metrics are authored from
 *   the rule's observed operation across 1978-2026; neither was tuned to
 *   match the other or any predicted engine output.
 *
 * KEY AGENTS:
 *   - supreme_court_colorblind_coalition: agenda setter (institutional/constrained) — holds exclusive custody of the clause's meaning; accrues interpretive authority from enforcing the categorical rule
 *   - historically_excluded_applicants: primary target (powerless/constrained) — compete from positions shaped by prior exclusion the rule forbids anyone to weigh
 *   - formally_advantaged_applicants: primary beneficiary (moderate/mobile) — odds improve when group-conscious evaluation ends
 *   - colorblind_legal_movement: secondary beneficiary (powerful/mobile) — law firms, donors, and think tanks collecting the returns of a forty-year litigation campaign
 *   - civil_rights_advocacy_organizations: payer (organized/identity_locked) — mission fused to the remedial project whose legal instrument was removed
 *   - selective_university_administrators: payer (powerful/constrained) — wealthy and expert but unable to exit funding or review; rebuild admissions through unnamed proxies
 *   - majority_minority_school_districts: payer (moderate/constrained) — integration planning invalidated absent provable prior state-enforced segregation
 *   - federally_recognized_tribal_nations: excluded voice (organized/constrained) — sovereign actors swept into a racial frame they never joined, without party status
 *   - constitutional_scholars: analytical observer — traces reach, coherence, and historical fidelity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_kernel__colorblind_reading, 0.68).
domain_priors:suppression_score(equal_protection_kernel__colorblind_reading, 0.64).
domain_priors:theater_ratio(equal_protection_kernel__colorblind_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_kernel__colorblind_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_kernel__colorblind_reading, "Colorblind Reading of Equal Protection: Categorical Bar on State Racial Classifications").
narrative_ontology:topic_domain(equal_protection_kernel__colorblind_reading, "legal/political").

domain_priors:requires_active_enforcement(equal_protection_kernel__colorblind_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_kernel__colorblind_reading, 'a360d6a8-f4ca-4314-a051-a54c44e6b076').
narrative_ontology:cs_kernel_codification('a360d6a8-f4ca-4314-a051-a54c44e6b076', fixed_text).
narrative_ontology:cs_authority_grounding('a360d6a8-f4ca-4314-a051-a54c44e6b076', lineage).
narrative_ontology:cs_interpretation_layer_present('a360d6a8-f4ca-4314-a051-a54c44e6b076').
narrative_ontology:cs_reading_relation('a360d6a8-f4ca-4314-a051-a54c44e6b076', equal_protection_kernel__remedial_reading, forecloses).
narrative_ontology:cs_reading_relation('a360d6a8-f4ca-4314-a051-a54c44e6b076', equal_protection_kernel__antisubordination_reading, forecloses).
narrative_ontology:cs_axiom('a360d6a8-f4ca-4314-a051-a54c44e6b076', foundational, racial_classification_per_se_unconstitutional).
narrative_ontology:cs_axiom_status(racial_classification_per_se_unconstitutional, holdable).
narrative_ontology:cs_axiom_grounding('a360d6a8-f4ca-4314-a051-a54c44e6b076', racial_classification_per_se_unconstitutional, deontological).
narrative_ontology:cs_axiom('a360d6a8-f4ca-4314-a051-a54c44e6b076', secondary, ancestry_confers_no_present_obligation).
narrative_ontology:cs_axiom_status(ancestry_confers_no_present_obligation, holdable).
narrative_ontology:cs_axiom_grounding('a360d6a8-f4ca-4314-a051-a54c44e6b076', ancestry_confers_no_present_obligation, deontological).
narrative_ontology:cs_reference_frame('a360d6a8-f4ca-4314-a051-a54c44e6b076', original_meaning_formal_equality_baseline).
narrative_ontology:cs_drift_state('a360d6a8-f4ca-4314-a051-a54c44e6b076', contemporary_post_categorical_turn, gap(repudiation_pressure, minor, true)).
narrative_ontology:cs_created_at('a360d6a8-f4ca-4314-a051-a54c44e6b076', '').
narrative_ontology:cs_kernel_id(equal_protection_kernel__colorblind_reading, equal_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_kernel__colorblind_reading, formally_advantaged_applicants).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__colorblind_reading, colorblind_legal_movement).
narrative_ontology:constraint_victim(equal_protection_kernel__colorblind_reading, historically_excluded_applicants).
narrative_ontology:constraint_victim(equal_protection_kernel__colorblind_reading, selective_university_administrators).
narrative_ontology:constraint_victim(equal_protection_kernel__colorblind_reading, majority_minority_school_districts).
narrative_ontology:constraint_victim(equal_protection_kernel__colorblind_reading, civil_rights_advocacy_organizations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds exclusive custody of the clause's meaning and writes the operative rule: classification requests fall unless the state can show they serve no racial purpose at all. Each ruling extends or trims the rule's reach; the coalition's members accumulate doctrinal legacy, citation networks, and interpretive authority from administering it. Departing from the rule would mean repudiating their own published reasoning.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, supreme_court_colorblind_coalition, agenda_setter,
    institutional, generational, constrained, national).

% Compete for selective seats from starting positions shaped by generations of exclusion that the rule forbids anyone to weigh. Race-neutral channels — geography, income, adversity narratives — remain open, with uncertain yield. Exiting the pool means forgoing the selective institutions altogether; waiting means the rule hardens further within their lifetimes.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, historically_excluded_applicants, payer,
    powerless, biographical, constrained, national).

% Gain admission odds when group-conscious evaluation ends, since competition resets to unmarked metrics where their profiles fare well. Many supported the change through organized litigation contributions. Their stake is direct and material, and they can move freely among institutions under any regime.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, formally_advantaged_applicants, beneficiary,
    moderate, biographical, mobile, national).

% A coordinated network of law firms, donors, and research organizations that spent four decades building the litigation path to this rule. The win yields fees, professional prestige, clerkship pipelines, and agenda-setting power. Members can redeploy to new fronts — compliance auditing, proxy policing — anywhere in the country.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, colorblind_legal_movement, beneficiary,
    powerful, generational, mobile, national).

% Built their litigation and policy machinery around securing race-conscious remedies. The rule removes their principal legal instrument. Their identity, staff culture, and donor base are fused to the remedial project — abandoning it would dissolve the organizations' reason to exist — yet every courtroom route they know is now closed, leaving ballot measures and persuasion as slow substitutes.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, civil_rights_advocacy_organizations, payer,
    organized, generational, identity_locked, national).

% Designed and defended the programs the rule invalidated; now rebuilding admissions through proxies that satisfy reviewers without naming race. They retain wealth, data, and expertise, but cannot exit federal funding conditions or the reviewing authority, and open defiance invites loss of funds and fresh litigation.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, selective_university_administrators, payer,
    powerful, generational, constrained, national).

% Districts serving predominantly non-white student bodies that once used weighted attendance zones and themed magnets to integrate. The rule invalidates such designs absent proof of prior state-enforced segregation. Planning staff absorb redesign costs; the available toolkit narrows to whichever instruments survive review.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, majority_minority_school_districts, payer,
    moderate, biographical, constrained, regional).

% Sovereign peoples whose citizenship criteria and serving institutions are increasingly discussed through a racial-preference lens they never consented to. They held no party status in the litigation that produced the rule; their treaty and consultation relationships with the federal government now run through doctrines they did not shape.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, federally_recognized_tribal_nations, excluded,
    organized, generational, constrained, national).

% Map the rule's reach, test its coherence against the documentary record, and forecast its next applications. They neither collect from nor bear the rule; their analyses feed both the governing coalition and its opponents.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, constitutional_scholars, observer,
    analytical, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_kernel__colorblind_reading, formally_advantaged_applicants).
narrative_ontology:fixing_cost_class(equal_protection_kernel__colorblind_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies every state actor one uniform, administrable answer to classification requests — refuse them — eliminating per-case motive testing, proportionality analysis, and group-by-group adjudication, and guaranteeing members of any group identical formal treatment under state procedures, in either direction of favoritism.
% TRANSFER_FUNCTION: Moves selective-education opportunity, contracting set-asides, and local integration-tool discretion away from race-designated recipients and race-conscious planners toward formally unmarked competitors; moves interpretive discretion upward from local institutions to the reviewing coalition.
% ABSENT_VOICES: Descendant communities of enslaved and Jim Crow populations hold no seat — standing doctrine admits only litigants, so those bearing the longest accumulation of the harm never voted in the rulings that closed their remedial route. Federally recognized tribal nations learned of the rule's reach toward their institutions through litigation footnotes. Future applicant cohorts cannot yet object. These voices appear as amicus briefs and dissenting commentary, not as seated parties.
% DISAPPEARANCE_RATIONALE: Overnight removal would reopen every closed channel at once: institutions shelving race-conscious designs would reinstate them within an admissions cycle, K-12 integration planning would restart where invalidated, contracting set-asides would return pending fresh review, the litigation economy built around attacking or defending classifications would reorganize around the new frontier, and the reviewing coalition's recent rulings would stand as orphaned precedent. The underlying demand for the banned tools has not disappeared; only their permission has.
% FOUNDING_PROBLEM: Prevent the United States government from creating, enforcing, or ratifying racial caste — articulated in Harlan's nineteenth-century dissent against legalized caste and carried through the mid-century assault on Jim Crow; the modern coalition redeploys the same problem-statement against remedial and diversity classifications themselves.
% FOUNDING_PROBLEM_CORROBORATION: Historians of Reconstruction and of the civil-rights movement, writing from outside the benefiting coalition, corroborate that the founding problem was real and that the clause was deployed against it. What no source outside the contest adjudicates is whether the problem remains live today: the colorblind coalition attests it persists in every remaining classification, while civil-rights scholarship attests that the live caste-risk now includes the rule's own blind operation. Corroboration exists for the origin; the present tense is deliberately left uncorroborated, which is itself the signal.
narrative_ontology:disappearance_verdict(equal_protection_kernel__colorblind_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_kernel__colorblind_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_kernel__colorblind_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equal_protection_kernel__colorblind_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_kernel__colorblind_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_kernel__colorblind_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equal_protection_kernel__colorblind_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equal_protection_kernel__colorblind_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Why tangled_rope: the rule solves a real coordination problem — one administrable standard replacing per-case motive testing, protecting members of every group from official sorting — and it requires continuous enforcement (the 1989-2023 decision chain progressively widened its bite; agency investigation and accreditation leverage now patrol compliance), so it is not a self-sustaining norm. The same structure transfers asymmetrically: the designated pathway closes for groups whose starting positions still encode past exclusion, while freed capacity accrues to unmarked competitors and to the interpreting coalition. Beneficiaries and victims are both declared and enforcement is active, giving the hybrid signature.
 *   
 *   Metric rationale. Extractiveness 0.68 reflects a rule whose bite is now categorical and whose costs land on identifiable payers with thin exits. Suppression 0.64 is raw structural foreclosure — judicial supremacy leaves state and local actors no compliant route back to race-conscious design — authored unscaled, since only extractiveness is scaled by directionality and spatial scope in the engine. Theater_ratio 0.34 is modest: the rule bites, but a visible share of compliance activity is proxy choreography (adversity essays, income bands tuned to track race) that maintains the appearance of neutrality while approximating the forbidden design. Accessibility_collapse 0.55: open classification is fully closed once the rule is understood, but race-neutral substitutes persist and are themselves contestable. Resistance 0.58: published dissents, scholarly opposition, and organizational counterpressure persist without current leverage to reverse the rule.
 *   
 *   Temporal shape. All three series share one eight-point grid (1978, 1989, 1995, 2003, 2007, 2016, 2023, 2026), every tracked metric authored at every shared point. The trajectory is a ratchet, not a cycle: set-aside strikethroughs, scrutiny-without-remedial-leniency for federal programs, K-12 plan invalidation, then the categorical turn each permanently widened enforcement, with suppression_requirement rising accordingly. Theater declined as rhetoric became law, then ticked up as proxy compliance grew after the categorical turn.
 *   
 *   Coalition note: the primary payers' historic coalition channel was litigation infrastructure the rule itself dismantled; remaining channels (ballot initiative, legislation) meet the same reading at review, which is why resistance stays high without becoming effective.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat the rule computes as lawful coordination finally vindicated: neutral, uniform, requiring no further justification. From the payer seats it computes as enforced subtraction: a bar arriving with no compensating channel, policed by review the payers cannot decline. Formally advantaged applicants experience neither — simple fairness restoring unmarked evaluation. Same nominal subject, three different constraint-shapes by seat; the engine derives this divergence from power, exit, and role data, and the divergence — not any single seat's report — is the measurement.
 *   
 *   Inter-institutional dynamics: the reviewing coalition holds jurisdictional monopoly; selective universities retain wealth and expertise but no exit from funding conditions or review; districts hold neither wealth nor exit; the excluded tribes hold neither seat nor veto. Identically bound by the rule, the institutions differ wholly in what compliance costs them.
 *   
 *   Same-stage asymmetry: two applicant populations occupy the identical procedural position, yet exit and incidence differ completely — mobility and metric-fit for one, constrained pools and metric-mismatch for the other. The constraint differentiates nominal peers by structural fit, not by rank.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive directionality toward the subsidy end: formally_advantaged_applicants (mobile exit, direct material gain) sit nearest zero; colorblind_legal_movement (mobile, generational horizon) collects derivative gains — prestige, pipeline, agenda-setting — damping its value similarly. Victim declarations drive directionality toward the target end: historically_excluded_applicants (powerless, constrained — the pool cannot be exited without forfeiting the prize) approach the full-target pole; selective_university_administrators (constrained by funding and review) and majority_minority_school_districts (few surviving tools, regional scope) sit close behind; civil_rights_advocacy_organizations carry identity_locked exit — their mission-fusion means the loss travels with them wherever they go — placing them effectively at the target pole despite organized power. Effective extraction concentrates where exit is thinnest, and the rule's national scope amplifies verification difficulty for every payer simultaneously.
 *   
 *   One override: supreme_court_colorblind_coalition would derive ambiguously from its role alone (an administrator is not obviously a beneficiary), but structurally the coalition accrues interpretive authority — a rent collected in precedent, citation, and doctrinal legacy — from sole custody of the reading, so its directionality is pinned near the beneficiary end at 0.15. Identity-lock note: the advocacy organizations' lock is ideological and institutional — the organizations became the remedial project; if the remedial frame regained custody of the kernel, their exit would reopen and their directionality would swing back toward the beneficiary side of the same structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two mislabels at once. Read flatly, the rule presents as a rope: everyone equally bound by one fair standard. Seat-level computation exposes the gradient — identical formal treatment redistributes real capacity — blocking the rope mislabel. The opponents' counterclaim, that the arrangement is pure extraction wearing fairness as a costume, is blocked by the declared coordination function: the uniform administrable standard and bidirectional favoritism-prevention are genuine, and removing them would leave state actors with no common answer to classification requests at all. Genealogy: the reading's founding problem — preventing state-made racial caste — was articulated by Harlan against legalized caste and is attested by historians outside the benefiting coalition; whether that problem is live, dead, or transformed is precisely what the sibling readings dispute, so founding_problem_status is authored contested rather than resolved. Because status is contested (not dead) alongside a world_rearranges verdict, the mismatch consumer finds no zombie signature; the arrangement persists because undoing it is prohibitive for the only agent positioned to undo it, not because anyone believes its work is finished.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    equal_protection_kernel_membership,
    'This constraint instantiates the colorblind_reading of the equal_protection_kernel; what changes structurally if a sibling reading takes custody of the kernel instead?',
    'Compare per-seat classifications across the three reading-stories of the kernel: the remedial reading restores conditional permission and shrinks the burdened set to deniers of documented exclusion; the antisubordination reading flips the beneficiary and victim sets entirely, targeting hierarchy-entrenching use and freeing dismantling use.',
    'Kernel custody determines who counts as protected and who as burdened. A sibling takeover converts this story''s beneficiaries into payers and vice versa, moving the family''s center of gravity away from the hybrid coordination-plus-transfer shape toward a cleaner coordination shape.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(equal_protection_kernel_membership, conceptual, 'Committer-frame indexicality: one kernel, three incompatible constraint emissions; this file is the categorical emission.').

omega_variable(
    touchstone_disagreement_location,
    'Where in the structure do the readings actually diverge — on whether racial classification per se is the harm, on whether purpose or subordination-effect is the touchstone, or on whether documented remedial need licenses departure?',
    'Trace which premise each reading refuses in appellate reasoning that must choose one touchstone: categorical refusal (this reading), evidence-keyed conditional permission (remedial), direction-keyed hierarchy test (antisubordination).',
    'If the operative touchstone migrates toward purpose-testing, this reading''s categorical axiom loses its load-bearing status and the constraint drifts toward the remedial emission''s behavior.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(touchstone_disagreement_location, conceptual, 'The readings disagree on the test''s touchstone, not on the clause''s existence or authority.').

omega_variable(
    race_neutral_proxy_equivalence,
    'Can race-neutral instruments (income tiers, geographic weighting, adversity essays) reproduce the compositional outcomes of the prohibited classifications, or do they systematically underdeliver?',
    'Longitudinal enrollment and outcome data at transitioning institutions, compared against pre-transition baselines under matched selectivity controls.',
    'Proxy equivalence would lower the measured burden (the coordination aim achieved at formal-equality prices); systematic underdelivery confirms the prohibition''s costs concentrate on historically excluded applicants with no functional substitute, pushing the story toward the pure-extraction boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(race_neutral_proxy_equivalence, empirical, 'Whether the banned tool has a working substitute determines how subtractive the ban is.').

omega_variable(
    service_academy_carveout_stability,
    'The categorical rule was announced with an express carve-out for military service academies; does that carve-out persist, revealing purpose-sensitivity inside the supposedly purpose-blind rule?',
    'Track subsequent litigation, agency guidance, and institutional practice touching academy admissions and Native-serving institutions; a durable carve-out indicates the categorical axiom bends where the coalition''s own institutional interests sit.',
    'A stable carve-out weakens the foundational axiom from within, dating an axiom-overriding drift earlier than the surface doctrine admits and softening the foreclosure edges declared against both sibling readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(service_academy_carveout_stability, empirical, 'The internal exception tests the categorical claim''s sincerity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_kernel__colorblind_reading, 1978, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epk_colorblind_tr_t1978, equal_protection_kernel__colorblind_reading, theater_ratio, 1978, 0.44).
narrative_ontology:measurement(epk_colorblind_tr_t1989, equal_protection_kernel__colorblind_reading, theater_ratio, 1989, 0.4).
narrative_ontology:measurement(epk_colorblind_tr_t1995, equal_protection_kernel__colorblind_reading, theater_ratio, 1995, 0.38).
narrative_ontology:measurement(epk_colorblind_tr_t2003, equal_protection_kernel__colorblind_reading, theater_ratio, 2003, 0.36).
narrative_ontology:measurement(epk_colorblind_tr_t2007, equal_protection_kernel__colorblind_reading, theater_ratio, 2007, 0.35).
narrative_ontology:measurement(epk_colorblind_tr_t2016, equal_protection_kernel__colorblind_reading, theater_ratio, 2016, 0.33).
narrative_ontology:measurement(epk_colorblind_tr_t2023, equal_protection_kernel__colorblind_reading, theater_ratio, 2023, 0.31).
narrative_ontology:measurement(epk_colorblind_tr_t2026, equal_protection_kernel__colorblind_reading, theater_ratio, 2026, 0.34).

% Extraction over time
narrative_ontology:measurement(epk_colorblind_be_t1978, equal_protection_kernel__colorblind_reading, base_extractiveness, 1978, 0.14).
narrative_ontology:measurement(epk_colorblind_be_t1989, equal_protection_kernel__colorblind_reading, base_extractiveness, 1989, 0.28).
narrative_ontology:measurement(epk_colorblind_be_t1995, equal_protection_kernel__colorblind_reading, base_extractiveness, 1995, 0.36).
narrative_ontology:measurement(epk_colorblind_be_t2003, equal_protection_kernel__colorblind_reading, base_extractiveness, 2003, 0.41).
narrative_ontology:measurement(epk_colorblind_be_t2007, equal_protection_kernel__colorblind_reading, base_extractiveness, 2007, 0.49).
narrative_ontology:measurement(epk_colorblind_be_t2016, equal_protection_kernel__colorblind_reading, base_extractiveness, 2016, 0.53).
narrative_ontology:measurement(epk_colorblind_be_t2023, equal_protection_kernel__colorblind_reading, base_extractiveness, 2023, 0.66).
narrative_ontology:measurement(epk_colorblind_be_t2026, equal_protection_kernel__colorblind_reading, base_extractiveness, 2026, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(epk_colorblind_su_t1978, equal_protection_kernel__colorblind_reading, suppression_requirement, 1978, 0.24).
narrative_ontology:measurement(epk_colorblind_su_t1989, equal_protection_kernel__colorblind_reading, suppression_requirement, 1989, 0.31).
narrative_ontology:measurement(epk_colorblind_su_t1995, equal_protection_kernel__colorblind_reading, suppression_requirement, 1995, 0.37).
narrative_ontology:measurement(epk_colorblind_su_t2003, equal_protection_kernel__colorblind_reading, suppression_requirement, 2003, 0.43).
narrative_ontology:measurement(epk_colorblind_su_t2007, equal_protection_kernel__colorblind_reading, suppression_requirement, 2007, 0.47).
narrative_ontology:measurement(epk_colorblind_su_t2016, equal_protection_kernel__colorblind_reading, suppression_requirement, 2016, 0.5).
narrative_ontology:measurement(epk_colorblind_su_t2023, equal_protection_kernel__colorblind_reading, suppression_requirement, 2023, 0.61).
narrative_ontology:measurement(epk_colorblind_su_t2026, equal_protection_kernel__colorblind_reading, suppression_requirement, 2026, 0.64).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_kernel__colorblind_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_protection_kernel__colorblind_reading, equal_protection_kernel__remedial_reading).
narrative_ontology:affects_constraint(equal_protection_kernel__colorblind_reading, equal_protection_kernel__antisubordination_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'equal protection' covers three structurally distinct claims that cannot share one epsilon. The colorblind emission (this file, categorical forbiddance) sits downstream in custody: its rulings strip the remedial emission of the permission space it presumes, and they invert the antisubordination emission's direction test. Each member carries its own beneficiaries, victims, and claimed type; the family is linked through affects_constraints so contamination and custody-shift propagate visibly.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(equal_protection_kernel__colorblind_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
