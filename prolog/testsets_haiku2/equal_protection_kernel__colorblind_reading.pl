% ============================================================================
% CONSTRAINT STORY: equal_protection_kernel__colorblind_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: equal_protection_kernel__colorblind_reading
 *   human_readable: Equal Protection Colorblind Reading: Categorical Race Classification Ban
 *   domain: constitutional/educational/civil_rights
 *
 * SUMMARY:
 *   The colorblind reading of the Equal Protection Clause interprets the
 *   Fourteenth Amendment as a categorical prohibition on state use of racial
 *   classification, regardless of purpose or intent to remedy. Under this
 *   reading, any race-conscious admissions policy, affirmative action
 *   program, or targeted remedial initiative is per se unconstitutional. The
 *   reading frames constitutional obligation as formal equality (treating all
 *   races identically) rather than substantive equality (addressing the
 *   effects of historical discrimination). This is ONE contested reading of
 *   the equal protection kernel; the antisubordination reading and remedial
 *   reading offer structurally different interpretations of the same
 *   constitutional text and advance different definitions of what equal
 *   protection demands.
 *
 * KEY AGENTS:
 *   - Supreme Court (colorblind doctrine adherents): institutional agenda-setter; enforces the categorical ban on racial classification through injunction and precedent
 *   - historically excluded racial groups (particularly Black Americans, Latinx Americans, Native Americans): structural victims; lose access to race-conscious remedial pathways and affirmative action; bear the accumulated costs of historical discrimination without institutional corrective mechanisms
 *   - educational institutions and employers: partially coerced beneficiaries; forced to adopt race-neutral admissions and hiring procedures; comply under legal threat
 *   - applicants from majority-preferred groups (white applicants at selective institutions): framed as beneficiaries; present themselves as victims of reverse discrimination; actual structural benefit is complex
 *   - civil rights advocates and antisubordination theorists: excluded voices; argue the colorblind reading entrenches rather than dismantles racial hierarchy; would reshape the constraint if admitted to framing authority
 *   - originalist and formal-equality constitutional scholars: carriers of the color-blind doctrine; derive professional authority and legitimacy from defending colorblindness; institutional beneficiary
 *   - Congress and state legislatures proposing race-conscious programs: constrained by the doctrine; legislative remedies are judicially struck down
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_kernel__colorblind_reading, 0.68).
domain_priors:suppression_score(equal_protection_kernel__colorblind_reading, 0.72).
domain_priors:theater_ratio(equal_protection_kernel__colorblind_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_kernel__colorblind_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_kernel__colorblind_reading, "Equal Protection Colorblind Reading: Categorical Race Classification Ban").
narrative_ontology:topic_domain(equal_protection_kernel__colorblind_reading, "constitutional/educational/civil_rights").

domain_priors:requires_active_enforcement(equal_protection_kernel__colorblind_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_kernel__colorblind_reading, '5dd1dfc3-0317-46f9-b512-bc8fea203371').
narrative_ontology:cs_kernel_codification('5dd1dfc3-0317-46f9-b512-bc8fea203371', formalized).
narrative_ontology:cs_authority_grounding('5dd1dfc3-0317-46f9-b512-bc8fea203371', lineage).
narrative_ontology:cs_interpretation_layer_present('5dd1dfc3-0317-46f9-b512-bc8fea203371').
narrative_ontology:cs_reading_relation('5dd1dfc3-0317-46f9-b512-bc8fea203371', equal_protection_kernel__antisubordination_reading, forecloses).
narrative_ontology:cs_reading_relation('5dd1dfc3-0317-46f9-b512-bc8fea203371', equal_protection_kernel__remedial_reading, coexists_with).
narrative_ontology:cs_axiom('5dd1dfc3-0317-46f9-b512-bc8fea203371', foundational, racial_classification_categorically_impermissible).
narrative_ontology:cs_axiom_status(racial_classification_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('5dd1dfc3-0317-46f9-b512-bc8fea203371', racial_classification_categorically_impermissible, deontological).
narrative_ontology:cs_axiom('5dd1dfc3-0317-46f9-b512-bc8fea203371', foundational, constitution_is_color_blind).
narrative_ontology:cs_axiom_status(constitution_is_color_blind, holdable).
narrative_ontology:cs_axiom_grounding('5dd1dfc3-0317-46f9-b512-bc8fea203371', constitution_is_color_blind, deontological).
narrative_ontology:cs_reference_frame('5dd1dfc3-0317-46f9-b512-bc8fea203371', formal_equality_principle).
narrative_ontology:cs_drift_state('5dd1dfc3-0317-46f9-b512-bc8fea203371', contemporary_remedial_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5dd1dfc3-0317-46f9-b512-bc8fea203371', '').
narrative_ontology:cs_kernel_id(equal_protection_kernel__colorblind_reading, equal_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_kernel__colorblind_reading, applicants_claiming_race_neutral_harm).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__colorblind_reading, color_blind_doctrine_carriers).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__colorblind_reading, judges_enforcing_formal_equality).
narrative_ontology:constraint_victim(equal_protection_kernel__colorblind_reading, historically_excluded_racial_groups).
narrative_ontology:constraint_victim(equal_protection_kernel__colorblind_reading, race_conscious_remedial_programs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__colorblind_reading, educational_institutions).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__colorblind_reading, applicants_from_preferred_groups).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__colorblind_reading, constitutional_scholars_colorblind_tradition).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__colorblind_reading, employers_private_sector).
narrative_ontology:constraint_victim(equal_protection_kernel__colorblind_reading, educational_institutions).
narrative_ontology:constraint_victim(equal_protection_kernel__colorblind_reading, congress_and_legislatures).
narrative_ontology:constraint_victim(equal_protection_kernel__colorblind_reading, employers_private_sector).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Institutional seat that interprets the Constitution and sets the authoritative reading of the Equal Protection Clause. The majority faction (colorblind adherents) has established precedent making any race-conscious state action presumptively unconstitutional. They justify this as enforcing the Constitution's text and preventing racial discrimination. They control the doctrinal framework through opinion-writing and precedent-setting authority. A potential shift in Court composition or judicial philosophy could change the reading authority, but current majority maintains colorblind doctrine through active enforcement.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, supreme_court_colorblind_majority, agenda_setter,
    institutional, generational, arbitrage, national).

% Structurally barred from race-conscious remedial programs, affirmative action, and targeted diversity initiatives by the colorblind reading. Cannot exit the racial category used by the constraint to classify them. Suffer from accumulated historical discrimination (slavery, segregation, colonialism, immigration exclusion) whose effects persist across generations, yet the constraint forbids state action to remedy those effects. Their exit options are trapped/identity-locked: the racial classification is not a choice they can abandon or arbitrage away. They bear the extraction: remedial pathways are foreclosed, yet historical disadvantage compounds across generations.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, historically_excluded_racial_groups, payer,
    organized, generational, identity_locked, national).

% Universities and professional schools must comply with the colorblind reading under threat of litigation and loss of federal funding. They benefit from reduced legal exposure and litigation risk under a simple race-blind admissions standard. They pay by losing the ability to pursue diversity goals or remedial admissions aligned with their stated educational missions. Their exit is constrained: they cannot legally adopt race-conscious policies even if they believe those policies serve educational or remedial goals. The constraint forces a choice between legal compliance and institutional mission.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, educational_institutions, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_kernel__colorblind_reading, educational_institutions, payer).

% Applicants from majority-preferred racial backgrounds (principally white applicants at selective institutions) are protected from race-conscious admissions decisions by the colorblind reading. They argue they are victims of 'reverse discrimination' when race-conscious admissions consider historical exclusion of minority groups. The constraint frames their admission as based on purely individual merit. Their actual structural benefit is contested: some gain admission slots that would have gone to diversity-enhancing candidates; others would have been admitted anyway under any standard. Their exit options are mobile—if they face admissions rejection, they can apply to other institutions, relocate, pursue different careers.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, applicants_from_preferred_groups, beneficiary,
    moderate, biographical, mobile, national).

% Elected branches attempting to enact race-conscious remedial programs (voting rights protection, employment discrimination remedies, educational equity initiatives) find their authority constrained by the colorblind reading. Legislation can be struck down as unconstitutional, vetoed through judicial review, or preemptively abandoned when legislatures anticipate legal challenge. Their power to address historical discrimination through race-conscious policy is substantially reduced.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, congress_and_legislatures, payer,
    organized, generational, constrained, national).

% Advocates for antisubordination doctrine, remedial justice, and affirmative action are excluded from the Court's colorblind framing authority. Their arguments are heard in litigation but not adopted by the controlling majority. They would reshape the constraint if admitted to authoritative interpretation, but they hold no institutional seat in the Court that sets the reading.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, civil_rights_advocates, excluded,
    organized, generational, constrained, national).

% Originalist and formal-equality legal scholars derive professional authority and institutional standing from advancing the colorblind reading. Their scholarship is cited in Court opinions, shapes judicial reasoning, and establishes academic legitimacy. They benefit from the constraint by having their preferred interpretation become authoritative doctrine. They can arbitrage by publishing in elite venues, obtaining appointments at prestigious law schools, influencing judicial nominations and confirmation.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, constitutional_scholars_colorblind_tradition, beneficiary,
    moderate, generational, arbitrage, national).

% Private employers are constrained by employment discrimination law (Title VII, ADEA) but have some autonomy around affirmative action and diversity initiatives. The colorblind reading creates legal exposure for race-conscious hiring; some employers benefit from the constraint by being able to abandon costly diversity programs under cover of legal requirement. Others pay by losing ability to pursue workforce diversity aligned with their stated values.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, employers_private_sector, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_kernel__colorblind_reading, employers_private_sector, payer).

% The federal judiciary (particularly appellate courts) administers and enforces the colorblind reading through injunctions, summary judgments, and precedent. Lower courts apply or resist the Supreme Court's doctrine. Judicial power to enforce the constraint creates institutional stakes and career incentives for judges who develop colorblind jurisprudence.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, judicial_system_institutional, agenda_setter,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_kernel__colorblind_reading, supreme_court_colorblind_majority).
narrative_ontology:fixing_cost_class(equal_protection_kernel__colorblind_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a shared societal commitment that the state shall not make explicit classifications based on race; removes racial decision-making from formal state authority; establishes a baseline that state action should be race-neutral. This solves a coordination problem for a multiracial polity: agreement on formal legal equality (the state treats all races the same) is easier than debating legitimate uses of racial classification in law.
% TRANSFER_FUNCTION: Transfers remedial authority away from state institutions (courts, legislatures, agencies) to private actors (individuals, employers, educational institutions) to address historical racial discrimination. Also transfers the framing of race-conscious admissions from 'correction of historical injustice' to 'violation of equal protection.' Those who would implement race-conscious programs lose institutional access; those who implement race-neutral programs are protected from legal challenge.
% ABSENT_VOICES: Civil rights advocates who endorse the antisubordination reading or remedial reading; scholars and judges arguing that formal equality perpetuates substantive inequality; historians and economists documenting the persistent effects of historical discrimination; representatives of historically excluded racial groups advocating for remedial state action. These voices argue the constraint entrenches rather than dismantles racial hierarchy, but they are excluded from the Supreme Court majority that sets the authoritative reading.
% DISAPPEARANCE_RATIONALE: If the colorblind reading vanished and were replaced by a remedial or antisubordination reading, educational institutions would readopt race-conscious admissions; legislatures would reinstate race-conscious voting rights protections and employment remedies; agency guidance would permit targeted diversity initiatives; the racial composition of institutions would shift substantially. The constraint's disappearance would restore remedial pathways currently foreclosed. Conversely, without the constraint, the trajectory of institutional diversification would likely accelerate and historical barriers would begin to be actively corrected rather than passive.
% FOUNDING_PROBLEM: The founding problem has multiple layers. Original Reconstruction-era intent: prevent the state from discriminating against freed slaves and ensure equal citizenship rights. Modern colorblind formulation: ensure the state does not make explicit racial distinctions, treating racial classification itself as the harm. The constraint was partly justified as preventing discrimination, but modern application primarily suppresses remedial programs.
% FOUNDING_PROBLEM_CORROBORATION: The Supreme Court majority attests the founding problem is live: preventing racial discrimination by the state remains essential. Civil rights advocates, historians, and economists outside the benefiting institutional set attest the founding problem has mutated: the constraint now primarily suppresses remedial state action while not addressing persistent hierarchical effects of historical discrimination. Historians document that the Reconstruction framers intended to protect freed slaves from discrimination, not to forbid remedial race-conscious action. The contestant voices (excluded from the Court's interpretive authority) argue the constraint's stated founding problem and its actual operation have diverged.
narrative_ontology:disappearance_verdict(equal_protection_kernel__colorblind_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_kernel__colorblind_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_kernel__colorblind_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(equal_protection_kernel__colorblind_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_kernel__colorblind_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   The colorblind reading operates as a tangled rope, not a pure snare. Coordinate function: it coordinates a shared commitment to formal legal equality and removes the state from making explicit racial distinctions, which has genuine coordination value for a multiracial polity (coordinating on 'the state shall not classify by race' is easier than debating legitimate uses of classification). Asymmetric extraction: it extracts from historically excluded groups (blocking remedial pathways, requiring them to compete without historical-discrimination correction) while benefiting doctrine carriers and (ambiguously) applicants from preferred groups. Active enforcement: the constraint persists because federal courts actively enforce it, striking down remedial programs and issuing injunctions against race-conscious policies. The measurement series show rising extractiveness over the interval (t=0 to t=30): initial colorblind doctrine was justified partly as preventing discrimination; over three decades it accumulates extraction as remedial programs fade and historical gaps persist. Theater ratio rises because enforcement activity increasingly defends the colorblind frame against empirical evidence of continued stratification (remedial advocacy is performed as 'equal protection violation' rather than as response to documented discrimination). Suppression requirement rises because the constraint must actively suppress legislative and institutional remedial initiatives—absent court enforcement, race-conscious programs would re-emerge. The shared time grid applies all three metrics at every point (0, 6, 12, 18, 24, 30) because all three track the same historical trajectory: colorblind doctrine's increasing role in suppressing visible remedial action while stratification persists.
 *
 * PERSPECTIVAL GAP:
 *   From the Supreme Court's institutional seat, the colorblind reading appears as a genuine coordination mechanism and truth-telling about the Constitution. From the seat of historically excluded groups, the same constraint appears as coercive suppression of remedial pathways. From institutional beneficiaries (universities, employers in colorblind compliance), the constraint is an external legal requirement forcing difficult choices. These divergences are not reconciled—they are the structural asymmetry that makes this a tangled rope rather than a rope. The engine computes per-seat classifications: the Court's seat will likely compute the constraint as rope or tangled rope (coordination + enforcement); the historically excluded groups' seat will compute snare (pure extraction, no participation benefit); the institutional seats will compute tangled rope (coordinated on colorblindness, extracted from by having to abandon proven remedies). Do not tune the metrics toward any predicted output—the divergence IS the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from structural position relative to the constraint. For the Supreme Court (institutional agenda-setter, powerful, remote time horizon): d ≈ 0.2 (full beneficiary in the structural derivation—it sets and enforces the constraint, derives authority from it). For historically excluded racial groups (organized, biographical horizon, trapped/identity-locked exit): d ≈ 0.95 (full targets—bear extraction, no exit, classified by the constraint as ineligible for remedial programs). For educational institutions (institutional, generational horizon, constrained exit: legal obligation to comply): d ≈ 0.6 (symmetric, conflicted—they benefit from reduced litigation exposure but lose remedial program autonomy). For majority-preferred applicants (moderate to powerful, biographical horizon, mobile exit): d ≈ 0.1-0.3 (beneficiary in the formal reading, but the actual benefit distribution is contested and tracked in omega_beneficiary_identity_ambiguity). These directionalities feed the effective extraction calculation: high extraction concentrated on trapped, identity-locked groups; damped or inverted for beneficiary seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing racial discrimination in law) is not dead, but its relationship to the constraint has degraded. The constraint was justified partly as a mechanism to prevent discrimination (coordination function: law treats all races equally). That problem persists—discrimination is still a live threat. However, the colorblind reading has accumulated a different function: blocking remedial programs aimed at correcting historical discrimination. The measurement series document this accumulation: extractiveness and theater ratio both rise while suppression requirement rises, suggesting the constraint is increasingly deployed to suppress remedial advocacy rather than to prevent discrimination. The constraint persists not because the founding problem requires it, but because institutional carriers (judges, scholars) maintain it through precedent and doctrinal authority. This is not classical mandatrophy (mandate outlived its function entirely), but a mutation: the founding problem still lives, the constraint still addresses it, but the constraint has also accumulated a secondary extraction function that was not in the founding mandate. This secondary function is not organic to the coordination problem—it is imposed by the way the doctrine is administered.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    colorblind_vs_antisubordination_premise,
    'Does the Equal Protection Clause target the USE of racial classification itself, or the ENTRENCHMENT of racial hierarchy through state action?',
    'Historical analysis of Reconstruction-era legislative intent; comparison with how the clause has been applied to race-neutral policies that entrench subordination versus race-conscious policies that dismantle it; examination of whether colorblindness doctrine forecloses anti-subordination doctrine or coexists with it across different institutional contexts.',
    'If the clause targets classification per se (colorblind reading), then all race-conscious remedies are per se unconstitutional and historically excluded groups lose the remedial pathway. If it targets entrenchment of hierarchy (antisubordination reading), then remedial and dismantling-oriented race-conscious action becomes permissible, and the constraint''s classification boundary shifts fundamentally.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(colorblind_vs_antisubordination_premise, conceptual, 'Colorblindness vs. antisubordination: does the clause prohibit classification or subordination?').

omega_variable(
    formal_versus_substantive_equality,
    'Is the colorblind reading''s prescription of formal equality (identical treatment regardless of circumstance) compatible with the clause''s stated purpose of securing equal protection of the laws?',
    'Empirical analysis: measure educational and life-outcome disparities between historically advantaged and historically excluded groups under colorblind formal equality versus under remedial race-conscious policies. Legal analysis: examine whether the clause''s text or legislative history prioritizes formal equality or substantive equality of access and opportunity.',
    'If formal equality systematically produces unequal substantive outcomes for historically subordinated groups, the colorblind reading''s claim that it secures ''equal protection'' becomes internally contradictory, and the antisubordination or remedial readings gain structural support. If formal equality proves sufficient for substantive equality, the colorblind reading is validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formal_versus_substantive_equality, empirical, 'Whether colorblind formal equality achieves equal protection in practice.').

omega_variable(
    beneficiary_identity_ambiguity,
    'Who actually benefits from the colorblind reading as a constraint: applicants from non-preferred groups (the declared beneficiary), or the institutional carriers of the color-blind doctrine itself (judges, constitutional theorists, institutional actors who derive authority from upholding formal equality)?',
    'Trace the distribution of remedial wins and losses under colorblind doctrine; examine which institutional actors mobilize colorblindness framing in litigation and policy; measure whether non-preferred applicants experience net benefit or whether the primary beneficiary is the authority structure enforcing colorblind doctrine.',
    'If the primary beneficiary is the institutional carrier of the doctrine rather than the declared beneficiary class, the constraint is better characterized as a snare (pure extraction dressed in coordination language) than as a tangled rope. If non-preferred applicants genuinely benefit (because colorblind doctrine prevents discrimination they face), the tangled-rope characterization holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_identity_ambiguity, empirical, 'Distribution of actual benefits under the colorblind reading.').

omega_variable(
    suppression_internalization,
    'Is the suppression of remedial race-conscious programs structural (external barriers: court injunctions, legislative prohibitions, resource denial) or internalized (institutions themselves adopt the colorblind frame as legitimate, abandon remedial programs voluntarily)?',
    'Post-constraint-change testing: measure whether remedial initiatives resume if external legal barriers are removed (structural suppression) versus whether the colorblind doctrine has become institutionally internalized and voluntarily perpetuated. Interview institutional actors about framing constraints.',
    'If suppression is primarily structural, alternative policies could emerge if the legal constraint changed. If internalized, the colorblind frame persists as institutional orthodoxy even after legal pressure eases, making the constraint more resilient to change and suggesting deeper extraction mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Structural vs. internalized suppression of remedial programs.').

omega_variable(
    kernel_reading_coexistence_test,
    'Can the colorblind reading and the antisubordination reading coexist within the same legal framework, or does colorblindness logically foreclose antisubordination?',
    'Examine judicial opinions where courts have attempted to apply both readings; check whether any coherent legal doctrine incorporates both the categorical ban on racial classification (colorblind) and permission for anti-subordination action. If both readings appear in the same opinion without resolution, they coexist; if one is explicitly rejected as incompatible, foreclosure is established.',
    'If readings foreclose each other, the kernel contest is zero-sum and only one can be authoritatively adopted. If they coexist, the legal system tolerates contradictory readings held by different coalitions, suggesting the kernel is genuinely contested and multiple constraint stories validly capture different seats'' experience. This affects whether the kernel is best modeled as a single binary dispute or as a genuinely polyphonic institutional structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_coexistence_test, conceptual, 'Logical relationship between colorblind and antisubordination readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_kernel__colorblind_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t0, equal_protection_kernel__colorblind_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(equa_tr_t6, equal_protection_kernel__colorblind_reading, theater_ratio, 6, 0.24).
narrative_ontology:measurement(equa_tr_t12, equal_protection_kernel__colorblind_reading, theater_ratio, 12, 0.31).
narrative_ontology:measurement(equa_tr_t18, equal_protection_kernel__colorblind_reading, theater_ratio, 18, 0.36).
narrative_ontology:measurement(equa_tr_t24, equal_protection_kernel__colorblind_reading, theater_ratio, 24, 0.39).
narrative_ontology:measurement(equa_tr_t30, equal_protection_kernel__colorblind_reading, theater_ratio, 30, 0.41).

% Extraction over time
narrative_ontology:measurement(equa_be_t0, equal_protection_kernel__colorblind_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(equa_be_t6, equal_protection_kernel__colorblind_reading, base_extractiveness, 6, 0.51).
narrative_ontology:measurement(equa_be_t12, equal_protection_kernel__colorblind_reading, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(equa_be_t18, equal_protection_kernel__colorblind_reading, base_extractiveness, 18, 0.63).
narrative_ontology:measurement(equa_be_t24, equal_protection_kernel__colorblind_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement(equa_be_t30, equal_protection_kernel__colorblind_reading, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t0, equal_protection_kernel__colorblind_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(equa_su_t6, equal_protection_kernel__colorblind_reading, suppression_requirement, 6, 0.55).
narrative_ontology:measurement(equa_su_t12, equal_protection_kernel__colorblind_reading, suppression_requirement, 12, 0.62).
narrative_ontology:measurement(equa_su_t18, equal_protection_kernel__colorblind_reading, suppression_requirement, 18, 0.67).
narrative_ontology:measurement(equa_su_t24, equal_protection_kernel__colorblind_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement(equa_su_t30, equal_protection_kernel__colorblind_reading, suppression_requirement, 30, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_kernel__colorblind_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(equal_protection_kernel__colorblind_reading, 0.12).
narrative_ontology:affects_constraint(equal_protection_kernel__colorblind_reading, equal_protection_kernel__remedial_reading).
narrative_ontology:affects_constraint(equal_protection_kernel__colorblind_reading, equal_protection_kernel__antisubordination_reading).

% DUAL FORMULATION NOTE:
% The equal_protection_kernel constraint family decomposes into three structurally distinct constraint stories, each instantiating a different reading of the Fourteenth Amendment's Equal Protection Clause. This story (colorblind_reading) asserts categorical prohibition on racial classification. The remedial_reading permits race-conscious remedies meeting strict scrutiny. The antisubordination_reading permits non-classification-based approaches to hierarchical entenchment but denies that classification per se is the clause's target. All three readings share the same constitutional text (the kernel) and compete in constitutional law, but produce different ε values, different beneficiary/victim structures, and different institutional distributions of extraction. The readings are networked: colorblind reading influences both siblings (by establishing a doctrinal baseline that remedial and antisubordination readings must work against or reframe); remedial reading coexists across institutional seats; antisubordination reading is structurally foreclosed from colorblind (cannot hold both in one framework).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(equal_protection_kernel__colorblind_reading, organized, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
