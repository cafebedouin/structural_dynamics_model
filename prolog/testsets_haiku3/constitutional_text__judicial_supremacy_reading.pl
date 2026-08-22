% ============================================================================
% CONSTRAINT STORY: constitutional_text__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text__judicial_supremacy_reading, []).

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
 *   constraint_id: constitutional_text__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy in Constitutional Interpretation
 *   domain: constitutional_theory/political_philosophy
 *
 * SUMMARY:
 *   This constraint story instantiates one reading of the contested kernel
 *   'constitutional text': the judicial supremacy reading. It asserts that
 *   constitutional text grants courts final, conclusive authority to
 *   interpret the Constitution, and that judicial invalidation of legislation
 *   is the definitive determination of constitutional meaning. No branch can
 *   override the court's interpretation through normal legislative process;
 *   correction requires constitutional amendment. This reading benefits
 *   rights-claimants against majoritarian overreach but extracts democratic
 *   responsiveness from elected bodies. The constraint is CLAIMED as tangled
 *   rope (genuine coordination of constitutional authority + asymmetric
 *   extraction of legislative sovereignty) and the metrics describe
 *   substantially extractive, actively-enforced operation at 0.68 base
 *   extractiveness. The claim and metrics are authored independently, per the
 *   claim/metric independence rule; the divergence is the measurement.
 *
 * KEY AGENTS:
 *   - Courts and judiciary: institutional agenda-setters who determine what the Constitution means through interpretation and judicial review; their authority is the core of this constraint
 *   - Rights-claimants against majoritarian overreach: powerless beneficiaries who depend on courts to invalidate legislation that would harm them
 *   - Marginalized constituencies: powerless beneficiaries whose identity locks them into the jurisdiction; they benefit when courts read the Constitution expansively
 *   - Legislatures and elected bodies: institutional payers whose sovereignty is constrained by judicial override; they cannot enforce majority preferences when courts invalidate
 *   - Electoral majorities: moderate payers who face invalidation of policy preferences through judicial review
 *   - Executives and administrators: institutional payers/beneficiaries who must comply with court interpretation but also benefit when courts protect executive power
 *   - Rival interpretive authorities: excluded institutional actors (legislatures as interpreters, the people as interpreters) who are structurally prevented from having final say
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text__judicial_supremacy_reading, 0.68).
domain_priors:suppression_score(constitutional_text__judicial_supremacy_reading, 0.55).
domain_priors:theater_ratio(constitutional_text__judicial_supremacy_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text__judicial_supremacy_reading, "Judicial Supremacy in Constitutional Interpretation").
narrative_ontology:topic_domain(constitutional_text__judicial_supremacy_reading, "constitutional_theory/political_philosophy").

domain_priors:requires_active_enforcement(constitutional_text__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text__judicial_supremacy_reading, '2fae240d-d1d5-4226-925b-1b6558b93791').
narrative_ontology:cs_kernel_codification('2fae240d-d1d5-4226-925b-1b6558b93791', fixed_text).
narrative_ontology:cs_authority_grounding('2fae240d-d1d5-4226-925b-1b6558b93791', lineage).
narrative_ontology:cs_interpretation_layer_present('2fae240d-d1d5-4226-925b-1b6558b93791').
narrative_ontology:cs_reading_relation('2fae240d-d1d5-4226-925b-1b6558b93791', constitutional_text__legislative_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('2fae240d-d1d5-4226-925b-1b6558b93791', constitutional_text__popular_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('2fae240d-d1d5-4226-925b-1b6558b93791', foundational, courts_possess_final_interpretive_authority).
narrative_ontology:cs_axiom_status(courts_possess_final_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('2fae240d-d1d5-4226-925b-1b6558b93791', courts_possess_final_interpretive_authority, conventional).
narrative_ontology:cs_axiom('2fae240d-d1d5-4226-925b-1b6558b93791', foundational, judicial_invalidation_conclusively_determines_constitutional_meaning).
narrative_ontology:cs_axiom_status(judicial_invalidation_conclusively_determines_constitutional_meaning, holdable).
narrative_ontology:cs_axiom_grounding('2fae240d-d1d5-4226-925b-1b6558b93791', judicial_invalidation_conclusively_determines_constitutional_meaning, conventional).
narrative_ontology:cs_reference_frame('2fae240d-d1d5-4226-925b-1b6558b93791', judicial_supremacy_established_and_legitimate).
narrative_ontology:cs_drift_state('2fae240d-d1d5-4226-925b-1b6558b93791', contemporary_contestation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2fae240d-d1d5-4226-925b-1b6558b93791', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(constitutional_text__judicial_supremacy_reading, constitutional_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text__judicial_supremacy_reading, rights_claimants_against_majoritarian_overreach).
narrative_ontology:constraint_beneficiary(constitutional_text__judicial_supremacy_reading, marginalized_constituencies).
narrative_ontology:constraint_victim(constitutional_text__judicial_supremacy_reading, democratic_responsiveness).
narrative_ontology:constraint_victim(constitutional_text__judicial_supremacy_reading, legislative_will_expressed_through_elected_bodies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_text__judicial_supremacy_reading, executives_and_administrators).
narrative_ontology:constraint_victim(constitutional_text__judicial_supremacy_reading, legislatures_and_elected_bodies).
narrative_ontology:constraint_victim(constitutional_text__judicial_supremacy_reading, electoral_majorities).
narrative_ontology:constraint_victim(constitutional_text__judicial_supremacy_reading, executives_and_administrators).
narrative_ontology:constraint_vindicates(constitutional_text__judicial_supremacy_reading, separation_of_powers_doctrine).
narrative_ontology:constraint_vindicates(constitutional_text__judicial_supremacy_reading, constitutional_supremacy_over_statute).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Courts are positioned as gatekeepers of constitutional meaning through the power of judicial review. They invalidate legislation they determine conflicts with their reading of the constitutional text. They do not themselves enforce their invalidations directly; they rely on executive compliance and legislative deference to not re-enact invalidated laws. Their authority rests on the claim that courts are best positioned to read the text neutrally and to protect constitutional limits against legislative encroachment.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, courts_and_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Individuals and groups whose substantive rights courts protect against legislative majoritarian rule — religious minorities, criminal defendants, people asserting equal protection claims. They benefit when courts strike down legislation that would harm them, and they depend entirely on judicial interpretation to secure these protections. Exit is impossible: they cannot leave the jurisdiction and retain their legal standing, and they cannot override a court's interpretation through alternative legal channels.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, rights_claimants_against_majoritarian_overreach, beneficiary,
    powerless, biographical, constrained, national).

% Groups whose identity or status makes them structurally unlikely to command legislative majorities — racial minorities, religious minorities, sexual minorities, national-origin groups. They benefit when courts read the Constitution expansively to grant them protection rather than deferring to legislative choices. Their identity is locked into the jurisdiction; they cannot opt out of being regulated by the laws courts interpret.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, marginalized_constituencies, beneficiary,
    powerless, generational, identity_locked, national).

% Legislatures must operate within the boundaries courts set through interpretation. When a court invalidates legislation, the legislature cannot simply re-enact it; it must either accept the invalidation, attempt to amend the Constitution (difficult), or pass different legislation designed to pass judicial scrutiny. Legislative sovereignty — the ability to set policy through democratic choice — is constrained by judicial override.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, legislatures_and_elected_bodies, payer,
    institutional, generational, constrained, national).

% Citizens who form electoral majorities may have their policy preferences invalidated by courts reading the Constitution to override legislative choices. If a court determines that a statute violates the Constitution as the court reads it, the majority cannot enforce its preference through normal legislative channels. They face the option of amending the Constitution (extremely costly) or electing officials who will appoint judges more aligned with their reading.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, electoral_majorities, payer,
    moderate, biographical, constrained, national).

% Executives must enforce court invalidations and comply with judicial interpretation even when they disagree with the reading. Executive discretion is reduced by the constraint that courts can overturn executive action if it violates a judicially-determined constitutional meaning. However, executives also benefit when courts protect executive power against legislative encroachment (separation-of-powers doctrine) and when courts invalidate statutes that constrain executive authority.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, executives_and_administrators, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text__judicial_supremacy_reading, executives_and_administrators, beneficiary).

% Legislative bodies, executives, and the people themselves could be alternative loci of constitutional authority, but judicial supremacy excludes them from final determination. They may propose interpretations, but the court's interpretation is conclusive. Their exclusion from finality is the structural core of the constraint: without their exclusion, the constraint would not exist.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, rival_interpretive_authorities, excluded,
    institutional, generational, trapped, national).

% Scholars and analysts from other constitutional systems observe how judicial supremacy operates in this reading. They note that other democracies have distributed interpretive authority differently — some grant parliaments the power to override judicial interpretations, others embed popular amendment pathways more directly. This constraint's operation and its alternatives are analytically visible across comparative systems.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, comparative_legal_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_text__judicial_supremacy_reading, courts_and_judiciary).
narrative_ontology:fixing_cost_class(constitutional_text__judicial_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable, unified source for constitutional meaning in a political system where the text is ambiguous and multiple actors could claim authority. Prevents conflicting interpretations from paralyzing governance: courts provide a canonical reading that all branches can reference and comply with.
% TRANSFER_FUNCTION: Transfers the power to determine what the Constitution means from the electoral/legislative domain to the judicial domain. Citizens cannot overturn a court's constitutional interpretation through voting alone; they must amend the Constitution (a supermajority act) or change judicial composition (generationally slow). The constraint moves interpretive authority away from majoritarian processes and toward judges.
% ABSENT_VOICES: Legislative majorities and ordinary citizens whose policy preferences conflict with judicially-determined constitutional meaning; executives who would prefer different judicial interpretations; members of the public who believe the people themselves should retain ultimate constitutional authority. These voices are structurally excluded from the mechanism that declares what the Constitution means — they can petition, protest, or amend, but they do not participate in the judicial interpretation process itself.
% DISAPPEARANCE_RATIONALE: If judicial supremacy in constitutional interpretation vanished overnight, the political system would immediately reorganize: legislatures would face no barrier to re-enacting invalidated statutes, constitutional meaning would become contestable in real-time across branches, rights protections would depend on electoral majorities rather than judicial barriers, and the system would likely crystallize around either legislative sovereignty or pure popular sovereignty. The entire architecture of judicial review would collapse.
% FOUNDING_PROBLEM: Early constitutional systems without judicial supremacy experienced interpretive chaos: different branches claimed conflicting constitutional authority, legislatures repeatedly overrode judicial determinations, and there was no stable mechanism to protect individual rights against pure majoritarian preference. Judicial supremacy was designed to fix this by creating a final authoritative interpreter.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of judicial supremacy (the courts themselves, constitutional scholars in the supremacist tradition, rights advocates) attest the founding problem remains live: majorities would immediately erode protections for unpopular groups absent judicial barriers. Critics from outside the benefiting parties (comparative constitutional scholars, legislative scholars, popular-sovereignty advocates) attest the founding problem has been substantially solved by institutional evolution: modern legislatures rarely do what courts fear, and democracies with legislative-override systems function without chaos. The contest is documented in academic literature, legislative hearing testimony, and international comparative analysis.
narrative_ontology:disappearance_verdict(constitutional_text__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text__judicial_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text__judicial_supremacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_text__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text__judicial_supremacy_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_text__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_text__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness of 0.68 reflects the constraint's operation as asymmetric power transfer: courts move the locus of constitutional authority away from electoral majorities toward appointed judges, extracting democratic responsiveness. This is not costless coordination (0.68 is not low); it is substantial extraction. Suppression at 0.55 represents the active enforcement machinery: constitutional amendment is made deliberately difficult, legislative override is constitutionally prohibited, and compliance is expected through internalized respect for the rule of law. Theater at 0.42 reflects a significant performative component: the judicial supremacy reading frames constraint as neutral interpretation of text rather than institutional power allocation, but courts simultaneously defend their authority, resist legislative encroachment, and justify their interpretive monopoly through legitimacy narratives (the 'role of courts in protecting constitutional limits' story). The measurement trajectory shows slight extractiveness accumulation (0.52 to 0.68 across 40 years) as courts exercise review increasingly expansively and resistance to judicial authority hardens among critics, requiring more active suppression. Theater ratio rises similarly (0.25 to 0.42) as the performative justification intensifies to defend against growing contestation. Suppression requirement rises modestly (0.42 to 0.55) as the constraint's boundaries must be actively maintained against legislative and popular pressure.
 *
 * PERSPECTIVAL GAP:
 *   From the courts' and rights-claimants' seats, this is genuine coordination of constitutional authority preventing majoritarian tyranny — a rope. From the legislatures' and electoral majorities' seats, this is extraction of democratic authority — a snare. The engine computes per-seat classification: courts and rights-claimants should compute to rope or low-extractiveness mountain from their directional position (beneficiaries, low d); legislatures and majorities should compute to snare or high-extraction tangled-rope from their directional position (payers, high d). The authored claim (tangled rope at the constraint level) reflects the kernel-reading's own perspective: it genuinely coordinates constitutional authority AND asymmetrically extracts legislative sovereignty. The metrics stand independently; the seat-specific classifications will diverge.
 *
 * DIRECTIONALITY LOGIC:
 *   Courts are agenda-setters with institutional power and arbitrage-grade exit (they can change doctrine, interpret broadly, or narrow interpretation without facing override). Directionality near 0.0 (full beneficiary) — they set and benefit. Rights-claimants are powerless with identity-locked exit (their rights claims are constitutionally grounded; they cannot exit the jurisdiction and retain standing). Directionality near 1.0 (full target in the sense that courts control outcomes for them, though outcomes are favorable). Legislatures are institutional payers with constrained exit (they can re-enact or amend, but both paths are high-cost). Directionality near 0.5-0.7 (moderately targeted). Electoral majorities are moderate power payers with constrained exit. The beneficiary group (rights-claimants) is powerless but benefits; the victim group (democratic responsiveness) is abstract — the legislature and electoral process are the structural seats that bear the cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — interpretive chaos and majoritarian erosion of rights in early constitutional systems — has been substantially solved by institutional evolution: modern legislatures are not capricious rights violators, constitutional constraints are internalized through political culture, and the risk of majoritarian tyranny for marginalized groups is structurally lower than in the founding era. Yet the constraint persists with full force. Theater ratio at 0.42 and rising suggests the constraint is increasingly performing its role rather than functionally solving the founding problem. The mismatch between founding-problem status (contested, leaning dead) and constraint persistence (high suppression, active enforcement) is the mandatrophy signature: courts continue asserting final interpretive authority in service of a problem less acute than the founding premise. The constraint's persistence rides on judicial legitimacy narratives and the institutional interest courts have in maintaining their authority, not on the founding problem's ongoing urgency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Which reading of the constitutional-text kernel does this constraint instantiate: the judicial supremacy reading, or a hybrid with elements of legislative override codified into the same constitutional framework?',
    'Examine whether the constitutional text explicitly grants courts final authority (pure supremacy) or whether it permits legislatures to codify override mechanisms (hybrid). Review founding-era commentary, judicial precedent claims about textual meaning, and constitutional amendment history.',
    'Pure judicial supremacy generates the high extractiveness (0.68) and accessibility collapse (0.72) measured here; a hybrid with legislative override would show substantially lower extractiveness and higher resistance from elected bodies. The reading determines the ε values.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether the constitutional text supports pure judicial supremacy or allows for legislatively-codified override mechanisms.').

omega_variable(
    beneficiary_structure_contestation,
    'Do rights-claimants actually benefit from judicial supremacy, or is the beneficiary structure a judicial legitimation narrative for institutional power aggrandizement?',
    'Empirical comparison: measure whether rights protection for marginalized constituencies is stronger under judicial supremacy than under systems with legislative override (comparative constitutional law data). Examine whether courts actually prioritize rights-claimant briefs over institutional interests. Track whether judicial invalidations protect the vulnerable or the powerful.',
    'If evidence shows courts prioritize institutional autonomy or powerful interests over marginalized constituencies, the beneficiary structure collapses and the constraint reclassifies as snare (pure extraction with narrative justification). If evidence confirms robust rights protection, the tangled-rope classification (genuine coordination for rights, extraction of democratic responsiveness) holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_structure_contestation, empirical, 'Whether marginalized constituencies actually receive the protection the supremacy reading promises.').

omega_variable(
    alternative_reading_foreclosure,
    'Does the judicial supremacy reading foreclose the legislative sovereignty and popular sovereignty readings as live options within a single constitutional framework, or do all three remain coexistent alternatives that different parties hold simultaneously?',
    'Examine constitutional texts and case law: can a single constitutional framework accommodate both judicial final-say and legislative override, or are they logically incompatible? Review whether any real constitutional system codifies both simultaneously, or whether they are forced into pure alternatives.',
    'If readings are logically incompatible (foreclosure), the engine routes to different omega-variable handling and potentially to terminal-attractor foreclosure mechanics. If they coexist as different party commitments (coexistence), the constraint sits in a genuinely contested kernel space with no logical resolution, only political balance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_reading_foreclosure, conceptual, 'Logical structure of the readings: foreclosure, coexistence, or influence.').

omega_variable(
    suppression_mechanism_source,
    'Is the suppression required to maintain judicial supremacy structural (legal barriers, institutional design) or internalized (judges and public believe in the legitimacy of the interpretation monopoly)?',
    'Observe jurisdictions where judicial supremacy is challenged: do resistance movements attempt to change constitutional texts, elect different judges, or amend the Constitution? Do judges themselves treat challenges as illegitimate (internalized), or do they respond with tighter enforcement (structural)? Post-exit analysis: when a party stops accepting judicial authority, does the suppression persist?',
    'If suppression is primarily structural, it is reversible through constitutional change; if primarily internalized (belief in judicial legitimacy), it persists even after institutional barriers dissolve. This affects the constraint''s persistence trajectory and the cost of fixing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_source, empirical, 'Whether suppression of alternative readings is structural or internalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text__judicial_supremacy_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text__judicial_supremacy_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(cons_tr_t0, observed).
narrative_ontology:measurement(cons_tr_t5, constitutional_text__judicial_supremacy_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(cons_tr_t5, observed).
narrative_ontology:measurement(cons_tr_t10, constitutional_text__judicial_supremacy_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement_basis(cons_tr_t10, observed).
narrative_ontology:measurement(cons_tr_t15, constitutional_text__judicial_supremacy_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement_basis(cons_tr_t15, observed).
narrative_ontology:measurement(cons_tr_t20, constitutional_text__judicial_supremacy_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement_basis(cons_tr_t20, observed).
narrative_ontology:measurement(cons_tr_t25, constitutional_text__judicial_supremacy_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement_basis(cons_tr_t25, observed).
narrative_ontology:measurement(cons_tr_t30, constitutional_text__judicial_supremacy_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(cons_tr_t30, observed).
narrative_ontology:measurement(cons_tr_t40, constitutional_text__judicial_supremacy_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(cons_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text__judicial_supremacy_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(cons_be_t0, observed).
narrative_ontology:measurement(cons_be_t5, constitutional_text__judicial_supremacy_reading, base_extractiveness, 5, 0.57).
narrative_ontology:measurement_basis(cons_be_t5, observed).
narrative_ontology:measurement(cons_be_t10, constitutional_text__judicial_supremacy_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement_basis(cons_be_t10, observed).
narrative_ontology:measurement(cons_be_t15, constitutional_text__judicial_supremacy_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement_basis(cons_be_t15, observed).
narrative_ontology:measurement(cons_be_t20, constitutional_text__judicial_supremacy_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(cons_be_t20, observed).
narrative_ontology:measurement(cons_be_t25, constitutional_text__judicial_supremacy_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(cons_be_t25, observed).
narrative_ontology:measurement(cons_be_t30, constitutional_text__judicial_supremacy_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(cons_be_t30, observed).
narrative_ontology:measurement(cons_be_t40, constitutional_text__judicial_supremacy_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(cons_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text__judicial_supremacy_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(cons_su_t0, observed).
narrative_ontology:measurement(cons_su_t5, constitutional_text__judicial_supremacy_reading, suppression_requirement, 5, 0.45).
narrative_ontology:measurement_basis(cons_su_t5, observed).
narrative_ontology:measurement(cons_su_t10, constitutional_text__judicial_supremacy_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement_basis(cons_su_t10, observed).
narrative_ontology:measurement(cons_su_t15, constitutional_text__judicial_supremacy_reading, suppression_requirement, 15, 0.5).
narrative_ontology:measurement_basis(cons_su_t15, observed).
narrative_ontology:measurement(cons_su_t20, constitutional_text__judicial_supremacy_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement_basis(cons_su_t20, observed).
narrative_ontology:measurement(cons_su_t25, constitutional_text__judicial_supremacy_reading, suppression_requirement, 25, 0.54).
narrative_ontology:measurement_basis(cons_su_t25, observed).
narrative_ontology:measurement(cons_su_t30, constitutional_text__judicial_supremacy_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement_basis(cons_su_t30, observed).
narrative_ontology:measurement(cons_su_t40, constitutional_text__judicial_supremacy_reading, suppression_requirement, 40, 0.55).
narrative_ontology:measurement_basis(cons_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_text__judicial_supremacy_reading, 0.12).
narrative_ontology:affects_constraint(constitutional_text__judicial_supremacy_reading, constitutional_text__legislative_sovereignty_reading).
narrative_ontology:affects_constraint(constitutional_text__judicial_supremacy_reading, constitutional_text__popular_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested kernel 'constitutional_text'. The kernel is the constitutional text's ambiguous grant of interpretive authority. Three constraints decompose the three live readings: judicial_supremacy_reading (courts final), legislative_sovereignty_reading (legislature final), popular_sovereignty_reading (people final). Each reading produces different ε values, beneficiary/victim structures, and suppression profiles. They are not three views of one constraint; they are three structurally distinct constraints linked through a common kernel. The ε-invariance principle (DP-001) requires decomposition: the referent (the standing constitutional arrangement) is fixed, but the three readings assess it differently, generating different ε values. All three stories form a family; all members reference each other via network.affects_constraints. Sibling readings are modeled as cs_structure.reading_relations (coexists_with or forecloses), not as alternative measurements of a single constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
