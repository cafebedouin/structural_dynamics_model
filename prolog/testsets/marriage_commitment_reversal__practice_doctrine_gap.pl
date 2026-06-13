% ============================================================================
% CONSTRAINT STORY: marriage_commitment_reversal__practice_doctrine_gap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_reversal__practice_doctrine_gap, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: marriage_commitment_reversal__practice_doctrine_gap
 *   human_readable: Marriage Commitment Doctrine-Practice Gap (1890-1904)
 *   domain: religious/institutional/political
 *
 * SUMMARY:
 *   Between 1890 and 1904, the institutional leadership of The Church of
 *   Jesus Christ of Latter-day Saints maintained the doctrinal principle of
 *   eternal plural marriage (Section 132 of the Doctrine and Covenants) as
 *   theological truth while simultaneously suspending and then prohibiting
 *   its practice in response to federal legal coercion. This constraint
 *   examines the structural ambiguity created by that gap: doctrine remained
 *   canonical and binding on belief; practice became legally and
 *   ecclesiastically forbidden. Members were expected to affirm the principle
 *   as eternally valid while conforming to the practice of monogamy. The gap
 *   between what members were taught to believe and what they were permitted
 *   to do created extractive pressure that persisted throughout the interval
 *   and beyond. This is one reading of the contested kernel
 *   marriage_commitment_reversal: the practice-doctrine_gap reading focuses
 *   on the structural consequences of maintaining both registers
 *   simultaneously rather than on either the endogenous reinterpretation
 *   (divine revelation) or the exogenous override (federal coercion)
 *   explanations for the reversal.
 *
 * KEY AGENTS:
 *   - Institutional leadership (First Presidency): sets and enforces the doctrine-practice separation; controls the narrative framing; benefits from organizational survival
 *   - General membership (powerless): expected to hold contradictory beliefs simultaneously; identity_locked by kinship and salvation narrative; victims of the gap
 *   - Fundamentalist dissenters (moderate power): refuse the gap; attempt to live the doctrine; expelled or marginalized; victims through exclusion
 *   - Plural wives (powerless, trapped): experience covenant nullification without explanation; lose status without gaining legitimacy; victims of the gap
 *   - New converts (powerless, constrained): encounter the gap on entry; no training for the contradiction; victims of incomplete information
 *   - Federal government (institutional power): applied coercion that created the structural pressure; excluded from the decision to manage it via doctrine-practice separation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_reversal__practice_doctrine_gap, 0.82).
domain_priors:suppression_score(marriage_commitment_reversal__practice_doctrine_gap, 0.71).
domain_priors:theater_ratio(marriage_commitment_reversal__practice_doctrine_gap, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, extractiveness, 0.82).
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 0.64).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_reversal__practice_doctrine_gap, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_reversal__practice_doctrine_gap, "Marriage Commitment Doctrine-Practice Gap (1890-1904)").
narrative_ontology:topic_domain(marriage_commitment_reversal__practice_doctrine_gap, "religious/institutional/political").

domain_priors:requires_active_enforcement(marriage_commitment_reversal__practice_doctrine_gap).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_reversal__practice_doctrine_gap, '3070de30-1221-4649-b677-453f1f387a2c').
narrative_ontology:cs_kernel_codification('3070de30-1221-4649-b677-453f1f387a2c', fixed_text).
narrative_ontology:cs_authority_grounding('3070de30-1221-4649-b677-453f1f387a2c', extraction).
narrative_ontology:cs_interpretation_layer_present('3070de30-1221-4649-b677-453f1f387a2c').
narrative_ontology:cs_reading_relation('3070de30-1221-4649-b677-453f1f387a2c', marriage_commitment_reversal__endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_reading_relation('3070de30-1221-4649-b677-453f1f387a2c', marriage_commitment_reversal__exogenous_override_reading, influences).
narrative_ontology:cs_axiom('3070de30-1221-4649-b677-453f1f387a2c', foundational, doctrine_practice_independence).
narrative_ontology:cs_axiom_status(doctrine_practice_independence, holdable).
narrative_ontology:cs_axiom_grounding('3070de30-1221-4649-b677-453f1f387a2c', doctrine_practice_independence, conventional).
narrative_ontology:cs_axiom('3070de30-1221-4649-b677-453f1f387a2c', secondary, institutional_ambiguity_as_strategy).
narrative_ontology:cs_axiom_status(institutional_ambiguity_as_strategy, holdable).
narrative_ontology:cs_axiom_grounding('3070de30-1221-4649-b677-453f1f387a2c', institutional_ambiguity_as_strategy, instrumental).
narrative_ontology:cs_reference_frame('3070de30-1221-4649-b677-453f1f387a2c', doctrine_practice_integration).
narrative_ontology:cs_drift_state('3070de30-1221-4649-b677-453f1f387a2c', federal_coercion_applied, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('3070de30-1221-4649-b677-453f1f387a2c', '').
narrative_ontology:cs_kernel_id(marriage_commitment_reversal__practice_doctrine_gap, marriage_commitment_reversal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__practice_doctrine_gap, institutional_leadership).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__practice_doctrine_gap, organizational_survival).
narrative_ontology:constraint_victim(marriage_commitment_reversal__practice_doctrine_gap, general_membership).
narrative_ontology:constraint_victim(marriage_commitment_reversal__practice_doctrine_gap, fundamentalist_dissenters).
narrative_ontology:constraint_victim(marriage_commitment_reversal__practice_doctrine_gap, plural_wives).
narrative_ontology:constraint_victim(marriage_commitment_reversal__practice_doctrine_gap, new_converts).
narrative_ontology:constraint_vindicates(marriage_commitment_reversal__practice_doctrine_gap, doctrinal_immutability_doctrine).
narrative_ontology:constraint_vindicates(marriage_commitment_reversal__practice_doctrine_gap, prophetic_flexibility_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The First Presidency and Apostolic Council maintain Section 132 as canonical doctrine while suspending its practice via public compliance with federal anti-polygamy law (Edmunds-Tucker Act, 1887). They manage the contradiction by framing it as temporary obedience to external authority while preserving the principle as eternally valid. They control the narrative, ritual, and membership clarity — the ambiguity is their strategy.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, institutional_leadership, agenda_setter,
    institutional, generational, arbitrage, national).

% Expected to affirm the doctrine as eternally true while conforming to the practice of monogamy. They experience confusion and betrayal when leadership teaches that plural marriage is doctrine but churches enforce monogamy as custom. Their identity as members depends on accepting both claims simultaneously, though they contradict. Exit means losing family, community, and salvation narrative.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, general_membership, payer,
    powerless, biographical, identity_locked, national).

% Attempt to live the doctrine as written (plural marriage as eternal principle). Leadership classifies them as schismatics and removes them from good standing. They bear the cost of institutional rejection while refusing to accept the doctrine-practice gap. They would object that the arrangement sacrifices membership truth for institutional flexibility, but their objections are treated as insubordination.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, fundamentalist_dissenters, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_reversal__practice_doctrine_gap, fundamentalist_dissenters, excluded).

% Women in plural marriages during the transition (1890-1904) experienced a de facto nullification of their covenant status. Previous wives were told their marriages remained eternally binding even as the institution publicly disavowed the practice and offered no legal recognition. They lost social standing without gaining any new legitimacy status.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, plural_wives, payer,
    powerless, biographical, trapped, regional).

% Converts joining after 1890 were taught Section 132 as doctrine but entered a community that practiced monogamy. They faced a gap between the doctrine they affirmed on baptism and the lived reality they encountered. This gap was never formally explained; they were expected to learn it through experience.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, new_converts, payer,
    powerless, immediate, constrained, global).

% Applied legal and economic coercion (Edmunds Act, 1882; Edmunds-Tucker Act, 1887; confiscation of church property) to suppress plural marriage. The institutional leadership's response — public abandonment of practice while preserving doctrine — satisfied federal enforcement without requiring doctrinal capitulation. Federal actors would argue they forced a choice; institutional leadership frames it as voluntary obedience.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, federal_government, excluded,
    institutional, generational, analytical, national).

% External analysts examining the constraint as a case of commitment-system drift: doctrine-practice separation, institutional survival through interpretive flexibility, and the costs borne by members who internalized the doctrine while the institution suspended its practice.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, historians_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_reversal__practice_doctrine_gap, institutional_leadership).
narrative_ontology:fixing_cost_class(marriage_commitment_reversal__practice_doctrine_gap, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Section 132 coordinates a theology of eternal family and progressive immortality: the doctrine binds spouses across death and creates a cosmology where family relationships are the fundamental structure of heaven. It integrates personal identity (as a family member in eternal progression) with institutional membership (the church mediates access to that eternal status). Without the doctrine, the entire institutional theology of family-centered salvation collapses.
% TRANSFER_FUNCTION: The constraint moves membership compliance from members to leadership: members surrender the coherence of their own belief systems (holding doctrine and denying practice simultaneously) and transfer the burden of managing that contradiction to leadership. In exchange, members retain access to the community and the theological narrative, even though the narrative is now internally contradictory.
% ABSENT_VOICES: Plural wives whose covenants were formally nullified have no seat at the table where the gap was decided. Fundamentalist dissenters who attempted to live the doctrine are excluded from institutional conversation. New converts who encountered the gap without training are structurally unable to provide feedback that might reshape how the gap is taught. Federal authorities who created the pressure are excluded from the decision to manage it via ambiguity rather than via doctrinal capitulation or institutional dissolution.
% DISAPPEARANCE_RATIONALE: If the constraint disappeared — if leadership either formally abrogated Section 132 or formally reinstated plural marriage — the institutional theology would have to be explicitly reorganized. The apparatus for managing the gap (teaching doctrine while enforcing monogamy; treating plural marriage as eternally valid but institutionally forbidden) would cease to function. Membership would face a stark choice: accept doctrinal revision (risking schism among traditionalists) or institutional dissolution (facing federal legal consequence). The gap's existence prevents either outcome from becoming mandatory.
% FOUNDING_PROBLEM: The founding problem was the collision between federal law (which prohibited plural marriage) and institutional doctrine (which prescribed it as eternally valid). Leadership faced three impossible choices: (1) abandon doctrine and suffer schism among traditionalists; (2) maintain practice and face institutional dissolution via federal enforcement; (3) find a way to preserve both doctrine and organizational survival despite the collision. The doctrine-practice gap is the solution chosen: maintain the doctrine as canonical truth (satisfying theological traditionalists), suspend the practice (satisfying federal law), and manage the gap through institutional discipline (controlling who knows what and when).
% FOUNDING_PROBLEM_CORROBORATION: Institutional leadership attests that the problem is solved — the doctrine is preserved, the organization survives, federal law is obeyed. Historians and legal scholars from outside the institution document that the problem is not solved but managed indefinitely: the doctrine-practice gap remains a permanent structural feature of the institution, suggesting the solution is maintenance of the contradiction rather than resolution of it. Descendants of plural wives and fundamentalist dissenters attest that the founding problem created cascading harms (nullified covenants, schism, family separation) that persist beyond the interval end. Contemporary legal scholars confirm that the federal coercion that created the founding problem never fully lifted — the institution remains under legal pressure that the gap allows it to navigate indefinitely.
narrative_ontology:disappearance_verdict(marriage_commitment_reversal__practice_doctrine_gap, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_reversal__practice_doctrine_gap, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_reversal__practice_doctrine_gap, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(marriage_commitment_reversal__practice_doctrine_gap, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_reversal__practice_doctrine_gap_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_reversal__practice_doctrine_gap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_reversal__practice_doctrine_gap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness score (0.82, rising from 0.64 over the interval) reflects that the constraint's primary function shifted from coordination (Section 132 as shared theology) to extraction (membership clarity sacrificed for institutional flexibility). The institution extracts member compliance with a contradiction: affirm the doctrine, practice the opposite, ask no questions. The suppression score (0.71, rising from 0.58) reflects the active machinery required to enforce this gap: doctrinal teaching continues, sermons invoke the principle, but practice is policed as forbidden. Theater ratio rises to 0.64 by interval end: a growing share of leadership activity defends the gap itself (explaining why doctrine and practice can coexist) rather than the original coordination function. Accessibility collapse at 0.68 reflects that exit is structurally unavailable to members — they cannot leave without sacrificing salvation narrative, family bonds, and community membership. Resistance at 0.73 is substantial: fundamentalist dissenters mounted organized resistance; plural wives pursued legal challenges; new members created confusion. The grid shows the coercion distribution: individual-level suppression is high (0.68→0.74) because members internalize the contradiction; organizational-level suppression is low (0.31→0.28) because leadership is not coerced from within; class-level suppression is high (0.64→0.72) because the gap affects all members as a cohort; structural-level suppression is moderate (0.52→0.58) because federal law drives the constraint but is not directly policing individual belief.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional leadership's seat, the arrangement is a rational response to federal coercion: preserve the doctrine (satisfying theology), obey the law (satisfying federal pressure), maintain organizational stability. From the powerless membership's seat, the same structure is experienced as betrayal — a doctrine they were taught to affirm eternally is publicly disavowed while they are expected to internalize the contradiction. From the fundamentalist dissenters' seat, the gap is institutional hypocrisy — the leadership chooses institutional survival over doctrinal fidelity. The engine computes these divergences from the power, exit_options, and time_horizon data: institutional leadership has arbitrage-level exit (can manage the ambiguity; can reframe it), while general membership is identity_locked (cannot exit without losing everything). This structural asymmetry drives the divergent classifications.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional leadership sits at d near 0.1–0.2 (beneficiary: controls the framing, collects organizational survival rent, faces minimal exit cost if the doctrine is revised). General membership sits at d near 0.75–0.85 (target: bears the cost of holding a contradiction, faces identity-lock exit barriers). Fundamentalist dissenters sit at d near 0.8 (full target: rejected by leadership, excluded from the institutional structure that creates the gap). Plural wives sit at d near 0.9 (full target: their covenant status is nullified; they experience the gap as personal loss). The gap itself — the structural requirement to hold contradictory commitments — is what extracts from the membership. The more thoroughly a member internalizes the doctrine, the more acutely they experience the gap as extractive.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — federal coercion against plural marriage — was genuinely live in 1890. By 1904, the problem had not disappeared; it had become structurally permanent (federal law did not reverse). The institution's solution was not to solve the founding problem but to manage it indefinitely by splitting doctrine and practice. The mandatrophy question: did the doctrine-practice gap solve the founding problem, or did it perpetuate it indefinitely? Leadership would argue the gap is temporary (until federal law changes or members develop the maturity to live it). Historians and dissenters would argue the gap became the permanent solution — the constraint evolved from a response to coercion into an institutional tool for managing membership ambiguity. The theater ratio rising to 0.64 suggests the latter: what started as a forced separation is becoming a theatrical performance of separation, with leadership investing more energy in defending the gap than in solving the original problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrine_practice_gap_permanence,
    'Was the doctrine-practice gap intended as a temporary emergency measure until federal law changed, or was it adopted as a permanent institutional strategy?',
    'Archival analysis of leadership communications (sealed and unsealed records) from 1890-1904 and beyond; comparison of rhetoric in private councils vs. public statements; analysis of whether preparation for reunion with plural practice ever resumed after federal threat subsided.',
    'If temporary, the constraint was an extractive response to coercion that persisted after the coercion lessened. If permanent, the constraint was an institutional innovation designed to manage membership theology indefinitely. The classification risk: if permanent, the constraint is closer to snare (pure extraction hiding behind doctrine) than tangled_rope (coordination + extraction). If temporary, it is tangled_rope (coordination + emergency extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_practice_gap_permanence, empirical, 'Whether the gap was emergency management or institutional strategy.').

omega_variable(
    suppression_structural_vs_internalized,
    'To what extent was the suppression of plural marriage practice structural (legal barriers, institutional prohibition) versus internalized (members came to believe they should not practice it)?',
    'Demographic analysis of conversion rates before/after 1890; study of member testimonies and private journals for internalization vs. external compliance; post-exit interviews with apostates about whether they experienced the prohibition as external or as adopted belief.',
    'If suppression is primarily structural, the constraint relies on external enforcement machinery (federal law, institutional policy). If internalized, members carry the suppression with them and the constraint is more durable. High internalization increases the effective suppression score and the identity-lock power of the constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Mechanism of suppression: structural barriers vs. internalized belief.').

omega_variable(
    membership_awareness_gap,
    'How many members during 1890-1904 were explicitly told that Section 132 remained valid doctrine while practice was forbidden, versus how many discovered the gap through experience?',
    'Analysis of leadership communications, sermons, and training materials from the period; interviews with descendants about what they were taught; comparison of understanding among converts vs. lifelong members.',
    'Explicit teaching of the gap (with explanation) is more transparent and less extractive than hidden-gap discovery. If leadership deliberately withheld the gap from new members or converts, the constraint is closer to snare (deception) than tangled_rope (coordination with extractive side effects). The gap''s visibility determines whether membership can give informed consent to the contradiction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(membership_awareness_gap, empirical, 'Was the doctrine-practice gap explicitly taught or discovered through experience?').

omega_variable(
    kernel_reading_underspecification,
    'Does the doctrine-practice-gap reading distinctively capture the constraint, or is it merely a restatement of the exogenous_override reading without the explicit federal-coercion frame?',
    'Structural analysis of what distinguishes the gap-reading from the override-reading: does the gap reading assert something about the kernel''s operation (simultaneous validity of doctrine and non-practice) that the override reading does not? Or is the gap a derived feature of any reading that preserves doctrine while suspending practice?',
    'If the gap is not distinctively constitutive of this reading, the reading collapses into the override reading and the kernel contest has fewer genuinely distinct positions. If the gap is distinctive (asserts that both registers are operationally required, not just passively coexisting), then the reading stands on its own.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_underspecification, conceptual, 'Whether the doctrine-practice-gap reading is a distinct position or derivative of the exogenous override.').

omega_variable(
    foundational_plural_marriage_validity,
    'Within this reading, is plural marriage as described in Section 132 valid in any actual institutional sense, or is its validity purely theoretical/doctrinal?',
    'Analysis of whether leadership ever performed plural marriages after 1890 with the expectation they would be eternally binding (even if not legally recognized); whether the doctrine was treated as enforceable in any institutional register, or only as a standing theological claim.',
    'If plural marriage remained institutionally valid (performed in sealed ordinances even if legally unrecognized), the gap is more narrowly defined (doctrine + secret practice). If it became purely theoretical (affirmed as true but never instantiated), the gap is more severe (doctrine with zero institutional manifestation). This affects the extractiveness score: theoretical-only doctrine is more extractive because it demands belief in something the institution will not enact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foundational_plural_marriage_validity, empirical, 'Institutional validity of plural marriage under this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_reversal__practice_doctrine_gap, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 0, 0.48).
narrative_ontology:measurement_basis(marr_tr_t0, observed).
narrative_ontology:measurement(marr_tr_t2, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 2, 0.53).
narrative_ontology:measurement_basis(marr_tr_t2, observed).
narrative_ontology:measurement(marr_tr_t4, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 4, 0.58).
narrative_ontology:measurement_basis(marr_tr_t4, observed).
narrative_ontology:measurement(marr_tr_t7, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 7, 0.63).
narrative_ontology:measurement_basis(marr_tr_t7, observed).
narrative_ontology:measurement(marr_tr_t10, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 10, 0.64).
narrative_ontology:measurement_basis(marr_tr_t10, observed).
narrative_ontology:measurement(marr_tr_t14, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 14, 0.64).
narrative_ontology:measurement_basis(marr_tr_t14, observed).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 0, 0.64).
narrative_ontology:measurement_basis(marr_be_t0, observed).
narrative_ontology:measurement(marr_be_t2, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 2, 0.7).
narrative_ontology:measurement_basis(marr_be_t2, observed).
narrative_ontology:measurement(marr_be_t4, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 4, 0.76).
narrative_ontology:measurement_basis(marr_be_t4, observed).
narrative_ontology:measurement(marr_be_t7, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 7, 0.8).
narrative_ontology:measurement_basis(marr_be_t7, observed).
narrative_ontology:measurement(marr_be_t10, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 10, 0.82).
narrative_ontology:measurement_basis(marr_be_t10, observed).
narrative_ontology:measurement(marr_be_t14, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 14, 0.82).
narrative_ontology:measurement_basis(marr_be_t14, observed).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(marr_su_t0, observed).
narrative_ontology:measurement(marr_su_t2, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 2, 0.63).
narrative_ontology:measurement_basis(marr_su_t2, observed).
narrative_ontology:measurement(marr_su_t4, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 4, 0.68).
narrative_ontology:measurement_basis(marr_su_t4, observed).
narrative_ontology:measurement(marr_su_t7, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 7, 0.71).
narrative_ontology:measurement_basis(marr_su_t7, observed).
narrative_ontology:measurement(marr_su_t10, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 10, 0.71).
narrative_ontology:measurement_basis(marr_su_t10, observed).
narrative_ontology:measurement(marr_su_t14, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 14, 0.71).
narrative_ontology:measurement_basis(marr_su_t14, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=14
narrative_ontology:measurement(marr_grid_01, marriage_commitment_reversal__practice_doctrine_gap, accessibility_collapse(class), 0, 0.65).
narrative_ontology:measurement(marr_grid_02, marriage_commitment_reversal__practice_doctrine_gap, accessibility_collapse(class), 14, 0.68).
narrative_ontology:measurement(marr_grid_03, marriage_commitment_reversal__practice_doctrine_gap, accessibility_collapse(individual), 0, 0.71).
narrative_ontology:measurement(marr_grid_04, marriage_commitment_reversal__practice_doctrine_gap, accessibility_collapse(individual), 14, 0.72).
narrative_ontology:measurement(marr_grid_05, marriage_commitment_reversal__practice_doctrine_gap, accessibility_collapse(organizational), 0, 0.42).
narrative_ontology:measurement(marr_grid_06, marriage_commitment_reversal__practice_doctrine_gap, accessibility_collapse(organizational), 14, 0.48).
narrative_ontology:measurement(marr_grid_07, marriage_commitment_reversal__practice_doctrine_gap, accessibility_collapse(structural), 0, 0.55).
narrative_ontology:measurement(marr_grid_08, marriage_commitment_reversal__practice_doctrine_gap, accessibility_collapse(structural), 14, 0.62).
narrative_ontology:measurement(marr_grid_09, marriage_commitment_reversal__practice_doctrine_gap, resistance(class), 0, 0.74).
narrative_ontology:measurement(marr_grid_10, marriage_commitment_reversal__practice_doctrine_gap, resistance(class), 14, 0.68).
narrative_ontology:measurement(marr_grid_11, marriage_commitment_reversal__practice_doctrine_gap, resistance(individual), 0, 0.61).
narrative_ontology:measurement(marr_grid_12, marriage_commitment_reversal__practice_doctrine_gap, resistance(individual), 14, 0.58).
narrative_ontology:measurement(marr_grid_13, marriage_commitment_reversal__practice_doctrine_gap, resistance(organizational), 0, 0.82).
narrative_ontology:measurement(marr_grid_14, marriage_commitment_reversal__practice_doctrine_gap, resistance(organizational), 14, 0.76).
narrative_ontology:measurement(marr_grid_15, marriage_commitment_reversal__practice_doctrine_gap, resistance(structural), 0, 0.79).
narrative_ontology:measurement(marr_grid_16, marriage_commitment_reversal__practice_doctrine_gap, resistance(structural), 14, 0.72).
narrative_ontology:measurement(marr_grid_17, marriage_commitment_reversal__practice_doctrine_gap, stakes_inflation(class), 0, 0.72).
narrative_ontology:measurement(marr_grid_18, marriage_commitment_reversal__practice_doctrine_gap, stakes_inflation(class), 14, 0.75).
narrative_ontology:measurement(marr_grid_19, marriage_commitment_reversal__practice_doctrine_gap, stakes_inflation(individual), 0, 0.78).
narrative_ontology:measurement(marr_grid_20, marriage_commitment_reversal__practice_doctrine_gap, stakes_inflation(individual), 14, 0.81).
narrative_ontology:measurement(marr_grid_21, marriage_commitment_reversal__practice_doctrine_gap, stakes_inflation(organizational), 0, 0.38).
narrative_ontology:measurement(marr_grid_22, marriage_commitment_reversal__practice_doctrine_gap, stakes_inflation(organizational), 14, 0.35).
narrative_ontology:measurement(marr_grid_23, marriage_commitment_reversal__practice_doctrine_gap, stakes_inflation(structural), 0, 0.61).
narrative_ontology:measurement(marr_grid_24, marriage_commitment_reversal__practice_doctrine_gap, stakes_inflation(structural), 14, 0.65).
narrative_ontology:measurement(marr_grid_25, marriage_commitment_reversal__practice_doctrine_gap, suppression(class), 0, 0.64).
narrative_ontology:measurement(marr_grid_26, marriage_commitment_reversal__practice_doctrine_gap, suppression(class), 14, 0.72).
narrative_ontology:measurement(marr_grid_27, marriage_commitment_reversal__practice_doctrine_gap, suppression(individual), 0, 0.68).
narrative_ontology:measurement(marr_grid_28, marriage_commitment_reversal__practice_doctrine_gap, suppression(individual), 14, 0.74).
narrative_ontology:measurement(marr_grid_29, marriage_commitment_reversal__practice_doctrine_gap, suppression(organizational), 0, 0.31).
narrative_ontology:measurement(marr_grid_30, marriage_commitment_reversal__practice_doctrine_gap, suppression(organizational), 14, 0.28).
narrative_ontology:measurement(marr_grid_31, marriage_commitment_reversal__practice_doctrine_gap, suppression(structural), 0, 0.52).
narrative_ontology:measurement(marr_grid_32, marriage_commitment_reversal__practice_doctrine_gap, suppression(structural), 14, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_reversal__practice_doctrine_gap, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_commitment_reversal__practice_doctrine_gap, 0.14).
narrative_ontology:affects_constraint(marriage_commitment_reversal__practice_doctrine_gap, marriage_commitment_reversal__endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(marriage_commitment_reversal__practice_doctrine_gap, marriage_commitment_reversal__exogenous_override_reading).

% DUAL FORMULATION NOTE:
% The marriage_commitment_reversal kernel splits into three constraint stories representing three readings of how the doctrine-practice separation operated: (1) practice_doctrine_gap (this story) — structural ambiguity maintained both registers simultaneously; (2) endogenous_reinterpretation_reading — the doctrine itself reinterpreted via Woodruff vision, making the suspension a doctrinal shift; (3) exogenous_override_reading — federal coercion forced practice abandonment without doctrinal change, presenting it as external constraint only. The readings differ in ε (the practice_doctrine_gap reading has higher extractiveness because the gap itself is the mechanism, not just the outcome of reinterpretation or override) and in their beneficiary/victim structures (who benefits from maintaining the gap varies by reading). All three are live in the historical record; different seats of the institution adopted different readings. The ε-invariance principle requires three separate stories because measuring the constraint through different interpretive frames yields different extractiveness profiles. Network edges connect all three: the practice_doctrine_gap reading influences and coexists with the other two.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_commitment_reversal__practice_doctrine_gap, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
