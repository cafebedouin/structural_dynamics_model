% ============================================================================
% CONSTRAINT STORY: us_constitution_interpretive__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_interpretive__originalist_reading, []).

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
 *   constraint_id: us_constitution_interpretive__originalist_reading
 *   human_readable: Originalist Constitutional Interpretation
 *   domain: constitutional_law
 *
 * SUMMARY:
 *   The originalist reading of the U.S. Constitution treats constitutional
 *   meaning as fixed at ratification and derives interpretive authority from
 *   fidelity to the framers' intent or original public meaning. This
 *   constraint limits judicial discretion, narrows federal power to
 *   enumerated categories, and reserves broad authority to states. It is
 *   contested by living constitutionalism and popular constitutionalism. The
 *   constraint functions as a methodological commitment system with a fixed
 *   textual kernel (the Constitution) and a lineage-based authority structure
 *   (originalist jurists, Federalist Society networks, and conservative legal
 *   institutions). Structural beneficiaries include federalism advocates,
 *   religious liberty claimants, and property rights defenders; victims
 *   include unenumerated rights claimants and administrative state advocates.
 *   This is ONE READING of the kernel us_constitution_interpretive; sibling
 *   readings instantiate different constraints with different epsilon values
 *   and beneficiary structures.
 *
 * KEY AGENTS:
 *   - originalist_jurists: Primary agenda_setter (institutional/analytical) â administer and enforce original public meaning methodology through judicial opinions, clerkship training, and professional gatekeeping
 *   - federalism_advocates: Primary beneficiary (organized/mobile) â collect constrained federal power and broad state reserved powers constitutionalized by fixed 1787 meaning
 *   - property_rights_defenders: Secondary beneficiary (powerful/mobile) â benefit from historical takings and due process doctrines that limit regulatory redistribution
 *   - unenumerated_rights_claimants: Primary payer (powerless/trapped) â bear the cost of enumerated-rights limitation through denied constitutional protection for privacy, abortion, and intimate association
 *   - administrative_state_advocates: Secondary payer (organized/constrained) â bear the cost of narrowed Commerce Clause and spending power that threatens modern regulatory architecture
 *   - popular_constitutionalism_movements: Excluded voice (organized/constrained) â democratic movement-based interpretation structurally excluded from judicial authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_interpretive__originalist_reading, 0.58).
domain_priors:suppression_score(us_constitution_interpretive__originalist_reading, 0.62).
domain_priors:theater_ratio(us_constitution_interpretive__originalist_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_interpretive__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_interpretive__originalist_reading, "Originalist Constitutional Interpretation").
narrative_ontology:topic_domain(us_constitution_interpretive__originalist_reading, "constitutional_law").

domain_priors:requires_active_enforcement(us_constitution_interpretive__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_interpretive__originalist_reading, '91f1bb54-17cb-4539-834b-ca6d5db0dfd5').
narrative_ontology:cs_kernel_codification('91f1bb54-17cb-4539-834b-ca6d5db0dfd5', fixed_text).
narrative_ontology:cs_authority_grounding('91f1bb54-17cb-4539-834b-ca6d5db0dfd5', lineage).
narrative_ontology:cs_interpretation_layer_present('91f1bb54-17cb-4539-834b-ca6d5db0dfd5').
narrative_ontology:cs_reading_relation('91f1bb54-17cb-4539-834b-ca6d5db0dfd5', us_constitution_interpretive__living_constitution_reading, forecloses).
narrative_ontology:cs_reading_relation('91f1bb54-17cb-4539-834b-ca6d5db0dfd5', us_constitution_interpretive__popular_constitutionalism_reading, coexists_with).
narrative_ontology:cs_axiom('91f1bb54-17cb-4539-834b-ca6d5db0dfd5', foundational, original_public_meaning_authority).
narrative_ontology:cs_axiom_status(original_public_meaning_authority, holdable).
narrative_ontology:cs_axiom_grounding('91f1bb54-17cb-4539-834b-ca6d5db0dfd5', original_public_meaning_authority, conventional).
narrative_ontology:cs_axiom('91f1bb54-17cb-4539-834b-ca6d5db0dfd5', foundational, enumerated_powers_federalism).
narrative_ontology:cs_axiom_status(enumerated_powers_federalism, holdable).
narrative_ontology:cs_axiom_grounding('91f1bb54-17cb-4539-834b-ca6d5db0dfd5', enumerated_powers_federalism, conventional).
narrative_ontology:cs_reference_frame('91f1bb54-17cb-4539-834b-ca6d5db0dfd5', ratification_public_meaning).
narrative_ontology:cs_drift_state('91f1bb54-17cb-4539-834b-ca6d5db0dfd5', contemporary_jurisprudential_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('91f1bb54-17cb-4539-834b-ca6d5db0dfd5', '').
narrative_ontology:cs_kernel_id(us_constitution_interpretive__originalist_reading, us_constitution_interpretive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, federalism_advocates).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, religious_liberty_claimants).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, property_rights_defenders).
narrative_ontology:constraint_victim(us_constitution_interpretive__originalist_reading, unenumerated_rights_claimants).
narrative_ontology:constraint_victim(us_constitution_interpretive__originalist_reading, administrative_state_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer and enforce the original public meaning framework through judicial opinions, clerkship training, and professional gatekeeping. Their authority and institutional role depend on maintaining the interpretive method's legitimacy and excluding rival methodologies from the bench and elite law schools.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, originalist_jurists, agenda_setter,
    institutional, generational, analytical, national).

% Benefit from a constitutional structure that constrains federal power to enumerated categories and reserves broad authority to the states. Their policy preferences are constitutionalized by the fixed 1787 understanding, giving them durable legal victories without needing to win contemporary national majorities.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, federalism_advocates, beneficiary,
    organized, generational, mobile, national).

% Invoke the original understanding of the Free Exercise Clause to claim exemptions from neutral laws. The originalist framework provides doctrinal grounding and judicial sympathy for their claims that would be unavailable under evolving-standards interpretation.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, religious_liberty_claimants, beneficiary,
    moderate, biographical, constrained, national).

% Benefit from takings and due process doctrines tied to historical 1791 or 1868 understandings, which limit regulatory redistribution and treat certain economic arrangements as constitutionally insulated from legislative change.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, property_rights_defenders, beneficiary,
    powerful, generational, mobile, national).

% Seek constitutional protection for rights not explicitly listed in the text, such as privacy, abortion, or intimate association. The originalist framework structurally denies such claims because no ratification-era public meaning supports them, leaving them without federal constitutional recourse.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, unenumerated_rights_claimants, payer,
    powerless, biographical, trapped, national).

% Advance broad Commerce Clause and spending power theories to support modern administrative state programs. The originalist framework constrains federal power to 18th-century enumerated categories, threatening the constitutional basis of contemporary regulatory and welfare institutions.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, administrative_state_advocates, payer,
    organized, generational, constrained, national).

% Argue that constitutional meaning should emerge from democratic social movements and political contestation rather than judicial historical inquiry. They are structurally excluded from the interpretive authority structure, which channels all legitimate constitutional meaning into federal courts applying originalist methods.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, popular_constitutionalism_movements, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fixes constitutional meaning to ratification-era public understanding, solving the coordination problem of judicial arbitrariness by constraining unelected judges from imposing contemporary values and providing a stable, predictable legal framework for political and economic planning.
% TRANSFER_FUNCTION: Moves interpretive authority from contemporary majorities, social movements, and evolving democratic values to the historical public meanings of 1787 and 1791, and to the jurists who claim expertise in recovering them; simultaneously transfers federal regulatory capacity to state governments and private ordering.
% ABSENT_VOICES: Popular constitutionalism movements and living constitution adherents are structurally excluded from judicial interpretive authority; they would argue for democratically responsive and evolving interpretation but are kept out by the appointment and clerkship filters that enforce originalist methodology.
% DISAPPEARANCE_RATIONALE: If the originalist constraint vanished overnight, federal courts would reinterpret the Commerce Clause and Spending Power broadly, the administrative state would expand without structural challenge, unenumerated rights would resurface under substantive due process orPrivileges or Immunities, and state power would contract relative to federal authority.
% FOUNDING_PROBLEM: The constraint was built to solve judicial tyranny and arbitrary constitutional interpretation â preventing unelected judges from imposing personal preferences under the guise of constitutional law, and preserving the fixed deal struck by the ratifying public against erosion by temporary majorities.
% FOUNDING_PROBLEM_CORROBORATION: Originalist jurists and legal historians attest the problem is still live, citing ongoing judicial activism under different methodological names. Progressive legal scholars and living constitution adherents attest the problem has inverted â originalism now functions as a mechanism for conservative judicial activism that blocks democratic majorities. Corroboration from outside the benefiting parties: critical legal historians document originalism's emergence as a political movement in the 1970s-80s, suggesting the founding problem narrative is retrospective justification.
narrative_ontology:disappearance_verdict(us_constitution_interpretive__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_interpretive__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_interpretive__originalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(us_constitution_interpretive__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_interpretive__originalist_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_interpretive__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_interpretive__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_interpretive__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) is moderate-high because the originalist framework, while providing genuine interpretive stability and rule-of-law predictability, asymmetrically blocks federal regulatory capacity and unenumerated rights claims that lack 18th-century analogues. Suppression (0.62) reflects the active enforcement of originalist methodology through judicial appointment battles, clerkship selection networks, law school prestige hierarchies, and amicus brief infrastructures that marginalize living constitutionalism. Theater ratio (0.38) captures the growing performative dimension of originalist historical analysis, where outcomes frequently precede historical reasoning and law-office history substitutes for genuine archival inquiry. Accessibility collapse (0.48) indicates that alternative interpretive methodologies are professionally costly but not impossible â a progressive judge or scholar can still articulate living constitutionalism, though at career risk. Resistance (0.55) measures sustained opposition from progressive legal scholars, critical historians, and living constitution judges. The measurement series on a shared time grid captures the institutional maturation of originalism from academic theory in the 1980s to dominant judicial methodology in the 2020s.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter seat (originalist jurists) experiences the constraint as a discipline that limits their own power and preserves democratic legitimacy through fixed meaning. The beneficiary seats experience it as a constitutional bulwark against federal overreach and judicial improvisation. The payer seats (unenumerated rights claimants, administrative state advocates) experience it as an externally imposed historical straitjacket that blocks contemporary problem-solving and democratic adaptation. The engine computes this divergence from the structural data â the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Originalist jurists have low directionality (they administer the constraint and gain institutional authority from its enforcement). Federalism advocates, religious liberty claimants, and property rights defenders have very low directionality (the constraint subsidizes their legal positions and policy preferences). Unenumerated rights claimants have high directionality (the constraint directly targets their claims for constitutional protection). Administrative state advocates have high directionality (the constraint extracts federal regulatory capacity and administrative legitimacy). Popular constitutionalism movements are excluded and bear the externalized costs of interpretive closure without being directly governed by the constraint's extraction mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   The originalist framework was built to solve judicial tyranny and protect the fixed constitutional deal against erosion. If the founding problem is dead (judges no longer impose personal preferences arbitrarily), but the constraint persists to block democratic majorities and progressive legislation, it risks mandatrophy. The measurement series shows rising extraction and theater over the interval, consistent with coordination function decay as the methodology became more instrumentally deployed. The founding problem status is contested â originalists claim judicial activism is live in different methodological guises, while critics claim the problem has inverted and originalism now enables a different form of activism. The divergence between the coordination claim and the extraction metrics is the signal the engine is designed to detect; the claim/metric independence is deliberately maintained.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Is the constitutional interpretive constraint better modeled as the originalist reading, the living constitution reading, or the popular constitutionalism reading?',
    'Corpus comparison across the constraint family (us_constitution_interpretive kernel) â each reading produces a different epsilon, beneficiary structure, and classification.',
    'Selection of reading determines whether the constraint appears as coordination, extraction, or hybrid; no single reading captures the full structural reality of constitutional interpretation in practice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Commitment system kernel reading under-determination').

omega_variable(
    original_public_meaning_recoverability,
    'Is original public meaning a recoverable historical fact, or an epistemically underdetermined construct that licenses selective historical retrieval?',
    'Interdisciplinary historical linguistics, archival completeness analysis, and consensus assessment among professional historians about the recoverability of 18th-century public meaning.',
    'If underdetermined, the constraint''s coordination function (fixed meaning) is partially theatrical and the effective extraction is higher than measured, because outcomes drive historical reasoning rather than vice versa.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_public_meaning_recoverability, empirical, 'Historical epistemic basis of originalist methodology').

omega_variable(
    originalism_political_economy,
    'Does the originalist framework function independently of the political coalition that built its institutional infrastructure, or is it structurally coupled to the conservative legal movement?',
    'Institutional genealogy tracing Federalist Society funding, judicial appointment pipelines, clerkship network composition, and empirical outcome distributions across partisan lines.',
    'If structurally coupled to a political coalition, the constraint''s extraction is higher than the methodological story suggests, and its coordination function is partially a cover story for partisan entrenchment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalism_political_economy, empirical, 'Structural independence of originalist methodology from benefiting coalition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_interpretive__originalist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_interpretive__originalist_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(us_c_tr_t8, us_constitution_interpretive__originalist_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement(us_c_tr_t16, us_constitution_interpretive__originalist_reading, theater_ratio, 16, 0.3).
narrative_ontology:measurement(us_c_tr_t24, us_constitution_interpretive__originalist_reading, theater_ratio, 24, 0.34).
narrative_ontology:measurement(us_c_tr_t32, us_constitution_interpretive__originalist_reading, theater_ratio, 32, 0.36).
narrative_ontology:measurement(us_c_tr_t40, us_constitution_interpretive__originalist_reading, theater_ratio, 40, 0.38).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_interpretive__originalist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(us_c_be_t8, us_constitution_interpretive__originalist_reading, base_extractiveness, 8, 0.4).
narrative_ontology:measurement(us_c_be_t16, us_constitution_interpretive__originalist_reading, base_extractiveness, 16, 0.45).
narrative_ontology:measurement(us_c_be_t24, us_constitution_interpretive__originalist_reading, base_extractiveness, 24, 0.5).
narrative_ontology:measurement(us_c_be_t32, us_constitution_interpretive__originalist_reading, base_extractiveness, 32, 0.54).
narrative_ontology:measurement(us_c_be_t40, us_constitution_interpretive__originalist_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_interpretive__originalist_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(us_c_su_t8, us_constitution_interpretive__originalist_reading, suppression_requirement, 8, 0.48).
narrative_ontology:measurement(us_c_su_t16, us_constitution_interpretive__originalist_reading, suppression_requirement, 16, 0.53).
narrative_ontology:measurement(us_c_su_t24, us_constitution_interpretive__originalist_reading, suppression_requirement, 24, 0.57).
narrative_ontology:measurement(us_c_su_t32, us_constitution_interpretive__originalist_reading, suppression_requirement, 32, 0.6).
narrative_ontology:measurement(us_c_su_t40, us_constitution_interpretive__originalist_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_interpretive__originalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_interpretive__originalist_reading, living_constitution_reading).
narrative_ontology:affects_constraint(us_constitution_interpretive__originalist_reading, popular_constitutionalism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel us_constitution_interpretive. The kernel decomposes into structurally distinct constraints because the label 'constitutional interpretation' conflates multiple incompatible claims about meaning, authority, and legitimacy. Each reading has a different epsilon, beneficiary structure, and classification. This story links to its sibling readings as members of the same constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
