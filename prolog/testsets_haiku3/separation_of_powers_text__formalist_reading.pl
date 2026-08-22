% ============================================================================
% CONSTRAINT STORY: separation_of_powers_text__formalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_separation_of_powers_text__formalist_reading, []).

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
 *   constraint_id: separation_of_powers_text__formalist_reading
 *   human_readable: Formalist Separation of Powers Doctrine
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   The formalist reading of the separation of powers doctrine holds that the
 *   Constitution establishes strict, impermeable boundaries between
 *   legislative, executive, and judicial authority. Congress cannot delegate
 *   its legislative power to executive agencies; any rule issued by an agency
 *   in pursuance of a delegated authority claim is constitutionally suspect.
 *   This reading creates a tangled coordination/extraction structure: it
 *   coordinates by preventing executive expansion and maintaining democratic
 *   control through the legislative channel; it extracts by preventing expert
 *   agencies from adapting rules to complex conditions and forcing regulatory
 *   capacity into legislative gridlock. The reading is one of three competing
 *   interpretations of the same constitutional text. The formalist reading is
 *   claimed to protect the founding problem (preventing executive
 *   aggrandizement); functionalist and unitary-executive readings claim the
 *   same text yields different conclusions about what coordination is
 *   necessary and which branch should hold power.
 *
 * KEY AGENTS:
 *   - Congress legislative majority — benefits from exclusive legislative authority and constitutional rhetorical cover for regulatory rollback
 *   - Federalist legal establishment — benefits from intellectual authority of the formalist tradition
 *   - Administrative agencies — bear constitutional vulnerability and operate defensively under delegated-authority threat
 *   - Regulated industries — dual position: benefit from regulatory de-fanging, bear uncertainty from delegation challenges
 *   - Agency-dependent constituencies (environmental, labor, consumer advocates) — bear extraction as regulatory gap; excluded from legislative process
 *   - Judiciary formalist wing — agenda setter, enforcer of the doctrine through constitutional invalidation
 *   - Functionalist judiciary wing — excluded from current adjudication; represents alternative reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(separation_of_powers_text__formalist_reading, 0.68).
domain_priors:suppression_score(separation_of_powers_text__formalist_reading, 0.72).
domain_priors:theater_ratio(separation_of_powers_text__formalist_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(separation_of_powers_text__formalist_reading, tangled_rope).
narrative_ontology:human_readable(separation_of_powers_text__formalist_reading, "Formalist Separation of Powers Doctrine").
narrative_ontology:topic_domain(separation_of_powers_text__formalist_reading, "constitutional_law/political_theory").

domain_priors:requires_active_enforcement(separation_of_powers_text__formalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(separation_of_powers_text__formalist_reading, '0a39a360-cf6c-4746-aa54-9c44a11d0489').
narrative_ontology:cs_kernel_codification('0a39a360-cf6c-4746-aa54-9c44a11d0489', fixed_text).
narrative_ontology:cs_authority_grounding('0a39a360-cf6c-4746-aa54-9c44a11d0489', extraction).
narrative_ontology:cs_interpretation_layer_present('0a39a360-cf6c-4746-aa54-9c44a11d0489').
narrative_ontology:cs_reading_relation('0a39a360-cf6c-4746-aa54-9c44a11d0489', separation_of_powers_text__functionalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('0a39a360-cf6c-4746-aa54-9c44a11d0489', separation_of_powers_text__unitary_executive_reading, influences).
narrative_ontology:cs_axiom('0a39a360-cf6c-4746-aa54-9c44a11d0489', foundational, delegation_legislative_power_prohibited).
narrative_ontology:cs_axiom_status(delegation_legislative_power_prohibited, holdable).
narrative_ontology:cs_axiom_grounding('0a39a360-cf6c-4746-aa54-9c44a11d0489', delegation_legislative_power_prohibited, deontological).
narrative_ontology:cs_axiom('0a39a360-cf6c-4746-aa54-9c44a11d0489', foundational, separation_boundaries_impermeable).
narrative_ontology:cs_axiom_status(separation_boundaries_impermeable, holdable).
narrative_ontology:cs_axiom_grounding('0a39a360-cf6c-4746-aa54-9c44a11d0489', separation_boundaries_impermeable, conventional).
narrative_ontology:cs_reference_frame('0a39a360-cf6c-4746-aa54-9c44a11d0489', structural_separation_three_branches).
narrative_ontology:cs_drift_state('0a39a360-cf6c-4746-aa54-9c44a11d0489', administrative_state_maturity, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0a39a360-cf6c-4746-aa54-9c44a11d0489', '').
narrative_ontology:cs_kernel_id(separation_of_powers_text__formalist_reading, separation_of_powers_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(separation_of_powers_text__formalist_reading, congress_legislative_majority).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__formalist_reading, federalist_legal_establishment).
narrative_ontology:constraint_victim(separation_of_powers_text__formalist_reading, administrative_agencies).
narrative_ontology:constraint_victim(separation_of_powers_text__formalist_reading, regulated_industries_seeking_predictable_rules).
narrative_ontology:constraint_victim(separation_of_powers_text__formalist_reading, agency_dependent_constituencies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__formalist_reading, regulated_industries_seeking_predictable_rules).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retains exclusive legislative authority and the power to structure agency mandates narrowly. Can claim constitutional supremacy over executive implementation without bearing the friction costs of detailed statutory drafting. Collects political rent from reversing or constraining unpopular regulations through legislative veto mechanisms and appropriations riders. Benefits from the doctrine's claim that delegation is constitutionally prohibited — uses the prohibition as rhetorical cover when dismantling regulatory apparatus.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, congress_legislative_majority, beneficiary,
    institutional, generational, arbitrage, national).

% Academic and judicial advocates of the formalist reading whose professional standing and scholarly output depend on the doctrine's legal vitality. Includes judicial activists committed to the doctrine as constitutional restraint on executive expansion. Collects intellectual authority and influence over judicial doctrine from advocacy for formalism; the constraint's persistence vindicates their interpretive tradition.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, federalist_legal_establishment, beneficiary,
    institutional, generational, analytical, national).

% Bear direct constitutional challenge to their existence and authority. Under formalist reading, their delegated rulemaking authority is constitutionally suspect; every regulation is vulnerability. They must operate defensively, narrowly construing their mandates and deferring to Congress even where statutory ambiguity would normally permit expert judgment. Cannot build long-term policy coherence when each rule faces renewal under constitutional doctrine that denies their authority exists.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, administrative_agencies, payer,
    institutional, generational, trapped, national).

% Face regulatory uncertainty because agency rules are constitutionally contestable. Investment planning is disrupted when environmental, financial, or safety regulations can be invalidated on formalist delegation grounds. They simultaneously benefit from de-fanging the regulatory state — prefer legislative gridlock and narrow agency authority to aggressive regulation. The constraint both constrains capacity they fear and creates chaos they can exploit.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, regulated_industries_seeking_predictable_rules, payer,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(separation_of_powers_text__formalist_reading, regulated_industries_seeking_predictable_rules, beneficiary).

% Environmental advocates, workers, consumers, public health advocates who rely on agency expertise and authority to regulate pollutants, workplace safety, drug efficacy, etc. Under formalist doctrine, agencies move tentatively and defer to Congress when threatened with constitutional challenge. These constituencies have no seat in Congress and cannot access the legislative veto; they bear the extraction as regulatory gap.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, agency_dependent_constituencies, payer,
    powerless, immediate, trapped, national).

% Enforces the formalist reading through constitutional invalidation of agency rules deemed excessive delegation. Sets the outer boundary of what agencies may do; controls the threat level that agencies operate under. Judicial ideology determines whether the doctrine is actively enforced or dormant. Acts as administrator of the constraint and chief enforcer.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, judiciary_formalist_wing, agenda_setter,
    institutional, generational, analytical, national).

% Interprets the same constitutional text differently: separation of powers as a flexible arrangement permitting delegations under intelligible principle standard. Would oppose formalist enforcement but is currently in minority position on the bench. Their exclusion from agenda-setting is the constraint's enforcement mechanism.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, functionalist_judiciary_wing, excluded,
    institutional, generational, analytical, national).

% Analyze and debate the formalist reading's coherence and historical grounding. Provide ammunition for both sides of the dispute but do not control implementation. Witness the constraint's operation without adjudicating it.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, constitutional_scholars, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(separation_of_powers_text__formalist_reading, congress_legislative_majority).
narrative_ontology:fixing_cost_class(separation_of_powers_text__formalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for governance in which legislative authority remains with the people's elected representatives and cannot be permanently transferred to unelected executive officers. Solves the political theory problem: how to prevent executive aggrandizement while permitting government to function.
% TRANSFER_FUNCTION: Transfers regulatory capacity and expert discretion from executive agencies (who would act through flexible rules adapted to complex conditions) to Congress (who acts through rigid statutes requiring supermajority consensus). Moves from adaptive expertise to legislative gridlock as the operative constraint on regulatory scope.
% ABSENT_VOICES: Regulated industries and regulatory-dependent constituencies cannot vote in Congress and are excluded from the legislative process that would need to act to overturn the doctrine. Agency professionals whose expertise would otherwise govern are excluded by constitutional doctrine from the seats where policy is made. Their objections are structurally blocked.
% DISAPPEARANCE_RATIONALE: If the formalist doctrine were abandoned or reversed, agencies would regain authority to issue rules without constitutional vulnerability, environmental and financial regulation would expand capacity, legislative gridlock over regulatory details would ease, and the balance between expert judgment and democratic accountability would shift toward the expert seat. Congress would retain the power to override but not through constitutional prohibition.
% FOUNDING_PROBLEM: How to prevent executive officers from accumulating power beyond what the Constitution grants? The Framers established three branches with separate powers; delegation of legislative authority to the executive was read as constitutional violation of that separation.
% FOUNDING_PROBLEM_CORROBORATION: Formalist scholars (Epstein, McGinnis, Lawson) and conservative jurists cite the Framers' structural commitment and early Supreme Court dicta (e.g., Schechter Poultry v. United States). Functionalist scholars and agency practitioners cite 150+ years of operative delegation without constitutional objection and argue the founding problem was narrower than formalists claim (fear of specific abuses, not of delegation per se). Legislative history and practice evidence from the New Deal era and after is contested between the readings; no single corroborating source outside the formalist community attests the founding problem in its strong form.
narrative_ontology:disappearance_verdict(separation_of_powers_text__formalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(separation_of_powers_text__formalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(separation_of_powers_text__formalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(separation_of_powers_text__formalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(separation_of_powers_text__formalist_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(separation_of_powers_text__formalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(separation_of_powers_text__formalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(separation_of_powers_text__formalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderately high (0.68 at endpoint) because the doctrine suppresses alternative governance arrangements (functionalist interpretation, expert agency discretion, regulatory adaptation) and forces policy into a legislative-only channel where powerless constituencies cannot participate. Suppression is high (0.72) because the constraint is actively enforced through judicial invalidation and requires ongoing defense against functionalist alternatives. Theater ratio is moderate (0.41): the doctrine performs genuine constitutional principle (separation of powers), but performance activity is directed at defending the doctrine itself against functional pressure (agency adaptation that works well, regulatory gaps that functionalism would fill). The measurement series show extraction and suppression rising modestly through the interval as formalist judicial appointments reinforce enforcement; at plateau the constraint stabilizes when a sufficient judicial majority locks in the reading. Theater rises as functionalist pressure mounts and more enforcement effort goes to defending the boundary.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary formalist wing perspective, the doctrine is constitutional fidelity restraining executive power. From the agency perspective, it is constitutional vulnerability blocking necessary adaptation. From the functionalist judiciary perspective (excluded), the same text permits a more flexible arrangement. From agency-dependent constituencies, it is arbitrary suppression of expertise. The engine should compute seat-specific types: formalist beneficiaries should compute as rope-to-mountain (they coordinate against executive expansion); agencies and constituencies should compute as snare (pure extraction with suppression). The perspectival gap is the core measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Congress and federalist scholars are beneficiaries (d near 0.0): they collect authority and political rent from the doctrine's persistence. Agencies are targets (d near 1.0): they are constitutionally vulnerable and trapped under delegation doctrine. Regulated industries show symmetric extraction and benefit — they pay uncertainty costs (d~0.5) but benefit from regulatory de-fanging. Agency-dependent constituencies are targets (d near 1.0) because they bear the gap between what agencies could do and what formalism permits, with no exit. The functionalist judiciary wing, though excluded, would have low d if they gained power (they would dissolve the constraint). Directionality is heterogeneous by seat because the constraint operates differently from each position.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is contested and the founding_problem_status is authored as 'contested' by design. Formalists claim the problem (executive aggrandizement through delegation) remains live; functionalists and practitioners claim it is substantially solved by statutory safeguards and presidential accountability. The mandatrophy question: is formalism defending against a real threat or maintaining a doctrine whose original justification has atrophied? The measurement series shows moderate rise in theater ratio over time, suggesting enforcement effort is increasingly directed at defending the doctrine itself rather than at the putative founding problem. This is not yet mandatrophy (the doctrine is still enforced and has political beneficiaries), but the pattern is consistent with early stages of mandatrophy accumulation — the real functional problem has been solved by statutory framework and administrative law, but the constitutional doctrine persists and requires active judicial and scholarly defense. An omega captures the uncertainty.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_atrophy,
    'Is the founding problem (executive aggrandizement through unrestricted delegation) still a live constitutional threat, or has it been substantially solved by statutory safeguards, presidential accountability mechanisms, and administrative law doctrine?',
    'Historical analysis of agency overreach incidents and their remedy (statutory override, appropriations control, presidential removal, judicial review); comparative institutional analysis of actual executive power expansion vs. what formalism claims delegation permits.',
    'If the founding problem is substantially solved, the doctrine is maintaining a boundary that no longer defends against a live threat — reclassifying as piton (theatrical maintenance of doctrine itself, not defense of a functional problem). If the problem is live, the formalist extraction of regulatory capacity serves a real protective function and remains tangled rope or snare depending on whether the coordination benefit (preventing bad outcomes) exceeds the extraction cost (regulatory gap).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_atrophy, empirical, 'Whether the original justification for the formalist doctrine''s boundary still holds.').

omega_variable(
    reading_logical_compatibility,
    'Do the formalist and functionalist readings of ''separation of powers'' represent genuinely incompatible interpretations of a single constraint, or are they describing different aspects of a multifaceted constraint that could be satisfied under either reading?',
    'Constitutional jurisprudence examining whether a statute could be valid under BOTH readings'' criteria, or whether formalist validity and functionalist validity are mutually exclusive structural outcomes.',
    'If mutually exclusive (formalist validity entails functionalist invalidity and vice versa), the readings FORECLOSE each other within any framework. If compatible (a well-crafted statute could satisfy both), they COEXIST and the disagreement is about degree/calibration rather than binary contradiction. This determines reading_relations in cs_structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_logical_compatibility, conceptual, 'Whether formalist and functionalist readings are logically incompatible or can coexist within one framework.').

omega_variable(
    suppression_mechanism_internalized,
    'Is the measured suppression (0.72) structural (judicial threats, constitutional doctrine actively enforced by the judiciary formalist wing) or internalized (agencies and Congress have absorbed the doctrine''s claims and constrain themselves even without active judicial policing)?',
    'Counterfactual: if the formalist judiciary were replaced by functionalists, would agencies and Congress immediately expand their scope and delegation practices, or would the internalized doctrine persist even without external enforcement threat?',
    'If structural, the constraint''s suppression depends on the judiciary staying formalist; a shift to functionalist majority would destabilize it immediately. If internalized, the constraint persists even if judicial enforcement relaxes because institutions have incorporated the doctrine''s logic. If both, characterize the proportion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized, empirical, 'Whether suppression is maintained by active enforcement or internalized institutional constraint.').

omega_variable(
    kernel_reading_distinction,
    'Is this constraint truly ONE reading of a kernel (separation_of_powers_text), or is it a constraint on agency power that could be instantiated by multiple independent constitutional theories (formalism, originalism, structural constitutionalism)?',
    'Trace the intellectual genealogy: does the formalist reading claim to derive directly from the constitutional text''s meaning, or does it invoke a separate foundational theory that could generate other constraints?',
    'If one reading of a kernel, the sibling readings (functionalist, unitary executive) are competing interpretations of the same text and should be modeled as constraint family. If independent constraint instantiated by multiple theories, the ε-invariance principle might decompose differently — formalist enforcement and originalist enforcement might be separate constraints using the same doctrinal label.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_distinction, conceptual, 'Whether this constraint is a kernel reading or a theory-independent constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(separation_of_powers_text__formalist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sepa_tr_t0, separation_of_powers_text__formalist_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(sepa_tr_t8, separation_of_powers_text__formalist_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement(sepa_tr_t16, separation_of_powers_text__formalist_reading, theater_ratio, 16, 0.36).
narrative_ontology:measurement(sepa_tr_t24, separation_of_powers_text__formalist_reading, theater_ratio, 24, 0.39).
narrative_ontology:measurement(sepa_tr_t32, separation_of_powers_text__formalist_reading, theater_ratio, 32, 0.4).
narrative_ontology:measurement(sepa_tr_t40, separation_of_powers_text__formalist_reading, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(sepa_be_t0, separation_of_powers_text__formalist_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(sepa_be_t8, separation_of_powers_text__formalist_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(sepa_be_t16, separation_of_powers_text__formalist_reading, base_extractiveness, 16, 0.63).
narrative_ontology:measurement(sepa_be_t24, separation_of_powers_text__formalist_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement(sepa_be_t32, separation_of_powers_text__formalist_reading, base_extractiveness, 32, 0.67).
narrative_ontology:measurement(sepa_be_t40, separation_of_powers_text__formalist_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(sepa_su_t0, separation_of_powers_text__formalist_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(sepa_su_t8, separation_of_powers_text__formalist_reading, suppression_requirement, 8, 0.63).
narrative_ontology:measurement(sepa_su_t16, separation_of_powers_text__formalist_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(sepa_su_t24, separation_of_powers_text__formalist_reading, suppression_requirement, 24, 0.71).
narrative_ontology:measurement(sepa_su_t32, separation_of_powers_text__formalist_reading, suppression_requirement, 32, 0.72).
narrative_ontology:measurement(sepa_su_t40, separation_of_powers_text__formalist_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(separation_of_powers_text__formalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(separation_of_powers_text__formalist_reading, 0.14).
narrative_ontology:affects_constraint(separation_of_powers_text__formalist_reading, separation_of_powers_text__functionalist_reading).
narrative_ontology:affects_constraint(separation_of_powers_text__formalist_reading, separation_of_powers_text__unitary_executive_reading).

% DUAL FORMULATION NOTE:
% The constraint 'separation of powers text' decomposes into three structurally distinct constraint stories per the ε-invariance principle. Each reading of the contested constitutional text instantiates different ε (extent of extraction), different beneficiary/victim structures, and different types. The formalist reading extracts capacity from agencies (ε=0.68); the functionalist reading would permit more agency discretion (lower ε); the unitary-executive reading would concentrate power in the President (different victim set, different ε). All three share the referent (the constitutional text and its interpretation) but apply it differently. Linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(separation_of_powers_text__formalist_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
