% ============================================================================
% CONSTRAINT STORY: congressional_classified_oversight
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_congressional_classified_oversight, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: congressional_classified_oversight
 *   human_readable: Congressional Classified Oversight Constraint
 *   domain: political/institutional_governance
 *
 * SUMMARY:
 *   Congressional classified oversight represents a structural constraint
 *   where the constitutional authority of Congress to oversee executive
 *   activity conflicts with the operational necessity of protecting
 *   classified information from disclosure. The constraint exhibits
 *   properties of a tangled rope: it contains a genuine coordination function
 *   (legitimate need to protect sources, methods, and ongoing operations from
 *   foreign adversaries) alongside asymmetric extraction (the executive
 *   branch's control over what Congress can see enables the executive to
 *   limit effective oversight). The constraint has intensified over recent
 *   decades as the volume of classified information has expanded,
 *   compartmentation practices have become more restrictive, and
 *   congressional staffing for technical oversight has declined. The theater
 *   ratio has increased over the interval, indicating that formal oversight
 *   mechanisms (committee hearings, briefing protocols, inspector general
 *   reviews) maintain the appearance of congressional power while substantive
 *   control has migrated toward the executive.
 *
 * KEY AGENTS:
 *   - Non-Cleared Congress Members: Primary victims (powerless/trapped) — excluded from classified information necessary to exercise constitutional oversight authority
 *   - Cleared Committee Members: Secondary victims (organized/constrained) — possess access but face compartmentation, NDA penalties, career risk, and institutional pressure; also benefit from access privilege
 *   - Intelligence Agencies: Primary beneficiaries (institutional/arbitrage) — control information flow, set briefing agendas, determine what Congress can discuss; can shift information provision to alternative executive channels
 *   - Executive Branch: Co-beneficiary (institutional/arbitrage) — maintains sole discretion over classification decisions and can unilaterally restrict congressional access
 *   - Reform Movement: Organized challengers (powerful/mobile) — advocates for expanded clearance programs, improved briefing materials, declassification reviews
 *   - Classification System Institution: Maintains performative oversight rituals (piton perspective)
 *   - Democratic Accountability: Abstract victim (powerless/trapped) — the public interest in knowing whether executive actions comply with law and constitutional values
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(congressional_classified_oversight, 0.58).
domain_priors:suppression_score(congressional_classified_oversight, 0.68).
domain_priors:theater_ratio(congressional_classified_oversight, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(congressional_classified_oversight, extractiveness, 0.58).
narrative_ontology:constraint_metric(congressional_classified_oversight, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(congressional_classified_oversight, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(congressional_classified_oversight, tangled_rope).
narrative_ontology:human_readable(congressional_classified_oversight, "Congressional Classified Oversight Constraint").
narrative_ontology:topic_domain(congressional_classified_oversight, "political/institutional_governance").

domain_priors:requires_active_enforcement(congressional_classified_oversight).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(congressional_classified_oversight, executive_branch).
narrative_ontology:constraint_beneficiary(congressional_classified_oversight, intelligence_agencies).
narrative_ontology:constraint_victim(congressional_classified_oversight, congressional_representatives).
narrative_ontology:constraint_victim(congressional_classified_oversight, democratic_accountability).
narrative_ontology:constraint_victim(congressional_classified_oversight, public_oversight).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-CLEARED CONGRESSIONAL REPRESENTATIVE (SNARE) — Congress member without security clearance cannot access classified information needed for effective oversight. Trapped within the institution but structurally excluded from the information required to exercise constitutional duties. Bears full extraction cost: responsibility without power.
constraint_indexing:constraint_classification(congressional_classified_oversight, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CLEARED COMMITTEE MEMBER (TANGLED ROPE) — Possesses access but faces severe constraints: compartmentation restricts discussion, NDA penalties for disclosure, career risk for raising concerns, institutional pressure to defer to executive interpretation. Genuine coordination function exists (legitimate information control for national security) alongside asymmetric extraction (executive controls what is shown, when, and in what framing).
constraint_indexing:constraint_classification(congressional_classified_oversight, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INTELLIGENCE AGENCY (ROPE) — Experiences constraint as functional coordination mechanism: classification enables compartmentation necessary for operational security while allowing selective briefings to maintain congressional relationships. Beneficiary position — controls information flow, sets agenda for what Congress discusses. Exit option is arbitrage: can shift information provision to executive branch alternative channels if congressional resistance emerges.
constraint_indexing:constraint_classification(congressional_classified_oversight, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONGRESSIONAL REFORM MOVEMENT (SCAFFOLD) — Organized reform advocates (select committee leadership, transparency advocates, civil liberties groups) see the bottleneck as solvable through expanded clearance programs, improved briefing materials, and sunset review mechanisms. Some agency and some success in creating alternative pathways (inspector general reports, declassification reviews). Low effective extraction because the movement has resources and perceives an exit path through institutional change.
constraint_indexing:constraint_classification(congressional_classified_oversight, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: CLASSIFICATION SYSTEM AS INSTITUTION (PITON) — The formal oversight apparatus (committee hearings, classified briefings, inspector general mechanisms) has become largely performative. The rituals of oversight persist (briefings occur, questions are asked in secure settings, reports are filed) but the actual power to condition executive action has atrophied. Theater ratio high: the system maintains the appearance of congressional control while effective control resides in executive discretion about what to disclose and how.
constraint_indexing:constraint_classification(congressional_classified_oversight, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / CONSTITUTIONAL TENSION VIEW (MOUNTAIN) — From a civilizational view, the conflict between operational security and democratic accountability is constitutive of the intelligence state itself — no resolution is possible without eliminating either classification or congressional oversight. Some aspects of this tension are structural (information monopoly is necessary for covert action; transparency is necessary for democracy). However, the mountain classification risks naturalizing what is historically contingent — the specific form of the intelligence state emerged post-WWII and has expanded through choices about classification scope, compartmentation, and briefing protocols that are not immutable.
constraint_indexing:constraint_classification(congressional_classified_oversight, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(congressional_classified_oversight_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(congressional_classified_oversight, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(congressional_classified_oversight, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(congressional_classified_oversight, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(congressional_classified_oversight, TR),
    TR >= 0.70.

:- end_tests(congressional_classified_oversight_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The executive branch captures significant benefit from information asymmetry: it can represent its own actions to Congress in a curated way, withhold embarrassing information without congressional remedy, and maintain operational freedom from legislative constraints. The extraction is not at maximum (0.72+) because Congress retains some power — appropriations authority, confirmation authority, subpoena power — and has occasionally forced disclosure. The value reflects the average extraction rate accounting for cases where Congress successfully extracts concessions and cases where it fails. Suppression (0.68): High. Multiple barriers prevent Congress from exercising meaningful oversight: security clearance requirements exclude many representatives, compartmentation prevents full information assembly, classification itself conceals information, NDA penalties deter disclosure to colleagues or constituents, and career/reputational risk (committee assignment, future appointments) deters raising concerns. However, suppression is not absolute (0.85+) because some classified information does reach Congress through briefing protocols, and some Congress members with security clearances have successfully influenced executive decisions. Theater ratio (0.64): Moderately high. Formal oversight mechanisms (Intelligence Committee hearings, classified briefings, inspector general reports, Gang of Eight notifications) create the appearance of congressional power to condition executive action. However, the actual power of these mechanisms has declined as the executive has increasingly shielded decisions behind classification and compartmentation. The theater has increased because the formal apparatus persists but its functional capacity has atrophied.
 *
 * PERSPECTIVAL GAP:
 *   Why does the intelligence agency see Rope while Congress sees Snare or Tangled Rope? The agency experiences the classification system as enabling their core function (operational security) while maintaining the legitimacy of having Congress participate in oversight decisions. Congress experiences the same system as constraining their core function (effective oversight) while maintaining the fiction that they have power. This gap is not perceptual — both parties are correct about their own structural experience. The constraint genuinely coordinates (prevents information leaks that would harm operations) AND genuinely extracts (enables executive discretion beyond congressional check). The perspective gap is the signature of tangled rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation for this constraint maps structural positions to d values. Non-cleared Congress members are pure victims with no exit: d ≈ 0.95, producing high f(d). Cleared committee members are victims with constrained exit (can resign from committee but remain in Congress; can speak publicly but face career cost): d ≈ 0.65, producing moderate f(d). Intelligence agencies are beneficiaries with arbitrage exit (can threaten to withhold briefings or shift to executive alternatives): d ≈ 0.10, producing low/negative f(d). The executive branch benefits from the same arbitrage position. Reform movements have some power and mobility, so d ≈ 0.45, producing moderate f(d). The disparity in directionality values between beneficiaries and victims is the core mechanism of the tangled rope classification — coordination function exists (Congress and executive do coordinate on classified matters) but extraction is asymmetric (Congress cannot threaten exit effectively because Congress members cannot simply abandon constitutional oversight authority).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint serves a genuine coordination function (preventing information leaks that would compromise operations) alongside asymmetric extraction (enabling executive discretion). The classification system is not pure coordination (which would appear as Rope from all perspectives) because Congress cannot exit without abandoning constitutional duty — Congress is locked into participation on terms set by the executive. Nor is it pure extraction (Snare) because legitimate security concerns do require some information control and Congress does retain some power through appropriations and confirmation. The tangled rope classification captures the hybrid: coordination goal + asymmetric extraction mechanism. The false natural law risk is the mountain perspective, which naturalizes this as inherent constitutional tension. But the constraint's form (what gets classified, compartmentation depth, briefing protocols) reflects policy choices, not constitutional necessity. Intelligence oversight could function with narrower classification, less compartmentation, and clearer congressional right-to-know thresholds — not by eliminating security classification entirely but by narrowing its scope and limiting its use as a tool to prevent congressional oversight.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    classification_scope_boundary,
    'What constitutes legitimately classifiable information versus information Congress must have to exercise constitutional oversight?',
    'Comparative analysis of classification decisions across administrations; assessment of disclosed information that turned out non-sensitive; cost-benefit analysis of withheld vs compromised operations',
    'If scope can be narrowed: extraction mechanism weakens substantially. If scope is inherent to operational security: extraction is unavoidable and constraint becomes more mountain-like.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(classification_scope_boundary, conceptual, 'Boundary between legitimate classification and congressional right-to-know').

omega_variable(
    clearance_as_coercion,
    'Does the security clearance requirement function primarily as information control or as mechanism to coerce congressional silence through career/reputation risk?',
    'Analysis of instances where cleared officials faced repercussions for disclosing classified information to Congress vs to executive; comparison of information flow before/after clearance requirement was implemented',
    'If clearance is primarily coercive: suppression metric rises, classification becomes snare-like. If clearance is primarily functional: suppression metric was inflated, constraint becomes more rope-like.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(clearance_as_coercion, empirical, 'Whether clearance functions as information control or coercion mechanism').

omega_variable(
    compartmentation_effectiveness,
    'Does compartmentation (the practice of limiting access to subsets of classified information) serve legitimate operational security or primarily function to prevent Congress from assembling a comprehensive picture?',
    'Analysis of information crosswalking: whether Congress could reconstruct broader strategic picture if individual compartments were shared; assessment of compartmentation decisions that appear designed to prevent comprehensive oversight',
    'If compartmentation is operationally necessary: theater ratio is lower, constraint is more functional. If compartmentation is primarily about control: theater ratio is higher, constraint is more extractive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(compartmentation_effectiveness, empirical, 'Whether compartmentation serves operational security or prevents comprehensive oversight').

omega_variable(
    executive_good_faith,
    'Does the executive branch interpret Congress''s need-to-know requirement in good faith as a delegation of oversight power or as a tool to minimize information sharing?',
    'Longitudinal analysis of classification and declassification patterns; comparison of what executive chooses to brief Congress on vs what Congress specifically requests; assessment of post-hoc declassification revealing information was witheld without legitimate operational reason',
    'If interpreted in good faith: constraint functions as rope or scaffold. If interpreted as control tool: constraint functions as tangled rope or snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(executive_good_faith, preference, 'Executive good faith in interpreting congressional need-to-know').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(congressional_classified_oversight, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cco_tr_t0, congressional_classified_oversight, theater_ratio, 0, 0.48).
narrative_ontology:measurement(cco_tr_t20, congressional_classified_oversight, theater_ratio, 20, 0.56).
narrative_ontology:measurement(cco_tr_t40, congressional_classified_oversight, theater_ratio, 40, 0.64).

% Extraction over time
narrative_ontology:measurement(cco_be_t0, congressional_classified_oversight, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cco_be_t20, congressional_classified_oversight, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(cco_be_t40, congressional_classified_oversight, base_extractiveness, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(congressional_classified_oversight, enforcement_mechanism).
narrative_ontology:affects_constraint(congressional_classified_oversight, foreign_intelligence_surveillance).
narrative_ontology:affects_constraint(congressional_classified_oversight, executive_privilege_claim).
narrative_ontology:affects_constraint(congressional_classified_oversight, intelligence_community_autonomy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(congressional_classified_oversight, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
