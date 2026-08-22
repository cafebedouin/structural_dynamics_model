% ============================================================================
% CONSTRAINT STORY: gpl_copyleft_scope__enforcement_vacuum_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-28
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_copyleft_scope__enforcement_vacuum_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: gpl_copyleft_scope__enforcement_vacuum_reading
 *   human_readable: GPL Copyleft Scope — Enforcement Vacuum Reading
 *   domain: software_licensing/intellectual_property/open_source_governance
 *
 * SUMMARY:
 *   The GPL's copyleft scope (Section 2(b)'s 'work based on the Program') has
 *   never received definitive judicial interpretation in any major
 *   jurisdiction. The FSF advocates a broad reading (strong copyleft: dynamic
 *   linking, plugin architectures, and any combined work trigger copyleft).
 *   Industry actors (particularly those building proprietary ecosystems atop
 *   GPL foundations) favor a narrow reading (only direct derivative works in
 *   the traditional copyright sense). This enforcement_vacuum_reading treats
 *   the absence of precedent not as a gap to be resolved but as a structural
 *   feature: the constraint's actual force in any context depends on which
 *   interpretive community — FSF-aligned projects with community enforcement
 *   norms, or industry-dominated ecosystems with corporate legal capacity —
 *   holds enforcement capacity there. Adopters navigate this pluralism
 *   through risk assessment and community alignment. Clarity-seeking adopters
 *   (small organizations, risk-averse enterprises) face elevated transaction
 *   costs for legal counsel and compliance engineering. Pragmatic adopters
 *   exploit the ambiguity for flexibility, choosing the reading that suits
 *   their context.
 *
 * KEY AGENTS:
 *   - pragmatic_adopters: Beneficiary (moderate/constrained) — exploit ambiguity for flexibility across contexts
 *   - fsl_aligned_communities: Beneficiary/Agenda setter (organized/identity_locked) — maintain broad reading through community enforcement norms
 *   - industry_ecosystem_participants: Beneficiary (powerful/arbitrage) — leverage narrow reading while building proprietary extensions
 *   - clarity_seeking_adopters: Victim (moderate/constrained) — bear transaction costs for legal certainty
 *   - small_organizations_without_legal_counsel: Victim (powerless/trapped) — cannot afford risk assessment, effectively excluded or forced into over-compliance
 *   - legal_observers: Observer (analytical/analytical) — track interpretive drift and enforcement patterns
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_copyleft_scope__enforcement_vacuum_reading, 0.35).
domain_priors:suppression_score(gpl_copyleft_scope__enforcement_vacuum_reading, 0.45).
domain_priors:theater_ratio(gpl_copyleft_scope__enforcement_vacuum_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_copyleft_scope__enforcement_vacuum_reading, tangled_rope).
narrative_ontology:human_readable(gpl_copyleft_scope__enforcement_vacuum_reading, "GPL Copyleft Scope — Enforcement Vacuum Reading").
narrative_ontology:topic_domain(gpl_copyleft_scope__enforcement_vacuum_reading, "software_licensing/intellectual_property/open_source_governance").

domain_priors:requires_active_enforcement(gpl_copyleft_scope__enforcement_vacuum_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_copyleft_scope__enforcement_vacuum_reading, '799956f5-c389-48d5-af5f-5e33020cef9f').
narrative_ontology:cs_kernel_codification('799956f5-c389-48d5-af5f-5e33020cef9f', fixed_text).
narrative_ontology:cs_authority_grounding('799956f5-c389-48d5-af5f-5e33020cef9f', lineage).
narrative_ontology:cs_interpretation_layer_present('799956f5-c389-48d5-af5f-5e33020cef9f').
narrative_ontology:cs_reading_relation('799956f5-c389-48d5-af5f-5e33020cef9f', gpl_copyleft_scope__strong_copyleft_reading, coexists_with).
narrative_ontology:cs_reading_relation('799956f5-c389-48d5-af5f-5e33020cef9f', gpl_copyleft_scope__narrow_scope_reading, coexists_with).
narrative_ontology:cs_axiom('799956f5-c389-48d5-af5f-5e33020cef9f', foundational, enforcement_capacity_determines_effective_scope).
narrative_ontology:cs_axiom_status(enforcement_capacity_determines_effective_scope, holdable).
narrative_ontology:cs_axiom_grounding('799956f5-c389-48d5-af5f-5e33020cef9f', enforcement_capacity_determines_effective_scope, instrumental).
narrative_ontology:cs_axiom('799956f5-c389-48d5-af5f-5e33020cef9f', foundational, interpretive_pluralism_is_licensed_feature_not_bug).
narrative_ontology:cs_axiom_status(interpretive_pluralism_is_licensed_feature_not_bug, holdable).
narrative_ontology:cs_axiom_grounding('799956f5-c389-48d5-af5f-5e33020cef9f', interpretive_pluralism_is_licensed_feature_not_bug, conventional).
narrative_ontology:cs_reference_frame('799956f5-c389-48d5-af5f-5e33020cef9f', gplv2_textual_copyleft_promise).
narrative_ontology:cs_drift_state('799956f5-c389-48d5-af5f-5e33020cef9f', post_gplv3_no_precedent_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('799956f5-c389-48d5-af5f-5e33020cef9f', '').
narrative_ontology:cs_kernel_id(gpl_copyleft_scope__enforcement_vacuum_reading, gpl_copyleft_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__enforcement_vacuum_reading, pragmatic_adopters).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__enforcement_vacuum_reading, fsl_aligned_communities).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__enforcement_vacuum_reading, industry_ecosystem_participants).
narrative_ontology:constraint_victim(gpl_copyleft_scope__enforcement_vacuum_reading, clarity_seeking_adopters).
narrative_ontology:constraint_victim(gpl_copyleft_scope__enforcement_vacuum_reading, small_organizations_without_legal_counsel).
narrative_ontology:constraint_vindicates(gpl_copyleft_scope__enforcement_vacuum_reading, interpretive_pluralism_licensed_by_silence).
narrative_ontology:constraint_vindicates(gpl_copyleft_scope__enforcement_vacuum_reading, enforcement_capacity_determines_effective_scope).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the broad copyleft reading through community enforcement norms (social pressure, contribution gatekeeping, ecosystem cohesion). They do not litigate but their collective practice establishes de facto scope in FSF-aligned contexts. Their identity is fused with the broad reading — exit means abandoning the ideological project. They benefit from the enforcement vacuum because it lets their norms operate without judicial override.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, fsl_aligned_communities, agenda_setter,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(gpl_copyleft_scope__enforcement_vacuum_reading, fsl_aligned_communities, beneficiary).

% Build proprietary products and services atop GPL foundations using the narrow reading (dynamic linking, aggregation, plugin boundaries). They have legal teams to navigate ambiguity and the market power to set de facto standards in their ecosystems. They benefit from the enforcement vacuum because it lets them claim compliance while extracting commercial value. Their exit is arbitrage-grade — they can shift readings per jurisdiction or product line.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, industry_ecosystem_participants, beneficiary,
    powerful, biographical, arbitrage, global).

% Choose GPL for its network effects and community, then navigate scope ambiguity contextually — using broad reading when contributing upstream, narrow reading when linking proprietary modules. They exploit the enforcement vacuum for flexibility but lack the power to set norms. Their exit is constrained by ecosystem dependence.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, pragmatic_adopters, beneficiary,
    moderate, biographical, constrained, global).

% Need legal certainty for compliance, risk management, or investor requirements. They pay for legal counsel, compliance tooling, and conservative engineering (avoiding dynamic linking, over-licensing) to resolve the ambiguity the constraint leaves open. Their exit is constrained — migrating away from GPL ecosystems loses network effects and community.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, clarity_seeking_adopters, payer,
    moderate, biographical, constrained, global).

% Cannot afford legal risk assessment. They either over-comply (licensing proprietary code as GPL, losing commercialization options) or avoid GPL entirely (losing access to ecosystem). The enforcement vacuum effectively excludes them from meaningful participation — they bear the constraint's costs without its benefits.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, small_organizations_without_legal_counsel, payer,
    powerless, immediate, trapped, local).

% Track interpretive drift, enforcement actions, and compliance patterns across jurisdictions. They do not collect from or pay into the constraint. Their analysis feeds policy debates and potential future litigation.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, legal_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables a global commons of copyleft software by providing a shared license text that multiple interpretive communities can adopt simultaneously, allowing diverse projects to interoperate without resolving the derivative work boundary.
% TRANSFER_FUNCTION: Moves transaction costs and compliance burden from pragmatic adopters and industry actors (who exploit ambiguity) to clarity-seeking adopters and small organizations (who must resolve ambiguity at their own expense). No direct monetary transfer occurs; the extraction is in risk-bearing and legal cost.
% ABSENT_VOICES: Individual developers in jurisdictions with no FSF presence and no industry ecosystem — they adopt GPL by default (GitHub default, tutorial guidance) but have no community to interpret it for them and no legal capacity to interpret it themselves. They are structurally excluded from the interpretive pluralism that the constraint licenses.
% DISAPPEARANCE_RATIONALE: If the enforcement vacuum vanished (definitive precedent established), either the broad reading would become law (forcing industry actors to open proprietary extensions or migrate) or the narrow reading would become law (allowing proprietary enclosure of GPL ecosystems). In either case, the current pluralism — where both readings coexist as licensed practice — would collapse into a single enforceable regime, reorganizing the open source landscape.
% FOUNDING_PROBLEM: Prevent proprietary enclosure of free software by ensuring derivative works remain free. The GPL was built to solve this through a broad copyleft that captures any combined work.
% FOUNDING_PROBLEM_CORROBORATION: The FSF attests the problem is live (ongoing enclosure via SaaS, proprietary forks, cloud hosting). Industry actors attest the problem is substantially addressed by market norms and that the broad reading now chills innovation. Independent scholars (e.g., Vila, Rosen) document that the derivative work boundary remains legally unresolved — the founding problem's legal solution was never judicially ratified. No external corroboration exists for the claim that the enforcement vacuum itself serves the founding problem; that is this reading's distinctive claim.
narrative_ontology:disappearance_verdict(gpl_copyleft_scope__enforcement_vacuum_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_copyleft_scope__enforcement_vacuum_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_copyleft_scope__enforcement_vacuum_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(gpl_copyleft_scope__enforcement_vacuum_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_copyleft_scope__enforcement_vacuum_reading, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_copyleft_scope__enforcement_vacuum_reading_tests).
:- end_tests(gpl_copyleft_scope__enforcement_vacuum_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-moderate (0.35) because the constraint's primary extraction is transaction cost imposition on clarity-seekers, not direct rent collection. Suppression is moderate (0.45) — the constraint does not actively block exits (forking, relicensing, alternative licenses exist) but the interpretive uncertainty raises the cost of informed choice. Theater ratio is low (0.25) — the ambiguity is not performative; it reflects genuine legal indeterminacy. Accessibility collapse is moderate (0.4) — alternatives exist (MIT, Apache, proprietary) but the GPL's network effects and ideological gravity create partial lock-in. Resistance is moderate-high (0.55) — clarity-seekers actively push for precedent or migrate to permissive licenses; the FSF resists narrow precedent through community enforcement. The claimed type tangled_rope reflects genuine coordination (license interoperability, community formation) coexisting with asymmetric extraction (clarity-seekers pay, pragmatic actors benefit).
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute divergent seat types: from fsl_aligned_communities (beneficiary, identity_locked), the constraint appears as rope/coordination (d low, χ near zero). From industry_ecosystem_participants (beneficiary, arbitrage), it appears as rope with subsidy (d near 0). From clarity_seeking_adopters (payer, constrained), it appears as snare/tangled_rope (d high, χ amplified). From small_organizations_without_legal_counsel (victim, trapped), it appears as snare (d ~1.0, χ maximal). The enforcement vacuum is not experienced uniformly — it is a coordination tool for some, an extraction mechanism for others.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: fsl_aligned_communities (maintain broad scope through community norms without litigation cost), industry_ecosystem_participants (use narrow reading commercially while benefiting from GPL ecosystem), pragmatic_adopters (navigate ambiguity contextually). Victims: clarity_seeking_adopters (pay legal/compliance costs to resolve ambiguity), small_organizations_without_legal_counsel (face prohibitive risk assessment costs, effectively coerced into over-compliance or avoidance). The FSF is not listed as a beneficiary because it does not collect rents; it is an agenda_setter (see stakeholders). The vindicated propositions capture the doctrinal commitments each community maintains.
 *
 * MANDATROPHY ANALYSIS:
 *   The GPL's founding problem (preventing proprietary enclosure of free software) remains live — proprietary enclosure is an ongoing threat. However, the enforcement vacuum means the constraint's current operation only partially addresses this problem: it coordinates communities that share a reading, but fails to constrain actors who exploit the ambiguity. This is not mandatrophy (the problem is not dead) but a coordination-extraction hybrid: the constraint coordinates those who accept its terms while extracting from those who need clarity. The mandate has not atrophied; it has fragmented.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the enforcement_vacuum_reading a distinct constraint from the strong_copyleft_reading and narrow_scope_reading, or are these merely interpretive variants of a single constraint?',
    'Apply ε-invariance principle: if the enforcement vacuum reading''s extractiveness, suppression, and beneficiary/victim structure differ structurally from sibling readings — not merely in degree but in kind — then it is a separate constraint. The engine''s per-seat classification divergence (clarity-seekers as payers vs. pragmatic adopters as beneficiaries) provides the diagnostic.',
    'If distinct, the three readings form a constraint family linked by network.affects_constraints. If not, the kernel is a single constraint with observer-dependent classification — which the ε-invariance principle forbids.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether this reading instantiates a separate constraint per ε-invariance.').

omega_variable(
    enforcement_vacuum_naturalness,
    'Is the absence of definitive judicial precedent a natural feature of copyleft''s legal position, or a constructed ambiguity that benefits identifiable actors?',
    'Historical analysis of FSF''s litigation strategy and industry amicus patterns: did the FSF deliberately avoid test cases that might narrow scope? Did industry actors benefit from ambiguity while building proprietary ecosystems on GPL foundations?',
    'If constructed, the enforcement vacuum is a tangled_rope or snare feature (beneficiaries identified). If natural, the vacuum is a mountain-like legal indeterminacy — but then beneficiaries must be explained as incidental.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_vacuum_naturalness, empirical, 'Natural-law vs. constructed ambiguity in the enforcement vacuum.').

omega_variable(
    ambiguity_as_coordination_mechanism,
    'Does the interpretive pluralism itself function as a coordination mechanism (allowing diverse adopters to participate), or is it purely extractive (imposing transaction costs on clarity-seekers while pragmatic actors free-ride)?',
    'Survey adoption patterns: do projects choose GPL *because* the scope ambiguity lets them navigate between communities, or do they choose it despite the ambiguity? Measure transaction cost differential between clarity-seeking and pragmatic adopters.',
    'If coordination, the constraint has genuine rope-like function alongside extraction (tangled_rope confirmed). If purely extractive, the coordination story is cover (snare). The current metrics assume genuine but imperfect coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ambiguity_as_coordination_mechanism, conceptual, 'Whether ambiguity serves coordination or merely extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_copyleft_scope__enforcement_vacuum_reading, 1991, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t1991, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 1991, 0.1).
narrative_ontology:measurement(gpl__tr_t1999, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 1999, 0.15).
narrative_ontology:measurement(gpl__tr_t2007, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 2007, 0.2).
narrative_ontology:measurement(gpl__tr_t2015, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 2015, 0.22).
narrative_ontology:measurement(gpl__tr_t2023, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 2023, 0.25).

% Extraction over time
narrative_ontology:measurement(gpl__be_t1991, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 1991, 0.15).
narrative_ontology:measurement(gpl__be_t1999, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 1999, 0.22).
narrative_ontology:measurement(gpl__be_t2007, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 2007, 0.3).
narrative_ontology:measurement(gpl__be_t2015, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 2015, 0.33).
narrative_ontology:measurement(gpl__be_t2023, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 2023, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t1991, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 1991, 0.2).
narrative_ontology:measurement(gpl__su_t1999, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 1999, 0.3).
narrative_ontology:measurement(gpl__su_t2007, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 2007, 0.4).
narrative_ontology:measurement(gpl__su_t2015, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 2015, 0.43).
narrative_ontology:measurement(gpl__su_t2023, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 2023, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_copyleft_scope__enforcement_vacuum_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gpl_copyleft_scope__enforcement_vacuum_reading, 0.08).
narrative_ontology:affects_constraint(gpl_copyleft_scope__enforcement_vacuum_reading, gpl_copyleft_scope__strong_copyleft_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__enforcement_vacuum_reading, gpl_copyleft_scope__narrow_scope_reading).

% DUAL FORMULATION NOTE:
% This enforcement_vacuum_reading and its two siblings (strong_copyleft_reading, narrow_scope_reading) form a constraint family sharing kernel gpl_copyleft_scope. Each reading instantiates a different constraint: strong_copyleft has higher ε (active enforcement of broad scope), narrow_scope has lower ε but higher suppression for those who accept broad reading, enforcement_vacuum has moderate ε from transaction costs and asymmetric beneficiary structure. The ε-invariance principle requires three stories because the constraint's extractiveness and beneficiary structure differ by reading — they are not the same constraint viewed from different angles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gpl_copyleft_scope__enforcement_vacuum_reading, organized, 0.15).
constraint_indexing:directionality_override(gpl_copyleft_scope__enforcement_vacuum_reading, powerful, 0.1).
constraint_indexing:directionality_override(gpl_copyleft_scope__enforcement_vacuum_reading, moderate, 0.7).
constraint_indexing:directionality_override(gpl_copyleft_scope__enforcement_vacuum_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
