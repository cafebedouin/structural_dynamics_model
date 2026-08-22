% ============================================================================
% CONSTRAINT STORY: federation_membership_treaty__integration_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_treaty__integration_primary, []).

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
 *   constraint_id: federation_membership_treaty__integration_primary
 *   human_readable: Free Movement as Constitutive Principle of the Single Market (Integration-Primary Reading)
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This story instantiates the integration-primary reading of the federation
 *   membership treaty's free-movement kernel: free movement is treated as
 *   constitutive of the single market itself, not as one policy interest to
 *   be balanced against others, so that any national restriction bears the
 *   burden of narrow justification. Under this reading, mobile workers and
 *   cross-border employers become structural beneficiaries of an expanding,
 *   court-elaborated doctrine, while local labor-market incumbents, national
 *   welfare systems, and border municipalities absorb costs the doctrine does
 *   not itself internalize. This is a distinct constraint from the
 *   sovereignty-primary reading (which authors low suppression of national
 *   restriction and treats consent as the baseline) and the
 *   subsidiarity-balance reading (which authors a genuine multi-factor
 *   proportionality test with lower suppression and a broader
 *   victim/beneficiary overlap). Each reading has its own epsilon; they are
 *   linked, not merged, via network.affects_constraints.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_treaty__integration_primary, 0.62).
domain_priors:suppression_score(federation_membership_treaty__integration_primary, 0.78).
domain_priors:theater_ratio(federation_membership_treaty__integration_primary, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, extractiveness, 0.62).
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_treaty__integration_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_treaty__integration_primary, "Free Movement as Constitutive Principle of the Single Market (Integration-Primary Reading)").
narrative_ontology:topic_domain(federation_membership_treaty__integration_primary, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_treaty__integration_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_treaty__integration_primary, '11de0e0f-4997-408f-ab78-9f388b9bd256').
narrative_ontology:cs_kernel_codification('11de0e0f-4997-408f-ab78-9f388b9bd256', fixed_text).
narrative_ontology:cs_authority_grounding('11de0e0f-4997-408f-ab78-9f388b9bd256', lineage).
narrative_ontology:cs_interpretation_layer_present('11de0e0f-4997-408f-ab78-9f388b9bd256').
narrative_ontology:cs_reading_relation('11de0e0f-4997-408f-ab78-9f388b9bd256', federation_membership_treaty__sovereignty_primary, forecloses).
narrative_ontology:cs_reading_relation('11de0e0f-4997-408f-ab78-9f388b9bd256', federation_membership_treaty__subsidiarity_balance, influences).
narrative_ontology:cs_axiom('11de0e0f-4997-408f-ab78-9f388b9bd256', foundational, free_movement_constitutive_of_market_order).
narrative_ontology:cs_axiom_status(free_movement_constitutive_of_market_order, holdable).
narrative_ontology:cs_axiom_grounding('11de0e0f-4997-408f-ab78-9f388b9bd256', free_movement_constitutive_of_market_order, conventional).
narrative_ontology:cs_axiom('11de0e0f-4997-408f-ab78-9f388b9bd256', foundational, restriction_bears_burden_of_justification).
narrative_ontology:cs_axiom_status(restriction_bears_burden_of_justification, holdable).
narrative_ontology:cs_axiom_grounding('11de0e0f-4997-408f-ab78-9f388b9bd256', restriction_bears_burden_of_justification, instrumental).
narrative_ontology:cs_reference_frame('11de0e0f-4997-408f-ab78-9f388b9bd256', founding_treaty_single_market_mandate).
narrative_ontology:cs_drift_state('11de0e0f-4997-408f-ab78-9f388b9bd256', contemporary_expanded_case_law_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('11de0e0f-4997-408f-ab78-9f388b9bd256', '').
narrative_ontology:cs_kernel_id(federation_membership_treaty__integration_primary, federation_membership_treaty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_treaty__integration_primary, mobile_workers).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__integration_primary, cross_border_employers).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__integration_primary, integration_court_and_commission).
narrative_ontology:constraint_victim(federation_membership_treaty__integration_primary, local_labor_market_incumbents).
narrative_ontology:constraint_victim(federation_membership_treaty__integration_primary, national_welfare_systems).
narrative_ontology:constraint_victim(federation_membership_treaty__integration_primary, border_region_municipalities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Can relocate across member states to take jobs, access services, and bring family, without work permits or quotas. Their livelihoods depend on the free-movement guarantee being treated as near-absolute; any national carve-out threatens their ability to plan a cross-border life.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, mobile_workers, beneficiary,
    moderate, biographical, mobile, continental).

% Draw on a continent-wide labor pool without the friction of national work-permit regimes, allowing wage arbitrage and staffing flexibility across member states. Lobby against any narrowing of the free-movement default because it directly expands their hiring options.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, cross_border_employers, beneficiary,
    powerful, generational, arbitrage, continental).

% Adjudicates free-movement disputes and strikes down national restrictions that fail the narrow-justification test it has developed. Treats free movement as constitutive of the treaty order itself, not merely one policy among several, and expands the doctrine through case law rather than fresh treaty amendment. Its institutional authority and caseload depend on this reading remaining dominant.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, integration_court_and_commission, agenda_setter,
    institutional, civilizational, analytical, continental).

% Compete for the same low- and mid-skill jobs against an enlarged applicant pool with no local exit option; cannot relocate as easily as capital or highly skilled mobile workers. Wage and job-security pressure in their sector is treated by the integration reading as an acceptable byproduct of a constitutive freedom, not a harm requiring a remedy.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, local_labor_market_incumbents, payer,
    powerless, biographical, trapped, national).

% Must extend benefits, healthcare access, and social insurance to newly arrived mobile workers under equal-treatment principles derived from free movement, even where contribution histories are short. National governments that attempt residency or contribution thresholds face legal challenge and are told the burden of proof sits with the restriction, not with the mobility right.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, national_welfare_systems, payer,
    institutional, generational, constrained, national).

% Absorb disproportionate housing, school, and service demand from cross-border commuting and settlement patterns without proportional fiscal transfer, because the free-movement guarantee does not itself allocate the local costs it generates. Have no standing to seek exceptions calibrated to their specific absorption capacity.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, border_region_municipalities, payer,
    powerless, biographical, trapped, regional).

% Would prefer authority to impose quotas, residency waiting periods, or sector-specific labor market tests to protect domestic workers and welfare solvency, but under this reading any such measure is presumptively illegitimate and must survive a narrow-justification test set and applied by the integration court. Their electorates' preferences for restriction carry no independent weight in the doctrinal test.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, national_governments_seeking_restriction, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_treaty__integration_primary, diffuse).
narrative_ontology:fixing_cost_class(federation_membership_treaty__integration_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Guarantees a single, predictable labor and services market across member states so firms can hire and workers can move without renegotiating access state-by-state — removing the coordination failure of 27+ separate, potentially conflicting national labor-mobility regimes.
% TRANSFER_FUNCTION: Moves labor-market access and welfare entitlement from a nationally-gated allocation to a continent-wide default; local wage premiums, job security for incumbent workers, and control over welfare eligibility criteria flow from national labor markets and welfare systems to mobile workers and the employers who hire them.
% ABSENT_VOICES: Local labor market incumbents and border municipalities have no direct standing before the integration court; their interests are represented, if at all, through national governments whose restrictive measures the court reviews with a presumption of illegitimacy. Sub-national fiscal-burden claims are structurally invisible to a doctrine built around individual mobility rights.
% DISAPPEARANCE_RATIONALE: If the constitutive, presumption-against-restriction reading of free movement disappeared, member states would immediately begin reintroducing quotas, residency waiting periods, and sector protections; cross-border employers would lose an assured continent-wide labor pool; mobile workers already relocated would face new legal uncertainty about their status; the single market's labor and services pillars would fragment into a patchwork of bilateral or unilateral national regimes.
% FOUNDING_PROBLEM: Post-war economic integration required removing internal barriers to labor mobility so that a genuine common market could function, rather than a set of nominally linked but practically closed national economies.
% FOUNDING_PROBLEM_CORROBORATION: The integration court and pro-integration economists attest the founding problem remains live — that any weakening of the presumption reopens fragmentation risk. Independent labor economists studying border regions and national auditors reviewing welfare-system strain attest that the original coordination problem has been substantially solved and that the current doctrine now functions primarily to foreclose politically legitimate redistributive adjustments rather than to prevent renewed market fragmentation.
narrative_ontology:disappearance_verdict(federation_membership_treaty__integration_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_treaty__integration_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_treaty__integration_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(federation_membership_treaty__integration_primary, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_treaty__integration_primary, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_treaty__integration_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_treaty__integration_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_treaty__integration_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.62 at interval end) reflects a real but partial transfer: local incumbents and welfare systems bear costs whose scale grows as case law forecloses more restriction options over time, shown in the rising base_extractiveness series. Suppression is high (0.78) and rises faster than extraction because the doctrine's core mechanism is precisely to suppress the space of legitimate national restriction — the presumption-against-restriction structure is itself a suppression device, independent of how much material extraction results in any given year. Theater ratio stays low (0.20) because the enforcement mechanism (case-by-case judicial review) is functionally real, not primarily performative — restrictions actually get struck down, not merely criticized.
 *
 * PERSPECTIVAL GAP:
 *   From the integration court's seat, the doctrine is coordination infrastructure whose expansion is simply correct legal reasoning applied consistently. From the local-incumbent or border-municipality seat, the same expanding doctrine is experienced as an ever-tightening extraction mechanism they have no standing to resist. The engine's per-seat computation should diverge sharply between these two positions given the declared power/exit/scope data.
 *
 * DIRECTIONALITY LOGIC:
 *   Mobile workers and cross-border employers sit near the beneficiary end: the free-movement default is engineered around their access, and their exit options (mobile, arbitrage) reflect low structural dependency on any single state's restriction regime. Local labor incumbents and border municipalities sit near the target end: trapped exit options, powerless power atom, and no standing to contest the doctrine directly. National welfare systems occupy an institutional-but-constrained position — they administer the cost but cannot unilaterally exit the obligation, which the derivation captures via constrained exit despite institutional power.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (removing coordination failure across fragmented national labor regimes) was substantively live during early single-market construction. Under the integration-primary reading, the doctrine has since generalized well beyond that founding case into a broad, self-expanding presumption that forecloses even proportionate, evidence-based national adjustments — the R5 corroboration split (court/pro-integration economists say live; independent labor economists and auditors say the problem has been solved and the doctrine now serves a different function) is exactly the founding_problem_status=contested signal the mismatch consumer should read against the world_rearranges verdict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_free_movement,
    'Is the federation membership treaty''s free-movement clause correctly read as constitutive of the single market (integration-primary), as conditional on state consent (sovereignty-primary), or as bounded by proportionality (subsidiarity-balance)?',
    'No empirical resolution exists; this is a live constitutional-interpretive contest resolved provisionally by treaty amendment, constitutional court composition, and accumulated case law, not by any single dispositive fact. Each reading instantiates a structurally distinct constraint with its own beneficiary/victim set and its own epsilon.',
    'Under integration_primary, mobile workers and employers are beneficiaries and local labor markets/welfare systems are victims with high suppression of restriction (this story). Under sovereignty_primary, the victim/beneficiary sets substantially invert and suppression of restriction is low. Under subsidiarity_balance, a proportionality test produces a narrower, more contingent victim/beneficiary overlap with moderate suppression. The three are separate constraint files linked via network.affects_constraints, not one story with a hidden parameter.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_free_movement, conceptual, 'Committer-frame: this story is one reading among three of a contested treaty kernel; the disagreement is located in whether restriction or mobility bears the presumption.').

omega_variable(
    integration_doctrine_scope_creep,
    'Has the integration court''s case-law elaboration of the free-movement doctrine expanded materially beyond what the founding single-market coordination problem required, or does the expansion track a genuinely evolving coordination need (e.g., services and digital labor mobility not contemplated at founding)?',
    'Comparative doctrinal history: track the scope of restrictions struck down over time against the scope of restrictions that were actually causing single-market fragmentation at each period, using independent economic analysis of cross-border trade and labor-flow disruption.',
    'If expansion tracks genuine coordination need, the rising suppression/extraction series reflects legitimate doctrinal maturation. If expansion has outpaced the coordination need, the rising series indicates the constraint has drifted from coordination toward institutional self-perpetuation by the integration court itself, supporting a tangled_rope-to-snare trajectory concern.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(integration_doctrine_scope_creep, empirical, 'Whether doctrinal expansion tracks founding coordination need or reflects institutional scope creep.').

omega_variable(
    border_municipality_standing_gap,
    'Does the absence of direct standing for border municipalities and local labor-incumbent groups before the integration court reflect a deliberate institutional design choice (individual rights are the correct unit of adjudication) or an unaddressed structural gap in the treaty''s remedial architecture?',
    'Review of treaty drafting history and subsequent reform proposals for fiscal-transfer or standing mechanisms for sub-national bodies; absence of any serious reform attempt over multiple decades would support the structural-gap reading.',
    'A deliberate design choice suggests the excluded-voices problem is a stable feature the doctrine accepts as a cost of individual-rights-based adjudication. An unaddressed gap suggests genuine reform space exists that the current doctrine''s presumption-against-restriction structure is actively suppressing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(border_municipality_standing_gap, conceptual, 'Whether excluded sub-national voices reflect design intent or an unaddressed remedial gap.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_treaty__integration_primary, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_treaty__integration_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement(fede_tr_t8, federation_membership_treaty__integration_primary, theater_ratio, 8, 0.13).
narrative_ontology:measurement(fede_tr_t16, federation_membership_treaty__integration_primary, theater_ratio, 16, 0.15).
narrative_ontology:measurement(fede_tr_t24, federation_membership_treaty__integration_primary, theater_ratio, 24, 0.17).
narrative_ontology:measurement(fede_tr_t32, federation_membership_treaty__integration_primary, theater_ratio, 32, 0.19).
narrative_ontology:measurement(fede_tr_t40, federation_membership_treaty__integration_primary, theater_ratio, 40, 0.2).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_treaty__integration_primary, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(fede_be_t8, federation_membership_treaty__integration_primary, base_extractiveness, 8, 0.46).
narrative_ontology:measurement(fede_be_t16, federation_membership_treaty__integration_primary, base_extractiveness, 16, 0.51).
narrative_ontology:measurement(fede_be_t24, federation_membership_treaty__integration_primary, base_extractiveness, 24, 0.56).
narrative_ontology:measurement(fede_be_t32, federation_membership_treaty__integration_primary, base_extractiveness, 32, 0.6).
narrative_ontology:measurement(fede_be_t40, federation_membership_treaty__integration_primary, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_treaty__integration_primary, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(fede_su_t8, federation_membership_treaty__integration_primary, suppression_requirement, 8, 0.62).
narrative_ontology:measurement(fede_su_t16, federation_membership_treaty__integration_primary, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(fede_su_t24, federation_membership_treaty__integration_primary, suppression_requirement, 24, 0.72).
narrative_ontology:measurement(fede_su_t32, federation_membership_treaty__integration_primary, suppression_requirement, 32, 0.76).
narrative_ontology:measurement(fede_su_t40, federation_membership_treaty__integration_primary, suppression_requirement, 40, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_treaty__integration_primary, resource_allocation).
narrative_ontology:boltzmann_floor_override(federation_membership_treaty__integration_primary, 0.12).
narrative_ontology:affects_constraint(federation_membership_treaty__integration_primary, federation_membership_treaty__sovereignty_primary).
narrative_ontology:affects_constraint(federation_membership_treaty__integration_primary, federation_membership_treaty__subsidiarity_balance).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the federation_membership_treaty kernel (integration_primary, sovereignty_primary, subsidiarity_balance). Each reading is authored as a separate constraint file with its own epsilon, beneficiary/victim structure, and classification, per the epsilon-invariance principle — the natural-language label 'free movement clause' conflates three structurally distinct legal-political claims. Integration_primary authors the highest suppression value among the three (restriction bears the presumption of illegitimacy); sovereignty_primary authors the lowest (consent is the baseline); subsidiarity_balance sits between (proportionality test). All three link to each other via affects_constraints because judicial and legislative developments in any one reading's doctrinal dominance shift the practical legitimacy and resource availability of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
