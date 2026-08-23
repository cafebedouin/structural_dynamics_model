% ============================================================================
% CONSTRAINT STORY: federation_membership__sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership__sovereignty_reading, []).

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
 *   constraint_id: federation_membership__sovereignty_reading
 *   human_readable: Federation Membership as Conditional Treaty (Sovereignty Reading)
 *   domain: political/federalism/migration
 *
 * SUMMARY:
 *   This constraint story captures the sovereignty reading of federation
 *   membership: a conditional treaty among nation-states where each retains
 *   legitimate authority over its borders, and free movement is a negotiable
 *   policy concession rather than a constitutional right. The reading frames
 *   mobility restriction as sovereign prerogative; the integration reading
 *   frames it as treaty violation. The constraint is the operational border
 *   regime that results when national governments invoke sovereignty to limit
 *   mobility — a regime that coordinates trade and security (genuine
 *   coordination) while extracting mobility rights from citizens and workers
 *   for the benefit of local labor markets (asymmetric extraction).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership__sovereignty_reading, 0.78).
domain_priors:suppression_score(federation_membership__sovereignty_reading, 0.82).
domain_priors:theater_ratio(federation_membership__sovereignty_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership__sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership__sovereignty_reading, "Federation Membership as Conditional Treaty (Sovereignty Reading)").
narrative_ontology:topic_domain(federation_membership__sovereignty_reading, "political/federalism/migration").

domain_priors:requires_active_enforcement(federation_membership__sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership__sovereignty_reading, '986e6515-e546-47ad-ada1-e20078ad0f52').
narrative_ontology:cs_kernel_codification('986e6515-e546-47ad-ada1-e20078ad0f52', formalized).
narrative_ontology:cs_authority_grounding('986e6515-e546-47ad-ada1-e20078ad0f52', lineage).
narrative_ontology:cs_interpretation_layer_present('986e6515-e546-47ad-ada1-e20078ad0f52').
narrative_ontology:cs_reading_relation('986e6515-e546-47ad-ada1-e20078ad0f52', federation_membership__integration_reading, coexists_with).
narrative_ontology:cs_axiom('986e6515-e546-47ad-ada1-e20078ad0f52', foundational, national_border_sovereignty_primacy).
narrative_ontology:cs_axiom_status(national_border_sovereignty_primacy, holdable).
narrative_ontology:cs_axiom_grounding('986e6515-e546-47ad-ada1-e20078ad0f52', national_border_sovereignty_primacy, deontological).
narrative_ontology:cs_axiom('986e6515-e546-47ad-ada1-e20078ad0f52', secondary, free_movement_as_negotiated_concession).
narrative_ontology:cs_axiom_status(free_movement_as_negotiated_concession, holdable).
narrative_ontology:cs_axiom_grounding('986e6515-e546-47ad-ada1-e20078ad0f52', free_movement_as_negotiated_concession, conventional).
narrative_ontology:cs_reference_frame('986e6515-e546-47ad-ada1-e20078ad0f52', westphalian_treaty_framework).
narrative_ontology:cs_drift_state('986e6515-e546-47ad-ada1-e20078ad0f52', contemporary_migration_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('986e6515-e546-47ad-ada1-e20078ad0f52', '').
narrative_ontology:cs_kernel_id(federation_membership__sovereignty_reading, federation_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership__sovereignty_reading, local_labor_markets).
narrative_ontology:constraint_beneficiary(federation_membership__sovereignty_reading, national_governments).
narrative_ontology:constraint_victim(federation_membership__sovereignty_reading, mobile_citizens).
narrative_ontology:constraint_victim(federation_membership__sovereignty_reading, migrant_workers).
narrative_ontology:constraint_vindicates(federation_membership__sovereignty_reading, national_border_sovereignty).
narrative_ontology:constraint_vindicates(federation_membership__sovereignty_reading, conditional_treaty_doctrine).
narrative_ontology:constraint_vindicates(federation_membership__sovereignty_reading, labor_market_protection_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Negotiate and ratify federation treaties while retaining unilateral border control authority. Set immigration quotas, visa policies, and enforcement priorities. Benefit from federation trade and security coordination but treat free movement as a negotiable concession, not an obligation. Face domestic political pressure to restrict mobility.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, national_governments, agenda_setter,
    institutional, generational, constrained, national).

% Domestic workers and unions in member states who gain wage protection and reduced competition from restricted migrant inflows. Their political representation lobbies for mobility restrictions. Benefit is concentrated in sectors exposed to cross-border labor competition; they do not administer the constraint but capture its protective rents.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, local_labor_markets, beneficiary,
    organized, biographical, constrained, regional).

% Citizens of federation member states who would exercise treaty-guaranteed mobility rights for work, study, or family. Face visa barriers, quota limits, recognition hurdles, and enforcement discretion that the sovereignty reading legitimizes. Exit is identity-locked: their citizenship ties them to the federation, but the reading denies them the mobility that citizenship in an integration frame would guarantee.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, mobile_citizens, payer,
    moderate, biographical, identity_locked, continental).

% Non-citizen workers from member or candidate states seeking employment across borders. Bear the full cost of mobility restriction: denied entry, precarious legal status, wage suppression, and deportation risk. No political voice in the national governments that set the rules; exit means returning to origin-country labor markets with lower wages.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, migrant_workers, payer,
    powerless, immediate, trapped, continental).

% Federation-level bodies (commission, parliament, court) that interpret treaties toward integration. They issue rulings expanding mobility rights but lack direct enforcement against recalcitrant member states. Their authority is contested by the sovereignty reading; they observe and litigate but cannot compel compliance when national governments invoke border legitimacy.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, supranational_institutions, observer,
    institutional, generational, analytical, continental).
narrative_ontology:stakeholder_secondary_role(federation_membership__sovereignty_reading, supranational_institutions, excluded).

% National and federation-level competition regulators who assess whether mobility restrictions constitute anti-competitive market segmentation. They publish analyses showing welfare losses from labor market fragmentation but have no mandate to override treaty-based border controls.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, competition_authorities, observer,
    institutional, biographical, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Federation coordinates trade liberalization, security cooperation, regulatory harmonization, and dispute resolution among member states — genuine collective-action problems that bilateral treaties would solve less efficiently.
% TRANSFER_FUNCTION: Transfers mobility rights and labor market access from mobile citizens and migrant workers to local labor markets and national governments, via border enforcement that restricts entry, residence, and work authorization.
% ABSENT_VOICES: Would-be migrants from non-member states, divided families separated by border rules, and future-generation citizens who inherit a more fragmented federation. They are not represented in treaty negotiations or national political debates that set mobility policy.
% DISAPPEARANCE_RATIONALE: If conditional border controls vanished overnight, labor markets would rapidly equilibrate across the federation: wages would converge, sectoral shortages would fill through mobility, and national governments would lose a primary lever of domestic labor policy. The federation would functionally become the integration_reading's vision — a shift the sovereignty reading exists to prevent.
% FOUNDING_PROBLEM: Post-conflict reconstruction required a framework for economic coordination and mutual security among sovereign states that had recently been at war, without surrendering the national authority each state regarded as its core legitimacy.
% FOUNDING_PROBLEM_CORROBORATION: Treaty drafting records and contemporary statements by founding governments confirm the sovereignty-preserving intent. Integration_reading proponents (supranational institutions, federalist parties, legal scholars) attest the founding problem was always understood as a transitional compromise toward deeper union, citing preamble language and subsequent treaty amendments. No neutral arbiter exists; the contest is structural.
narrative_ontology:disappearance_verdict(federation_membership__sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership__sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership__sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(federation_membership__sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership__sovereignty_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership__sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership__sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership__sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.78) because the mobility restriction transfers substantial economic opportunity from mobile agents to sedentary labor markets, and the transfer is enforced by state coercion. Suppression is higher (0.82) because the constraint's persistence depends on active border enforcement, visa regimes, and deportation machinery — not on participant consent. Theater is moderate (0.38): border security rhetoric performs a protective function, but a growing share of enforcement targets economic migrants who pose no security threat. Accessibility collapse (0.72) reflects that mobile citizens cannot practically exercise treaty rights when national governments systematically obstruct them. Resistance (0.55) is significant but constrained by identity-locked exit for citizens and trapped exit for migrant workers.
 *
 * PERSPECTIVAL GAP:
 *   From the national government seat, the constraint is coordination with negotiated exceptions — a rope with sovereign reservations. From the mobile citizen seat, it is a snare: the treaty promises mobility but the sovereignty reading nullifies it through enforcement. From the migrant worker seat, it is a snare with no coordination benefit at all. The engine computes this divergence from the structural data; the claimed_type (tangled_rope) reflects the authoring seat's judgment that genuine coordination exists alongside extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   National governments are agenda_setters with constrained exit (they could leave the federation but face massive costs) — directionality near symmetric but tilted toward beneficiary as they capture political rents from restriction. Local labor markets are clear beneficiaries (organized, constrained exit, collect protection rents). Mobile citizens are payers with identity_locked exit: their federation citizenship makes exit existentially costly, so they bear extraction without escape. Migrant workers are payers with trapped exit: no political voice, no viable alternative. Supranational institutions are observers/excluded: they see the structure but cannot change it from their seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-war coordination without sovereignty surrender) is contested: sovereignty_reading says it remains live (security threats, economic asymmetry justify continued border control); integration_reading says it is dead (the federation has achieved the coordination, borders now serve only protectionism). The mandate has not atrophied into pure inertia — national governments actively defend and expand border enforcement — so this is not a piton. The theater rise tracks the widening gap between the treaty's mobility language and the enforcement reality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is federation membership a conditional treaty (sovereignty_reading) or an irreversible integration (integration_reading)?',
    'No legal resolution exists within the federation; the contest is political. A constitutional crisis or treaty revision conference could settle it, but neither reading''s coalition has the power to force one.',
    'If sovereignty_reading is structurally true, ε is high (mobility restriction = extraction). If integration_reading is structurally true, the same restriction is a treaty violation with ε ≈ 0 for the mobility right itself (the violation is the extractive act). The kernel contest directly determines ε''s referent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'The kernel-level framing ambiguity that makes ε reading-indexed.').

omega_variable(
    coordination_extraction_boundary,
    'Is the mobility restriction a necessary coordination cost (preventing welfare tourism, preserving fiscal sustainability) or pure extraction (protectionism disguised as sovereignty)?',
    'Natural experiment from member states that unilaterally liberalized: if fiscal systems held and labor markets adjusted without crisis, the restriction is not a coordination necessity. Comparative analysis of federation vs. non-federation mobility outcomes.',
    'If coordination necessity, part of measured ε is the price of the federation''s existence — tangled_rope with lower effective extraction. If pure protectionism, the coordination function is a cover — the constraint is a snare for mobile citizens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Whether the extraction component is structurally inseparable from the coordination function.').

omega_variable(
    local_labor_benefit_realization,
    'Do local labor markets actually capture net benefit from mobility restriction, or is the benefit illusory (offset by labor shortages, reduced innovation, demographic decline)?',
    'Longitudinal sectoral wage and employment data comparing restricted vs. liberalized periods; econometric decomposition of wage effects from mobility restriction vs. other factors.',
    'If benefit is illusory, local_labor_markets are not true beneficiaries — the constraint extracts from mobile citizens without delivering to the declared beneficiary, making it a piton or a snare with a false beneficiary story. If benefit is real, the tangled_rope structure holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(local_labor_benefit_realization, empirical, 'Whether the declared beneficiary actually benefits, or the beneficiary story is a coordination cover.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership__sovereignty_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership__sovereignty_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(fede_tr_t8, federation_membership__sovereignty_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(fede_tr_t16, federation_membership__sovereignty_reading, theater_ratio, 16, 0.27).
narrative_ontology:measurement(fede_tr_t24, federation_membership__sovereignty_reading, theater_ratio, 24, 0.31).
narrative_ontology:measurement(fede_tr_t32, federation_membership__sovereignty_reading, theater_ratio, 32, 0.35).
narrative_ontology:measurement(fede_tr_t40, federation_membership__sovereignty_reading, theater_ratio, 40, 0.38).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership__sovereignty_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(fede_be_t8, federation_membership__sovereignty_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(fede_be_t16, federation_membership__sovereignty_reading, base_extractiveness, 16, 0.61).
narrative_ontology:measurement(fede_be_t24, federation_membership__sovereignty_reading, base_extractiveness, 24, 0.68).
narrative_ontology:measurement(fede_be_t32, federation_membership__sovereignty_reading, base_extractiveness, 32, 0.74).
narrative_ontology:measurement(fede_be_t40, federation_membership__sovereignty_reading, base_extractiveness, 40, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership__sovereignty_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(fede_su_t8, federation_membership__sovereignty_reading, suppression_requirement, 8, 0.62).
narrative_ontology:measurement(fede_su_t16, federation_membership__sovereignty_reading, suppression_requirement, 16, 0.69).
narrative_ontology:measurement(fede_su_t24, federation_membership__sovereignty_reading, suppression_requirement, 24, 0.74).
narrative_ontology:measurement(fede_su_t32, federation_membership__sovereignty_reading, suppression_requirement, 32, 0.79).
narrative_ontology:measurement(fede_su_t40, federation_membership__sovereignty_reading, suppression_requirement, 40, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership__sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(federation_membership__sovereignty_reading, 0.12).
narrative_ontology:affects_constraint(federation_membership__sovereignty_reading, federation_membership__integration_reading).

% DUAL FORMULATION NOTE:
% This constraint and integration_reading form a kernel pair decomposing 'federation membership' into two structurally distinct claims with divergent ε. The sovereignty_reading has high ε from mobility restriction; the integration_reading has low ε for mobility rights but may have its own extraction from fiscal transfer rules. They are linked because the sovereignty_reading's border enforcement is the integration_reading's treaty violation — the same operational fact.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership__sovereignty_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
