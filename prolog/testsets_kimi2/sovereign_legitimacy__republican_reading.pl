% ============================================================================
% CONSTRAINT STORY: sovereign_legitimacy__republican_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sovereign_legitimacy__republican_reading, []).

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
 *   constraint_id: sovereign_legitimacy__republican_reading
 *   human_readable: Republican Legitimacy: Popular Sovereignty and Delegated Consent
 *   domain: political/constitutional
 *
 * SUMMARY:
 *   This constraint instantiates the republican reading of the
 *   sovereign_legitimacy kernel: legitimate authority flows upward from the
 *   people through delegated consent, grounded in popular sovereignty and
 *   social contract. It is one of three readings (alongside monarchical and
 *   constitutional_hybrid) of the contested kernel regarding the source of
 *   political legitimacy. The republican reading coordinates self-governance
 *   for the enfranchised while asymmetrically extracting compliance from the
 *   disenfranchised, who are subject to state power without voting rights or
 *   effective consent mechanisms.
 *
 * KEY AGENTS:
 *   - Enfranchised citizenry (organized/constrained): Primary beneficiary â holds voting rights and delegates authority through electoral cycles.
 *   - Disenfranchised population (powerless/trapped): Primary target â bears state coercion and majority-imposed rules without franchise.
 *   - Elected representatives (powerful/mobile): Agenda setter â administers delegated authority and maintains electoral validation machinery.
 *   - Constitutional judiciary (institutional/analytical): Observer â adjudicates legitimacy boundaries without direct cost or benefit.
 *   - Monarchist faction (moderate/constrained): Excluded voice â advances delegitimized inherited-authority claims.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereign_legitimacy__republican_reading, 0.48).
domain_priors:suppression_score(sovereign_legitimacy__republican_reading, 0.55).
domain_priors:theater_ratio(sovereign_legitimacy__republican_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereign_legitimacy__republican_reading, tangled_rope).
narrative_ontology:human_readable(sovereign_legitimacy__republican_reading, "Republican Legitimacy: Popular Sovereignty and Delegated Consent").
narrative_ontology:topic_domain(sovereign_legitimacy__republican_reading, "political/constitutional").

domain_priors:requires_active_enforcement(sovereign_legitimacy__republican_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sovereign_legitimacy__republican_reading, '8c8913b9-4c34-410f-a909-86ea4bb4b8c7').
narrative_ontology:cs_kernel_codification('8c8913b9-4c34-410f-a909-86ea4bb4b8c7', fixed_text).
narrative_ontology:cs_authority_grounding('8c8913b9-4c34-410f-a909-86ea4bb4b8c7', lineage).
narrative_ontology:cs_interpretation_layer_present('8c8913b9-4c34-410f-a909-86ea4bb4b8c7').
narrative_ontology:cs_reading_relation('8c8913b9-4c34-410f-a909-86ea4bb4b8c7', sovereign_legitimacy__monarchical_reading, forecloses).
narrative_ontology:cs_reading_relation('8c8913b9-4c34-410f-a909-86ea4bb4b8c7', sovereign_legitimacy__constitutional_hybrid_reading, influences).
narrative_ontology:cs_axiom('8c8913b9-4c34-410f-a909-86ea4bb4b8c7', foundational, authority_derives_from_consent).
narrative_ontology:cs_axiom_status(authority_derives_from_consent, holdable).
narrative_ontology:cs_axiom_grounding('8c8913b9-4c34-410f-a909-86ea4bb4b8c7', authority_derives_from_consent, deontological).
narrative_ontology:cs_axiom('8c8913b9-4c34-410f-a909-86ea4bb4b8c7', secondary, electoral_validation_required).
narrative_ontology:cs_axiom_status(electoral_validation_required, holdable).
narrative_ontology:cs_axiom_grounding('8c8913b9-4c34-410f-a909-86ea4bb4b8c7', electoral_validation_required, conventional).
narrative_ontology:cs_reference_frame('8c8913b9-4c34-410f-a909-86ea4bb4b8c7', consent_based_legitimacy).
narrative_ontology:cs_drift_state('8c8913b9-4c34-410f-a909-86ea4bb4b8c7', contemporary_democratic_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8c8913b9-4c34-410f-a909-86ea4bb4b8c7', '').
narrative_ontology:cs_kernel_id(sovereign_legitimacy__republican_reading, sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__republican_reading, enfranchised_citizenry).
narrative_ontology:constraint_victim(sovereign_legitimacy__republican_reading, disenfranchised_population).
narrative_ontology:constraint_vindicates(sovereign_legitimacy__republican_reading, popular_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(sovereign_legitimacy__republican_reading, social_contract_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold voting rights and participatory mechanisms through which they delegate authority to representatives. Benefit from policy alignment with majority preferences and from the structural exclusion of non-citizens from decision-making. Exit is constrained by territoriality, economic cost of emigration, and lack of comparable political frameworks elsewhere.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, enfranchised_citizenry, beneficiary,
    organized, generational, constrained, national).

% Subject to laws, taxation, and state coercion without voting rights or effective consent mechanisms. Includes non-citizens, excluded residents, and others outside the franchise who bear the costs of majority decisions without reciprocal influence. Exit is often blocked by legal status, economic dependency, or geographic barriers.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, disenfranchised_population, payer,
    powerless, biographical, trapped, national).

% Exercise delegated coercive authority claimed to flow upward from the people. Derive legitimacy from electoral validation and constitutional adherence. Can modify franchise boundaries and policy but are structurally incentivized to maintain the consent mechanism that sustains their position.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, elected_representatives, agenda_setter,
    powerful, biographical, mobile, national).

% Adjudicate disputes over the boundaries of delegated authority and constitutional adherence. Do not directly collect benefits or pay costs of the constraint, but interpret the rules that determine whose consent counts and whether authority remains within the republican framework.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, constitutional_judiciary, observer,
    institutional, generational, analytical, national).

% Advance claims for inherited or divine authority that are structurally delegitimized under the republican reading. Excluded from the dominant legitimacy discourse but retain cultural or symbolic presence. Cannot advance their claims within the operative framework without rejecting its foundational premise.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, monarchist_faction, excluded,
    moderate, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aggregates individual preferences into collective decisions through delegated representatives, solving the coordination problem of who may legitimately exercise coercive power over a population without resorting to inherited status or persistent violence.
% TRANSFER_FUNCTION: Transfers political authority and the power to make binding law from the populace to elected representatives, while transferring compliance burdens, taxation, and subjection from the disenfranchised to the state apparatus without reciprocal consent.
% ABSENT_VOICES: Disenfranchised residents, non-citizen subjects, and monarchist or traditionalist factions who reject popular sovereignty as a grounding for authority are structurally absent from the consent mechanism and from the legitimacy discourse that frames governance.
% DISAPPEARANCE_RATIONALE: If republican legitimacy vanished overnight, the authority of elected representatives would collapse; governance would revert to force, traditional hierarchy, or competing legitimacy claims, and the enfranchised would lose their privileged position as the nominal source of authority.
% FOUNDING_PROBLEM: How to ground coercive political authority in the consent of the governed rather than inherited divine right or arbitrary force, while enabling effective collective action and preventing tyranny.
% FOUNDING_PROBLEM_CORROBORATION: Critical theorists and postcolonial scholars outside the benefiting enfranchised majority attest that arbitrary rule persists for excluded populations; monarchical and hybrid readings attest the problem was solved through alternative grounding mechanisms. No corroboration exclusively from within the beneficiary set supports the claim that the founding problem is fully resolved.
narrative_ontology:disappearance_verdict(sovereign_legitimacy__republican_reading, world_rearranges).
narrative_ontology:founding_problem_status(sovereign_legitimacy__republican_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sovereign_legitimacy__republican_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sovereign_legitimacy__republican_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sovereign_legitimacy__republican_reading, 0.48, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereign_legitimacy__republican_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sovereign_legitimacy__republican_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sovereign_legitimacy__republican_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48) because the constraint genuinely coordinates collective self-governance for the enfranchised majority while systematically excluding a minority from the consent mechanism, creating asymmetric extraction. Suppression (0.55) reflects the active enforcement required to maintain franchise boundaries and the suppression of alternative legitimacy claims such as monarchical or divine right. Theater ratio (0.25) captures the partially performative nature of electoral rituals that validate authority. Accessibility collapse (0.60) indicates that while alternatives like monarchy or direct democracy are conceptually available, they are structurally marginalized within the operative framework. Resistance (0.50) reflects ongoing contestation from excluded groups and rival legitimacy frameworks.
 *
 * PERSPECTIVAL GAP:
 *   The enfranchised citizenry experiences this constraint as coordination â a mechanism for collective self-rule and protection against arbitrary authority. The disenfranchised population experiences the same structure as extraction â coercive power exercised over them without their consent. The engine computes this divergence from the identical structural data via directionality: the enfranchised are beneficiaries (low d) while the disenfranchised are victims (high d).
 *
 * DIRECTIONALITY LOGIC:
 *   The enfranchised citizenry is the declared beneficiary (low d, subsidized by the constraint's majoritarian bias and policy alignment). The disenfranchised population is the declared victim (high d, effective extraction amplified by their powerlessness and trapped exit). Elected representatives sit near symmetric: they are agents of the beneficiary class but also structurally constrained by the need for recurrent electoral validation. The constitutional judiciary is analytical (d near 0.5, observing without direct cost or benefit).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling by requiring both a genuine coordination function (solving the problem of legitimate authority for the enfranchised through delegated consent) and identifiable victims (the disenfranchised, who bear state coercion without consent). Without the victim set, it would be a rope; without the coordination function, it would be a snare. The presence of active enforcement (electoral machinery, franchise policing, constitutional maintenance) confirms the hybrid tangled_rope classification rather than a naturalized mountain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    franchise_exclusion_structural_necessity,
    'Is the exclusion of certain populations from the franchise a contingent historical feature of republican governance, or a structural necessity for maintaining delegated authority?',
    'Comparative analysis of republican regimes with varying franchise boundaries; historical tracing of whether universal inclusion dissolves or sustains the coordination function.',
    'If exclusion is structural, the constraint''s extractiveness is inherent and the classification remains tangled_rope; if contingent, extraction is a removable defect and the constraint tends toward rope as franchise expands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(franchise_exclusion_structural_necessity, conceptual, 'Whether disenfranchisement is inherent or contingent to republican legitimacy.').

omega_variable(
    majoritarian_tyranny_victimhood,
    'Does majoritarian rule under republican legitimacy structurally require a disenfranchised or marginalized class to function, or can universal enfranchisement eliminate the victim set?',
    'Empirical study of democratic regimes approaching universal franchise to test whether new victim configurations emerge or extraction dissipates.',
    'If new victims always emerge, the constraint is structurally tangled_rope; if extraction can dissipate, the reading may transition toward rope classification over time.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(majoritarian_tyranny_victimhood, empirical, 'Whether majoritarian systems inherently generate excluded victim classes.').

omega_variable(
    republican_monarchical_foreclosure,
    'Does the republican reading''s core premise logically foreclose the monarchical reading within any single legitimacy framework, or do they merely coexist as live options held by different political factions?',
    'Conceptual analysis of whether a single framework can simultaneously hold that authority derives exclusively from popular consent AND from inherited divine right.',
    'If foreclosure is genuine, the sibling relation should compute as forecloses; if not, the relation is coexists_with, altering the kernel''s structural topology.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(republican_monarchical_foreclosure, conceptual, 'Logical relationship between republican and monarchical readings of legitimacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereign_legitimacy__republican_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sove_tr_t0, sovereign_legitimacy__republican_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(sove_tr_t20, sovereign_legitimacy__republican_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(sove_tr_t40, sovereign_legitimacy__republican_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(sove_tr_t60, sovereign_legitimacy__republican_reading, theater_ratio, 60, 0.22).
narrative_ontology:measurement(sove_tr_t80, sovereign_legitimacy__republican_reading, theater_ratio, 80, 0.24).
narrative_ontology:measurement(sove_tr_t100, sovereign_legitimacy__republican_reading, theater_ratio, 100, 0.25).

% Extraction over time
narrative_ontology:measurement(sove_be_t0, sovereign_legitimacy__republican_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(sove_be_t20, sovereign_legitimacy__republican_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(sove_be_t40, sovereign_legitimacy__republican_reading, base_extractiveness, 40, 0.44).
narrative_ontology:measurement(sove_be_t60, sovereign_legitimacy__republican_reading, base_extractiveness, 60, 0.45).
narrative_ontology:measurement(sove_be_t80, sovereign_legitimacy__republican_reading, base_extractiveness, 80, 0.47).
narrative_ontology:measurement(sove_be_t100, sovereign_legitimacy__republican_reading, base_extractiveness, 100, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(sove_su_t0, sovereign_legitimacy__republican_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(sove_su_t20, sovereign_legitimacy__republican_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(sove_su_t40, sovereign_legitimacy__republican_reading, suppression_requirement, 40, 0.52).
narrative_ontology:measurement(sove_su_t60, sovereign_legitimacy__republican_reading, suppression_requirement, 60, 0.53).
narrative_ontology:measurement(sove_su_t80, sovereign_legitimacy__republican_reading, suppression_requirement, 80, 0.54).
narrative_ontology:measurement(sove_su_t100, sovereign_legitimacy__republican_reading, suppression_requirement, 100, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(sovereign_legitimacy__republican_reading, sovereign_legitimacy__monarchical_reading).
narrative_ontology:affects_constraint(sovereign_legitimacy__republican_reading, sovereign_legitimacy__constitutional_hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is the republican reading of the sovereign_legitimacy kernel, decomposed from the monarchical and constitutional_hybrid readings per the epsilon-invariance principle. Each reading represents a structurally distinct claim about the source of political legitimacy with different epsilon values and stakeholder configurations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
