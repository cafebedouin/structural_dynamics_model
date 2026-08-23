% ============================================================================
% CONSTRAINT STORY: sovereign_legitimacy__republican_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: sovereign_legitimacy__republican_reading
 *   human_readable: Republican Popular Sovereignty Legitimacy Constraint
 *   domain: political/philosophical/constitutional
 *
 * SUMMARY:
 *   This constraint story models the republican reading of sovereign
 *   legitimacy: authority flows upward from the people through delegated
 *   consent, grounded in popular sovereignty and social contract. The
 *   constraint operates through electoral cycles, constitutional adherence,
 *   and participatory mechanisms that validate legitimacy on an ongoing
 *   basis. It coordinates collective governance (benefiting the enfranchised
 *   citizenry and participatory infrastructure) while extracting compliance
 *   from excluded populations who bear obligations without accountability
 *   channels. The constraint requires active enforcement (election
 *   administration, constitutional courts, rights enforcement) and has
 *   persisted for ~250 years in modern form, with extractiveness declining as
 *   franchise expanded but theater ratio rising in late-stage mass democracy
 *   where performative participation substitutes for substantive influence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereign_legitimacy__republican_reading, 0.45).
domain_priors:suppression_score(sovereign_legitimacy__republican_reading, 0.4).
domain_priors:theater_ratio(sovereign_legitimacy__republican_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereign_legitimacy__republican_reading, tangled_rope).
narrative_ontology:human_readable(sovereign_legitimacy__republican_reading, "Republican Popular Sovereignty Legitimacy Constraint").
narrative_ontology:topic_domain(sovereign_legitimacy__republican_reading, "political/philosophical/constitutional").

domain_priors:requires_active_enforcement(sovereign_legitimacy__republican_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sovereign_legitimacy__republican_reading, '34af6921-a99e-4911-95d7-1c791027e2e0').
narrative_ontology:cs_kernel_codification('34af6921-a99e-4911-95d7-1c791027e2e0', formalized).
narrative_ontology:cs_authority_grounding('34af6921-a99e-4911-95d7-1c791027e2e0', lineage).
narrative_ontology:cs_interpretation_layer_present('34af6921-a99e-4911-95d7-1c791027e2e0').
narrative_ontology:cs_reading_relation('34af6921-a99e-4911-95d7-1c791027e2e0', sovereign_legitimacy__monarchical_reading, forecloses).
narrative_ontology:cs_reading_relation('34af6921-a99e-4911-95d7-1c791027e2e0', sovereign_legitimacy__constitutional_hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('34af6921-a99e-4911-95d7-1c791027e2e0', foundational, popular_sovereignty_grounds_authority).
narrative_ontology:cs_axiom_status(popular_sovereignty_grounds_authority, holdable).
narrative_ontology:cs_axiom_grounding('34af6921-a99e-4911-95d7-1c791027e2e0', popular_sovereignty_grounds_authority, deontological).
narrative_ontology:cs_axiom('34af6921-a99e-4911-95d7-1c791027e2e0', foundational, delegated_consent_requires_accountability).
narrative_ontology:cs_axiom_status(delegated_consent_requires_accountability, holdable).
narrative_ontology:cs_axiom_grounding('34af6921-a99e-4911-95d7-1c791027e2e0', delegated_consent_requires_accountability, deontological).
narrative_ontology:cs_axiom('34af6921-a99e-4911-95d7-1c791027e2e0', secondary, electoral_cycles_validate_legitimacy).
narrative_ontology:cs_axiom_status(electoral_cycles_validate_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('34af6921-a99e-4911-95d7-1c791027e2e0', electoral_cycles_validate_legitimacy, conventional).
narrative_ontology:cs_reference_frame('34af6921-a99e-4911-95d7-1c791027e2e0', founding_constitutional_moment).
narrative_ontology:cs_drift_state('34af6921-a99e-4911-95d7-1c791027e2e0', contemporary_democratic_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('34af6921-a99e-4911-95d7-1c791027e2e0', '').
narrative_ontology:cs_kernel_id(sovereign_legitimacy__republican_reading, sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__republican_reading, citizenry_with_voting_rights).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__republican_reading, participatory_mechanisms_beneficiaries).
narrative_ontology:constraint_victim(sovereign_legitimacy__republican_reading, franchise_excluded_populations).
narrative_ontology:constraint_victim(sovereign_legitimacy__republican_reading, consent_mechanism_excluded).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(sovereign_legitimacy__republican_reading, participatory_mechanisms_beneficiaries).
narrative_ontology:constraint_vindicates(sovereign_legitimacy__republican_reading, popular_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(sovereign_legitimacy__republican_reading, social_contract_theory).
narrative_ontology:constraint_vindicates(sovereign_legitimacy__republican_reading, delegated_consent_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold formal voting rights and participatory mechanisms (elections, referenda, civic organizations). Receive legitimate governance, rights protection, and public goods in exchange for compliance and taxation. Can exit through emigration or political disengagement, though high switching costs apply.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, citizenry_with_voting_rights, beneficiary,
    powerful, biographical, mobile, national).

% Subject to state authority (laws, taxation, policing) but denied formal voting rights — non-citizen residents, felons in disenfranchising jurisdictions, stateless persons, minors. Bear obligations without the accountability mechanism of electoral removal. Exit is structurally blocked by the same exclusion that defines their position.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, franchise_excluded_populations, payer,
    powerless, generational, trapped, national).

% Civil society organizations, political parties, labor unions, advocacy groups that operate within and benefit from the participatory infrastructure. They gain influence and legitimacy-channeling capacity but must comply with registration, transparency, and electoral laws that constrain their autonomy.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, participatory_mechanisms_beneficiaries, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__republican_reading, participatory_mechanisms_beneficiaries, payer).

% Citizens who formally hold rights but face practical barriers to effective participation — gerrymandered districts, voter suppression tactics, economic coercion, information asymmetries. They pay compliance costs (taxation, regulation) while their consent is manufactured or diluted. Exit requires collective action they are structurally impeded from mounting.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, consent_mechanism_excluded, payer,
    moderate, biographical, constrained, national).

% Hold delegated authority subject to electoral recall. They set legislative agendas, control state apparatus, and benefit from incumbency advantages. Their power derives from the legitimacy framework they administer; they are constrained by constitutional limits, electoral cycles, and institutional checks. Exit means returning to private life or losing re-election.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, elected_officials, agenda_setter,
    institutional, biographical, constrained, national).

% Adjudicate the boundary conditions of popular sovereignty — voting rights, districting, campaign finance, emergency powers. They do not collect rents from the constraint nor bear its extraction directly; they interpret the social contract's operational terms. Their analytical seat is institutionalized, not individual.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% Produce the normative vocabulary (consent, representation, legitimacy, sovereignty) that the constraint's operators and critics both deploy. They neither govern nor are governed by the constraint in the first instance; they map its conceptual architecture. Their exit is trivial — they can change frameworks without material cost.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, political_philosophers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of legitimate collective decision-making by providing a mechanism (delegated consent through elections) that converts plural preferences into authorized government action, generating compliance without continuous coercion.
% TRANSFER_FUNCTION: Moves political obligation (taxation, law-abidingness, military service) from citizenry to state, and moves accountability (removal power, policy responsiveness) from state to citizenry through electoral cycles. The excluded populations transfer obligation without receiving accountability.
% ABSENT_VOICES: Permanently excluded groups — non-citizen residents subject to full legal jurisdiction, stateless persons, disenfranchised felons, minors — would object to obligation without representation. They are structurally absent because the franchise boundary defines the constraint's constituency; their inclusion would alter the constraint's identity.
% DISAPPEARANCE_RATIONALE: If republican legitimacy vanished overnight, governance would reorganize around alternative legitimacy sources: technocratic expertise, traditional authority, charismatic leadership, or hybrid constitutional models. The entire architecture of representation, rights-adjudication, and peaceful transfer of power depends on this constraint's continued operation.
% FOUNDING_PROBLEM: The problem of legitimate authority after the collapse of divine-right monarchy — how to ground political obligation in human artifice rather than transcendent sanction, and how to bind governors to the governed without resort to force alone.
% FOUNDING_PROBLEM_CORROBORATION: Locke, Rousseau, Kant, and Rawls attest the founding problem from within the tradition. Critics from Hobbes (consent as fiction), Schmitt (sovereignty as exception), and contemporary democratic realists (Achen & Bartels, Brennan) dispute whether consent ever grounded actual authority or whether the constraint masks elite domination. No consensus exists outside the benefiting parties.
narrative_ontology:disappearance_verdict(sovereign_legitimacy__republican_reading, world_rearranges).
narrative_ontology:founding_problem_status(sovereign_legitimacy__republican_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sovereign_legitimacy__republican_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sovereign_legitimacy__republican_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sovereign_legitimacy__republican_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereign_legitimacy__republican_reading_tests).
:- end_tests(sovereign_legitimacy__republican_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is moderate: the constraint extracts compliance and resources from all subjected persons but returns governance goods to the enfranchised. Suppression (0.40) reflects the franchise boundary — not total exclusion but structurally maintained. Theater ratio (0.25) captures the gap between ritualized voting and actual policy responsiveness, rising recently as polarization and gerrymandering decouple electoral outcomes from accountability. Accessibility collapse (0.50) and resistance (0.40) are moderate: alternatives (monarchy, technocracy, sortition) exist conceptually but face high institutional switching costs; resistance from excluded groups is persistent but fragmented.
 *
 * PERSPECTIVAL GAP:
 *   The enfranchised citizenry experiences this as rope (genuine coordination with net benefit). The franchise-excluded experience it as snare (obligation without accountability). The consent-mechanism-excluded experience it as tangled rope (some participation, diluted influence). Elected officials experience it as scaffold (transitional mandate requiring constant renewal). Constitutional courts experience it as mountain (the constraint appears as fixed constitutional architecture). The engine computes these divergences from the structural data; the claimed_type (tangled_rope) reflects the system-level hybridity.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: citizenry_with_voting_rights (d ~ 0.15, mobile exit, net subsidy from governance goods) and participatory_mechanisms_beneficiaries (d ~ 0.30, constrained exit, institutional rents). Victims: franchise_excluded_populations (d ~ 0.95, trapped, pure extraction) and consent_mechanism_excluded (d ~ 0.70, constrained exit, asymmetric extraction). Agenda_setters: elected_officials (d ~ 0.40, constrained by electoral cycle, both extract and are extracted from). Observers sit at d ~ 0.50 (analytical symmetry). The engine derives these from roles + exit_options + power.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint was built to solve the post-monarchical legitimacy vacuum (founding_problem: live). It prevents mislabeling by openly declaring both coordination (legitimate governance, peaceful transfer) and extraction (excluded populations, diluted consent). The mandatrophy risk is that franchise expansion historically reduced extraction — but late-stage theater rise and consent-mechanism erosion may reverse this. The constraint resolves mandatrophy only if exclusion boundaries are treated as bugs, not features.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Does the republican reading''s core premise (all legitimate authority originates in popular consent) logically foreclose the constitutional hybrid reading, or do they occupy compatible positions in a broader legitimacy space?',
    'Comparative constitutional analysis: if hybrid systems (e.g., UK, Japan, Sweden) functionally instantiate republican mechanisms while retaining monarchical symbols without contradiction, the readings coexist; if the symbolic inheritance actively constrains democratic accountability, foreclosure obtains.',
    'If forecloses, the kernel admits only one coherent reading per polity; if coexists_with, the kernel supports stable pluralism. Affects whether the engine treats them as competing constraints or complementary facets.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Logical relationship between republican and hybrid legitimacy readings.').

omega_variable(
    extraction_coordination_boundary,
    'Is the moderate extractiveness (0.45) the necessary cost of the coordination function (legitimate governance), or does it contain separable rent extraction by elected officials and participatory intermediaries?',
    'Counterfactual institutional design: sortition-based citizen assemblies, liquid democracy, or algorithmic policy aggregation — if these achieve equivalent coordination with lower extraction, the delta is rent.',
    'If separable, the constraint is more snare-like than claimed; if inseparable, the extraction is the price of the coordination and the tangled_rope classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_coordination_boundary, empirical, 'Whether coordination and extraction components are structurally separable in republican legitimacy.').

omega_variable(
    majoritarian_tyranny_vulnerability,
    'Does the constraint''s validation mechanism (electoral cycles) structurally generate majoritarian tyranny against the franchise_excluded, or is tyranny a contingent failure of institutional design (courts, federalism, rights charters)?',
    'Longitudinal study of rights-protection regimes: if tyranny correlates with weak counter-majoritarian institutions across cases, it is contingent; if it persists even with strong courts/federalism, it is structural to the majoritarian validation logic.',
    'If structural, the constraint carries an inherent snare component for minorities that no reform within the republican frame can resolve — requiring hybrid or consociational supplementation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majoritarian_tyranny_vulnerability, empirical, 'Whether majoritarian tyranny is a structural feature or contingent bug of electoral validation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereign_legitimacy__republican_reading, 0, 250).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(slr_tr_t0, sovereign_legitimacy__republican_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(slr_tr_t50, sovereign_legitimacy__republican_reading, theater_ratio, 50, 0.35).
narrative_ontology:measurement(slr_tr_t100, sovereign_legitimacy__republican_reading, theater_ratio, 100, 0.3).
narrative_ontology:measurement(slr_tr_t150, sovereign_legitimacy__republican_reading, theater_ratio, 150, 0.25).
narrative_ontology:measurement(slr_tr_t200, sovereign_legitimacy__republican_reading, theater_ratio, 200, 0.22).
narrative_ontology:measurement(slr_tr_t250, sovereign_legitimacy__republican_reading, theater_ratio, 250, 0.25).

% Extraction over time
narrative_ontology:measurement(slr_be_t0, sovereign_legitimacy__republican_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(slr_be_t50, sovereign_legitimacy__republican_reading, base_extractiveness, 50, 0.55).
narrative_ontology:measurement(slr_be_t100, sovereign_legitimacy__republican_reading, base_extractiveness, 100, 0.48).
narrative_ontology:measurement(slr_be_t150, sovereign_legitimacy__republican_reading, base_extractiveness, 150, 0.45).
narrative_ontology:measurement(slr_be_t200, sovereign_legitimacy__republican_reading, base_extractiveness, 200, 0.42).
narrative_ontology:measurement(slr_be_t250, sovereign_legitimacy__republican_reading, base_extractiveness, 250, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(slr_su_t0, sovereign_legitimacy__republican_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(slr_su_t50, sovereign_legitimacy__republican_reading, suppression_requirement, 50, 0.5).
narrative_ontology:measurement(slr_su_t100, sovereign_legitimacy__republican_reading, suppression_requirement, 100, 0.45).
narrative_ontology:measurement(slr_su_t150, sovereign_legitimacy__republican_reading, suppression_requirement, 150, 0.4).
narrative_ontology:measurement(slr_su_t200, sovereign_legitimacy__republican_reading, suppression_requirement, 200, 0.38).
narrative_ontology:measurement(slr_su_t250, sovereign_legitimacy__republican_reading, suppression_requirement, 250, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereign_legitimacy__republican_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(sovereign_legitimacy__republican_reading, 0.12).
narrative_ontology:affects_constraint(sovereign_legitimacy__republican_reading, sovereign_legitimacy__monarchical_reading).
narrative_ontology:affects_constraint(sovereign_legitimacy__republican_reading, sovereign_legitimacy__constitutional_hybrid_reading).

% DUAL FORMULATION NOTE:
% The sovereign_legitimacy kernel decomposes into three readings with distinct ε values and beneficiary/victim structures. republican_reading (this story): moderate ε, enfranchised citizenry as beneficiaries, excluded populations as victims. monarchical_reading: low ε (tradition as coordination), monarch/aristocracy as beneficiaries, commoners as victims. constitutional_hybrid_reading: low-moderate ε, dual beneficiary structure (ceremonial monarch + elected government), victims vary by constitutional design. The readings are linked because the hybrid reading structurally incorporates republican mechanisms while retaining monarchical symbols.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sovereign_legitimacy__republican_reading, institutional, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
