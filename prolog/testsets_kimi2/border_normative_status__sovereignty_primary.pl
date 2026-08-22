% ============================================================================
% CONSTRAINT STORY: border_normative_status__sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_normative_status__sovereignty_primary, []).

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
 *   constraint_id: border_normative_status__sovereignty_primary
 *   human_readable: Sovereignty-Primary Reading of Territorial Border Legitimacy
 *   domain: political/international_law/migration
 *
 * SUMMARY:
 *   Territorial boundaries as legitimate instruments of collective
 *   self-determination represent the sovereignty_primary reading of the
 *   border_normative_status kernel. Under this reading, states possess
 *   foundational authority to exclude non-members, and border enforcement is
 *   a legitimate function of state apparatuses. The constraint coordinates a
 *   bounded political community (citizenry) around shared governance and
 *   resource allocation while asymmetrically extracting from excluded
 *   non-members through denial of entry, deportation, and family separation.
 *   This is a constructed normative framework rooted in Westphalian lineage,
 *   not a natural law; it requires active enforcement (border policing,
 *   detention, pushbacks) to persist. The claim/metric independence is
 *   maintained: the reading claims tangled_rope (coordination through
 *   self-determination) while the metrics acknowledge substantial extraction
 *   and suppression.
 *
 * KEY AGENTS:
 *   - state_apparatus: Primary agenda_setter (institutional/constrained) â administers border enforcement and collects legitimacy/authority from territorial control
 *   - member_citizenry: Primary beneficiary (organized/constrained) â receives collective self-determination goods, security, and welfare-state protections at the cost of bounded membership
 *   - excluded_migrants: Primary payer (powerless/trapped) â bears exclusion costs including denial of entry, precarity, and family separation
 *   - human_rights_institutions: Observer (institutional/analytical) â documents border violence and asserts competing human-rights frameworks
 *   - cosmopolitan_critics: Excluded voice (moderate/analytical) â argues for open borders and freedom of movement but is structurally marginalized in sovereignty-framing discourse
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_normative_status__sovereignty_primary, 0.68).
domain_priors:suppression_score(border_normative_status__sovereignty_primary, 0.72).
domain_priors:theater_ratio(border_normative_status__sovereignty_primary, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_normative_status__sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(border_normative_status__sovereignty_primary, "Sovereignty-Primary Reading of Territorial Border Legitimacy").
narrative_ontology:topic_domain(border_normative_status__sovereignty_primary, "political/international_law/migration").

domain_priors:requires_active_enforcement(border_normative_status__sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_normative_status__sovereignty_primary, 'ab9e4b96-c650-47fc-af06-0d05c24820bb').
narrative_ontology:cs_kernel_codification('ab9e4b96-c650-47fc-af06-0d05c24820bb', formalized).
narrative_ontology:cs_authority_grounding('ab9e4b96-c650-47fc-af06-0d05c24820bb', lineage).
narrative_ontology:cs_interpretation_layer_present('ab9e4b96-c650-47fc-af06-0d05c24820bb').
narrative_ontology:cs_reading_relation('ab9e4b96-c650-47fc-af06-0d05c24820bb', border_normative_status__freedom_primary, forecloses).
narrative_ontology:cs_reading_relation('ab9e4b96-c650-47fc-af06-0d05c24820bb', border_normative_status__qualified_sovereignty, influences).
narrative_ontology:cs_axiom('ab9e4b96-c650-47fc-af06-0d05c24820bb', foundational, collective_self_determination_entails_exclusion).
narrative_ontology:cs_axiom_status(collective_self_determination_entails_exclusion, holdable).
narrative_ontology:cs_axiom_grounding('ab9e4b96-c650-47fc-af06-0d05c24820bb', collective_self_determination_entails_exclusion, deontological).
narrative_ontology:cs_axiom('ab9e4b96-c650-47fc-af06-0d05c24820bb', foundational, state_foundational_authority_over_territory).
narrative_ontology:cs_axiom_status(state_foundational_authority_over_territory, holdable).
narrative_ontology:cs_axiom_grounding('ab9e4b96-c650-47fc-af06-0d05c24820bb', state_foundational_authority_over_territory, conventional).
narrative_ontology:cs_reference_frame('ab9e4b96-c650-47fc-af06-0d05c24820bb', westphalian_territorial_supremacy).
narrative_ontology:cs_drift_state('ab9e4b96-c650-47fc-af06-0d05c24820bb', contemporary_human_rights_regime, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ab9e4b96-c650-47fc-af06-0d05c24820bb', '').
narrative_ontology:cs_kernel_id(border_normative_status__sovereignty_primary, border_normative_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_normative_status__sovereignty_primary, member_citizenry).
narrative_ontology:constraint_beneficiary(border_normative_status__sovereignty_primary, state_apparatus).
narrative_ontology:constraint_victim(border_normative_status__sovereignty_primary, excluded_migrants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claims and exercises authority to admit or exclude persons from national territory. Operates border control agencies, detention facilities, and deportation programs. Derives legitimacy and territorial sovereignty from the exclusion function.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, state_apparatus, agenda_setter,
    institutional, generational, constrained, national).

% Receive public goods, democratic governance, and security framed as bounded to their territorial community. Their consent and tax contributions are tied to membership status that excludes non-citizens from equal access.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, member_citizenry, beneficiary,
    organized, generational, constrained, national).

% Seek entry or residence but are denied visas, stopped at borders, detained, or deported. Bear costs of family separation, precarious status, and blocked economic opportunity. Have no recourse against the state's exclusion decision under this normative framework.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, excluded_migrants, payer,
    powerless, biographical, trapped, regional).

% Monitor border practices and publish reports on detention conditions, pushbacks, and refugee rights. Lack enforcement power but provide an alternative normative vocabulary that contests the sovereignty-primary framing.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, human_rights_institutions, observer,
    institutional, civilizational, analytical, global).

% Academic and advocacy voices arguing that freedom of movement is a fundamental right and that territorial borders violate distributive justice. Their arguments are systematically marginalized in domestic policy discourse and treated as illegitimate interference.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, cosmopolitan_critics, excluded,
    moderate, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_normative_status__sovereignty_primary, member_citizenry).
narrative_ontology:fixing_cost_class(border_normative_status__sovereignty_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables collective self-determination by delimiting a political community with authority over its territory, allowing democratic governance and shared institutions to function within bounded jurisdictions.
% TRANSFER_FUNCTION: Transfers exclusion costs to non-members (denied entry, deportation, family separation, precarity) while preserving territorial control and associated goods (security, public services, cultural continuity) for members.
% ABSENT_VOICES: Cosmopolitan critics and excluded migrants themselves are structurally absent from the sovereignty-framing; their objections are treated as illegitimate interference rather than valid claims.
% DISAPPEARANCE_RATIONALE: If territorial boundaries lost normative legitimacy as instruments of self-determination, the current state system would reorganize: open movement would challenge welfare-state architectures, labor markets would reconfigure, and the foundational premise of international law (sovereign equality) would require reconstruction.
% FOUNDING_PROBLEM: How to secure collective self-determination and prevent external interference in a political community's governance without subordinating all peoples to a single empire or subjecting members to constant disruption from uncontrolled in-migration.
% FOUNDING_PROBLEM_CORROBORATION: Classical international-law theorists (Vattel, Westphalian tradition) attest the founding problem from outside the immediate beneficiary set of modern citizenries; cosmopolitan critics and postcolonial scholars contest that the problem was ever legitimately solved by territorial exclusion, arguing the arrangement was imposed through colonial violence and persists by structural inertia rather than consent.
narrative_ontology:disappearance_verdict(border_normative_status__sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_normative_status__sovereignty_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_normative_status__sovereignty_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(border_normative_status__sovereignty_primary, 'none', 1).
narrative_ontology:epsilon_provenance(border_normative_status__sovereignty_primary, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_normative_status__sovereignty_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_normative_status__sovereignty_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_normative_status__sovereignty_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.68) is substantial because the constraint systematically denies goods of membership and movement to excluded migrants. Suppression (0.72) is higher than extraction because the arrangement depends on active enforcement â border policing, detention, and pushbacks â to prevent movement that would otherwise occur. Theater_ratio (0.45) is moderate: a significant share of enforcement activity is performative deterrence (visible walls, patrols, publicized raids) exceeding the functional minimum of territorial control. Accessibility_collapse (0.70) is high because, within this normative framework, open-border alternatives are rendered unthinkable in mainstream policy discourse. Resistance (0.55) reflects sustained but institutionally weak opposition from migrants and human rights advocates. The measurement series run on one shared time grid so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The state_apparatus seat experiences the constraint as necessary coordination for governance; the excluded_migrant seat experiences it as active extraction backed by force. The engine computes this divergence from the structural asymmetry in power (institutional vs powerless) and exit options (constrained vs trapped). The member_citizenry sits near the beneficiary end, receiving subsidized self-determination, while the excluded_migrant sits near the full-target end, bearing amplified effective extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Member citizenry and state apparatus are structural beneficiaries: the constraint subsidizes their collective self-determination and territorial control (low d, damped effective extraction). Excluded migrants are the structural targets: the constraint extracts directly from them through physical and legal exclusion (high d, amplified effective extraction). The divergence is stark â the same border infrastructure appears as legitimate community boundary to the beneficiary seats and as violent exclusion to the payer seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope (rather than snare) depends on the presence of a genuine coordination function: bounded jurisdictions do enable democratic governance and public-goods provision that would be harder without membership rules. However, the classification also requires asymmetric extraction, which is present in the victim set of excluded migrants. If the coordination story were pure cover (no genuine self-determination function, only ethnic or economic closure), the metrics would shift toward snare. The authored theater_ratio (0.45) captures partial performativity in enforcement â deterrence displays that exceed strict functional necessity â without collapsing into pure piton because the extraction remains substantively functional.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_sovereignty_primary,
    'This constraint instantiates the sovereignty_primary reading of the border_normative_status kernel. How would classification change if the freedom_primary reading were adopted instead?',
    'Comparison of the sibling constraint story for freedom_primary, which reverses the beneficiary/victim directionality and treats border enforcement as the extraction mechanism.',
    'Under freedom_primary, the same physical borders would compute as a snare or tangled_rope with citizenry as payers (denied free association/movement benefits) and migrants as beneficiaries of liberated movement; the structural type may flip depending on enforcement and suppression levels.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_sovereignty_primary, conceptual, 'Kernel reading location and sibling structural delta for sovereignty_primary').

omega_variable(
    founding_problem_colonial_continuity,
    'Is the Westphalian border system a legitimate solution to the problem of collective self-determination, or a colonial inheritance that constructs the external other to consolidate state power?',
    'Historical genealogy of specific border regimes: postcolonial states inherit arbitrary colonial borders while asserting sovereignty against migrant mobility, suggesting the founding problem was solved through violence rather than consent.',
    'If the founding problem is itself a colonial construct, the constraint''s coordination story collapses and the classification shifts toward snare; if genuinely consensual, tangled_rope holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_colonial_continuity, conceptual, 'Colonial continuity ambiguity in the founding problem').

omega_variable(
    border_violence_legitimacy,
    'Does the active enforcement of borders (detention, pushbacks, family separation) constitute legitimate state violence or extractive suppression?',
    'Legal-empirical audit of border enforcement practices against human rights standards; if enforcement is systematically disproportionate to the self-determination interest, suppression is extractive rather than protective.',
    'If enforcement is disproportionate, suppression metric should rise and the coordination function diminishes, pushing classification toward snare; if proportionate, the tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(border_violence_legitimacy, empirical, 'Legitimacy ambiguity of border enforcement violence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_normative_status__sovereignty_primary, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bn_ssp_tr_t0, border_normative_status__sovereignty_primary, theater_ratio, 0, 0.2).
narrative_ontology:measurement(bn_ssp_tr_t10, border_normative_status__sovereignty_primary, theater_ratio, 10, 0.25).
narrative_ontology:measurement(bn_ssp_tr_t20, border_normative_status__sovereignty_primary, theater_ratio, 20, 0.32).
narrative_ontology:measurement(bn_ssp_tr_t30, border_normative_status__sovereignty_primary, theater_ratio, 30, 0.38).
narrative_ontology:measurement(bn_ssp_tr_t40, border_normative_status__sovereignty_primary, theater_ratio, 40, 0.42).
narrative_ontology:measurement(bn_ssp_tr_t50, border_normative_status__sovereignty_primary, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(bn_ssp_be_t0, border_normative_status__sovereignty_primary, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(bn_ssp_be_t10, border_normative_status__sovereignty_primary, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(bn_ssp_be_t20, border_normative_status__sovereignty_primary, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(bn_ssp_be_t30, border_normative_status__sovereignty_primary, base_extractiveness, 30, 0.64).
narrative_ontology:measurement(bn_ssp_be_t40, border_normative_status__sovereignty_primary, base_extractiveness, 40, 0.66).
narrative_ontology:measurement(bn_ssp_be_t50, border_normative_status__sovereignty_primary, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(bn_ssp_su_t0, border_normative_status__sovereignty_primary, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(bn_ssp_su_t10, border_normative_status__sovereignty_primary, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(bn_ssp_su_t20, border_normative_status__sovereignty_primary, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(bn_ssp_su_t30, border_normative_status__sovereignty_primary, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(bn_ssp_su_t40, border_normative_status__sovereignty_primary, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(bn_ssp_su_t50, border_normative_status__sovereignty_primary, suppression_requirement, 50, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


% DUAL FORMULATION NOTE:
% This constraint is one member of the border_normative_status kernel family, alongside freedom_primary and qualified_sovereignty. Decomposition follows the epsilon-invariance principle: each reading carries a distinct epsilon, beneficiary/victim structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
