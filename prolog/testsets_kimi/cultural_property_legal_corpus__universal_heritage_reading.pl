% ============================================================================
% CONSTRAINT STORY: cultural_property_legal_corpus__universal_heritage_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cultural_property_legal_corpus__universal_heritage_reading, []).

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
 *   constraint_id: cultural_property_legal_corpus__universal_heritage_reading
 *   human_readable: Universal Cultural Heritage Legal Framework (Universal Heritage Reading)
 *   domain: international_law/cultural_property/post_colonial_studies
 *
 * SUMMARY:
 *   This constraint instantiates the universal_heritage_reading of the
 *   cultural_property_legal_corpus kernel. Under this reading, cultural
 *   artifacts are declared humanity's shared heritage, and legitimate
 *   authority rests with institutionsâprincipally encyclopedic museums in
 *   former colonial powersâthat claim superior preservation capacity and
 *   universal access provision. The constraint coordinates global access and
 *   conservation while asymmetrically extracting legal costs, diplomatic
 *   friction, and identity harm from claimant states and indigenous
 *   communities whose repatriation demands are framed as particularist
 *   threats to the public good. The authored metrics and the claimed type are
 *   independent: the constraint is claimed as tangled_rope because a genuine
 *   coordination function (preservation, access) operates alongside
 *   asymmetric extraction, and the metrics describe the extractive trajectory
 *   without being tuned to match the claim.
 *
 * KEY AGENTS:
 *   - holding_institutions: Primary beneficiary and agenda-setter (institutional/arbitrage) â controls artifacts and sets the legal-institutional frame
 *   - claimant_states: Primary payer (institutional/constrained) â bears legal and diplomatic costs of repatriation claims treated as particularist
 *   - indigenous_communities: Secondary payer and excluded voice (powerless/identity_locked) â bears cultural and spiritual extraction, often outside the state-to-museum negotiation frame
 *   - repatriation_advocates: Analytical observer (moderate/analytical) â documents asymmetries and critiques the universal heritage frame from outside the benefiting parties
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cultural_property_legal_corpus__universal_heritage_reading, 0.78).
domain_priors:suppression_score(cultural_property_legal_corpus__universal_heritage_reading, 0.72).
domain_priors:theater_ratio(cultural_property_legal_corpus__universal_heritage_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cultural_property_legal_corpus__universal_heritage_reading, tangled_rope).
narrative_ontology:human_readable(cultural_property_legal_corpus__universal_heritage_reading, "Universal Cultural Heritage Legal Framework (Universal Heritage Reading)").
narrative_ontology:topic_domain(cultural_property_legal_corpus__universal_heritage_reading, "international_law/cultural_property/post_colonial_studies").

domain_priors:requires_active_enforcement(cultural_property_legal_corpus__universal_heritage_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cultural_property_legal_corpus__universal_heritage_reading, '103ac891-83ca-450e-a7d9-c9cace9e2ad7').
narrative_ontology:cs_kernel_codification('103ac891-83ca-450e-a7d9-c9cace9e2ad7', formalized).
narrative_ontology:cs_authority_grounding('103ac891-83ca-450e-a7d9-c9cace9e2ad7', expertise).
narrative_ontology:cs_interpretation_layer_present('103ac891-83ca-450e-a7d9-c9cace9e2ad7').
narrative_ontology:cs_reading_relation('103ac891-83ca-450e-a7d9-c9cace9e2ad7', cultural_property_legal_corpus__sovereign_repatriation_reading, coexists_with).
narrative_ontology:cs_reading_relation('103ac891-83ca-450e-a7d9-c9cace9e2ad7', cultural_property_legal_corpus__indigenous_stewardship_reading, coexists_with).
narrative_ontology:cs_axiom('103ac891-83ca-450e-a7d9-c9cace9e2ad7', foundational, humanity_common_heritage_title).
narrative_ontology:cs_axiom_status(humanity_common_heritage_title, holdable).
narrative_ontology:cs_axiom_grounding('103ac891-83ca-450e-a7d9-c9cace9e2ad7', humanity_common_heritage_title, conventional).
narrative_ontology:cs_axiom('103ac891-83ca-450e-a7d9-c9cace9e2ad7', foundational, preservation_capacity_legitimacy).
narrative_ontology:cs_axiom_status(preservation_capacity_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('103ac891-83ca-450e-a7d9-c9cace9e2ad7', preservation_capacity_legitimacy, instrumental).
narrative_ontology:cs_reference_frame('103ac891-83ca-450e-a7d9-c9cace9e2ad7', universal_cultural_stewardship).
narrative_ontology:cs_drift_state('103ac891-83ca-450e-a7d9-c9cace9e2ad7', contemporary_repatriation_pressure, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('103ac891-83ca-450e-a7d9-c9cace9e2ad7', '').
narrative_ontology:cs_kernel_id(cultural_property_legal_corpus__universal_heritage_reading, cultural_property_legal_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__universal_heritage_reading, holding_institutions).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__universal_heritage_reading, claimant_states).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__universal_heritage_reading, indigenous_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer international loan agreements, deaccession policies, and repatriation refusal frameworks. Justify retention through conservation capacity and universal museum narratives. Collect cultural prestige, research priority, visitor revenue, and institutional legitimacy from maintaining encyclopedic collections.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, holding_institutions, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__universal_heritage_reading, holding_institutions, beneficiary).

% Bear legal and diplomatic costs pursuing restitution of cultural artifacts removed during colonial periods. Their claims are routinely framed as particularist threats to the universal public good. Exit options are limited to lengthy bilateral negotiations, UNESCO intergovernmental committees, or litigation in foreign courts with uncertain jurisdiction.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, claimant_states, payer,
    institutional, generational, constrained, national).

% Experience cultural and spiritual extraction when sacred objects are classified as universal heritage specimens in foreign museums. Their identity and ceremonial practice depend on objects they cannot access. They are often excluded from repatriation negotiations conducted between states and museums, or subsumed under claimant-state frameworks that do not represent communal sovereignty.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, indigenous_communities, payer,
    powerless, civilizational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__universal_heritage_reading, indigenous_communities, excluded).

% Document provenance research gaps, publish restitution studies, and lobby for policy reform. They observe and critique the asymmetry between universal heritage claims and extraction from source communities but do not directly bear or collect the constraint's primary flows.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, repatriation_advocates, observer,
    moderate, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(cultural_property_legal_corpus__universal_heritage_reading, holding_institutions).
narrative_ontology:fixing_cost_class(cultural_property_legal_corpus__universal_heritage_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for preserving culturally significant objects in professionally maintained institutions and ensuring global public access to humanity's collective artistic and archaeological record, preventing dispersal or destruction through market fragmentation or political instability.
% TRANSFER_FUNCTION: Moves physical custody, interpretive authority, and cultural prestige from source communities and claimant states to encyclopedic museums and international holding institutions; transfers legal and diplomatic costs of contested retention to claimant states and indigenous communities.
% ABSENT_VOICES: Indigenous communities whose sacred or ceremonial objects are classified as cultural heritage of humanity rather than living spiritual necessities; successor state governments whose repatriation claims are framed as particularist threats to the universal public good; source community conservators and scholars who would advocate for localized stewardship but are excluded from institutional decision-making.
% DISAPPEARANCE_RATIONALE: If the universal heritage constraint vanished overnight, claimant states and indigenous communities would immediately advance repatriation and restitution claims; holding institutions would lose the legal and normative framework justifying retention, and the global distribution of cultural artifacts would shift substantially toward source nations and communities.
% FOUNDING_PROBLEM: Colonial extraction and twentieth-century looting dispersed cultural artifacts globally; weak or failed states lacked capacity to preserve objects from destruction, theft, or black-market trafficking; there was no coordinated international framework to prevent illicit trade or ensure professional conservation.
% FOUNDING_PROBLEM_CORROBORATION: Museum institutions and UNESCO attest the preservation crisis remains live, citing ongoing trafficking and conflict-zone destruction. Post-colonial scholars, claimant states, and indigenous rights advocates attest that the preservation rationale has become a cover for retention of looted objects, and that source communities today possess conservation capacity; independent restitution commissions and academic research outside the beneficiary set support the obsolescence reading for many categories of objects.
narrative_ontology:disappearance_verdict(cultural_property_legal_corpus__universal_heritage_reading, world_rearranges).
narrative_ontology:founding_problem_status(cultural_property_legal_corpus__universal_heritage_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cultural_property_legal_corpus__universal_heritage_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(cultural_property_legal_corpus__universal_heritage_reading, 'none', 1).
narrative_ontology:epsilon_provenance(cultural_property_legal_corpus__universal_heritage_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cultural_property_legal_corpus__universal_heritage_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(cultural_property_legal_corpus__universal_heritage_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(cultural_property_legal_corpus__universal_heritage_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because claimant states and indigenous communities bear substantial non-monetary costsâlegal fees, diplomatic erosion, and identity harmâfrom a framework that blocks their claims while rhetorically appropriating their heritage for humanity. Suppression is high (0.72) because the constraint's persistence depends on active legal and institutional enforcement: museum deaccession restrictions, international legal doctrines, and the classification of repatriation as particularist. Theater ratio is moderate-high (0.45) because the preservation and universal-access narrative increasingly functions as performative justification for retention rather than a transparent accounting of conservation need. Accessibility collapse (0.65) reflects that repatriation alternatives are institutionally blocked within the universal heritage frame but remain conceptually alive and actively pursued. Resistance (0.70) is high because post-colonial states and indigenous movements actively contest the frame. The measurement series share a single time grid to prevent misaligned substitution.
 *
 * PERSPECTIVAL GAP:
 *   From the holding institution seat, the constraint appears as necessary coordination: without centralized stewardship, artifacts face destruction, dispersal, or trafficking. From the claimant state and indigenous community seats, the same structure operates as enforced extraction that denies sovereign and communal title while externalizing the costs of retention. The engine preserves this divergence; the tangled_rope claim reflects the author's structural assessment that both perceptions are grounded in real features of the same arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Holding institutions are declared beneficiaries and agenda-setters with arbitrage-grade exit options, placing their directionality near the beneficiary pole (low d, low or negative effective extraction). Claimant_states are declared victims with constrained exit, pushing their directionality toward the target pole (high d, amplified effective extraction). Indigenous_communities are victims with identity_locked exitâtheir structural relationship is fused with the artifacts the constraint withholdsâplacing them near the full-target end. The engine computes divergent per-seat classifications from this structural data.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâpreservation of artifacts from destruction and illicit tradeâremains live in limited contexts (conflict zones, fragile states), which prevents pure snare classification. However, for vast categories of colonially looted objects, the preservation rationale has outlived its function: source communities and states now possess conservation capacity, and the constraint persists because holding institutions actively enforce it against repatriation. This is not piton inertia (theater without function) because the coordination function is partially genuine and the beneficiary actively maintains the structure. The mismatch between founding_problem_status=contested and disappearance_verdict=world_rearranges flags the mandatrophic tension without collapsing the classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    preservation_capacity_ambiguity,
    'Is the holding institution''s claimed preservation advantage genuine and insurmountable for all categories of retained artifacts, or has it become a rhetorical lock-in masking alternative stewardship capacities in source communities?',
    'Comparative conservation audit: independent assessment of source-community conservation infrastructure versus holding-institution infrastructure for matched artifact categories, paired with outcome data on preservation conditions.',
    'If alternative capacity is demonstrated, the coordination justification collapses for those categories and the constraint shifts toward snare classification; if the capacity gap is genuine and persistent, the tangled_rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preservation_capacity_ambiguity, empirical, 'Whether preservation rationale is empirically grounded or institutional cover.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of repatriation claims structural (international legal barriers, museum policy, diplomatic cost) or internalized (claimant states accepting the universal heritage frame as legitimate)?',
    'Post-recognition trajectory analysis: measure repatriation claim intensity and success rates in jurisdictions where indigenous or post-colonial legal authority has been formally recognized, comparing structural barrier removal against continued normative adherence.',
    'If claims surge when structural barriers drop, suppression was primarily structural; if deference to universal heritage persists, suppression is partially internalized and effective extraction is higher than structural measures suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism.').

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of a contested kernel. Would reallocation of authority to states or communities under sibling readings collapse the holding institution''s beneficiary position entirely, or would a hybrid stewardship model preserve partial coordination?',
    'Comparative case study of repatriation agreements that include loan-back or co-stewardship provisions, measuring whether coordination functions survive when beneficiary position is redistributed.',
    'If coordination is separable from the holding institution''s beneficiary role, the kernel decomposes cleanly into distinct constraints; if coordination atrophies without the institution, the universal heritage reading''s coordination claim gains structural support.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer frame ambiguity: separability of coordination and beneficiary position in this kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cultural_property_legal_corpus__universal_heritage_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cult_tr_t0, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(cult_tr_t12, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 12, 0.25).
narrative_ontology:measurement(cult_tr_t24, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 24, 0.3).
narrative_ontology:measurement(cult_tr_t36, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 36, 0.35).
narrative_ontology:measurement(cult_tr_t48, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 48, 0.4).
narrative_ontology:measurement(cult_tr_t60, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 60, 0.45).

% Extraction over time
narrative_ontology:measurement(cult_be_t0, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(cult_be_t12, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 12, 0.6).
narrative_ontology:measurement(cult_be_t24, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement(cult_be_t36, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 36, 0.71).
narrative_ontology:measurement(cult_be_t48, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 48, 0.75).
narrative_ontology:measurement(cult_be_t60, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 60, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(cult_su_t0, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(cult_su_t12, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 12, 0.55).
narrative_ontology:measurement(cult_su_t24, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 24, 0.6).
narrative_ontology:measurement(cult_su_t36, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 36, 0.65).
narrative_ontology:measurement(cult_su_t48, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 48, 0.68).
narrative_ontology:measurement(cult_su_t60, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 60, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cultural_property_legal_corpus__universal_heritage_reading, resource_allocation).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__universal_heritage_reading, sovereign_repatriation_reading).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__universal_heritage_reading, indigenous_stewardship_reading).

% DUAL FORMULATION NOTE:
% This constraint is the universal_heritage_reading of the cultural_property_legal_corpus kernel. It decomposes from the colloquial label 'cultural property legal corpus' per the Îµ-invariance principle: the universal heritage claim (artifacts belong to humanity; authority rests with preserving institutions), the sovereign repatriation claim (artifacts belong to successor states; colonial taking was illegitimate), and the indigenous stewardship claim (artifacts belong to communities; authority rests with cultural continuity) are structurally distinct constraints with different beneficiary/victim structures, different Îµ values, and different authority groundings. This reading extracts from claimant states and indigenous communities while coordinating global preservation and access through holding institutions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
