% ============================================================================
% CONSTRAINT STORY: jati_practice_norm__colonial_census_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jati_practice_norm__colonial_census_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: jati_practice_norm__colonial_census_reading
 *   human_readable: Jati Reification via Colonial Census Administration
 *   domain: social/anthropological/political
 *
 * SUMMARY:
 *   Under colonial rule, jati categories—historically fluid, locally
 *   negotiated, occupation-contingent—were standardized, enumerated, and
 *   reified via the census into fixed legal categories. This reading
 *   instantiates the colonial census as an external administrative apparatus
 *   that froze jati boundaries in service of state legibility and governance
 *   efficiency, transforming what had been continuous renegotiation into
 *   fixed identity codified in law. The constraint operates as a tangled
 *   rope: the census solves the genuine coordination problem of how to count
 *   and govern a heterogeneous population (coordination function), but it
 *   simultaneously extracts autonomy from communities and locks subordinated
 *   groups into disadvantageous categories (extraction function). The
 *   constraint's persistence depends on active enforcement: census precedent,
 *   legal codification, and administrative repetition maintain the
 *   reification even as local practice continues to pressure against it.
 *
 * KEY AGENTS:
 *   - colonial_administrative_apparatus: Sets the enumeration rules, maintains the reified categories through census repetition and legal precedent, benefits from simplified governance (institutional power, arbitrage exit)
 *   - subordinated_jati_communities: Locked into assigned categories that may not match historical practice or self-understanding, bear discriminatory costs in law and resource allocation (powerless, identity-locked exit)
 *   - locally_mobile_jati_practitioners: Historically occupied fluid boundaries; the census forecloses occupational and geographical mobility by fixing them into a single category for life (moderate power, constrained exit)
 *   - upper_jati_beneficiaries: Dominant groups whose codified position aligns with varna hierarchy; reification crystallizes their advantage (powerful, mobile exit)
 *   - indigenous_jati_councils: Historically held definitional authority; the census bypasses them entirely (organized, trapped by displacement of authority)
 *   - anti_colonial_reformers: Document the mechanism and costs of reification; their analysis establishes the causal link between enumeration and the freezing of previously fluid categories (analytical observer)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jati_practice_norm__colonial_census_reading, 0.68).
domain_priors:suppression_score(jati_practice_norm__colonial_census_reading, 0.72).
domain_priors:theater_ratio(jati_practice_norm__colonial_census_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jati_practice_norm__colonial_census_reading, tangled_rope).
narrative_ontology:human_readable(jati_practice_norm__colonial_census_reading, "Jati Reification via Colonial Census Administration").
narrative_ontology:topic_domain(jati_practice_norm__colonial_census_reading, "social/anthropological/political").

domain_priors:requires_active_enforcement(jati_practice_norm__colonial_census_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jati_practice_norm__colonial_census_reading, '0f0e6de5-0493-4bfd-bb02-5d2706fb82a1').
narrative_ontology:cs_kernel_codification('0f0e6de5-0493-4bfd-bb02-5d2706fb82a1', distributed).
narrative_ontology:cs_authority_grounding('0f0e6de5-0493-4bfd-bb02-5d2706fb82a1', extraction).
narrative_ontology:cs_interpretation_layer_present('0f0e6de5-0493-4bfd-bb02-5d2706fb82a1').
narrative_ontology:cs_reading_relation('0f0e6de5-0493-4bfd-bb02-5d2706fb82a1', jati_practice_norm__orthodox_textual_reading, coexists_with).
narrative_ontology:cs_reading_relation('0f0e6de5-0493-4bfd-bb02-5d2706fb82a1', jati_practice_norm__localized_practice_reading, influences).
narrative_ontology:cs_axiom('0f0e6de5-0493-4bfd-bb02-5d2706fb82a1', foundational, jati_requires_external_enumeration).
narrative_ontology:cs_axiom_status(jati_requires_external_enumeration, holdable).
narrative_ontology:cs_axiom_grounding('0f0e6de5-0493-4bfd-bb02-5d2706fb82a1', jati_requires_external_enumeration, instrumental).
narrative_ontology:cs_axiom('0f0e6de5-0493-4bfd-bb02-5d2706fb82a1', foundational, reification_enables_governability).
narrative_ontology:cs_axiom_status(reification_enables_governability, holdable).
narrative_ontology:cs_axiom_grounding('0f0e6de5-0493-4bfd-bb02-5d2706fb82a1', reification_enables_governability, empirically_contingent).
narrative_ontology:cs_reference_frame('0f0e6de5-0493-4bfd-bb02-5d2706fb82a1', pre_census_jati_fluidity).
narrative_ontology:cs_drift_state('0f0e6de5-0493-4bfd-bb02-5d2706fb82a1', post_1850_legal_codification, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('0f0e6de5-0493-4bfd-bb02-5d2706fb82a1', '').
narrative_ontology:cs_kernel_id(jati_practice_norm__colonial_census_reading, jati_practice_norm).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jati_practice_norm__colonial_census_reading, colonial_administrative_apparatus).
narrative_ontology:constraint_victim(jati_practice_norm__colonial_census_reading, subordinated_jati_communities).
narrative_ontology:constraint_victim(jati_practice_norm__colonial_census_reading, locally_mobile_jati_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jati_practice_norm__colonial_census_reading, upper_jati_beneficiaries).
narrative_ontology:constraint_vindicates(jati_practice_norm__colonial_census_reading, governance_requires_stable_categories).
narrative_ontology:constraint_vindicates(jati_practice_norm__colonial_census_reading, enumeration_freezes_fluidity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The colonial state census apparatus designs and enforces standardized jati categories as the primary organizing scheme for taxation, recruitment, and legal liability. Officials justify this as necessary for rational governance; they benefit from the simplified ledger and the categories' stasis. The apparatus maintains the reification through census enforcement, legal codification, and administrative precedent.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, colonial_administrative_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Bear the costs of forced standardization: their actual jati identity (historically fluid, locally negotiated, occupation-based) is frozen into an official category that may not match their self-understanding or local practice. This reification becomes the basis for legal discrimination, occupational restrictions, and census-based resource allocation that disadvantages them. Exit is identity-locked: refusing the assigned category invokes state sanction; accepting it entails subordination embedded in law.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, subordinated_jati_communities, payer,
    powerless, generational, identity_locked, national).

% Historically occupied ambiguous or fluid jati boundaries—occupation-changers, inter-jati workers, regional migrants whose jati identity shifted with function and geography. The census freezes them into a single category for life, foreclosing the practice of boundary renegotiation that previously allowed mobility and economic flexibility. Their exclusion from the category-design process is structural: enumeration precedes them, not the reverse.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, locally_mobile_jati_practitioners, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jati_practice_norm__colonial_census_reading, locally_mobile_jati_practitioners, excluded).

% Dominant jati groups whose categories and hierarchical position align with colonial-codified varna rankings. The reification crystallizes their advantage: law and census encoding reinforce their claims to ritual purity, occupational monopoly, and resource priority. They can navigate between local practice and official category as needed; their power allows selective compliance.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, upper_jati_beneficiaries, beneficiary,
    powerful, generational, mobile, national).

% Local bodies that historically negotiated jati membership, boundary disputes, and ritual standing. The colonial census apparatus bypasses them: the state's enumeration and codification displace local authority with external, written, static rules. Their exclusion from definition-power is the primary mechanism that extracts jati autonomy from community hands into administrative hands.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, indigenous_jati_councils, excluded,
    organized, generational, trapped, regional).

% Nineteenth and twentieth-century movements that documented the colonial reification and its costs—anthropologists, social reformers, nationalist intellectuals who analyzed how the census froze previously fluid categories and how that freezing became the basis for legal discrimination and caste rigidity. Their analysis establishes the factual relationship between enumeration and reification.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, anti_colonial_reformers, observer,
    organized, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jati_practice_norm__colonial_census_reading, colonial_administrative_apparatus).
narrative_ontology:fixing_cost_class(jati_practice_norm__colonial_census_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a simplified enumeration scheme for state taxation, legal liability, recruitment, and administrative record-keeping. Governance at scale requires some stable basis for categorizing subjects; the census provides that basis across a heterogeneous population.
% TRANSFER_FUNCTION: Transfers jati definitional authority from local communities (councils, practitioners, occupational groups) to the colonial state apparatus. The state gains legibility and control; communities lose autonomy over self-classification and boundary-negotiation. The constraint extracts the freedom to renegotiate jati identity locally and transfers it to written, fixed, state-enforced categories.
% ABSENT_VOICES: Locally-mobile jati practitioners and indigenous jati councils are structurally excluded from the enumeration process itself—they are subjects of the census, not participants in designing it. Their objections to the fixed categories appear post-hoc, after the apparatus has already reified them in law and precedent.
% DISAPPEARANCE_RATIONALE: If the colonial census reification and its legal enforcement apparatus vanished, jati categories would re-fluidify: occupation-changing, inter-regional mobility, and local boundary renegotiation would resume; legal discrimination based on fixed categories would collapse; communities would reconstitute their own authority over jati definition and membership. The administrative legibility would be lost; governance would revert to decentralized negotiation.
% FOUNDING_PROBLEM: Heterogeneous populations under colonial rule required a legible, uniform basis for taxation, legal liability, and recruitment. The colonial state encountered a bewildering diversity of local jati-like categories, occupational groups, and kinship frames that did not fit into a simple list. The census was built to solve the state's governance problem: how to know, count, and control subjects across a continent.
% FOUNDING_PROBLEM_CORROBORATION: Colonial officials and state administrators attested this problem continuously in census documentation and policy statements. Independent historical analysis by anti-colonial scholars and anthropologists (Arjun Appadurai, Dirks, Cohn) confirms the state's framing of its own problem: the state DID encounter heterogeneity and DID design enumeration to solve legibility. What those scholars contest is whether the state's solution was legitimate—whether freezing fluid categories served genuine coordination or extractive state control. The corroboration comes from state archives themselves, interpreted by scholars outside the benefiting apparatus.
narrative_ontology:disappearance_verdict(jati_practice_norm__colonial_census_reading, world_rearranges).
narrative_ontology:founding_problem_status(jati_practice_norm__colonial_census_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jati_practice_norm__colonial_census_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(jati_practice_norm__colonial_census_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jati_practice_norm__colonial_census_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jati_practice_norm__colonial_census_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jati_practice_norm__colonial_census_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness accumulates from 0.15 (pre-census, local jati practice with minimal state extraction) to 0.68 (full colonial apparatus, reified categories locked in law). Theater ratio rises from 0.08 to 0.41: early censuses genuinely solved a legibility problem; by mid-colonial period, increasing share of enforcement machinery goes to maintaining category boundaries against local fluidity rather than to actual governance function. Suppression requirement rises from 0.2 to 0.72: initially, census categories could be treated as advisory (local mobility continued); by 1950, legal codification, police enforcement, and administrative precedent make the categories mandatory and identity-locked. The measurement series on a shared time grid (every metric authored at 1750, 1810, 1870, 1910, 1950) captures the transformation from light bureaucratic convenience to heavy enforced extraction.
 *
 * PERSPECTIVAL GAP:
 *   From the colonial apparatus's seat, the census is rational governance—mapping chaos into order, enabling fair taxation and recruitment. From the subordinated community seat, the same machinery is identity freezing—the loss of the historical freedom to renegotiate jati membership and the embedding of disadvantage in law. The apparatus seat experiences the constraint as coordination (genuine problem solved); the community seat experiences it as extraction (autonomy lost). The engine should compute these divergent classifications from the structural data: high exit options and power differential should produce apparatus-as-beneficiary, community-as-trapped-target, and the directionality should show the asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Colonial apparatus: d near 0.0 (beneficiary—collects legibility, exercises definitional power, faces no exit cost). Subordinated communities: d near 1.0 (targets—locked into assigned categories, bear discriminatory costs, exit is identity-locked). Locally-mobile practitioners: d near 0.85 (high target, constrained exit—their historical fluidity is foreclosed). Upper jati beneficiaries: d near 0.2 (minor beneficiary—codification benefits them, but they retain mobile exit). Indigenous councils: d near 0.9 (high target—displaced from authority, trapped in new apparatus). The directionality overrides should NOT be necessary here; structural derivation from beneficiary/victim + exit should produce the right d for each seat automatically. If the engine derives apparatus-as-symmetric, that would signal the analysis missed something; the story should be revised, not overridden.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (governance legibility) is live at 1750 and remains live at 1950—the state's need to count and control subjects never goes away. But the founding problem is NOT the primary driver of extraction by 1950. By mid-colonial period, the constraint persists because reified categories have become embedded in law, precedent, and institutional routine. The constraint has acquired a secondary function: maintaining upper-jati hierarchical advantage and state control. The theater ratio rising (0.08 → 0.41) indicates performative maintenance: increasingly, census enforcement is about defending fixed categories against local fluidity rather than about solving legibility. The mandate (census for governance) has outlived its original necessity (legibility could be achieved through non-reifying mechanisms—registries, rolling updates). The extraction persists because the apparatus itself benefits and because the reification has become institutionalized. This is NOT a piton (the apparatus actively maintains it); it is a tangled rope whose extraction component has grown to dominate over time.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pre_colonial_jati_fluidity,
    'How fluid were jati boundaries and occupational categories in pre-colonial South Asia? Was the variability documented in colonial records (which becomes the baseline for claiming reification) accurate or a colonial misinterpretation?',
    'Textual analysis of pre-colonial epigraphic and literary sources on occupational groups, marriage rules, and ritual standing; comparison with independent anthropological accounts of neighboring societies with pre-state jati-like systems.',
    'If pre-colonial jati was genuinely fluid (as this reading claims), the colonial reification is an external intervention freezing fluidity—a clear extraction of autonomy. If pre-colonial jati was already locally fixed (but varied by region), the colonial reification is less extractive—it standardizes existing rigidity rather than creating it. The reading''s extraction estimate depends on this.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pre_colonial_jati_fluidity, empirical, 'Whether the colonial reification introduced fixity or standardized pre-existing local fixity.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.72) primarily structural (enforced by legal penalties, census precedent, administrative machinery) or internalized (communities have adopted the census categories as self-evident, making exit identity-locked)?',
    'Post-colonial trajectory: if the legal enforcement apparatus is removed (as happened at independence), does suppression persist? Does the reified category remain in place due to institutional inertia, or do communities re-fluidify their boundaries? Contemporary ethnographic evidence from India''s post-colonial jati practices would show whether boundaries remain fixed due to internalized identification or active structural enforcement.',
    'If suppression is primarily structural, removing the apparatus could unlock fluidity. If internalized, the constraint persists even without enforcement, and exit remains identity-locked even post-colonially. This affects the post-colonial constraint''s type and severity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural enforcement or internalized identity-fusion.').

omega_variable(
    coordination_extraction_boundary,
    'Could the genuine coordination problem (legible enumeration) have been solved without reifying jati categories? Could census enumeration proceed with rolling, provisional, locally-variable categories?',
    'Analysis of census methodology in other diverse polities (Ottoman millet system, Austro-Hungarian nationality statistics, contemporary development surveys with rotating identities). If legibility is achievable without reification, the reification is extraction pure; if reification is technically necessary for enumeration, part of the measured extraction is the structural cost of coordination.',
    'If coordination could be separated from reification, the entire constraint is extractive (snare masquerading as rope). If inseparable, the measured extraction accurately reflects the tangled rope structure—both coordination and extraction are baked in.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Whether enumeration for governance requires reified categories or could proceed with fluid ones.').

omega_variable(
    competing_kernel_readings,
    'Are the three readings (colonial_census, orthodox_textual, localized_practice) genuinely distinct constraints with different ε values, or are they one constraint measured from three frames?',
    'ε-invariance test: do the three readings produce different structural beneficiary/victim sets and different functions? (a) Colonial_census reading: beneficiary=apparatus, victims=subordinated communities, function=enumeration/extraction. (b) Orthodox_textual reading: beneficiary=ritual-purity-enforcement elites, victims=ritual-polluted occupations, function=cosmic order maintenance. (c) Localized_practice reading: beneficiary=fluid communities, victims=reification apparatus, function=negotiated coordination. All three have different ε, different victims, different functions—these are three distinct constraints, not three measurements of one constraint. The kernel (jati itself) is the contested object; the three readings instantiate three different constraints overlaid on the same social fact.',
    'If the three readings are separate constraints (as ε-invariance suggests), the constraint family should be formally decomposed: three separate JSON files linked by network.affects_constraints. If they are one constraint with measurement ambiguity, they should be compressed. The reading is confident they are separate constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competing_kernel_readings, conceptual, 'Whether the kernel contest represents three distinct constraints or three framings of one constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jati_practice_norm__colonial_census_reading, 1750, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jati_tr_t1750, jati_practice_norm__colonial_census_reading, theater_ratio, 1750, 0.08).
narrative_ontology:measurement_basis(jati_tr_t1750, projected).
narrative_ontology:measurement(jati_tr_t1810, jati_practice_norm__colonial_census_reading, theater_ratio, 1810, 0.14).
narrative_ontology:measurement_basis(jati_tr_t1810, observed).
narrative_ontology:measurement(jati_tr_t1870, jati_practice_norm__colonial_census_reading, theater_ratio, 1870, 0.28).
narrative_ontology:measurement_basis(jati_tr_t1870, observed).
narrative_ontology:measurement(jati_tr_t1910, jati_practice_norm__colonial_census_reading, theater_ratio, 1910, 0.38).
narrative_ontology:measurement_basis(jati_tr_t1910, observed).
narrative_ontology:measurement(jati_tr_t1950, jati_practice_norm__colonial_census_reading, theater_ratio, 1950, 0.41).
narrative_ontology:measurement_basis(jati_tr_t1950, observed).

% Extraction over time
narrative_ontology:measurement(jati_be_t1750, jati_practice_norm__colonial_census_reading, base_extractiveness, 1750, 0.15).
narrative_ontology:measurement_basis(jati_be_t1750, projected).
narrative_ontology:measurement(jati_be_t1810, jati_practice_norm__colonial_census_reading, base_extractiveness, 1810, 0.28).
narrative_ontology:measurement_basis(jati_be_t1810, observed).
narrative_ontology:measurement(jati_be_t1870, jati_practice_norm__colonial_census_reading, base_extractiveness, 1870, 0.52).
narrative_ontology:measurement_basis(jati_be_t1870, observed).
narrative_ontology:measurement(jati_be_t1910, jati_practice_norm__colonial_census_reading, base_extractiveness, 1910, 0.65).
narrative_ontology:measurement_basis(jati_be_t1910, observed).
narrative_ontology:measurement(jati_be_t1950, jati_practice_norm__colonial_census_reading, base_extractiveness, 1950, 0.68).
narrative_ontology:measurement_basis(jati_be_t1950, observed).

% Suppression requirement over time
narrative_ontology:measurement(jati_su_t1750, jati_practice_norm__colonial_census_reading, suppression_requirement, 1750, 0.2).
narrative_ontology:measurement_basis(jati_su_t1750, projected).
narrative_ontology:measurement(jati_su_t1810, jati_practice_norm__colonial_census_reading, suppression_requirement, 1810, 0.38).
narrative_ontology:measurement_basis(jati_su_t1810, observed).
narrative_ontology:measurement(jati_su_t1870, jati_practice_norm__colonial_census_reading, suppression_requirement, 1870, 0.58).
narrative_ontology:measurement_basis(jati_su_t1870, observed).
narrative_ontology:measurement(jati_su_t1910, jati_practice_norm__colonial_census_reading, suppression_requirement, 1910, 0.68).
narrative_ontology:measurement_basis(jati_su_t1910, observed).
narrative_ontology:measurement(jati_su_t1950, jati_practice_norm__colonial_census_reading, suppression_requirement, 1950, 0.72).
narrative_ontology:measurement_basis(jati_su_t1950, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jati_practice_norm__colonial_census_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jati_practice_norm__colonial_census_reading, 0.12).
narrative_ontology:affects_constraint(jati_practice_norm__colonial_census_reading, jati_practice_norm__orthodox_textual_reading).
narrative_ontology:affects_constraint(jati_practice_norm__colonial_census_reading, jati_practice_norm__localized_practice_reading).

% DUAL FORMULATION NOTE:
% The jati_practice_norm kernel is instantiated in three structurally distinct constraints reflecting three readings of the kernel: colonial_census_reading (this file) asserts jati was reified by external state apparatus, orthodox_textual_reading asserts jati derives from fixed cosmic varna law, localized_practice_reading asserts jati is continuous local renegotiation. All three have different ε, different beneficiary/victim sets, and different functions. They are linked as a constraint family because they contest the same kernel and each reading's viability depends on how the others are empirically resolved (epistemic coupling). The colonial_census_reading produces moderate extractiveness (tangled_rope); the orthodox_textual_reading produces mountain-type fixity (cosmic law, no extracted victims); the localized_practice_reading produces rope-type coordination (fluid negotiation, minimal extraction). The three readings coexist in contemporary South Asian society: orthodox traditionalists, colonial-legacy administrators, and local practitioners simultaneously inhabit different readings. The family structure enables the corpus to register this contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
