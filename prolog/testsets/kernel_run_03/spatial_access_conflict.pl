% ============================================================================
% CONSTRAINT STORY: spatial_access_conflict
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_spatial_access_conflict, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: spatial_access_conflict
 *   human_readable: Spatial Access Conflict: Sex-Segregated Spaces and Category Boundary Disputes
 *   domain: social_ontology/medical_classification/rights_frameworks
 *
 * SUMMARY:
 *   Spatial access conflict in sex-segregated spaces (healthcare facilities,
 *   shelters, correctional institutions, sports, changing areas) represents a
 *   contested institutional constraint where the same structural phenomenon —
 *   the boundary between categories that govern access — can be read as
 *   either a natural law about human biology or a contingent institutional
 *   arrangement. This constraint is ONE READING of the kernel 'woman/female
 *   category': the reading that privileges spatial access enforcement as the
 *   operational definition. This reading instantiates the constraint by
 *   focusing on how institutions use sex/gender categories to control access
 *   to spaces and services. The conflict emerges because boundary-ambiguous
 *   individuals (trans, non-binary, intersex agents) expose the contingency
 *   of the categorization system: their existence reveals that the boundary
 *   cannot be naturally given — it must be institutionally maintained through
 *   active enforcement. The constraint exhibits genuine coordination benefits
 *   (institutions can coordinate resource allocation, privacy expectations,
 *   and safety protocols through spatial segregation) alongside asymmetric
 *   extraction (certain agents are excluded from legitimate access, while
 *   institutional gatekeepers benefit from simplified classification
 *   systems). The theater ratio has increased over the measurement interval
 *   as enforcement mechanisms have become more performative: institutions
 *   increasingly conduct explicit eligibility verification (documentation
 *   review, physical examination, institutional status monitoring) rather
 *   than relying on taken-for-granted categorization. This rising theater
 *   indicates that the categorization system's functional clarity has
 *   degraded — boundary cases now require active institutional work to
 *   maintain, rather than naturally sorting themselves. The constraint is
 *   fundamentally about whether sex/gender categories are properties of
 *   nature (mountain perspective) or institutional tools (tangled rope
 *   perspective). The structural data supports the latter reading:
 *   beneficiary and victim structures are clear; active enforcement is
 *   required; the theater ratio shows increasing institutional work.
 *
 * KEY AGENTS:
 *   - Excluded Access Seekers: Primary victims (powerless/trapped) — denied access to facilities and services based on boundary categorization; cannot exit without abandoning legitimate institutional needs
 *   - Boundary-Ambiguous Agents: Secondary victims (moderate/constrained) — face forced categorization with internalized cognitive costs; can challenge categorization but face material legal and social penalties
 *   - Institutional Gatekeepers: Primary beneficiaries (institutional/arbitrage) — benefit from simplified classification systems; can arbitrage between different categorization policies across contexts
 *   - Rights Coalition: Organized challengers (organized/constrained) — work to establish alternative access norms and inclusive categorization; face institutional suppression and legal uncertainty
 *   - Medical Classification System: Institutional actor (institutional/arbitrage) — maintains categorization machinery through inertial enforcement; function has atrophied but mechanism persists
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the institutional arrangement as biological necessity; needs committer-frame analysis to recognize it as one reading among alternatives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(spatial_access_conflict, 0.58).
domain_priors:suppression_score(spatial_access_conflict, 0.65).
domain_priors:theater_ratio(spatial_access_conflict, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(spatial_access_conflict, extractiveness, 0.58).
narrative_ontology:constraint_metric(spatial_access_conflict, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(spatial_access_conflict, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(spatial_access_conflict, tangled_rope).
narrative_ontology:human_readable(spatial_access_conflict, "Spatial Access Conflict: Sex-Segregated Spaces and Category Boundary Disputes").
narrative_ontology:topic_domain(spatial_access_conflict, "social_ontology/medical_classification/rights_frameworks").

domain_priors:requires_active_enforcement(spatial_access_conflict).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(spatial_access_conflict, 'e3214d57-b2e4-4683-9994-bfe7b50a04da').
narrative_ontology:cs_created_at('e3214d57-b2e4-4683-9994-bfe7b50a04da', '').
narrative_ontology:cs_kernel_codification('e3214d57-b2e4-4683-9994-bfe7b50a04da', formalized).
narrative_ontology:cs_authority_grounding('e3214d57-b2e4-4683-9994-bfe7b50a04da', lineage).
narrative_ontology:cs_interpretation_layer_present('e3214d57-b2e4-4683-9994-bfe7b50a04da').
narrative_ontology:cs_kernel_id(spatial_access_conflict, woman_female_category).
narrative_ontology:cs_axiom('e3214d57-b2e4-4683-9994-bfe7b50a04da', foundational, spatial_segregation_functionally_necessary).
narrative_ontology:cs_axiom_status(spatial_segregation_functionally_necessary, holdable).
narrative_ontology:cs_axiom_grounding('e3214d57-b2e4-4683-9994-bfe7b50a04da', spatial_segregation_functionally_necessary, empirically_contingent).
narrative_ontology:cs_axiom('e3214d57-b2e4-4683-9994-bfe7b50a04da', secondary, biological_categories_naturally_salient).
narrative_ontology:cs_axiom_status(biological_categories_naturally_salient, holdable).
narrative_ontology:cs_axiom_grounding('e3214d57-b2e4-4683-9994-bfe7b50a04da', biological_categories_naturally_salient, conventional).
narrative_ontology:cs_reference_frame('e3214d57-b2e4-4683-9994-bfe7b50a04da', binary_spatial_segregation_framework).
narrative_ontology:cs_drift_state('e3214d57-b2e4-4683-9994-bfe7b50a04da', contemporary_boundary_contestation_era, gap(codification_collapse, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(spatial_access_conflict, institutional_gatekeepers).
narrative_ontology:constraint_beneficiary(spatial_access_conflict, category_maintenance_systems).
narrative_ontology:constraint_victim(spatial_access_conflict, boundary_ambiguous_agents).
narrative_ontology:constraint_victim(spatial_access_conflict, excluded_access_seekers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED ACCESS SEEKER (SNARE) — Structurally trapped. Cannot exit the constraint without abandoning legitimate access needs (healthcare, shelters, correctional institutions, sports facilities). Experiences maximum suppression: institutional barriers, legal prohibition, and social stigma all reinforce exclusion. The constraint extracts dignity, safety, and equal access while offering no coordination benefit. No escape route that preserves participation in the institution itself.
constraint_indexing:constraint_classification(spatial_access_conflict, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: BOUNDARY-AMBIGUOUS AGENT (TANGLED ROPE) — Constrained by both structural barriers and internalized classification anxiety. Faces material costs of challenging category assignments (legal action, institutional retaliation, social exposure) plus cognitive costs of persistent identity dissonance. Experiences real coordination benefits from institutional participation (healthcare access, legal recognition, social services) alongside asymmetric extraction (forced categorization, surveillance, mandatory disclosure). Both the coordination function and the extraction mechanism are active.
constraint_indexing:constraint_classification(spatial_access_conflict, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSTITUTIONAL GATEKEEPER (ROPE) — Experiences the constraint primarily as a coordination mechanism: spatial segregation enables claimed epistemic clarity about categories, simplifies risk assessment protocols, and reduces institutional liability. Can arbitrage between different institutional contexts (e.g., a hospital system can adopt different categorization policies for different units). Benefits from the constraint without experiencing extraction costs. Net beneficiary position with low suppression.
constraint_indexing:constraint_classification(spatial_access_conflict, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: RIGHTS COALITION (TANGLED ROPE) — Organized agents (trans rights organizations, civil rights bodies, medical ethics committees) see genuine coordination benefits from inclusive categorization (reduction of harm, improved data validity, ethical integrity) but face suppression from institutional resistance and legal uncertainty. The constraint extracts from this coalition's capacity (litigation costs, advocacy overhead, institutional pressure) while they work to establish alternative coordination norms. Active enforcement required to maintain the old boundary; active enforcement equally required to shift it.
constraint_indexing:constraint_classification(spatial_access_conflict, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: MEDICAL CLASSIFICATION SYSTEM (PITON) — The system persists through institutional inertia and embedded assumptions about biological categories. High theater ratio: much of the classification machinery is maintenance ritual (repeated diagnostic protocols, documentation standards, training curricula) rather than functional verification. The system's own evidence base increasingly contradicts its categorization premises, yet institutional momentum sustains it. Primary function (epistemic clarity about biology) has atrophied; primary mechanism (enforcement of boundaries) remains active.
constraint_indexing:constraint_classification(spatial_access_conflict, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, sex-based spatial segregation appears to derive from immutable biological facts (chromosomal sex, reproductive anatomy, developmental biology) that no policy can overcome. This perspective risks naturalizing what is actually a contingent institutional arrangement: which biological facts are salient for spatial access decisions depends on the institution's actual functional requirements, not on immutable nature. The engine's false summit detector will identify this as naturalization of a classification choice.
constraint_indexing:constraint_classification(spatial_access_conflict, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(spatial_access_conflict_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(spatial_access_conflict, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(spatial_access_conflict, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(spatial_access_conflict, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(spatial_access_conflict, TR),
    TR >= 0.70.

:- end_tests(spatial_access_conflict_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts from boundary-ambiguous agents (forced categorization, loss of autonomy in self-identification) and from excluded agents (loss of access), while benefiting institutional gatekeepers (simplified risk management, liability protection through categorical clarity). The extractiveness is not maximal because some coordination benefits are genuine — institutions do need to coordinate spatial access — but the extraction is substantial because the categorization system privileges institutional clarity over agent autonomy. Suppression (0.65): High. Multiple suppression mechanisms: legal prohibition of alternative categorization in many jurisdictions; institutional barriers to challenging category assignments; social stigma and discrimination; lack of accessible appeals processes; medical gatekeeping (requirement for professional diagnosis/authorization to change category); documentation requirements that force persistent disclosure. Suppression is not total because some institutional contexts are beginning to loosen boundaries, creating precedent for alternatives. Theater ratio (0.48): Moderate, rising. The constraint initially relied on taken-for-granted categorization (theater low) but has increasingly required active verification procedures (documentation review, eligibility confirmation, explicit boundary maintenance). Rising theater indicates that the categorization system's legitimacy as 'natural' has eroded — institutional work is now required to maintain what was once assumed. The rise from 0.35 to 0.48 tracks the period when boundary-ambiguous agents became increasingly visible and institutional gatekeepers shifted from implicit to explicit categorization protocols.
 *
 * PERSPECTIVAL GAP:
 *   The excluded access seeker and the institutional gatekeeper experience opposite structural positions: one experiences maximal extraction and suppression (snare), the other experiences coordination benefit and arbitrage mobility (rope). The boundary-ambiguous agent occupies an intermediate position: they experience both coordination benefit (access to the institution) and extraction (forced categorization). The rights coalition experiences a different tangled rope: they work to establish alternative coordination norms (inclusive access, self-determination in categorization) while facing extraction through institutional resistance. The medical classification system's piton perspective reveals that much institutional activity is performative maintenance rather than functional verification. The analytical observer's mountain perspective naturalizes the institutional arrangement as biological necessity, but the structural data contradicts this: the presence of identified beneficiaries, the need for active enforcement, and the rising theater ratio all indicate that this is a contingent institutional arrangement, not a natural law. The false summit detector should flag this as naturalization of institutional choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's structural position determines its experienced extractiveness through the directionality derivation: beneficiaries with arbitrage options (institutional gatekeepers) have low d values, experiencing minimal effective extraction through the constraint. Victims with trapped exits (excluded agents) have high d values, experiencing maximum effective extraction. Organized challengers working to establish alternatives occupy a middle position: they have some agency (organized power) but face structural barriers (suppression), resulting in moderate d values. The institutional classification system as an agent has arbitrage mobility (can switch categorization schemes) but is actually captured by inertial enforcement (identity-locked to its own categorization logic), which is why it appears in the piton perspective rather than showing true arbitrage mobility. The commentary on directionality is drawn from this structural mapping, not from arithmetic anchoring that would overspecify the derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does NOT resolve to a single type — it is legitimately tangled rope from the analytical center, with valid snare readings from the powerless perspective and rope readings from the beneficiary perspective. The mandatrophy surfaces a deeper question: what does 'coordination' mean when the boundaries being coordinated are contingent rather than natural? If the categories themselves are contestable, then maintaining them requires active enforcement, and the 'coordination' benefit becomes suspect. The tangled rope classification holds because (a) genuine coordination benefits exist (institutions do coordinate spatial access through categories), (b) genuine extraction exists (certain agents are excluded or forced into categories), and (c) both mechanisms are active. The constraint does NOT collapse into snare because the institutional gatekeepers genuinely see coordination benefits; it does NOT collapse into rope because the excluded agents genuinely experience extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    functional_requirement_vs_category_mapping,
    'What is the actual functional requirement that spatial segregation serves, and does sex-based categorization map to that requirement?',
    'Empirical analysis of stated vs. actual institutional purposes (e.g., for changing facilities: privacy, security, or accident prevention?); comparative examination of alternative categorization schemes that achieve the same functional goal with different boundaries.',
    'If functional requirement maps only to binary sex categories: constraint is genuine coordination mechanism (Rope). If multiple categorization schemes achieve the same function: constraint is category enforcement (Snare/Tangled Rope) with institutional beneficiaries.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(functional_requirement_vs_category_mapping, empirical, 'Whether sex-based categories are functionally necessary or interchangeable with other categorization schemes').

omega_variable(
    naturalization_of_institutional_choice,
    'Is the spatial segregation constraint grounded in natural law or in contingent institutional decisions about which biological features to use for categorization?',
    'Historical and comparative institutional analysis: how have different societies, time periods, and institutional contexts categorized access? What changed when policy changed? Do biological facts explain the changes, or do institutional choices explain the changes?',
    'If grounded in natural law: mountain classification is appropriate. If grounded in institutional choice: constraint is false summit (should reclassify to tangled_rope or snare depending on enforceability and extraction). This is the core committer ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalization_of_institutional_choice, conceptual, 'Whether the constraint derives from natural law or institutional categorization choices').

omega_variable(
    access_harm_quantification,
    'What is the empirical magnitude of harm from boundary-ambiguous access (privacy violation, safety risk, athletic unfairness) compared to the empirical magnitude of harm from exclusion?',
    'Systematic review of incident reports, safety audits, privacy complaints, and health outcome data from institutions that have eliminated spatial segregation vs. those that maintain it; quantitative comparison of claimed harms to actual harms.',
    'If exclusion harm >> access harm: constraint is extraction mechanism (Snare). If harms are comparable: constraint is genuine coordination problem (Tangled Rope). If access harm >> exclusion harm: category maintenance is justified (Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(access_harm_quantification, empirical, 'Comparative magnitude of harms from boundary-ambiguous access vs. categorical exclusion').

omega_variable(
    identity_locked_mechanism_clarity,
    'For institutional gatekeepers and classification systems, is the commitment to binary spatial segregation grounded in structural dependencies or in identity-fused institutional identity?',
    'Institutional analysis of resistance to reclassification: Do institutions resist because of genuine functional necessity (structural), because of legal liability concerns (structural), or because the institution''s identity is constituted through maintenance of categorical boundaries (identity-locked)?',
    'If structural: institutional perspective is arbitrage-mobile (truly has exit options). If identity-locked: institutional perspective should be coded as identity_locked rather than arbitrage, revealing that institutional resistance is cognitive capture rather than functional necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_mechanism_clarity, conceptual, 'Whether institutional resistance to reclassification is structural or identity-fused').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(spatial_access_conflict, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spat_tr_t0, spatial_access_conflict, theater_ratio, 0, 0.35).
narrative_ontology:measurement(spat_tr_t15, spatial_access_conflict, theater_ratio, 15, 0.42).
narrative_ontology:measurement(spat_tr_t30, spatial_access_conflict, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(spat_be_t0, spatial_access_conflict, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(spat_be_t15, spatial_access_conflict, base_extractiveness, 15, 0.54).
narrative_ontology:measurement(spat_be_t30, spatial_access_conflict, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(spatial_access_conflict, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one component of the 'woman/female category' kernel. Sibling readings would include biological_determination_reading (what categorization criteria are medically necessary), legal_status_reading (what categorization criteria are legally required), and social_coordination_reading (what categorization criteria enable social cooperation). Each sibling would have its own ε value reflecting different functional requirements and extraction mechanisms. Linked via kernel_id rather than affects_constraints because they are alternative framings of the same kernel, not causal dependents.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(spatial_access_conflict, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
