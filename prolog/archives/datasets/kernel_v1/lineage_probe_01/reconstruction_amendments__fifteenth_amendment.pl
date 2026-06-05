% ============================================================================
% CONSTRAINT STORY: reconstruction_amendments__fifteenth_amendment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fifteenth_amendment, []).

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
 *   constraint_id: reconstruction_amendments__fifteenth_amendment
 *   human_readable: The Fifteenth Amendment: Prohibition of Racial Disfranchisement
 *   domain: political/legal/voting_rights
 *
 * SUMMARY:
 *   The Fifteenth Amendment (ratified February 3, 1870) explicitly forbids
 *   the United States and the states from denying the vote on account of
 *   race, color, or previous condition of servitude. It represents the
 *   Reconstruction coalition's attempt to anchor Black political
 *   participation in constitutional law. However, its history demonstrates
 *   the gap between formal legal prohibition and actual enforcement. The
 *   amendment is immediately challenged through facially race-neutral
 *   mechanisms — literacy tests (South Carolina, 1882), poll taxes (Georgia,
 *   1877), grandfather clauses (Louisiana, 1898), and all-white primaries
 *   (established through Democratic Party rule, not state law). Federal
 *   enforcement capacity collapses with Reconstruction's end (1876-1877). By
 *   the early 20th century, the amendment is largely vestigial: formally on
 *   the books but practically unenforced, its stated purpose negated through
 *   mechanisms that are technically race-neutral. This constraint exemplifies
 *   the difference between a genuine natural law (immutable across all
 *   systems) and a false summit (a contingent institutional arrangement
 *   naturalized as law). The analytical observer risks seeing immutable
 *   democratic principle; structural analysis reveals contingent enforcement
 *   mechanisms and political choice.
 *
 * KEY AGENTS:
 *   - Black male freedmen / voters (powerless/trapped): Primary beneficiaries in law; primary victims in practice — trapped by suppression mechanisms that operate despite the amendment's prohibition.
 *   - Reconstruction coalition (organized/constrained): Northern Republicans and freedmen's advocates — experience mixed coordination and extraction; have agency but face Southern resistance and Northern backsliding.
 *   - Federal legislative authority (institutional/arbitrage): Establishes the coordinate standard at the national level; benefits from standardized franchise across states; experiences rope-type coordination.
 *   - Southern Democratic planter regime (powerful/mobile): Experiences the amendment as attempt to extract their political monopoly; retains capacity to evade through facially neutral mechanisms; operates as snare through evasion rather than direct suppression.
 *   - Reconstruction enforcement apparatus (organized/constrained): Federal oversight structures with explicit sunset — temporary scaffold that withdraws after 1876.
 *   - The amendment as legal text (institutional/arbitrage): Vestigial by early 20th century — maintained performatively while suppression mechanisms operate around it; piton classification.
 *   - Analytical observer (analytical/analytical): Risks naturalizing contingent institutional arrangements as immutable democratic principles.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reconstruction_amendments__fifteenth_amendment, 0.52).
domain_priors:suppression_score(reconstruction_amendments__fifteenth_amendment, 0.68).
domain_priors:theater_ratio(reconstruction_amendments__fifteenth_amendment, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reconstruction_amendments__fifteenth_amendment, extractiveness, 0.52).
narrative_ontology:constraint_metric(reconstruction_amendments__fifteenth_amendment, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(reconstruction_amendments__fifteenth_amendment, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reconstruction_amendments__fifteenth_amendment, tangled_rope).
narrative_ontology:human_readable(reconstruction_amendments__fifteenth_amendment, "The Fifteenth Amendment: Prohibition of Racial Disfranchisement").
narrative_ontology:topic_domain(reconstruction_amendments__fifteenth_amendment, "political/legal/voting_rights").

domain_priors:requires_active_enforcement(reconstruction_amendments__fifteenth_amendment).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reconstruction_amendments__fifteenth_amendment, 'a8b66165-b085-46fb-b924-913a11e31946').
narrative_ontology:cs_kernel_codification('a8b66165-b085-46fb-b924-913a11e31946', formalized).
narrative_ontology:cs_authority_grounding('a8b66165-b085-46fb-b924-913a11e31946', lineage).
narrative_ontology:cs_interpretation_layer_present('a8b66165-b085-46fb-b924-913a11e31946').
narrative_ontology:cs_reading_relation('a8b66165-b085-46fb-b924-913a11e31946', reconstruction_amendments__thirteenth_amendment, coexists_with).
narrative_ontology:cs_reading_relation('a8b66165-b085-46fb-b924-913a11e31946', reconstruction_amendments__fourteenth_amendment, coexists_with).
narrative_ontology:cs_axiom('a8b66165-b085-46fb-b924-913a11e31946', foundational, race_cannot_be_basis_for_disfranchisement).
narrative_ontology:cs_axiom_status(race_cannot_be_basis_for_disfranchisement, holdable).
narrative_ontology:cs_axiom_grounding('a8b66165-b085-46fb-b924-913a11e31946', race_cannot_be_basis_for_disfranchisement, deontological).
narrative_ontology:cs_axiom('a8b66165-b085-46fb-b924-913a11e31946', secondary, voting_rights_require_federal_enforcement).
narrative_ontology:cs_axiom_status(voting_rights_require_federal_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('a8b66165-b085-46fb-b924-913a11e31946', voting_rights_require_federal_enforcement, empirically_contingent).
narrative_ontology:cs_reference_frame('a8b66165-b085-46fb-b924-913a11e31946', reconstruction_federal_suffrage_authority).
narrative_ontology:cs_drift_state('a8b66165-b085-46fb-b924-913a11e31946', jim_crow_full_maturity, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('a8b66165-b085-46fb-b924-913a11e31946', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(reconstruction_amendments__fifteenth_amendment, reconstruction_amendments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reconstruction_amendments__fifteenth_amendment, black_male_voters).
narrative_ontology:constraint_beneficiary(reconstruction_amendments__fifteenth_amendment, freedmen_communities).
narrative_ontology:constraint_victim(reconstruction_amendments__fifteenth_amendment, white_supremacist_electoral_regimes).
narrative_ontology:constraint_victim(reconstruction_amendments__fifteenth_amendment, democratic_legitimacy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FREEDMAN / BLACK MALE VOTER (SNARE) — Despite the amendment's formal prohibition, systematic suppression through literacy tests, poll taxes, grandfather clauses, and white primaries traps Black voters in political non-participation. The prohibition is unenforced; the suppression mechanisms are active and escalating. Maximum extraction: the promise of suffrage is negated in practice while the legal mechanism remains dormant.
constraint_indexing:constraint_classification(reconstruction_amendments__fifteenth_amendment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RECONSTRUCTION COALITION (TANGLED ROPE) — Organized Northern Republican faction and freedmen's advocates experience the amendment as both genuine coordination (establishing suffrage rights) and asymmetric extraction (the federal enforcement apparatus is weak; Southern states retain control of implementation). The coalition has agency but constrained capacity — faces Southern resistance, Northern backsliding, and the loss of Republican political will as Reconstruction ends.
constraint_indexing:constraint_classification(reconstruction_amendments__fifteenth_amendment, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FEDERAL LEGISLATIVE AUTHORITY (ROPE) — The amendment establishes pure coordination at the federal level: a uniform suffrage standard across states, enabling democratic participation and reducing sectional conflict through a common franchise rule. The federal authority benefits from standardization and reduces the coordination overhead of managing 50 separate electoral regimes.
constraint_indexing:constraint_classification(reconstruction_amendments__fifteenth_amendment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: SOUTHERN DEMOCRATIC PLANTER REGIME (SNARE) — The Southern white-supremacist electoral regime experiences the amendment as an attempt to extract their political monopoly. However, they retain material and institutional capacity to evade enforcement — they are not trapped. The classification remains snare because their capacity to evade is grounded in regional dominance and federal disengagement, not in legitimate coordination. The amendment suppresses their preferred extraction mechanism (explicit racial law) but they develop substitutes (literacy tests, poll taxes, grandfather clauses) that achieve the same outcome through formally race-neutral language.
constraint_indexing:constraint_classification(reconstruction_amendments__fifteenth_amendment, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: RECONSTRUCTION FEDERAL ENFORCEMENT APPARATUS (SCAFFOLD) — The temporary federal oversight structure (Freedmen's Bureau, occupying forces, federal marshals) sees the amendment as a coordination mechanism with explicit sunset. As Reconstruction ends (circa 1876-1877), the enforcement apparatus withdraws, the theater of federal protection collapses, and the amendment becomes formally on the books but practically unenforced. This is a genuine scaffold: active enforcement with a temporal limit, replaced by state-level mechanisms that prove hostile to the amendment's stated aim.
constraint_indexing:constraint_classification(reconstruction_amendments__fifteenth_amendment, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: THE AMENDMENT AS VESTIGIAL LEGAL TEXT (PITON) — By the early 20th century, the amendment is largely theatrical: the formal prohibition remains but enforcement has atrophied. States use ostensibly race-neutral mechanisms (literacy tests, poll taxes, grandfather clauses, all-white primaries) that accomplish the racial disfranchisement the amendment forbids. The amendment persists as a performative commitment while the actual electoral regime remains segregated. Theater_ratio is high because the legal text is maintained as a legitimating cover for mechanisms that contradict its stated purpose.
constraint_indexing:constraint_classification(reconstruction_amendments__fifteenth_amendment, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL / NATURAL LAW VIEW (MOUNTAIN) — From the civilizational view, the amendment expresses an unchangeable principle: that democratic legitimacy cannot rest on arbitrary exclusion of citizens by race. This principle appears immutable across all democratic regimes; any system that excludes by race is systematically delegitimated. However, this perspective naturalizes what is structurally a contested, enforcement-dependent institutional arrangement. The engine's false-summit detector will identify this classification as a false summit: the 'principle' is upheld in text but negated in practice, revealing that it is not an immutable constraint on political systems but a contingent commitment requiring active enforcement.
constraint_indexing:constraint_classification(reconstruction_amendments__fifteenth_amendment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reconstruction_amendments__fifteenth_amendment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(reconstruction_amendments__fifteenth_amendment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(reconstruction_amendments__fifteenth_amendment, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(reconstruction_amendments__fifteenth_amendment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(reconstruction_amendments__fifteenth_amendment, TR),
    TR >= 0.70.

:- end_tests(reconstruction_amendments__fifteenth_amendment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The amendment establishes a formal prohibition but lacks enforcement mechanisms sufficient to overcome systematic evasion through facially neutral voting restrictions. The Black voter cannot exit the political system (no material alternatives for political representation); the Southern regime can and does evade the amendment's stated purpose. The extraction is substantial — the promise of suffrage is negated in practice — but not maximal because the amendment does create some legal pressure and some enforcement capacity (at least during Reconstruction). The measurement trajectory shows rising extractiveness as enforcement capacity declines and Southern states develop more sophisticated evasion mechanisms. Suppression (0.68): High and rising. Initial suppression at ratification (0.45) reflects the collapse of the slave system and formal abolition of explicit racial voting restrictions. But suppression requirement rises to 0.78 by Jim Crow peak because systematic suppression through literacy tests, poll taxes, grandfather clauses, and all-white primaries is required to maintain racial exclusion despite the amendment's formal prohibition. The rising trajectory reflects an intensification of suppressive mechanisms as alternatives to explicit racial law become necessary. Theater ratio (0.55): Moderate and rising. At ratification, the amendment's enforcement is relatively functional (low theater) — federal marshals actually oversee elections during Reconstruction. As Reconstruction ends and enforcement apparatus withdraws, theater rises because the amendment becomes increasingly performative: the legal text remains but practical enforcement atrophies. By Jim Crow period, theater is high (0.55) because the electoral regime operates through ostensibly race-neutral mechanisms that contradict the amendment's stated purpose.
 *
 * PERSPECTIVAL GAP:
 *   This constraint generates a sharp perspectival divide between the beneficiary's experience and the analytical observer's risk. The freedman experiences maximum extraction despite the amendment's formal prohibition — this is the snare perspective. The Northern Republican coalition experiences mixed coordination and extraction — the tangled rope perspective. The Southern regime experiences the constraint as a temporary obstacle overcome through sophisticated evasion — another snare, but from a different structural position (powerful rather than powerless). The federal enforcement apparatus sees a temporary coordination mechanism with explicit sunset — the scaffold perspective. The amendment as legal text becomes a piton — maintained performatively while its function atrophies. The analytical observer risks a false summit — naturalizing the amendment as an immutable democratic principle rather than seeing it as a contingent institutional arrangement requiring sustained political commitment. The perspectival gap reveals that the amendment's extractiveness is not a fixed property but an outcome of enforcement capacity, political will, and the sophistication of evasion mechanisms.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from the agent's structural position relative to the amendment's extraction flow. The freedman is the formal beneficiary (lower d — should experience low extraction) but the structural victim in practice (actual experience is trapped powerlessness — high d, maximum extraction experienced). This gap is diagnostic: it reveals that the amendment's beneficiary structure does not match its actual enforcement outcome. The Northern coalition has constrained exit and mixed benefits (moderate d). The Southern regime has high exit capacity and gains from evasion (lower d in the formal sense, but the measurement is from their perspective as would-be targets, not beneficiaries). The federal authority benefits from standardization (low d, rope perspective). The analytical observer's d is derived from the universal/civilizational position, treating the amendment as if it were an immutable law — this perspective's directionality is a false summit indicator.
 *
 * MANDATROPHY ANALYSIS:
 *   The Fifteenth Amendment resolves mandatrophy by demonstrating that legal form and structural reality can diverge sharply. The formal classification (rope: a coordinate standard forbidding racial disfranchisement) contradicts the experienced classification (snare: systematic suppression despite formal prohibition). The mandatrophy resolution is not to choose one type but to recognize that the constraint is genuinely tangled-rope: it contains both coordination (establishing a uniform franchise standard) and extraction (the systematic negation of that standard through evasion mechanisms). The amendment's extractiveness is contingent on enforcement capacity, political will, and the sophistication of evasion mechanisms. When enforcement capacity is high (Reconstruction era), extractiveness is lower and theater is low (functional amendment). When enforcement capacity collapses (post-1876), extractiveness rises and theater rises (performative amendment). The constraint is not mislabeled — it is tangled-rope throughout — but the realization of that type changes radically as enforcement mechanisms change.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_capacity_collapse,
    'What causes the dissolution of federal enforcement capacity post-1876, and was it inevitable or politically chosen?',
    'Historical analysis of Republican Party strategic calculations, Northern electoral dynamics, and federal-state power distribution; counterfactual analysis of sustained federal oversight scenarios',
    'If inevitable: the amendment''s extractiveness is inherently high (enforcement cannot persist). If chosen: the extractiveness is politically malleable (sustained federal commitment could reduce extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_capacity_collapse, empirical, 'Causes and contingency of federal enforcement withdrawal post-Reconstruction').

omega_variable(
    formalism_versus_implementation_gap,
    'Is the gap between the amendment''s formal prohibition and its actual enforcement a defect in implementation or a structurally intended design?',
    'Analysis of congressional intent (Reconstruction debates, enforcement clause language); comparison with enforcement mechanisms in other amendments (Thirteenth, Fourteenth); examination of whether Southern states were designed to retain de facto veto power',
    'If structural design: the amendment is a tangled rope with embedded extraction from the outset. If implementation defect: it is ideally a rope that became a snare/piton through failure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formalism_versus_implementation_gap, conceptual, 'Whether the amendment''s implementation gap reflects design or defect').

omega_variable(
    reading_contention_with_fourteenth,
    'Does the Fifteenth Amendment''s specific focus on race, color, and condition of servitude foreclose or coexist with the Fourteenth Amendment''s broader equal protection reading?',
    'Doctrinal analysis of constitutional interpretation across time; examination of whether narrow race-based interpretation of Fifteenth precludes broader equal protection readings; study of cases invoking Fourteenth without Fifteenth for voting rights claims',
    'If forecloses: the readings cannot both hold in one framework. If coexists: the readings are in competition but neither logically eliminates the other.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contention_with_fourteenth, conceptual, 'Structural relationship between Fifteenth Amendment''s race-specific reading and Fourteenth Amendment''s broader equal protection framework').

omega_variable(
    literacy_test_validity_ambiguity,
    'Are literacy tests and poll taxes genuinely race-neutral, or do they constitute a form of racial disfranchisement forbidden by the amendment?',
    'Empirical analysis of literacy test administration (disparate impact rates); correlation between literacy and race in post-Reconstruction period; documentation of when literacy tests were adopted in relation to Black enfranchisement',
    'If genuinely race-neutral: the amendment is satisfied (suppression_requirement can decline). If effectively racial: the amendment''s prohibition is extended to facially neutral mechanisms (suppression_requirement stays high).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(literacy_test_validity_ambiguity, empirical, 'Whether facially race-neutral voting restrictions constitute prohibited racial disfranchisement').

omega_variable(
    beneficiary_identity_lock,
    'To what degree are freed Black voters'' identities constituted through the struggle for suffrage rights, such that identity-based barriers to exit (internalized perceptions of political powerlessness) reinforce material barriers?',
    'Oral history analysis of freedmen''s self-conception; study of cultural narratives around voting and political participation; examination of whether political engagement increases after Voting Rights Act when material barriers are reduced',
    'If identity lock is significant: the constraint''s suppression is both structural and internalized, requiring cultural reframing alongside legal enforcement. If primarily structural: material enforcement suffices.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_identity_lock, empirical, 'Role of identity-based (cognitive/psychological) suppression versus structural suppression in disfranchisement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reconstruction_amendments__fifteenth_amendment, 1870, 1920).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fifteenth_theater_t0_ratification, reconstruction_amendments__fifteenth_amendment, theater_ratio, 0, 0.25).
narrative_ontology:measurement(fifteenth_theater_t5_postreconstruction, reconstruction_amendments__fifteenth_amendment, theater_ratio, 5, 0.42).
narrative_ontology:measurement(fifteenth_theater_t15_jim_crow, reconstruction_amendments__fifteenth_amendment, theater_ratio, 15, 0.55).

% Extraction over time
narrative_ontology:measurement(fifteenth_extract_t0_ratification, reconstruction_amendments__fifteenth_amendment, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(fifteenth_extract_t5_postreconstruction, reconstruction_amendments__fifteenth_amendment, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(fifteenth_extract_t15_jim_crow, reconstruction_amendments__fifteenth_amendment, base_extractiveness, 15, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(fifteenth_suppression_t0_ratification, reconstruction_amendments__fifteenth_amendment, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(fifteenth_suppression_t5_postreconstruction, reconstruction_amendments__fifteenth_amendment, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(fifteenth_suppression_t15_jim_crow_peak, reconstruction_amendments__fifteenth_amendment, suppression_requirement, 15, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reconstruction_amendments__fifteenth_amendment, enforcement_mechanism).
narrative_ontology:affects_constraint(reconstruction_amendments__fifteenth_amendment, thirteenth_amendment).
narrative_ontology:affects_constraint(reconstruction_amendments__fifteenth_amendment, fourteenth_amendment).
narrative_ontology:affects_constraint(reconstruction_amendments__fifteenth_amendment, jim_crow_suppression_regime).

% DUAL FORMULATION NOTE:
% The Fifteenth Amendment is part of the Reconstruction amendments kernel family. It affects the Thirteenth (which it interprets by specifying that political participation is a protected status that cannot be denied on racial grounds) and the Fourteenth (which it specializes to the voting context). It is downstream of the Thirteenth's abolition of slavery and upstream of the Jim Crow suppression regime, which develops facially neutral mechanisms to evade the amendment's stated prohibition. Constraint family: reconstruction_amendments {thirteenth_amendment → fourteenth_amendment, fourteenth_amendment → fifteenth_amendment, fifteenth_amendment → jim_crow_suppression_regime}.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
