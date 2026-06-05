% ============================================================================
% CONSTRAINT STORY: reconstruction_amendments__fourteenth_amendment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reconstruction_amendments__fourteenth_amendment, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: reconstruction_amendments__fourteenth_amendment
 *   human_readable: Fourteenth Amendment: National Citizenship and State Subordination Suppression
 *   domain: political/legal/constitutional
 *
 * SUMMARY:
 *   The Fourteenth Amendment (1868) nationalizes citizenship and prohibits
 *   states from abridging privileges or immunities, denying due process, or
 *   denying equal protection. This constraint resolves a core ambiguity of
 *   the post-Civil War constitutional order: whether the national government
 *   can use positive law to suppress state-level extraction of civil status
 *   through caste. The amendment's mechanism is suppression — it forbids
 *   states from performing certain extractive acts (explicit race
 *   discrimination, denial of equal protection). The beneficiary is the
 *   citizen against their own state; the victim is state autonomy over the
 *   allocation of civil status. This reading instantiates ONE interpretation
 *   of the Reconstruction amendments kernel. The competing readings
 *   (Thirteenth Amendment abolishing slavery everywhere, Fifteenth Amendment
 *   forbidding racial voting discrimination) address overlapping but distinct
 *   mechanisms. The Fourteenth Amendment's reading distinguishes itself by:
 *   (1) focusing on state action rather than private conduct (unlike the
 *   Thirteenth); (2) establishing a general equal protection principle rather
 *   than a specific political right (unlike the Fifteenth); (3) centering
 *   citizenship as a national rather than state matter. The amendment
 *   exhibits all six constraint types from different perspectives: a snare
 *   for trapped citizens trying to exercise forbidden rights despite formal
 *   protection; a tangled rope for regional coalitions seeking to enforce it;
 *   a rope for the federal union coordinating civil rights; a piton for
 *   segregationist state apparatus that complied nominally while extracting
 *   functionally; a scaffold for the civil rights movement treating the
 *   amendment as a temporary support toward internalized norms; and a
 *   false-summit mountain for observers who naturalize equal protection as an
 *   immutable right rather than a political commitment.
 *
 * KEY AGENTS:
 *   - Formerly enslaved and subordinated citizens: Primary beneficiary (powerless/trapped) — the amendment nominally forbids state extraction of caste status but enforcement oscillates with federal political will
 *   - State governments (antebellum extractive apparatus): Primary victim (institutional power) — the amendment strips states of authority to explicitly codify racial subordination, but states retain capacity to extract through facially neutral means
 *   - Federal union and national government: Secondary beneficiary (institutional/arbitrage) — the amendment strengthens federal authority, enables uniform commercial law, coordinates interstate relations
 *   - Regional civic coalitions (Northern merchants, reconstructionist groups, freed people's bureaus): Mixed actors (moderate/constrained) — benefit from national equal protection rules but bear enforcement burden and regional resistance
 *   - Segregationist state apparatus: Institutional actor treating constraint as theater (organized/constrained) — pass facially compliant laws while extracting through non-explicit channels
 *   - Civil rights movement and federal enforcement coalition: Organized actors (organized/constrained) — view amendment as scaffolding toward internalized norms; organized capacity to enforce varies across epochs
 *   - Analytical observer: Civilizational lens (analytical/analytical) — risks naturalizing equal protection as immutable rather than contingent political commitment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reconstruction_amendments__fourteenth_amendment, 0.38).
domain_priors:suppression_score(reconstruction_amendments__fourteenth_amendment, 0.62).
domain_priors:theater_ratio(reconstruction_amendments__fourteenth_amendment, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reconstruction_amendments__fourteenth_amendment, extractiveness, 0.38).
narrative_ontology:constraint_metric(reconstruction_amendments__fourteenth_amendment, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(reconstruction_amendments__fourteenth_amendment, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reconstruction_amendments__fourteenth_amendment, tangled_rope).
narrative_ontology:human_readable(reconstruction_amendments__fourteenth_amendment, "Fourteenth Amendment: National Citizenship and State Subordination Suppression").
narrative_ontology:topic_domain(reconstruction_amendments__fourteenth_amendment, "political/legal/constitutional").

domain_priors:requires_active_enforcement(reconstruction_amendments__fourteenth_amendment).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reconstruction_amendments__fourteenth_amendment, '6b654c30-5577-418c-8caf-d3ae531696f7').
narrative_ontology:cs_kernel_codification('6b654c30-5577-418c-8caf-d3ae531696f7', fixed_text).
narrative_ontology:cs_authority_grounding('6b654c30-5577-418c-8caf-d3ae531696f7', lineage).
narrative_ontology:cs_interpretation_layer_present('6b654c30-5577-418c-8caf-d3ae531696f7').
narrative_ontology:cs_reading_relation('6b654c30-5577-418c-8caf-d3ae531696f7', reconstruction_amendments__thirteenth_amendment, influences).
narrative_ontology:cs_reading_relation('6b654c30-5577-418c-8caf-d3ae531696f7', reconstruction_amendments__fifteenth_amendment, coexists_with).
narrative_ontology:cs_axiom('6b654c30-5577-418c-8caf-d3ae531696f7', foundational, national_citizenship_supremacy).
narrative_ontology:cs_axiom_status(national_citizenship_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('6b654c30-5577-418c-8caf-d3ae531696f7', national_citizenship_supremacy, deontological).
narrative_ontology:cs_axiom('6b654c30-5577-418c-8caf-d3ae531696f7', foundational, equal_protection_state_action_limitation).
narrative_ontology:cs_axiom_status(equal_protection_state_action_limitation, holdable).
narrative_ontology:cs_axiom_grounding('6b654c30-5577-418c-8caf-d3ae531696f7', equal_protection_state_action_limitation, conventional).
narrative_ontology:cs_reference_frame('6b654c30-5577-418c-8caf-d3ae531696f7', national_equal_citizenship).
narrative_ontology:cs_drift_state('6b654c30-5577-418c-8caf-d3ae531696f7', post_slaughterhouse_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('6b654c30-5577-418c-8caf-d3ae531696f7', '').
narrative_ontology:cs_kernel_id(reconstruction_amendments__fourteenth_amendment, reconstruction_amendments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reconstruction_amendments__fourteenth_amendment, formerly_enslaved_citizens).
narrative_ontology:constraint_beneficiary(reconstruction_amendments__fourteenth_amendment, racial_minorities).
narrative_ontology:constraint_beneficiary(reconstruction_amendments__fourteenth_amendment, disenfranchised_groups).
narrative_ontology:constraint_victim(reconstruction_amendments__fourteenth_amendment, state_autonomy_over_civil_status).
narrative_ontology:constraint_victim(reconstruction_amendments__fourteenth_amendment, antebellum_power_structure).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FREEDMEN AND SUBORDINATED CITIZENS (SNARE) — Trapped within state borders where suppression via law is the mechanism (Black Codes, Jim Crow, segregation statutes). The Fourteenth Amendment forbids state extraction of caste status, but enforcement requires federal power that oscillates with political will. Biographical horizon: within a single life, the constraint may be honored or gutted depending on enforcement. Maximum experienced suppression due to trapped exit option and powerless position.
constraint_indexing:constraint_classification(reconstruction_amendments__fourteenth_amendment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REGIONAL CIVIC COALITIONS (TANGLED ROPE) — Groups (Northern merchants, reconstructionist coalition, freed people's bureaus) benefit from national citizenship and equal protection rules (enables commercial integration, civic participation) but face significant costs through enforcement burden and regional resistance. Exit is constrained by political pressure and resource limitations. Mixed coordination (enabling interstate commerce and civic participation) and extraction (enforcement labor, political risk).
constraint_indexing:constraint_classification(reconstruction_amendments__fourteenth_amendment, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FEDERAL UNION (ROPE) — The federal government experiences the Fourteenth Amendment as coordination: nationalizing citizenship and equal protection enables uniform commercial law, prevent race-based extraction of labor, and unify the polity. The amendment strengthens federal authority while coordinating interstate relations. Net beneficiary position with arbitrage: federal actors can shift enforcement intensity without fundamental threat to their power.
constraint_indexing:constraint_classification(reconstruction_amendments__fourteenth_amendment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: SEGREGATIONIST STATE APPARATUS (PITON) — Post-Reconstruction Southern state governments treated the Fourteenth Amendment's nominal protections as theater: passing statutes (Black Codes, Jim Crow laws) that formally complied with the letter while violating the spirit, relying on federal enforcement capacity being insufficient or politically unmotivated. The constraint persisted as a performative prohibition on explicitly caste-based law while the functional extraction (segregation enforcement, labor subordination) continued beneath nominal legal compliance. Theater ratio high (0.58+) because the constraint was symbolically present but functionally degraded for decades.
constraint_indexing:constraint_classification(reconstruction_amendments__fourteenth_amendment, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CIVIL RIGHTS MOVEMENT AND ENFORCEMENT COALITION (SCAFFOLD) — Organized actors (NAACP, federal agencies, progressive courts) view the Fourteenth Amendment as a temporary support structure during the transition from state-level caste to federal equal protection norms. The constraint has a sunset embedded in its structure: as norms internalize (federal courts enforce, states comply, Jim Crow erodes), the active enforcement mechanism becomes less necessary. The coalition sees the amendment not as permanent extraction but as scaffolding toward a stable norm-based equal protection regime. Sunset logic: estimated 50-70 years from passage for norms to stabilize (1868-1950s), with subsequent reinforcement cycles (1960s-1970s).
constraint_indexing:constraint_classification(reconstruction_amendments__fourteenth_amendment, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational horizon, the Fourteenth Amendment appears to codify a natural law of human dignity: the inherent rights to due process and equal protection cannot be abridged by any entity, including states. This perspective treats the amendment as discovering immutable principles rather than constructing a coordination mechanism. The mountain classification naturalizes the constraint, obscuring that equal protection itself is a constructed political commitment with competing readings (textualist, living constitutionalist, originalist, etc.). The false summit detector will flag this: identifiable beneficiaries exist (formerly enslaved people, minorities), and extractiveness data shows this is a political constraint, not a law of nature.
constraint_indexing:constraint_classification(reconstruction_amendments__fourteenth_amendment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reconstruction_amendments__fourteenth_amendment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(reconstruction_amendments__fourteenth_amendment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(reconstruction_amendments__fourteenth_amendment, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(reconstruction_amendments__fourteenth_amendment, TR),
    TR >= 0.70.

:- end_tests(reconstruction_amendments__fourteenth_amendment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The Fourteenth Amendment's core function is to suppress state extraction of civil status through explicit caste law (Thirteenth handled slavery; Fifteenth handled voting). The base extractiveness reflects that the amendment eliminates a major extraction mechanism (explicit state caste law) but permits alternative extraction channels (facially neutral laws, administrative discretion, enforcement variance). The beneficiaries (formerly enslaved, subordinated citizens) experience significant extraction suppression at the nominal level, but the victim (state autonomy) is partially real — states do lose authority, but retain capacity for functional extraction. The constraint is not a pure snare (which would have ε > 0.46, suppression > 0.60) nor a pure rope (which would have ε < 0.35, beneficiaries and no victims). It is tangled: genuine suppression of caste law (coordination function) combined with persistent capacity for state extraction through non-explicit means (extraction function). Suppression (0.62): High. This is deliberately high because the amendment's core mechanism IS suppression — forbidding states from performing certain acts. The constitutional text explicitly restrains state power. The empirical history shows that enforcement of this suppression oscillated (high 1868-1876, low 1877-1963, high 1964-1970, declining 1980-2013), but the legal restraint itself was always nominally present. The 0.62 value reflects that the constraint had real suppressive force (Black Codes were struck down, Jim Crow laws faced federal courts) but that enforcement capacity was never sufficient to eliminate functional state-level extraction. Theater ratio (0.58): Moderate-high. The amendment's theater arises from the gap between formal protection and functional extraction. After Slaughterhouse (1873), the Privileges or Immunities Clause was gutted, and the constraint operated primarily through Due Process and Equal Protection clauses, which permitted facially neutral state law. Segregationist states exploited this gap: Black Codes were replaced by Jim Crow laws that were facially race-neutral but functionally subordinating (separate-but-equal, literacy tests applied unequally, etc.). The constraint persisted as a performative prohibition on explicit discrimination while functional extraction continued. The theater ratio's historical trajectory (0.25 → 0.72 → 0.58) reflects this: initially low (1868) because federal enforcement was active and state explicit subordination was suppressed; rising sharply (1878-1888) as Slaughterhouse took hold and states learned to extract through neutral-appearing law; declining somewhat (1968) as strict scrutiny and civil rights enforcement created pressure against even facially neutral discrimination, though the underlying functional extraction persisted.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gaps in this constraint are among the deepest in the constitutional order. The freedmen and trapped citizens experience the amendment as a snare: formally protected but with no exit option from the jurisdiction applying discriminatory law, facing oscillating federal enforcement. Regional moderates see tangled rope: coordinating national commerce while bearing enforcement burden. Federal institutions experience rope: coordinating the union. Segregationist states experience piton: the constraint is nominally present but increasingly performative as they learn to extract through neutral law. The civil rights coalition experiences scaffold: the amendment as temporary support toward norms that will eventually eliminate the need for active enforcement. The analytical observer risks mountain: naturalizing equal protection as a timeless principle. The perspectival gaps reveal that no single view captures the constraint's structure. The gap between the snare (victim's view) and the rope (federal view) is the most diagnostically significant: the federal government experiences the amendment as coordinating the nation while the trapped citizens experience the same amendment as providing nominal protection without functional escape. This is the classic gap between institutional beneficiary (federal authority) and powerless victim (the citizens the amendment purports to protect).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values for each perspective are derived from the structural relationship to extraction flow. Trapped citizens (d ≈ 0.95): full targets of the nominally suppressed extraction, but extraction persists through non-explicit channels; they bear the maximum cost of oscillating enforcement. Segregationist states (d ≈ 0.05): nominal victims (lose authority to explicitly extract) but actual beneficiaries (retain capacity to extract through facially neutral means); low directionality because the constraint's practical effect is modest. Federal union (d ≈ 0.15): beneficiary, experiences coordinate function, arbitrage exit. Regional moderates (d ≈ 0.65): both bears costs (enforcement labor) and benefits (commercial coordination); symmetric enough to show moderate directionality. Civil rights coalition (d ≈ 0.55): organized beneficiaries (can shift enforcement), constrained exit. The chi formula (χ = ε × f(d) × σ(S)) applies: trapped citizens with high d produce high f(d) (sigmoid ≈ 1.42), amplifying experienced extraction; institutional beneficiaries with low d produce negative f(d) (sigmoid ≈ -0.12), negating extraction they bear; at national scope (σ = 1.0), the scope modifier is neutral. The piton perspective (high theater, low functional extraction despite high nominal suppression) follows from treating the constraint as performative — the amendment persists through institutional inertia rather than active function.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that the Fourteenth Amendment simultaneously exhibits genuine suppression (of state-level caste law) and genuine extraction (state capacity to extract through facially neutral means). The mandatrophy is NOT 'is it suppression or extraction?' but 'which mechanisms does each reading emphasize?' The snare reading emphasizes that extraction persists despite nominal suppression (functional caste through Jim Crow and facially neutral law). The rope reading emphasizes the coordination function (national commerce, equal civil status). The piton reading emphasizes that the formal suppression became theater as states learned to extract through neutral means. The tangled rope reading emphasizes both the genuine suppression of explicit subordination AND the real extraction through non-explicit channels. The false-summit mountain reading naturalizes equal protection, obscuring its contingent political construction. The mandatrophy is resolved by recognizing that ε = 0.38 captures a real mixed constraint: the amendment genuinely suppresses some extraction mechanisms (explicit caste law) while permitting others (facially neutral extraction). This is the definition of tangled rope — genuine coordination (equal protection enables commerce, civic participation) combined with genuine asymmetric extraction (non-explicit state mechanisms permit functional subordination). The oscillating measurements (extractiveness rising to 0.68 in 1888 as states learned neutral-law extraction, declining to 0.45 by 1968 as strict scrutiny and federal enforcement tightened) show that the constraint's functional profile changed, but its structural type (tangled rope) remained constant across the interval.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    privileges_or_immunities_collapse,
    'Does the Slaughterhouse Cases'' gutting of the Privileges or Immunities Clause represent a legitimate interpretive boundary or an extractive reversion masquerading as constitutional law?',
    'Textual analysis of original intent vs. Slaughterhouse rationale; historical record of legislative intent in Section 1; subsequent jurisprudence attempting to recover the clause (McDonald v. Chicago, recent originalist revival)',
    'If legitimate boundary: the Fourteenth Amendment''s core mechanism for protecting citizenship was never fully activated, and the constraint is weaker than its text suggests. If extractive reversion: the Slaughterhouse decision represents a critical foreclosure point where state autonomy over civil status was restored to states, undermining the amendment''s central purpose.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(privileges_or_immunities_collapse, conceptual, 'Whether Slaughterhouse gutting of Privileges or Immunities Clause is legitimate or extractive reversion').

omega_variable(
    due_process_incorporation_mechanism,
    'Are the Fourteenth Amendment''s due process protections primarily negative (preventing state action) or positive (requiring state affirmative duties)?',
    'Jurisprudential analysis of substantive vs. procedural due process; comparison of state action doctrine across different rights (voting, property, association); historical record of amendment drafters'' intent regarding affirmative obligations',
    'If primarily negative: the constraint suppresses explicit caste law but permits state neglect of equal protection (constrains only overt discrimination, not systemic exclusion). If affirmative: the constraint requires active state intervention to equalize conditions (higher extractiveness from states but stronger beneficiary protection).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(due_process_incorporation_mechanism, conceptual, 'Whether Fourteenth Amendment''s due process is primarily negative or affirmative').

omega_variable(
    equal_protection_strict_scrutiny_threshold,
    'Does strict scrutiny applied to race-based classifications fundamentally eliminate state capacity for race-conscious law, or does it merely raise the justification bar while preserving state discretion?',
    'Doctrinal analysis of strict scrutiny success rates; comparison of race-based vs. race-neutral policies that achieve similar subordination effects; examination of whether race-neutral proxies (class, geography, school district funding) permit functional re-segregation',
    'If eliminates: the constraint prevents caste extraction at the expense of state flexibility for remedial race-conscious policies (affirmative action, redistricting for majority-minority representation). If preserves discretion: state actors can achieve subordination through facially neutral means that strict scrutiny permits (functional re-segregation through housing law, school funding, criminal justice).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equal_protection_strict_scrutiny_threshold, empirical, 'Whether strict scrutiny eliminates state race-consciousness or merely raises the bar').

omega_variable(
    federal_enforcement_capacity_drift,
    'What explains the oscillation between high (1868-1876, 1964-1970) and low (1877-1963, 1980-2013) federal enforcement of the Fourteenth Amendment?',
    'Political economy analysis of federal enforcement will; correlation with electoral coalitions controlling Congress and Presidency; measurement of federal prosecutions under 42 USC §1983, §242; comparison of state-level civil rights complaints vs. federal docket capacity',
    'If oscillation reflects constitutional constraints: Fourteenth Amendment enforcement depends on political will external to its structure (high suppression from the victim''s perspective). If oscillation reflects institutional design: the amendment''s enforcement mechanism was always engineered to rise and fall with political factions (piton perspective validated).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(federal_enforcement_capacity_drift, empirical, 'What explains oscillating enforcement of the Fourteenth Amendment').

omega_variable(
    state_sovereignty_extraction_boundary,
    'Does the Fourteenth Amendment eliminate state extraction capacity over civil status, or does it permit states to extract through non-explicit channels (facially neutral law, administrative discretion, enforcement variance)?',
    'Comparative legal analysis of explicit vs. facially neutral subordination mechanisms; measurement of disparate impact across racial groups in criminal justice, housing, voting, education under nominally equal laws; historical record of state tactics post-Reconstruction',
    'If permits non-explicit extraction: the constraint only suppresses overt caste law; state extraction migrates to neutral-appearing mechanisms (selectivity in law enforcement, school funding formulas, redlining through lending criteria). If eliminates: states truly cannot extract on the basis of race, color, or previous condition of servitude.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_sovereignty_extraction_boundary, empirical, 'Whether Fourteenth Amendment eliminates state extraction or permits non-explicit channels').

omega_variable(
    reading_boundary_thirteenth_amendment,
    'Does this Fourteenth Amendment reading foreclose or coexist with the Thirteenth Amendment''s direct prohibition on slavery and involuntary servitude?',
    'Textual analysis of each amendment''s scope (Thirteenth binds private actors; Fourteenth binds state action); historical record of post-Reconstruction debates about which amendment should bear enforcement weight; jurisprudential treatment of private discrimination under equal protection vs. Thirteenth Amendment contract doctrine',
    'If forecloses: accepting the Fourteenth''s state-action limitation forecloses the Thirteenth''s private-actor prohibition (the private/public distinction becomes central). If coexists: both readings are live — some actors appeal to Thirteenth to reach private conduct, others to Fourteenth''s state-action bar. Current doctrine reflects coexistence with the state-action doctrine privileged in equal protection law.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_boundary_thirteenth_amendment, conceptual, 'Reading boundary between Fourteenth and Thirteenth Amendment').

omega_variable(
    reading_boundary_fifteenth_amendment,
    'Does the Fourteenth Amendment''s equal protection bar already forbid racial discrimination in voting, or does the Fifteenth Amendment''s explicit vote prohibition represent a separate guarantee with distinct enforcement pathways?',
    'Textualist analysis of whether voting is encompassed in ''privileges or immunities'' or ''equal protection''; historical record of whether Fourteenth drafters intended to protect voting; jurisprudential treatment of voting rights under Fourteenth vs. Fifteenth (compare Harper v. Virginia Board of Elections [Fourteenth applied to poll tax] vs. City of Boerne [Fifteenth not pre-emptive of state autonomy over voting])',
    'If Fourteenth already forbids racial voting discrimination: the Fifteenth is redundant and represents an abundance of caution (coexists as reinforcement). If Fifteenth supplies distinct guarantee: the Fourteenth''s equal protection does not inherently protect voting, and the Fifteenth''s separate existence signals a narrower Fourteenth reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_boundary_fifteenth_amendment, conceptual, 'Reading boundary between Fourteenth and Fifteenth Amendment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reconstruction_amendments__fourteenth_amendment, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ra14_theater_1868, reconstruction_amendments__fourteenth_amendment, theater_ratio, 0, 0.25).
narrative_ontology:measurement(ra14_theater_1878, reconstruction_amendments__fourteenth_amendment, theater_ratio, 10, 0.55).
narrative_ontology:measurement(ra14_theater_1888, reconstruction_amendments__fourteenth_amendment, theater_ratio, 20, 0.72).
narrative_ontology:measurement(ra14_theater_1968, reconstruction_amendments__fourteenth_amendment, theater_ratio, 100, 0.58).

% Extraction over time
narrative_ontology:measurement(ra14_extract_1868, reconstruction_amendments__fourteenth_amendment, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(ra14_extract_1878, reconstruction_amendments__fourteenth_amendment, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(ra14_extract_1888, reconstruction_amendments__fourteenth_amendment, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(ra14_extract_1968, reconstruction_amendments__fourteenth_amendment, base_extractiveness, 100, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(ra14_suppress_1868, reconstruction_amendments__fourteenth_amendment, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(ra14_suppress_1878, reconstruction_amendments__fourteenth_amendment, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(ra14_suppress_1888, reconstruction_amendments__fourteenth_amendment, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(ra14_suppress_1968, reconstruction_amendments__fourteenth_amendment, suppression_requirement, 100, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reconstruction_amendments__fourteenth_amendment, enforcement_mechanism).
narrative_ontology:affects_constraint(reconstruction_amendments__fourteenth_amendment, reconstruction_amendments__thirteenth_amendment).
narrative_ontology:affects_constraint(reconstruction_amendments__fourteenth_amendment, reconstruction_amendments__fifteenth_amendment).
narrative_ontology:affects_constraint(reconstruction_amendments__fourteenth_amendment, jim_crow_state_subordination).
narrative_ontology:affects_constraint(reconstruction_amendments__fourteenth_amendment, equal_protection_strict_scrutiny).

% DUAL FORMULATION NOTE:
% The Fourteenth Amendment reading is one of three Reconstruction amendments, each with distinct constraint structure. The Thirteenth Amendment (abolishing slavery) targets private and state actors directly, with high suppression of the slavery mechanism (ε ≈ 0.05-0.15, mountain or rope). The Fifteenth Amendment (voting protection) targets state racial discrimination in voting specifically, with moderate suppression of a narrower extraction mechanism. The Fourteenth Amendment reading (this story) targets state-level civil status extraction broadly, permitting state actors to achieve functional extraction through facially neutral means. The three constraints are linked: the Fourteenth was drafted partly to address gaps in Thirteenth and Fifteenth enforcement, and all three target overlapping but distinct extraction mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reconstruction_amendments__fourteenth_amendment, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
