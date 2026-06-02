% ============================================================================
% CONSTRAINT STORY: fourteenth_amendment_equal_protection__anti_caste_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fourteenth_amendment_equal_protection__anti_caste_reading, []).

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
 *   constraint_id: fourteenth_amendment_equal_protection__anti_caste_reading
 *   human_readable: Fourteenth Amendment Equal Protection (Anti-Caste Reading): Affirmative State Dismantling of Structural Hierarchy
 *   domain: constitutional_law/civil_rights/political_philosophy
 *
 * SUMMARY:
 *   The anti-caste reading of the Fourteenth Amendment posits that equal
 *   protection requires active state dismantling of racial, gender, and
 *   status hierarchy through corrective action — affirmative action, voting
 *   rights enforcement, targeted resource reallocation, and institutional
 *   integration. This reading is one interpretation of the Fourteenth
 *   Amendment's core commitment, competing with a formal equality reading
 *   that treats equal protection as requiring colorblind, gender-neutral law
 *   without reference to subordination's structural effects. The anti-caste
 *   reading makes subordination itself the constitutional concern and
 *   legitimates state action aimed not merely at removing explicit barriers
 *   but at affirmatively reconstructing access and opportunity across
 *   institutions. The constraint exhibits tangled rope structure: it
 *   coordinates genuine access redistribution (coordination function) while
 *   extracting significant costs from multiple agents — subordinated groups
 *   bear the burden of remedial integration, hierarchy incumbents lose prior
 *   advantage, and the state's enforcement capacity is consumed in sustained
 *   remedial oversight. Theater has increased over time as formal
 *   institutions perform compliance with anti-caste requirements while
 *   simultaneously maintaining structures of privilege, creating a gap
 *   between the remedial rhetoric and the structural reality. Suppression has
 *   intensified as political backlash against affirmative action has forced
 *   remedial advocates to work within narrowing legal constraints,
 *   concentrating extraction pressure on enforcement mechanisms.
 *
 * KEY AGENTS:
 *   - Subordinated racial, gender, and status groups (powerless/trapped) — historically excluded from access; the reading makes their subordination visible as a state concern but also enrolls them in an intensive and contested remedial program
 *   - State remedial apparatus (institutional/arbitrage) — deploys enforcement to redistribute access; experiences the constraint as legitimate corrective action; maintains authority to define and implement remediation
 *   - Hierarchy incumbents, legacy advantage beneficiaries (powerful/mobile) — lose prior access advantage through remedial requirements; can exit through institutional alternatives (private schools, private employers, geographic mobility); experience extraction but not entrapment
 *   - Remedial beneficiaries in mixed position (moderate/constrained) — gain access through state action while bearing integration burden, potential stigma, and institutional hostility; constrained because refusing remediation means forgoing access
 *   - State enforcement infrastructure (institutional, treated as victim) — consumed by sustained remedial oversight, litigation, compliance monitoring, and ongoing defense against challenge; trapped because reducing enforcement allows prior hierarchies to reconstitute
 *   - Civil rights coalitions and organized movements (organized/constrained) — push for anti-caste enforcement; constrained because movements cannot withdraw without allowing hierarchy reconstitution; see remediation as transitional/sunsetted toward genuine structural change
 *   - Formal equality institutional legacy (institutional/arbitrage) — persists as a piton, performatively complying with anti-caste requirements while maintaining formal equality framing; theater-heavy because it absorbs remedial demands without structural restructuring
 *   - Analytical observer at universal scope (analytical/analytical) — risks naturalizing the anti-caste reading as transcendent principle rather than contingent interpretation; false summit candidate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fourteenth_amendment_equal_protection__anti_caste_reading, 0.58).
domain_priors:suppression_score(fourteenth_amendment_equal_protection__anti_caste_reading, 0.72).
domain_priors:theater_ratio(fourteenth_amendment_equal_protection__anti_caste_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fourteenth_amendment_equal_protection__anti_caste_reading, tangled_rope).
narrative_ontology:human_readable(fourteenth_amendment_equal_protection__anti_caste_reading, "Fourteenth Amendment Equal Protection (Anti-Caste Reading): Affirmative State Dismantling of Structural Hierarchy").
narrative_ontology:topic_domain(fourteenth_amendment_equal_protection__anti_caste_reading, "constitutional_law/civil_rights/political_philosophy").

domain_priors:requires_active_enforcement(fourteenth_amendment_equal_protection__anti_caste_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fourteenth_amendment_equal_protection__anti_caste_reading, 'bd6f04d9-78b3-44dd-a7e7-cd9a1eb9d477').
narrative_ontology:cs_kernel_codification('bd6f04d9-78b3-44dd-a7e7-cd9a1eb9d477', formalized).
narrative_ontology:cs_authority_grounding('bd6f04d9-78b3-44dd-a7e7-cd9a1eb9d477', lineage).
narrative_ontology:cs_interpretation_layer_present('bd6f04d9-78b3-44dd-a7e7-cd9a1eb9d477').
narrative_ontology:cs_reading_relation('bd6f04d9-78b3-44dd-a7e7-cd9a1eb9d477', fourteenth_amendment_equal_protection__formal_equality_reading, coexists_with).
narrative_ontology:cs_axiom('bd6f04d9-78b3-44dd-a7e7-cd9a1eb9d477', foundational, subordination_is_state_concern).
narrative_ontology:cs_axiom_status(subordination_is_state_concern, holdable).
narrative_ontology:cs_axiom_grounding('bd6f04d9-78b3-44dd-a7e7-cd9a1eb9d477', subordination_is_state_concern, deontological).
narrative_ontology:cs_axiom('bd6f04d9-78b3-44dd-a7e7-cd9a1eb9d477', foundational, affirmative_action_is_constitutionally_legitimate).
narrative_ontology:cs_axiom_status(affirmative_action_is_constitutionally_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('bd6f04d9-78b3-44dd-a7e7-cd9a1eb9d477', affirmative_action_is_constitutionally_legitimate, deontological).
narrative_ontology:cs_reference_frame('bd6f04d9-78b3-44dd-a7e7-cd9a1eb9d477', constitutional_remedial_obligation).
narrative_ontology:cs_drift_state('bd6f04d9-78b3-44dd-a7e7-cd9a1eb9d477', contemporary_judicial_narrowing, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('bd6f04d9-78b3-44dd-a7e7-cd9a1eb9d477', '').
narrative_ontology:cs_kernel_id(fourteenth_amendment_equal_protection__anti_caste_reading, fourteenth_amendment_equal_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__anti_caste_reading, subordinated_racial_groups).
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__anti_caste_reading, gender_subordinated_groups).
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__anti_caste_reading, status_hierarchy_targets).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__anti_caste_reading, beneficiaries_of_prior_regime).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__anti_caste_reading, state_enforcement_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HISTORICALLY SUBORDINATED GROUP (SNARE) — Locked into caste position by prior regime; trapped by structural exclusion from economic, political, and social participation. The anti-caste reading makes the subordination itself visible as a state concern requiring active dismantling. The target experiences the constraint as the only mechanism that can dismantle the pre-existing extraction regime. But the reading also imposes a new constraint: subordinated groups must participate in the state's remedial program, must endure the political backlash, and must navigate the performative aspects of remedial action. Experienced extractiveness is high because exit from the remedial program (refusing affirmative action, opting out) means returning to the prior extraction regime.
constraint_indexing:constraint_classification(fourteenth_amendment_equal_protection__anti_caste_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REMEDIAL BENEFICIARY IN MIXED STRUCTURAL POSITION (TANGLED ROPE) — Some individuals from historically subordinated groups gain access to education, employment, and political participation through state remedial action. These individuals benefit from the affirmative state action while also bearing some costs: potential stigma, pressure to 'prove merit,' exposure to hostile institutional environments (colleges, workplaces), and the psychological burden of visibility in previously exclusive spaces. The constraint coordinates resource allocation (genuine affirmative action opens pathways) while extracting a cost (institutional integration that normalizes rather than dismantles the hierarchy itself). Constrained exit because refusing remedial benefits means forgoing access, but accepting them means enduring the integration burden.
constraint_indexing:constraint_classification(fourteenth_amendment_equal_protection__anti_caste_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE STATE AS REMEDIAL ENFORCEMENT AGENT (ROPE) — From the state's perspective, the anti-caste reading is a coordination mechanism: the state deploys its enforcement power to coordinate the redistribution of access across institutions (schools, employers, public agencies). The state experiences the constraint as legitimate authority — using state power to correct for prior state failure (enforcing segregation, tolerating private discrimination). The coordination function is genuine: without state action, segregation and subordination would persist. Arbitrage exit because the state can choose different approaches to remediation (colorblind vs. colorconscious, targeted vs. universal) and can exit the remedial framework by returning to formal equality. For the state, extractiveness is low to negative because the remedial framework legitimates state authority.
constraint_indexing:constraint_classification(fourteenth_amendment_equal_protection__anti_caste_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: BENEFICIARIES OF PRIOR REGIME / HIERARCHY INCUMBENTS (TANGLED ROPE, LOSING POSITION) — Groups that benefited from segregation, gender subordination, or caste-like hierarchies experience the anti-caste reading as extraction: affirmative action reduces their access, diversity requirements constrain their hiring preferences, and caste-based remediation upends the prior distribution of prestige and resources. However, this perspective is NOT pure snare because hierarchy incumbents retain significant mobility and power. They can exit (move to private institutions less subject to affirmative action requirements, relocate to lower-enforcement jurisdictions, invest in private alternatives). The constraint extracts from their prior advantage but does not trap them. The classification is tangled_rope rather than snare because: (a) the constraint also coordinates a new institutional order (integration reduces some transaction costs, creates new market opportunities for diverse workforce management); (b) hierarchy incumbents have agency and exit options; (c) the extraction is constrained to the enforcement domain, not total.
constraint_indexing:constraint_classification(fourteenth_amendment_equal_protection__anti_caste_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: STATE ENFORCEMENT CAPACITY AS VICTIM OF THE REMEDIAL PROGRAM (SNARE) — The anti-caste reading requires sustained, intensive state machinery to implement remediation: courts review hiring and admissions; agencies monitor compliance; institutions collect demographic data and report; legal processes challenge remedial measures. This machinery extracts from enforcement capacity — the state must continually defend remedial action against challenge, must maintain oversight infrastructure, and must navigate the boundary between remediation and overreach. The enforcement apparatus becomes trapped in the remedial program: withdrawing or reducing enforcement allows prior hierarchies to reconstitute. The constraint suppresses the question of whether the state can actually sustain this level of enforcement long-term. Extractiveness is high because the machinery persists regardless of efficacy, driven by the reading's legitimacy rather than by measured outcomes.
constraint_indexing:constraint_classification(fourteenth_amendment_equal_protection__anti_caste_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 6: CIVIL RIGHTS COALITION AND ORGANIZED MOVEMENTS (SCAFFOLD) — From the perspective of civil rights organizations, voting rights coalitions, labor unions, and other organized agents pushing for anti-caste remediation, the constraint is temporary and has a sunset clause. The reading's success would create conditions for its own transformation: if subordination is genuinely dismantled (rather than simply managed), the need for active state remediation decreases. Organized movements see affirmative action as a transition mechanism toward a future state where race, gender, and caste are no longer predictive of access and opportunity. Constrained exit because movements cannot simply withdraw from pursuing remediation — doing so would allow prior hierarchies to reconstitute — but they can envision a world where the remedial constraint is no longer necessary. Theater is relatively low because organized movements are focused on material outcomes (changed access patterns, shifted resource distribution) rather than on the symbolic validation of remediation itself.
constraint_indexing:constraint_classification(fourteenth_amendment_equal_protection__anti_caste_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: THE FORMAL EQUALITY INSTITUTIONAL LEGACY (PITON) — The prior regime of 'colorblind' formal equality and gender-neutral law persists as institutional structure and cultural narrative despite the anti-caste reading's challenge. Courts, legislatures, and institutions continue to operate according to formal equality principles while formally acknowledging anti-caste remediation. This institutional inertia — the persistence of formal equality framing as the dominant legitimacy narrative even after the anti-caste reading claims authority — is the piton: a degraded but residually functional structure that maintains itself through procedural ritual rather than through genuine operational logic. The piton's theater is high because institutions perform formal equality compliance (colorblind hiring, gender-neutral promotion criteria) while simultaneously implementing anti-caste remediation, creating performative contradiction. The piton extracts from the remedial program by diluting its force — the formal equality institution absorbs the anti-caste reading without fundamentally restructuring the hierarchy.
constraint_indexing:constraint_classification(fourteenth_amendment_equal_protection__anti_caste_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 8: THE ANALYTICAL OBSERVER AT UNIVERSAL SCOPE (MOUNTAIN — FALSE SUMMIT CANDIDATE) — From a civilizational/universal analytical perspective, there is a risk of naturalizing the anti-caste reading as a transcendent truth ('equality requires active remediation') that appears as a fixed law of justice rather than as one historically contingent reading of the Fourteenth Amendment. This perspective treats the anti-caste reading as an immutable principle — part of the deep structure of constitutional legitimacy itself. However, this classification is likely a false summit: the reading is a contested interpretation of a kernel (the Fourteenth Amendment), not a natural law. Beneficiaries of the anti-caste reading (subordinated groups, remedial advocates) and beneficiaries of the competing formal equality reading (hierarchy incumbents, minimalist state actors) both benefit from their respective readings being treated as natural law rather than as contingent political choices.
constraint_indexing:constraint_classification(fourteenth_amendment_equal_protection__anti_caste_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fourteenth_amendment_equal_protection__anti_caste_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fourteenth_amendment_equal_protection__anti_caste_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fourteenth_amendment_equal_protection__anti_caste_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(fourteenth_amendment_equal_protection__anti_caste_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(fourteenth_amendment_equal_protection__anti_caste_reading, TR),
    TR >= 0.70.

:- end_tests(fourteenth_amendment_equal_protection__anti_caste_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): The anti-caste reading legitimates active state remediation aimed at dismantling hierarchy, producing tangled rope structure. The value reflects that genuine coordination (access redistribution, institutional integration) coexists with significant extraction. Extractiveness is not higher (0.65+) because the reading explicitly aims at dismantling rather than merely managing subordination, and because subordinated groups have some agency and exit-aversion (the remedial program is their only realistic path to access despite its burdens). Extractiveness is not lower (0.45) because the reading extracts substantially from hierarchy incumbents, requires sustained state enforcement machinery, and embeds remedial beneficiaries in hostile institutional environments. The intermediate value reflects the genuine mixed character — real coordination paired with real extraction. Suppression (0.72): High. Multiple mechanisms suppress alternatives to anti-caste remediation: legal doctrines treating formal equality as the default; political backlash that constrains remedial scope; institutional capacity limits that prevent sustained enforcement; the threat that withdrawal of remediation will allow prior hierarchies to reconstitute. Subordinated groups are suppressed from exit (remediation is their only institutional pathway, but it is contested and often hostile). Hierarchy incumbents are suppressed from openly defending the prior regime (remediation is legitimized as corrective justice, making explicit hierarchy defense costly). The enforcement apparatus is suppressed from reducing intensity (doing so signals failure and allows reconstitution). Theater ratio (0.48): Moderate, increasing over time. Early remedial action (1960s) had relatively lower theater because institutions were undergoing genuine structural change (integration was not yet normalized, remedial programs were new). Contemporary theater has risen because institutions now perform compliance with anti-caste requirements while maintaining structures that reproduce hierarchy — diversity hiring with wage gaps, minority student recruitment with degree completion gaps, affirmative action alongside school funding inequality. The gap between remedial rhetoric and structural reality is the theater.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the perspectival variation characteristic of tangled rope. The formally subordinated groups see the anti-caste reading as the only mechanism that makes their extraction visible to the state; they experience it as both escape and new entrapment (snare perspective). Hierarchy incumbents see it as extraction from their prior advantage; they retain agency through institutional alternatives (tangled rope perspective, losing position). The state sees it as legitimate corrective authority justified by prior state failure in enforcing segregation (rope perspective). The enforcement apparatus experiences the constraint as trap — mandated to sustain remediation indefinitely or risk reconstitution of prior regime (snare perspective from the state's own infrastructure). Organized movements see remediation as transitional toward genuine structural change, with a sunset clause (scaffold perspective). The formal equality institutional legacy persists as piton — performatively complying while maintaining prior framing. The analytical observer at universal scope risks naturalizing the reading as transcendent principle (false summit mountain). The gap between the rope perspective (state's legitimate remediation) and the snare perspective (trapped subordinated groups) reveals that the reading legitimates state action but does not dismantle the underlying structural constraints that make state action necessary. The reading makes subordination visible but does not eliminate it — it institutionalizes the response to subordination, which can become its own extraction mechanism if the remedial program is captured or degraded.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values for each perspective are computed from the agent's structural position relative to the constraint. Subordinated groups occupy d ≈ 0.95 (nearly total extraction target) because they are victims of the prior regime and bear integration burden in the remedial regime. The state as remedial agent occupies d ≈ 0.10 (near-complete beneficiary) because it legitimates its own authority through remediation. Hierarchy incumbents occupy d ≈ 0.65 (mixed; they lose advantage but retain mobile exit options). Remedial beneficiaries in mixed position occupy d ≈ 0.58 (symmetric costs and benefits, with slight extraction tilt because integration burden may exceed access gains). The enforcement apparatus occupies d ≈ 0.92 (trapped victim, high extraction). Organized movements occupy d ≈ 0.45 (slight beneficiary position because movements gain legitimacy and resources from remedial framing, offset by the constraint that they cannot reduce pressure). The formal equality legacy occupies d ≈ 0.15 (beneficiary of piton status, because it absorbs remedial demands while maintaining its framing). These directionality values, passed through f(d), produce the measured χ values and feed the classification logic. The perspectival gap reflects genuine structural differences in d across agents, not differences of opinion about what equal protection means.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION VIA KERNEL DECOMPOSITION: The mandatrophy in equal protection law — the apparent contradiction between treating all persons as equal (formal equality) and actively distinguishing persons by race/gender/caste to remedy subordination (anti-caste remediation) — is resolved by recognizing these as two readings of a contested kernel rather than two different constraint types applied to the same facts. The anti-caste reading is not a misclassified mountain (a natural law of justice) but a contingent interpretation of a constitutive commitment (the 14A). The formal equality reading is not a degraded rope (failed coordination) but a competing interpretation of the same commitment. Both readings are legitimate within the framework of constitutional government; neither is self-evidently correct. The mandatrophy dissolves when we recognize that the Fourteenth Amendment is a kernel — a stabilized but not fully specified commitment that grounds legitimacy for competing interpretations — rather than a fully determinate text. Once the kernel is recognized as contested, the question shifts from 'which reading is right?' to 'how is this kernel being interpreted in different institutional and political contexts?' The anti-caste reading is dominant in civil rights institutions (courts applying constitutional doctrine, agencies implementing remedial programs, universities adopting affirmative action) and marginal in others (states restricting remedial scope, courts narrowing remedial doctrines). The formal equality reading remains dominant in abstract principle ('equal protection means colorblindness') and marginal in actual remedial policy. The mandatrophy is not about which reading matches the true constraint type but about which reading will dominate the kernel's institutional interpretation — a political and legal struggle, not a classification problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    remediation_as_cover_for_structural_inequality,
    'Does affirmative state action actually dismantle caste-like hierarchy, or does it reproduce the hierarchy while appearing to dismantle it (integration without structural change)?',
    'Long-term outcome tracking: intergenerational wealth transfer, occupational segregation patterns, institutional leadership composition, and whether remedial beneficiaries'' children require renewed remediation. If structural inequality persists across generations despite intensive remedial action, the constraint is reproducing rather than dismantling hierarchy.',
    'If reproduces: effective extractiveness rises from 0.58 to 0.72+ (true snare beneath remedial framing). If dismantles: extractiveness may fall to 0.35 (successful tangled rope moving toward pure coordination). If partially addresses: current 0.58 classification is correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remediation_as_cover_for_structural_inequality, empirical, 'Whether affirmative action dismantles or reproduces caste-like hierarchy').

omega_variable(
    remedial_beneficiary_stigma_mechanism,
    'Does remedial action stigmatize its beneficiaries (creating new extraction mechanism) or does it provide genuine access that outweighs stigma costs?',
    'Comparative institutional analysis: measure stigma effects vs. access gains in controlled cohorts; track beneficiary career satisfaction, income outcomes, and retention rates in integrated vs. segregated pathways; survey beneficiary self-reported costs and benefits.',
    'If stigma dominates: the constraint extracts from its nominal beneficiaries (ε rises to 0.65+, classification moves toward pure snare). If access dominates: extraction from beneficiaries is offset by material gains (ε stable at 0.58, tangled rope confirmed). If equal: 0.58 confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(remedial_beneficiary_stigma_mechanism, empirical, 'Whether remedial stigma outweighs access gains for beneficiaries').

omega_variable(
    formal_equality_vs_anti_caste_foreclosure,
    'Do the core premises of the anti-caste reading logically foreclose the formal equality reading within a single constitutional framework, or can both coexist as live political positions?',
    'Constitutional interpretation analysis: examine whether a coherent doctrine can hold both colorblind formal equality AND anti-caste remediation simultaneously, or whether they entail contradictory commitments about what equal protection requires. If coexistence is logically possible (different parties can hold both), relation is coexists_with. If one necessarily denies the other''s legitimacy (e.g., one says remediation is unconstitutional, the other says it is mandated), relation is forecloses.',
    'If forecloses: only one reading can legitimate state action at a time; winner takes the constitutional field. If coexists: both readings compete in ongoing political dispute; neither eliminates the other. The relation type determines network structure and cascade effects.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(formal_equality_vs_anti_caste_foreclosure, conceptual, 'Logical compatibility of anti-caste and formal equality readings').

omega_variable(
    false_summit_natural_law_risk,
    'Is the anti-caste reading treated as a transcendent natural law principle (equality requires remedial action) or recognized as one contingent constitutional interpretation?',
    'Institutional discourse analysis: track how the reading is defended (cited as ''justice requires,'' ''equality demands,'' universal principle vs. ''this interpretation of the 14A,'' contextual doctrine). If consistently naturalized, false summit signature fires. If contextualized, mountain classification is rejected.',
    'If naturalized: false summit detector flags the reading; beneficiaries of the anti-caste framing gain legitimacy cover; structural contingency is hidden. If contextualized: the reading remains contestable, opening space for the formal equality reading to resurface. The status of mountain classification determines whether the reading is shielded from revision challenge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law_risk, conceptual, 'Whether anti-caste reading naturalizes itself as law of justice').

omega_variable(
    state_capacity_sustainability,
    'Can the state sustain intensive remedial enforcement indefinitely, or does enforcement capacity degrade, allowing prior hierarchies to reconstitute?',
    'Historical analysis: examine enforcement intensity over decades; track political backlash effects; measure sustained commitment to remedial oversight in regulatory cycles. If enforcement oscillates (strong → weak → strong), the constraint may be unstable. If enforcement decays monotonically, the anti-caste reading is not sustainable without permanent crisis.',
    'If sustainable: extractiveness from enforcement capacity (perspective 5) is real but not catastrophic. If unsustainable: extractiveness rises sharply (ε → 0.72+) and the constraint oscillates between snare (high enforcement) and piton (degraded enforcement). Current 0.58 assumes sustainable enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_capacity_sustainability, empirical, 'Whether state capacity sustains remedial enforcement long-term').

omega_variable(
    kernel_interpretation_authority,
    'Who has legitimate authority to interpret the Fourteenth Amendment kernel — courts, legislatures, social movements, or the historical practices of subordinated groups?',
    'Jurisprudential and democratic theory analysis: examine how different authority structures (judicial review, democratic amendment, insurgent reinterpretation) approach the kernel; assess whether the anti-caste reading can survive judicial reversal, legislative amendment, or institutional refusal to enforce.',
    'If courts hold sole authority: the reading is vulnerable to judicial reversal (Dobbs-style precedent collapse). If movements hold authority: the reading is vulnerable to deprioritization as political attention shifts. If distributed: the reading is resilient but also perpetually contested. Authority structure affects long-term stability of the reading itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_interpretation_authority, conceptual, 'Authority structure for interpreting Fourteenth Amendment kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fourteenth_amendment_equal_protection__anti_caste_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(measure_theater_1960s, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(measure_theater_1980s, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(measure_theater_2000s, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(measure_extractiveness_1960s, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(measure_extractiveness_1980s, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(measure_extractiveness_2000s, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(measure_suppression_1960s, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(measure_suppression_1980s, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(measure_suppression_2000s, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fourteenth_amendment_equal_protection__anti_caste_reading, resource_allocation).
narrative_ontology:affects_constraint(fourteenth_amendment_equal_protection__anti_caste_reading, fourteenth_amendment_equal_protection__formal_equality_reading).
narrative_ontology:affects_constraint(fourteenth_amendment_equal_protection__anti_caste_reading, affirmative_action_university_admissions).
narrative_ontology:affects_constraint(fourteenth_amendment_equal_protection__anti_caste_reading, voting_rights_enforcement).
narrative_ontology:affects_constraint(fourteenth_amendment_equal_protection__anti_caste_reading, employment_discrimination_law).

% DUAL FORMULATION NOTE:
% The anti-caste reading is one interpretation of the Fourteenth Amendment kernel. The formal equality reading is the competing interpretation, linked by network relationship. This story models the anti-caste reading with ε=0.58 (tangled rope); the formal equality reading would have different ε reflecting its different structural properties (likely lower ε as pure coordination, depending on whether the reading legitimates state action or restricts it). The two stories form a constraint family representing the kernel's contested interpretation space.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fourteenth_amendment_equal_protection__anti_caste_reading, institutional, 0.1).
constraint_indexing:directionality_override(fourteenth_amendment_equal_protection__anti_caste_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
