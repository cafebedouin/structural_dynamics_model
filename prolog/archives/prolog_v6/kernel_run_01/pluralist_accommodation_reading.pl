% ============================================================================
% CONSTRAINT STORY: pluralist_accommodation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_pluralist_accommodation_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: pluralist_accommodation_reading
 *   human_readable: Pluralist Accommodation of Family Law Authority (Articles 25-28 & 44 Reading)
 *   domain: constitutional_law/legal_pluralism/family_law
 *
 * SUMMARY:
 *   The pluralist accommodation reading interprets the Indian Constitution as
 *   establishing a genuine legal pluralism in family law: Articles 25-28
 *   (religious freedom, community autonomy) and Article 44 (Directive
 *   Principle toward uniform civil code) are framed as complementary rather
 *   than contradictory. Under this reading, the constitutional settlement is
 *   that religious communities retain genuine authority to govern family
 *   relations (marriage, divorce, inheritance, succession) according to their
 *   own laws, while secular courts provide appellate review and harmonization
 *   on individual rights issues. This reading treats the Uniform Civil Code
 *   not as a mandatory endpoint but as an aspirational goal toward which
 *   communities and courts gradually move through dialogue and internal legal
 *   reform. The reading instantiates a stable structure of dual sovereignty:
 *   individuals have rights both as citizens (under secular constitution) and
 *   as community members (under religious law), and these are treated as
 *   mutually reinforcing rather than mutually exclusive. The constraint shows
 *   high extractiveness (0.38) because community authorities capture
 *   gatekeeping power over family relations and use pluralist framing to
 *   resist secular review; moderate suppression (0.42) because individual
 *   appeals to secular law remain formally available; and moderate theater
 *   ratio (0.55) because much of the harmony narrative obscures genuine
 *   conflict between Articles 25-28 and 44.
 *
 * KEY AGENTS:
 *   - Religious Community Authorities: Institutional beneficiaries (institutional/arbitrage) — retain autonomous governance authority over family law; control boundary-setting and internal dispute resolution; benefit from pluralist framing as legitimacy protection
 *   - Individual Rights Claimants (primarily women): Moderate agents caught between frameworks (moderate/constrained) — experience both enabling coordination (can appeal to either system) and extractive constraint (caught between incompatible standards); face career/housing/identity costs for either exit path
 *   - Minorities Within Communities: Powerless victims (powerless/identity_locked) — governed by community law with minimal formal escape; identity constituted through membership; bear costs of community-enforced norms with no secular alternative accessible without identity dissolution
 *   - Secular Codification Movement: Institutional actors (institutional/constrained) — law commissions, women's rights organizations, reformers seeking uniform civil code; structurally excluded from family law domain by constitutional deference to Articles 25-28; constrained by political difficulty of displacing established community authority
 *   - Judicial Harmonization Apparatus: Organized institutional actors (organized/constrained) — family courts, constitutional benches, case-law harmonizers; operate under genuine authority to shape trajectory through selective review and procedural requirements; constrained by appellate deference to community decisions and legislative gridlock on UCC
 *   - Analytical Observer (Civilizational view): Sees structure as natural (analytical/analytical) — perspectives legal pluralism as immutable feature of heterogeneous constitutional democracy; risks naturalizing contingent institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pluralist_accommodation_reading, 0.38).
domain_priors:suppression_score(pluralist_accommodation_reading, 0.42).
domain_priors:theater_ratio(pluralist_accommodation_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pluralist_accommodation_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(pluralist_accommodation_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(pluralist_accommodation_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(pluralist_accommodation_reading, tangled_rope).
narrative_ontology:human_readable(pluralist_accommodation_reading, "Pluralist Accommodation of Family Law Authority (Articles 25-28 & 44 Reading)").
narrative_ontology:topic_domain(pluralist_accommodation_reading, "constitutional_law/legal_pluralism/family_law").

domain_priors:requires_active_enforcement(pluralist_accommodation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(pluralist_accommodation_reading, '48705665-9637-4b46-baa1-d57b2968655c').
narrative_ontology:cs_created_at('48705665-9637-4b46-baa1-d57b2968655c', '').
narrative_ontology:cs_kernel_codification('48705665-9637-4b46-baa1-d57b2968655c', fixed_text).
narrative_ontology:cs_authority_grounding('48705665-9637-4b46-baa1-d57b2968655c', lineage).
narrative_ontology:cs_interpretation_layer_present('48705665-9637-4b46-baa1-d57b2968655c').
narrative_ontology:cs_kernel_id(pluralist_accommodation_reading, family_law_authority).
narrative_ontology:cs_reading_relation('48705665-9637-4b46-baa1-d57b2968655c', religious_autonomy_reading, coexists_with).
narrative_ontology:cs_reading_relation('48705665-9637-4b46-baa1-d57b2968655c', constitutional_supremacy_reading, coexists_with).
narrative_ontology:cs_axiom('48705665-9637-4b46-baa1-d57b2968655c', foundational, dual_sovereignty_complementary).
narrative_ontology:cs_axiom_status(dual_sovereignty_complementary, holdable).
narrative_ontology:cs_axiom('48705665-9637-4b46-baa1-d57b2968655c', foundational, harmonization_through_dialogue_not_hierarchy).
narrative_ontology:cs_axiom_status(harmonization_through_dialogue_not_hierarchy, holdable).
narrative_ontology:cs_reference_frame('48705665-9637-4b46-baa1-d57b2968655c', articles_25_28_and_44_coexistent).
narrative_ontology:cs_drift_state('48705665-9637-4b46-baa1-d57b2968655c', contemporary_post_independence, gap(stable, minor, true)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(pluralist_accommodation_reading, religious_communities).
narrative_ontology:constraint_beneficiary(pluralist_accommodation_reading, individual_autonomy_claimants).
narrative_ontology:constraint_victim(pluralist_accommodation_reading, uniform_codification_advocates).
narrative_ontology:constraint_victim(pluralist_accommodation_reading, disadvantaged_minorities_within_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MINORITY WITHIN RELIGIOUS COMMUNITY (SNARE) — Identity locked by religious/cultural membership; community law governs family relations (marriage, divorce, inheritance) with minimal alternative. Exit options structurally narrow (relocation costly, identity severing impossible) and cognitively captured (community framing as natural/inevitable). Experiences extraction under pluralist framework: no appeal to secular codification, no protection from community enforcement, no exit without identity loss.
constraint_indexing:constraint_classification(pluralist_accommodation_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: INDIVIDUAL RIGHTS CLAIMANT (TANGLED ROPE) — Constrained by both community law and secular state law; experiences dual governance as enabling and extractive. Coordination function: dual system allows navigation between frameworks (secular court appeal, community mediation). Extraction: caught between incompatible standards, career/housing consequences of either exit path. Moderate power — can litigate but against institutional weight.
constraint_indexing:constraint_classification(pluralist_accommodation_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: RELIGIOUS COMMUNITY AUTHORITY (ROPE) — Institutional actor with arbitrage exit (can shift between community law and secular law as advantageous, or maintain parallel legitimacy). Experiences pluralist accommodation as pure coordination: Articles 25-28 protect religious freedom and autonomous governance; secular courts respect community decisions without replacing them. Benefits from dual legitimacy — can claim both constitutional authority and religious authenticity. Extraction runs toward this agent through resource control and boundary-setting authority.
constraint_indexing:constraint_classification(pluralist_accommodation_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: SECULAR CODIFICATION MOVEMENT (TANGLED ROPE) — Institutional actor (law commissions, women's rights organizations, uniform civil code advocates) constrained by constitutional deference to Articles 25-28 and by political difficulty of displacing established community authority. Coordination function: participates in harmonization efforts, registered reports on best practices, dialogue with communities. Extraction: structurally excluded from family law domain in pluralist reading; benefits from generational shift in which categories (women's rights, children's welfare) become legible within secular frame.
constraint_indexing:constraint_classification(pluralist_accommodation_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: JUDICIAL HARMONIZATION APPARATUS (SCAFFOLD) — Organized institutional actors (family courts, constitutional benches, law reformers) operating under a sunset logic: harmonization through case law and selective statutory reform rather than wholesale codification. See pluralist accommodation as temporary structure that will naturally evolve as communities interpret their own law through secular rights language. Chi is lower because these actors have genuine agency to shape the trajectory — the constraint is not imposed but jointly constructed and refinable.
constraint_indexing:constraint_classification(pluralist_accommodation_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, legal pluralism emerges as an immutable structural feature of heterogeneous constitutional democracy: states with multiple identity-constituting communities cannot impose uniform law without either expulsion or assimilation. This perspective sees Articles 25-28 coexisting with Article 44 as a natural settlement of the tension between state authority and community autonomy — unchangeable by policy choice alone. However, the structural data contradicts this: beneficiaries and victims are identifiable; extraction flows toward communities; exit options are differentially distributed. The engine's false summit detector identifies this as naturalization of a contingent constitutional arrangement.
constraint_indexing:constraint_classification(pluralist_accommodation_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pluralist_accommodation_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(pluralist_accommodation_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(pluralist_accommodation_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(pluralist_accommodation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate-high. Religious communities capture substantial gatekeeping power over family relations through pluralist framing. However, this is not maximum extraction because (1) individual appeals to secular courts remain available as formal escape valve, (2) no single community monopolizes the space (competition between communities can create exit options for some individuals), and (3) the constraint requires active enforcement through both community and state cooperation, which creates friction. The value reflects that extraction exists but is constrained by the coordination requirement — communities cannot enforce without state deference, state defers by choice not law. Suppression (0.42): Moderate. Barriers to exit include identity lock (cognitive capture through community membership), constrained alternatives (secular law available but at social/career cost), and community enforcement (both social and legal). Suppression is not higher because formal exit mechanisms exist (secular court appeal, apostasy as legal right) even if costly. Theater ratio (0.55): Moderate. The pluralist harmony narrative (Articles 25-28 and 44 coexisting, dual rights-holders, selective codification as legitimate) contains performative elements: the zero-sum tension between community autonomy and uniform law is masked as coexistence, and judicial harmonization is framed as inevitable evolution rather than contested choice. However, theater is not dominant (>0.70) because the structural mechanics of pluralism do produce some real coordination function — communities genuinely govern, individuals genuinely navigate dual systems, courts genuinely harmonize cases. The tension is real even if the framing obscures it.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full perspectival range from a single structural arrangement. Religious communities see pure coordination (Rope): pluralist accommodation enables them to govern autonomously and legitimately. Individual rights claimants see mixed coordination and extraction (Tangled Rope): they can navigate both systems but at cost. Minorities within communities see pure extraction (Snare): locked into community law with no genuine alternative. Secular codification advocates see a temporary coordination problem (Scaffold): pluralism is a stage in generational evolution toward uniform law, which harmonization and dialogue gradually achieve. The judicial apparatus sees a stable scaffold: harmonization through case law and selective reform, with genuine agency and sunset logic. The civilizational analytical observer sees immutable natural law (Mountain): legal pluralism is inherent to heterogeneous democracy. The perspectival gaps are significant and diagnostically important: each observer's classification follows from their structural position (power level, exit options, beneficiary/victim status), revealing that no single classification captures the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from each agent's structural position in the extraction flow. Religious communities (beneficiaries + arbitrage exit) have low d and negative effective chi — they experience the constraint as enabling rather than extractive. Individual rights claimants (neither beneficiary nor full victim + constrained exit) have mid-range d — they experience mixed extraction and benefit. Minorities within communities (victims + identity_locked exit) have high d and maximum experienced extraction. The analytical observer (neutral + analytical exit) has mid-high d, reflecting their role as neutral documentarian who cannot exit the analytical stance. The derived d values feed into the sigmoid f(d) function to produce effective extractiveness chi, which varies across perspectives even though base extractiveness is fixed. This perspectival variance is the diagnostic content: it reveals which agents bear costs and which capture benefits.
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING CASE: This constraint is one reading of the family_law_authority kernel. The pluralist accommodation reading resolves the mandatrophy by treating Articles 25-28 and 44 as genuinely coexistent rather than forcing a choice. However, the mandatrophy is not fully resolved — it is instantiated within the pluralist reading. The constraint exhibits the core mandatrophy structure: it has extractive properties (beneficiary communities, victim minorities, gatekeeping power) but is framed as coordination (dual rights-holders, harmonization, mutual reinforcement). The pluralist reading neither eliminates the extraction nor fully acknowledges it — it naturalizes the coexistence of both as constitutional settlement. This is the legitimacy mechanism of the pluralist reading: by framing Articles 25-28 and 44 as complementary, it converts what might appear as zero-sum conflict (individual rights vs. community autonomy) into stable dual sovereignty. The mandatrophy is thus resolved not by showing that extraction is absent, but by showing that extraction and coordination coexist structurally — the constraint is genuinely tangled rope, not snare disguised as rope. However, the false summit risk (perspective 6) suggests that the mountain framing naturalizes this arrangement in ways that obscure the extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harmonization_direction_ambiguity,
    'Is harmonization a one-way street toward secular codification, or does it preserve genuine community law evolution within pluralist frame?',
    'Historical trajectory analysis: cases in which courts have integrated community law concepts into secular reasoning vs. cases in which community practices have been rejected; generational tracking of which values become internalized by each framework',
    'If harmonization is one-directional: pluralist accommodation is a transition mechanism, not a stable settlement (scaffold with longer sunset, or tangled rope with accelerating extraction). If bidirectional: pluralist accommodation is genuinely structural (rope from institutional perspective, tangled rope from moderate perspective with lower chi).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harmonization_direction_ambiguity, empirical, 'Whether judicial harmonization is bidirectional or directional toward secular law').

omega_variable(
    minority_protection_substitutability,
    'Can individual rights within community law (e.g., right to petition, interpretive change driven by community advocacy) substitute for secular codification protections, or is substitutability illusory?',
    'Outcome parity analysis: cases brought within community law framework vs. within secular framework for same substantive issue; measurement of remedial adequacy and enforcement success rates; comparative cost-benefit to claimants',
    'If substitutable: pluralist reading reduces extraction burden on powerless agents (minority within community reclassifies from snare toward tangled rope). If illusory: extraction is higher than measured (snare classification confirmed; potential falsification of moderate agent''s tangled rope perception).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_protection_substitutability, empirical, 'Whether internal community protections substitute for secular codification').

omega_variable(
    articles_25_28_versus_44_zero_sum_logic,
    'Are Articles 25-28 (religious freedom, community autonomy) and Article 44 (uniform civil code aspiration) genuinely complementary or fundamentally zero-sum despite the pluralist reading''s claim of coexistence?',
    'Constitutional jurisprudence analysis: cases in which courts have had to choose between Arts 25-28 and Art 44 values; identification of domains in which choice-forcing has occurred; measurement of meta-rule evolution (how do courts decide which article applies in boundary cases?)',
    'If complementary: pluralist reading is structurally valid (tangled rope with stable extraction). If zero-sum: pluralist reading is aspirational theater (piton classification, or tangled rope with higher theater ratio indicating performative coexistence masking underlying conflict).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(articles_25_28_versus_44_zero_sum_logic, conceptual, 'Whether Arts 25-28 and 44 are complementary or zero-sum').

omega_variable(
    identity_lock_versus_constrained_exit,
    'For individuals within religious communities, is the exit barrier primarily identity-cognitive (the individual''s self-concept is constituted through community membership) or structurally material (economic dependency, legal prohibition, geographic isolation)?',
    'Post-exit qualitative data: interviews with individuals who have exited communities regarding the binding mechanisms they experienced; comparison of exit rates across communities with varying material barriers (e.g., economic self-sufficiency of women) and identity barriers (e.g., cultural integration history)',
    'If identity-cognitive: perspective 1 (minority within community) is correctly classified as identity_locked (snare), which signals that cognitive reframing could shift classification. If material: exit is trapped rather than identity_locked, indicating structural immobility rather than perceptual capture. This affects mandatrophy resolution: identity_locked permits rope classification at biographical horizon (the individual could perceive mutability if identity frame shifted), while trapped does not.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_versus_constrained_exit, empirical, 'Binding mechanism for individuals within religious communities: identity-cognitive vs. material barriers').

omega_variable(
    pluralist_settlement_reading_vs_constitutional_supremacy_reading,
    'This constraint is one reading of the family_law_authority kernel. The sibling readings (religious_autonomy_reading and constitutional_supremacy_reading) offer fundamentally different interpretations of Articles 25-28 and 44. Does the pluralist accommodation reading foreclose either sibling, or do all three coexist as live positions held by different constitutional actors?',
    'Constitutional jurisprudence and political-institutional analysis: identification of courts, legislators, religious bodies, and civil society organizations that actively hold each reading; measurement of whether any reading has been formally overridden or superseded within its own tradition; determination of whether the three readings occupy different institutional contexts (appellate courts vs. legislative bodies vs. community authorities) or genuinely compete within the same forum',
    'If pluralist reading forecloses religious autonomy or supremacy readings: they are incoherent within this frame (rare). If coexists_with: the readings are live positions held by different factions; no single framework can hold all three, but all three remain operative in actual constitutional discourse. If influences: pluralist reading creates structural pressure on siblings (e.g., by establishing procedural requirements for community law that are costly for supremacy reading to meet) without logically foreclosing them. This determines the axiom status of pluralist_dual_authority: holdable (coexists_with) or foreclosed (if a sibling''s core premise directly contradicts it).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pluralist_settlement_reading_vs_constitutional_supremacy_reading, conceptual, 'Relationship between pluralist accommodation reading and religious autonomy and constitutional supremacy readings').

omega_variable(
    false_summit_natural_law_risk,
    'Is the mountain classification at the analytical level (perspective 6) revealing a genuine immutable structural feature of heterogeneous constitutional democracy, or is it naturalizing a contingent institutional arrangement (beneficiary-driven false summit)?',
    'Comparative constitutional analysis: identification of heterogeneous democracies that have chosen uniform civil codes despite identity-constituting diversity (counter-examples to the natural law claim); analysis of power dynamics in pluralist accommodation adoption (which actors benefit from treating pluralism as inevitable rather than chosen)',
    'If genuine natural law: analytical perspective mountain classification stands; Articles 25-28 and 44 coexistence is immutable. If false summit: pluralist accommodation is a contingent institutional choice driven by beneficiary interests (religious communities, certain moderates); the mountain classification is naturalization; the constraint''s extractiveness may be higher than measured if false-summit framing obscures it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law_risk, empirical, 'Whether pluralist accommodation is immutable natural law or contingent institutional choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(pluralist_accommodation_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plur_tr_t0, pluralist_accommodation_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(plur_tr_t5, pluralist_accommodation_reading, theater_ratio, 5, 0.52).
narrative_ontology:measurement(plur_tr_t10, pluralist_accommodation_reading, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(plur_be_t0, pluralist_accommodation_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(plur_be_t5, pluralist_accommodation_reading, base_extractiveness, 5, 0.33).
narrative_ontology:measurement(plur_be_t10, pluralist_accommodation_reading, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(pluralist_accommodation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(pluralist_accommodation_reading, 0.12).
narrative_ontology:affects_constraint(pluralist_accommodation_reading, religious_autonomy_reading).
narrative_ontology:affects_constraint(pluralist_accommodation_reading, constitutional_supremacy_reading).

% DUAL FORMULATION NOTE:
% The family_law_authority kernel decomposes into three structurally distinct constraints, one for each reading. Each reading has its own beneficiary/victim structure, its own extractiveness value, and its own perspectives. The pluralist_accommodation_reading is one member of this constraint family. All three readings must be authored to represent the full constitutional dispute. The network edges establish that these are not independent constraints but readings of a common kernel with defined relationships: the readings coexist, influence each other, and shape the judicial and political landscape.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
