% ============================================================================
% CONSTRAINT STORY: untouchability_abolition_article_17__horizontal_application_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_untouchability_abolition_article_17__horizontal_application_reading, []).

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
 *   constraint_id: untouchability_abolition_article_17__horizontal_application_reading
 *   human_readable: Article 17 Horizontal Application: Untouchability Forbidden by Neighbors
 *   domain: constitutional_law/fundamental_rights/horizontal_application
 *
 * SUMMARY:
 *   Article 17 of the Indian Constitution declares: 'Untouchability is
 *   abolished and its practice in any form is forbidden.' This constraint
 *   story instantiates ONE reading of the contested kernel
 *   untouchability_abolition_article_17: the horizontal_application_reading,
 *   which holds that Article 17 directly binds private actors (neighbors,
 *   caste gatekeepers, community enforcers), not merely the state. This
 *   reading treats untouchability exclusion as a constraint enforced by
 *   social actors with direct power to harm Dalits — exclusion from wells,
 *   temples, markets, occupations, marriage pools — and claims that Article
 *   17 makes such enforcement actionable without state intermediation. The
 *   horizontal application reading confronts two sibling readings: the
 *   enforcement_gap_reading (which emphasizes that private enforcement of
 *   caste remains practically unenforceable despite the constitutional
 *   command) and the structural_persistence_reading (which claims
 *   untouchability survives abolition through economic and occupational
 *   mechanisms that lie beyond the scope of a legal prohibition). This
 *   constraint story focuses exclusively on the
 *   horizontal_application_reading's structural claim: Article 17 creates a
 *   directly actionable prohibition on private untouchability enforcement.
 *   The sibling readings are OTHER constraints; they are not analyzed within
 *   this story. The kernel contest is documented in omega variables and
 *   cs_structure entries.
 *
 * KEY AGENTS:
 *   - Dalit community (formerly Untouchables): Primary beneficiary (powerless/trapped at local level, organized/constrained at regional/national level) — Article 17 nominally protects against private exclusion; enabler of complaint mechanisms; target of enforcement.
 *   - Community gatekeepers (caste associations, temples, wells, occupational guilds): Primary victim/enforcer (institutional/arbitrage at organizational level, powerful/constrained at local level) — lose capacity to enforce untouchability openly; face legal liability for ritual exclusion; extract legitimacy from caste hierarchy.
 *   - State enforcement apparatus (police, courts, civil rights authorities): Secondary beneficiary (institutional/arbitrage) — Article 17 shifts enforcement burden to civil society and individual complaints; enables low-cost regulation of private conduct.
 *   - Progressive neighbors and inter-caste communities: Secondary actors (moderate/constrained) — benefit from reduced exclusion; constrained by remaining social penalties for norm violation.
 *   - Judicial system: Tertiary institutional actor (institutional/arbitrage) — operationalizes horizontal application through landmark cases; performance is largely theater relative to actual enforcement in villages.
 *   - Analytical observer: Civilizational view (analytical/analytical) — risks naturalizing social stratification as immutable; detector of false summit (natural law framing of contingent institutional arrangements).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(untouchability_abolition_article_17__horizontal_application_reading, 0.68).
domain_priors:suppression_score(untouchability_abolition_article_17__horizontal_application_reading, 0.72).
domain_priors:theater_ratio(untouchability_abolition_article_17__horizontal_application_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(untouchability_abolition_article_17__horizontal_application_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(untouchability_abolition_article_17__horizontal_application_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(untouchability_abolition_article_17__horizontal_application_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(untouchability_abolition_article_17__horizontal_application_reading, snare).
narrative_ontology:human_readable(untouchability_abolition_article_17__horizontal_application_reading, "Article 17 Horizontal Application: Untouchability Forbidden by Neighbors").
narrative_ontology:topic_domain(untouchability_abolition_article_17__horizontal_application_reading, "constitutional_law/fundamental_rights/horizontal_application").

domain_priors:requires_active_enforcement(untouchability_abolition_article_17__horizontal_application_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(untouchability_abolition_article_17__horizontal_application_reading, 'b143dc9a-f8b2-45b3-98e0-25eedede3a5f').
narrative_ontology:cs_kernel_codification('b143dc9a-f8b2-45b3-98e0-25eedede3a5f', formalized).
narrative_ontology:cs_authority_grounding('b143dc9a-f8b2-45b3-98e0-25eedede3a5f', lineage).
narrative_ontology:cs_interpretation_layer_present('b143dc9a-f8b2-45b3-98e0-25eedede3a5f').
narrative_ontology:cs_reading_relation('b143dc9a-f8b2-45b3-98e0-25eedede3a5f', untouchability_abolition_article_17__enforcement_gap_reading, coexists_with).
narrative_ontology:cs_reading_relation('b143dc9a-f8b2-45b3-98e0-25eedede3a5f', untouchability_abolition_article_17__structural_persistence_reading, influences).
narrative_ontology:cs_axiom('b143dc9a-f8b2-45b3-98e0-25eedede3a5f', foundational, horizontal_direct_binding).
narrative_ontology:cs_axiom_status(horizontal_direct_binding, holdable).
narrative_ontology:cs_axiom_grounding('b143dc9a-f8b2-45b3-98e0-25eedede3a5f', horizontal_direct_binding, deontological).
narrative_ontology:cs_axiom('b143dc9a-f8b2-45b3-98e0-25eedede3a5f', foundational, private_enforcement_actionability).
narrative_ontology:cs_axiom_status(private_enforcement_actionability, holdable).
narrative_ontology:cs_axiom_grounding('b143dc9a-f8b2-45b3-98e0-25eedede3a5f', private_enforcement_actionability, instrumental).
narrative_ontology:cs_reference_frame('b143dc9a-f8b2-45b3-98e0-25eedede3a5f', constitutional_horizontal_command).
narrative_ontology:cs_drift_state('b143dc9a-f8b2-45b3-98e0-25eedede3a5f', contemporary_village_enforcement, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b143dc9a-f8b2-45b3-98e0-25eedede3a5f', '').
narrative_ontology:cs_kernel_id(untouchability_abolition_article_17__horizontal_application_reading, untouchability_abolition_article_17).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(untouchability_abolition_article_17__horizontal_application_reading, dalits).
narrative_ontology:constraint_victim(untouchability_abolition_article_17__horizontal_application_reading, community_enforcers).
narrative_ontology:constraint_victim(untouchability_abolition_article_17__horizontal_application_reading, caste_gatekeepers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DALIT EXCLUDED (SNARE) — Trapped in village; faces exclusion from public wells, temples, cremation grounds enforced by social sanction without state apparatus. Article 17 nominally protects but offers no exit — the Dalit cannot leave the village economy, cannot sever caste bonds, cannot avoid the community that enforces untouchability. Maximum suppression: all material alternatives require severing kinship and economic ties. High extractiveness: the constraint extracts labor segregation, ritual pollution, and denial of access. Snare classification from the target's position.
constraint_indexing:constraint_classification(untouchability_abolition_article_17__horizontal_application_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: PROGRESSIVE NEIGHBOR (TANGLED ROPE) — Constrained by caste norms but benefits from coordination on labor access and resource sharing. A neighbor who rejects untouchability still faces social penalties (marriage bar, ritual exclusion from caste events, business ostracism) but can partially exit through urban migration, inter-caste marriage, or community switching. The constraint has a genuine coordination function — caste hierarchy allocates occupations and ritual roles — alongside extractive asymmetry. Article 17 makes the extraction actionable; enforcement against private actors increases the cost of non-compliance.
constraint_indexing:constraint_classification(untouchability_abolition_article_17__horizontal_application_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: STATE ENFORCEMENT APPARATUS (ROPE) — The state benefits from Article 17's horizontal reach: it enables prosecution of private actors for untouchability without direct state coercion, shifting enforcement burden to civil society and individual complaint mechanisms. The state coordinates a low-cost prohibition mechanism: neighbors police each other's compliance. The state sees Article 17 as pure coordination — standardizing what untouchability means, enabling complaints, reducing state enforcement load.
constraint_indexing:constraint_classification(untouchability_abolition_article_17__horizontal_application_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: ORGANIZED CASTE GATEKEEPER (SNARE) — When Article 17 is enforced horizontally, organized caste associations face legal liability for ritual exclusion, business boycott, and social sanction enforcement. They are constrained (can formally comply with non-discrimination norms while preserving exclusion through economic channels) but still experience Article 17 as a snare: the constraint extracts legitimacy from their own governance mechanisms. They lose the capacity to enforce untouchability openly, forced to resort to coded discrimination or economic exclusion. High suppression of their prior enforcement toolkit; high extractiveness of their social authority.
constraint_indexing:constraint_classification(untouchability_abolition_article_17__horizontal_application_reading, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: JUDICIAL REVIEW MACHINERY (PITON) — Courts operationalize Article 17's horizontal reach through landmark judgments (Ambedkar v. state, Adi Dravidar v. state, manual scavenger cases) but the actual enforcement remains theater: low conviction rates, high burden of proof on Dalits, routine intimidation of witnesses. The judicial machinery performs horizontal application doctrine while private enforcement persists. The doctrine has become largely performative — a ritual that satisfies constitutional obligation without disrupting social reality. Theater ratio high because cases proceed but extraction (untouchability in practice) remains unchanged.
constraint_indexing:constraint_classification(untouchability_abolition_article_17__horizontal_application_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, caste exclusion might be treated as an immutable social structure rooted in occupational specialization and ritual order — a 'natural law' of South Asian social organization that cannot be abolished by constitutional command alone. Article 17 is framed as a futile gesture against inherent social stratification. However, this perspective naturalizes what the horizontal application reading treats as a contingent constraint: social enforcement by actors with power to harm Dalits. The engine will detect this as a false summit.
constraint_indexing:constraint_classification(untouchability_abolition_article_17__horizontal_application_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(untouchability_abolition_article_17__horizontal_application_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(untouchability_abolition_article_17__horizontal_application_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(untouchability_abolition_article_17__horizontal_application_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(untouchability_abolition_article_17__horizontal_application_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(untouchability_abolition_article_17__horizontal_application_reading, TR),
    TR >= 0.70.

:- end_tests(untouchability_abolition_article_17__horizontal_application_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   EXTRACTIVENESS (0.68): High, reflecting the structural asymmetry of Article 17's horizontal application. The constraint extracts from community gatekeepers (loss of enforcement legitimacy, legal liability for conduct previously protected as social practice) and extracts FOR Dalits (protection against private exclusion, actionable grievance mechanisms). The extractiveness value reflects the cost imposed on enforcers relative to the benefit to targets. The measurement trajectory shows extractiveness rising from 0.55 (pre-Article 17 or in early post-ratification phases when enforcement was rare) to 0.68 (contemporary period when case law and civil rights legislation have operationalized horizontal application). The plateau at 0.68 reflects that while doctrine has solidified, actual enforcement remains below theoretical potential — hence the theater ratio. SUPPRESSION (0.72): High and stable. Article 17's prohibition suppresses the traditional enforcement mechanisms available to caste gatekeepers (ritual sanction, occupational exclusion, marriage bar enforcement, water-well access control). Suppression is not total because alternative enforcement pathways exist (coded discrimination, economic channels, informal social sanctions). The measurement shows suppression high before Article 17 (gatekeepers had unrestricted enforcement capacity, 0.80) and declining slightly post-implementation (0.72) as some alternative enforcement methods emerge and some gatekeepers lose confidence in traditional mechanisms. The plateau reflects structural suppression — the constraint removes caste gatekeepers' ability to enforce untouchability THROUGH DIRECT SOCIAL FORCE, even if coded alternatives persist. THEATER RATIO (0.58): Moderate-high, reflecting the performative gap between doctrine and enforcement. Early post-ratification (t=0, value 0.35), courts were not routinely operationalizing Article 17; the prohibition existed but enforcement was minimal — low theater because low performance. Over time (t=15, 0.58), landmark cases and civil rights legislation created the appearance of enforcement (cases filed, judgments rendered, convictions occurred) while actual untouchability continued in villages and occupational structures — rising theater reflects increasing gap between judicial ritual and social reality. The plateau reflects sustained judicial performance without proportional enforcement in practice.
 *
 * PERSPECTIVAL GAP:
 *   The snare perspective (Dalit target, powerless/trapped) perceives Article 17 as maximum extraction: the constraint nominally protects but offers no practical exit from village structures, kinship obligations, or economic dependence. The tangled_rope perspective (progressive neighbor, moderate/constrained) perceives mixed coordination and extraction: Article 17 enables reduced exclusion AND constrains their exit through remaining social penalties. The rope perspective (state apparatus, institutional/arbitrage) perceives pure coordination: Article 17 enables regulation of private conduct with minimal state overhead. The snare perspective (caste gatekeeper, organized/constrained) perceives extraction of legitimacy: traditional enforcement becomes legally actionable. The piton perspective (judicial system, institutional/arbitrage) perceives performative enforcement: cases proceed but actual untouchability persists. The mountain perspective (analytical observer, analytical/analytical) risks naturalizing caste exclusion as immutable social order, which the engine flags as false summit. The constraint is diagnostically rich because it instantiates five distinct types from structurally coherent positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) in this constraint is determined by each actor's structural position relative to Article 17's prohibition. Dalits: d ≈ 0.95 (trapped victims of untouchability, no exit capacity, structural dependence on village and kinship systems). Community gatekeepers: d ≈ 0.72 (powerful institutional enforcers, some exit capacity through legal defense and coded alternatives, but primary function threatened by horizontal application). State apparatus: d ≈ 0.05 (institutional beneficiary, arbitrage capacity, low-cost regulation enabled by Article 17). Progressive neighbors: d ≈ 0.55 (symmetric — costs of non-compliance balanced by coordination benefits, moderate power, constrained exit). Judicial system: d ≈ 0.25 (secondary beneficiary, operationalizes doctrine, low direct extraction). The engine derives d from beneficiary/victim declarations and exit options; overrides are not necessary because structural data is clear.
 *
 * MANDATROPHY ANALYSIS:
 *   Article 17's horizontal application resolves mandatrophy by clarifying that the constraint is not purely extractive (snare-only) but genuinely prohibits private untouchability enforcement while generating secondary extraction through enforcement gaps and coded alternatives. The constraint is best classified as SNARE from the Dalit target's position (trapped, maximum experienced extraction) and the caste gatekeeper's position (organized extraction of legitimacy). From the state's position it is ROPE (pure coordination enabling low-cost regulation). From the progressive neighbor's position it is TANGLED ROPE (mixed coordination and extraction). The mandatrophy resolves by recognizing that Article 17 genuinely prohibits untouchability AND that multiple enforcement mechanisms (legal, social, institutional) interact to produce different experienced extractiveness for different actors. The constraint is not misclassified as coordination when it is extraction; rather, it IS coordinating a prohibition on private untouchability enforcement while generating secondary extraction through implementation gaps.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    horizontal_enforceability_scope,
    'Does Article 17''s prohibition on untouchability bind private actors only when they possess direct enforcement power (ritual gatekeepers, occupational controllers) or does it extend to all social actors (including those who merely participate in caste norms)?',
    'Case law analysis: which private actors courts have held liable under Article 17; whether liability requires institutional gatekeeping power or merely social participation in exclusion. Cross-reference to Ambedkar, Shameem v. State, and manual scavenger cases.',
    'If scope is narrow (direct enforcers only): suppression remains high because community gatekeepers retain enforcement power; extractiveness focuses on ritual/occupational exclusion. If scope is broad (all social actors): suppression becomes community-wide; extractiveness extends to informal exclusion, making the constraint pervasive but potentially more visible and actionable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(horizontal_enforceability_scope, empirical, 'Scope of private actors bound by Article 17').

omega_variable(
    distinction_horizontal_vertical,
    'Is this reading''s core premise — that Article 17 binds neighbors and non-state actors — logically distinct from or foreclosed by a reading that emphasizes enforcement gaps (state cannot practically implement prohibition against dispersed private actors)?',
    'Doctrinal analysis: whether horizontal application is a constitutional mandate (this reading''s premise) or a practical impossibility that the enforcement_gap_reading emphasizes. Can both readings coexist in one framework? Are they logically incompatible?',
    'If compatible: both readings are live — horizontal application is the doctrine, enforcement gap is the reality gap. If incompatible: this reading''s premise (neighbors are bound) forecloses the enforcement_gap reading''s premise (neighbors cannot be efficiently bound). This determines the reading_relations classification (coexists_with vs forecloses).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(distinction_horizontal_vertical, conceptual, 'Logical relationship between horizontal application and enforcement gap readings').

omega_variable(
    private_actor_definition,
    'What counts as a ''private actor'' for purposes of Article 17 horizontal enforcement? Does it include: (a) family members enforcing caste endogamy, (b) employers enforcing occupational segregation, (c) neighbors participating in water-well exclusion, (d) all of the above, or (e) only formal gatekeeping institutions?',
    'Constitutional exegesis and case law: Ambedkar''s constitutional intent (direct command to all), court interpretations (which actors have been held liable), and social practice (which actors actually enforce untouchability in contemporary villages).',
    'If definition includes family: suppression extends into intimate sphere; extractiveness includes control over marriage and reproduction. If limited to formal institutions: suppression focuses on occupational and ritual gatekeeping. Definition determines the actual scope of the constraint in practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(private_actor_definition, conceptual, 'Definition of private actors bound by Article 17').

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of the untouchability_abolition_article_17 kernel. The sibling readings emphasize different structural features: enforcement_gap_reading focuses on the gap between doctrine and practice; structural_persistence_reading focuses on untouchability persisting through economic mechanisms even after legal abolition. Does this horizontal_application_reading''s core premise (Article 17 binds neighbors directly) foreclose, coexist with, or influence those alternative readings?',
    'Conceptual analysis: (1) Does holding that Article 17 directly binds neighbors logically rule out the enforcement_gap reading''s claim that enforcement is practically impossible? (2) Does it rule out the structural_persistence reading''s claim that untouchability survives through occupational and economic structures even after legal abolition? (3) Or are these readings examining different aspects of the same kernel that can coexist in a complete analysis?',
    'If forecloses: this reading''s premise eliminates the others in a single framework. If coexists_with: multiple readings remain live for different parties or aspects. If influences: this reading creates pressure on the others (e.g., if Article 17 successfully binds neighbors, the enforcement_gap is narrowed but not eliminated; structural persistence must then be explained differently). The resolution determines the cs_structure.reading_relations entries.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Relationship of this reading to sibling readings of the untouchability kernel').

omega_variable(
    social_sanction_vs_legal_obligation,
    'When Article 17 makes untouchability enforcement actionable without state action, does it convert a social norm (caste hierarchy) into a legal obligation (non-discrimination)? Or does it merely provide legal remedies for violation of a norm that remains primarily social?',
    'Analysis of burden-shifting: in the absence of Article 17, who bears the burden of maintaining untouchability (the caste community enforces it socially)? With Article 17, who bears the burden (the accused private actor must defend their conduct in court)? If burdens shift to legal defense, the constraint is actionable; if they remain social, legalization is theater.',
    'If conversion is real: suppression decreases because Article 17 enforcement reduces the social cost of non-compliance and increases the social cost of enforcement. If conversion is theater: suppression persists because social sanction remains the primary enforcement mechanism; legal liability is secondary and rarely applied. Theater ratio interpretation depends on this resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_sanction_vs_legal_obligation, empirical, 'Whether Article 17 converts social norm to legal obligation or remains largely performative').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(untouchability_abolition_article_17__horizontal_application_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(untouchabil_horiz_theater_t0, untouchability_abolition_article_17__horizontal_application_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(untouchabil_horiz_theater_t15, untouchability_abolition_article_17__horizontal_application_reading, theater_ratio, 15, 0.58).
narrative_ontology:measurement(untouchabil_horiz_theater_t30, untouchability_abolition_article_17__horizontal_application_reading, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(untouchabil_horiz_extract_t0, untouchability_abolition_article_17__horizontal_application_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(untouchabil_horiz_extract_t15, untouchability_abolition_article_17__horizontal_application_reading, base_extractiveness, 15, 0.68).
narrative_ontology:measurement(untouchabil_horiz_extract_t30, untouchability_abolition_article_17__horizontal_application_reading, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(untouchabil_horiz_suppress_t0, untouchability_abolition_article_17__horizontal_application_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(untouchabil_horiz_suppress_t15, untouchability_abolition_article_17__horizontal_application_reading, suppression_requirement, 15, 0.72).
narrative_ontology:measurement(untouchabil_horiz_suppress_t30, untouchability_abolition_article_17__horizontal_application_reading, suppression_requirement, 30, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(untouchability_abolition_article_17__horizontal_application_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(untouchability_abolition_article_17__horizontal_application_reading, untouchability_abolition_article_17__enforcement_gap_reading).
narrative_ontology:affects_constraint(untouchability_abolition_article_17__horizontal_application_reading, untouchability_abolition_article_17__structural_persistence_reading).

% DUAL FORMULATION NOTE:
% The untouchability_abolition_article_17 kernel has three structurally distinct constraint readings. This horizontal_application_reading focuses on the direct prohibition on private enforcement. The enforcement_gap_reading focuses on practical enforcement barriers despite doctrine. The structural_persistence_reading focuses on how untouchability survives through economic mechanisms. Each reading has its own ε value reflecting different aspects of the constraint's operation. The three readings are linked by network.affects_constraints to enable contamination analysis across the kernel family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
