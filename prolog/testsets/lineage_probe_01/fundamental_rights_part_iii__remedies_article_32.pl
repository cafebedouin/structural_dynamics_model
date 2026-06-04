% ============================================================================
% CONSTRAINT STORY: fundamental_rights_part_iii__remedies_article_32
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fundamental_rights_part_iii__remedies_article_32, []).

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
 *   constraint_id: fundamental_rights_part_iii__remedies_article_32
 *   human_readable: Article 32: The Right to Remedies and Direct Supreme Court Access
 *   domain: constitutional_law/fundamental_rights
 *
 * SUMMARY:
 *   Article 32 of the Indian Constitution establishes the right to
 *   constitutional remedies and direct access to the Supreme Court for
 *   enforcement of fundamental rights. Ambedkar, the principal architect of
 *   the Constitution, called it the Constitution's 'heart and soul' because
 *   it provides the enforcement mechanism without which substantive rights
 *   (equality, freedoms, and others) would be declarative only. This
 *   constraint story instantiates ONE reading of a contested constitutional
 *   kernel: the remedies reading, which emphasizes that the direct-access
 *   architecture solves the coordination problem of uniform rights
 *   enforcement at the apex, bypassing lower-court procedural gatekeeping and
 *   enabling both individual petitioners and organized civil rights actors to
 *   seek binding remedies immediately. The structural delta distinguishing
 *   this reading from the equality and freedoms readings is that Article 32
 *   is not a substantive right itself — it is the enforcement mechanism that
 *   makes other rights actionable. Suppression is low (the legal right to
 *   petition exists and is enforceable); extractiveness is minimal (the
 *   mechanism coordinates access without extracting rent); theater is low
 *   (writs bind the state substantively). This is a rope constraint — pure
 *   coordination with minimal overhead — from the perspectives of
 *   petitioners, organized actors, and the analytical observer. The Supreme
 *   Court's role is tangled: it coordinates remedies (pure function) while
 *   concentrating institutional power (extraction side effect). Lower courts'
 *   capacity determines whether Article 32 is a permanent coordinate apex
 *   (rope) or a temporary scaffold awaiting lower-court maturation.
 *
 * KEY AGENTS:
 *   - Rights Petitioners (powerless/mobile): Individual or group members facing rights violations; gain direct apex access without lower-court hierarchy
 *   - Marginalized Groups (powerless/mobile): Dalit communities, religious minorities, women, labor groups historically excluded from justice access; Article 32 enables direct petitions
 *   - Civil Rights Organizations (organized/mobile): Public interest litigation bodies (PUCL, PUDR, environmental groups, women's rights orgs) coordinating systemic challenges; Article 32 enables scaled PIL
 *   - The Supreme Court (institutional/constrained): Apex remedy-enforcer; both coordinates unified remedy architecture and concentrates institutional power; docket constraints create suppression
 *   - Lower Courts (institutional/constrained): Trial and appellate courts whose procedural gatekeeping Article 32 bypasses; capacity maturation determines whether Article 32 scaffold can sunset
 *   - The State/Executive (institutional/arbitrage): Respondent to Article 32 petitions; bound by Supreme Court writs; experiences the constraint as enforcement overhead
 *   - Analytical Observer (analytical/analytical): Civilizational view of coordination architecture without attending to power concentration effects
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fundamental_rights_part_iii__remedies_article_32, 0.18).
domain_priors:suppression_score(fundamental_rights_part_iii__remedies_article_32, 0.12).
domain_priors:theater_ratio(fundamental_rights_part_iii__remedies_article_32, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fundamental_rights_part_iii__remedies_article_32, extractiveness, 0.18).
narrative_ontology:constraint_metric(fundamental_rights_part_iii__remedies_article_32, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(fundamental_rights_part_iii__remedies_article_32, theater_ratio, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fundamental_rights_part_iii__remedies_article_32, rope).
narrative_ontology:human_readable(fundamental_rights_part_iii__remedies_article_32, "Article 32: The Right to Remedies and Direct Supreme Court Access").
narrative_ontology:topic_domain(fundamental_rights_part_iii__remedies_article_32, "constitutional_law/fundamental_rights").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fundamental_rights_part_iii__remedies_article_32, '4a5a010a-3cfb-4e92-b3cf-43e7efee3e11').
narrative_ontology:cs_kernel_codification('4a5a010a-3cfb-4e92-b3cf-43e7efee3e11', formalized).
narrative_ontology:cs_authority_grounding('4a5a010a-3cfb-4e92-b3cf-43e7efee3e11', lineage).
narrative_ontology:cs_interpretation_layer_present('4a5a010a-3cfb-4e92-b3cf-43e7efee3e11').
narrative_ontology:cs_reading_relation('4a5a010a-3cfb-4e92-b3cf-43e7efee3e11', fundamental_rights_part_iii__equality_code, influences).
narrative_ontology:cs_reading_relation('4a5a010a-3cfb-4e92-b3cf-43e7efee3e11', fundamental_rights_part_iii__freedoms_article_19, influences).
narrative_ontology:cs_axiom('4a5a010a-3cfb-4e92-b3cf-43e7efee3e11', foundational, remedies_foundational_to_right_existence).
narrative_ontology:cs_axiom_status(remedies_foundational_to_right_existence, holdable).
narrative_ontology:cs_axiom_grounding('4a5a010a-3cfb-4e92-b3cf-43e7efee3e11', remedies_foundational_to_right_existence, deontological).
narrative_ontology:cs_axiom('4a5a010a-3cfb-4e92-b3cf-43e7efee3e11', secondary, apex_coordination_solves_enforcement_asymmetry).
narrative_ontology:cs_axiom_status(apex_coordination_solves_enforcement_asymmetry, holdable).
narrative_ontology:cs_axiom_grounding('4a5a010a-3cfb-4e92-b3cf-43e7efee3e11', apex_coordination_solves_enforcement_asymmetry, conventional).
narrative_ontology:cs_reference_frame('4a5a010a-3cfb-4e92-b3cf-43e7efee3e11', direct_remedy_access_constitutional_right).
narrative_ontology:cs_drift_state('4a5a010a-3cfb-4e92-b3cf-43e7efee3e11', contemporary_post_pil_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4a5a010a-3cfb-4e92-b3cf-43e7efee3e11', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(fundamental_rights_part_iii__remedies_article_32, fundamental_rights_part_iii).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fundamental_rights_part_iii__remedies_article_32, rights_petitioners).
narrative_ontology:constraint_beneficiary(fundamental_rights_part_iii__remedies_article_32, marginalized_groups_seeking_enforcement).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RIGHTS PETITIONER (ROPE) — A marginalized group member or individual petitioner gains direct access to the Supreme Court without traversing lower court hierarchies. Article 32 coordinates the remedy-seeking process: the petitioner can file directly, the Court can issue writs (habeas corpus, mandamus, prohibition, certiorari, quo warranto), and the enforcement mechanism is unified at the apex. This is pure coordination — the beneficiary is the petitioner who now has access, suppression is low (legal right exists), and extraction overhead is minimal (filing fees, no requirement to hire elite advocates for lower courts first). Mobility is high because the petitioner can exercise this right immediately upon violation.
constraint_indexing:constraint_classification(fundamental_rights_part_iii__remedies_article_32, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 2: CIVIL RIGHTS ORGANIZATIONS (ROPE) — Organizations that mount public interest litigation (PIL) coordinated around Article 32 have built a functional mechanism for systemic rights enforcement. The Article 32 remedy architecture enables them to challenge state action, administrative policy, and institutional practices directly at the apex without lower-court gatekeeping. Suppression is low; extraction overhead is minimal (PIL coordinates collective action at scale). The mechanism has generated structural benefits — the Supreme Court's PIL jurisprudence (Menaka Gandhi, Hussainara Khatoon, Sunil Gupta onwards) shows the coordinating function of Article 32 remedies enabling organized actors to scale rights enforcement.
constraint_indexing:constraint_classification(fundamental_rights_part_iii__remedies_article_32, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE SUPREME COURT (TANGLED ROPE) — The Supreme Court simultaneously coordinates remedy-seeking (genuine coordination function: unified apex arbiter, writs that bind all lower courts and executive) and extracts institutional power through its role as final interpreter of rights. The Court's power grows as Article 32 cases multiply — it becomes the de facto enforcer of the Constitution, concentrating authority at the apex. Suppression is moderate (the Court's docket is constrained, filtering what reaches it; many petitions are disposed of on procedural grounds). The Court is both the solution (coordinating remedies) and an extractor (concentrating power, creating delay through case backlog, imposing its own interpretation of rights as final). This is not pure extraction — the Court genuinely enables remedies — but enforcement asymmetry exists: the Court's interpretation is final, lower courts must comply, and litigants bear the time cost.
constraint_indexing:constraint_classification(fundamental_rights_part_iii__remedies_article_32, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ARTICLE 32 AS SCAFFOLD (SCAFFOLD) — From the view of constitutional architects and reformers, Article 32 is a transitional remedy structure designed to enable rights enforcement until lower-court capacity matures. The direct-access architecture sidesteps procedural gatekeeping (lower court hierarchies, standing requirements, exhaustion doctrines) because the drafters did not trust lower courts to protect fundamental rights consistently. As lower courts develop capacity, PIL jurisprudence matures, and public interest mechanisms stabilize, the direct-access bypass becomes less necessary. The sunset clause is structural: if lower-court enforcement becomes reliable, the asymmetry driving Article 32 petitions dissolves. Theater ratio is moderate (procedural elements exist but are minimized). The constraint is scaffolding — temporary support structure solving a coordination gap until institutional maturation enables distributed remedy-seeking.
constraint_indexing:constraint_classification(fundamental_rights_part_iii__remedies_article_32, scaffold,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (ROPE) — From a civilizational view, Article 32 is a pure coordination mechanism: it solves the collective action problem of rights enforcement by designating a single apex authority empowered to issue binding remedies. The constraint's function is to coordinate petitioner access, remedy specification, and enforcement cascade (lower courts must comply with apex writs). Suppression is low (the legal right to petition exists and is enforceable). Theater is low (the writ mechanism is substantive, not performative — writs bind the state). Extractiveness is minimal (no extraction overhead; the petitioner gains direct remedy access). This reading emphasizes Article 32's pure coordination function without attending to institutional power concentration.
constraint_indexing:constraint_classification(fundamental_rights_part_iii__remedies_article_32, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fundamental_rights_part_iii__remedies_article_32_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fundamental_rights_part_iii__remedies_article_32, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fundamental_rights_part_iii__remedies_article_32, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(fundamental_rights_part_iii__remedies_article_32_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. Article 32 remedies impose minimal overhead on petitioners — the right to petition exists, filing fees are nominal (or waived for indigent petitioners), and the mechanism directly connects rights claim to remedy without extracting value. The slight rise from 0.12 to 0.20 over 70 years reflects growing case backlog (increasing wait time, a form of extraction through delay) but the core mechanism remains efficient. Suppression (0.12): Very low. The right to petition is explicitly guaranteed and enforceable; the Supreme Court cannot refuse to hear Article 32 petitions on procedural grounds alone; writs are binding. Suppression exists (docket constraints, elite access bias in petition drafting, geographic access barriers) but is structurally minimal — the right itself is not suppressed. Theater (0.15–0.25): Low. The writ mechanism is substantive, not performative — mandamus commands state action, habeas corpus produces physical release, certiorari quashes illegal orders. Theater ratio rises slightly over time as procedural motions and dismissals on technical grounds increase (a sign of apex gatekeeping emerging), but remains low relative to lower-court litigation theater. Claimed type (rope): Pure coordination. No beneficiary group extracts value from the constraint; all parties benefit from unified remedy architecture. The Supreme Court's power concentration is a side effect (tangled_rope from the Court's institutional perspective) but does not constitute extraction from the petitioner's view.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the petitioner's rope classification (direct access, minimal suppression, pure coordination benefit) and the Supreme Court's tangled_rope classification (genuine coordination function but power concentration and docket bottleneck effects) reveals the asymmetry in how the same constraint is experienced. The petitioner sees relief; the Court sees burden and authority. The analytical observer's rope view (pure coordination) erases the power concentration by adopting a civilizational time horizon and universal scope, where individual case outcomes aggregate into a unified rule-of-law architecture. The scaffold perspective (temporary support until lower-court capacity matures) offers a resolution path: if lower courts develop reliable PIL and constitutional enforcement capacity, the direct-access bypass becomes unnecessary and the constraint sunset. If lower-court capacity stagnates, Article 32 remains structurally necessary, and the scaffold classification is aspirational framing of a permanent institutional feature.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies by actor. Rights petitioners experience low d (they are beneficiaries of direct access; d ≈ 0.15) → f(d) ≈ -0.01 → chi near zero (minimal experienced extraction). Civil rights organizations experience low d (beneficiaries of PIL architecture; d ≈ 0.20) → f(d) ≈ 0.02 → chi near zero. The Supreme Court experiences moderate d (both beneficiary of increased authority and burden-bearer of docket load; d ≈ 0.50) → f(d) ≈ 0.65 → moderate chi (reflects tangled coordination/extraction hybrid). Lower courts experience high d (victims of jurisdictional bypass; constrained to implement apex writs without say in remedy architecture; d ≈ 0.75) → f(d) ≈ 1.15 → chi ≈ 0.21 (moderate experienced extraction). The distribution shows the constraint functions as rope for direct beneficiaries (petitioners, PIL orgs), tangled_rope for the apex authority (Supreme Court), and snare-adjacent for lower courts (constrained by apex hierarchy without remedial agency). No single directionality overrides needed; the canonical derivation captures the structural landscape.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    apex_concentration_vs_coordination_boundary,
    'Does concentrating remedy-seeking power at the Supreme Court apex constitute a coordination benefit (unified authority, consistent jurisprudence) or an extraction mechanism (institutional power concentration, delay bottleneck)?',
    'Comparative analysis: petition disposal times under Article 32 vs lower-court remedy timelines; correlation between case backlog growth and petition filing growth; empirical assessment of lower-court remedy reliability with current capacity.',
    'If coordination dominates: Article 32 is rope from all perspectives, benign institutional concentration. If extraction dominates: Supreme Court''s apex role becomes an extractive bottleneck despite nominal coordination function — reclassify as tangled_rope or snare depending on suppression magnitude.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(apex_concentration_vs_coordination_boundary, empirical, 'Whether apex concentration is coordination benefit or extraction mechanism').

omega_variable(
    lower_court_capacity_maturation_trajectory,
    'Is lower-court institutional capacity for rights enforcement improving, stagnant, or deteriorating relative to the demand for Article 32 remedies?',
    'Longitudinal analysis of lower-court PIL jurisprudence quality and innovativeness; comparative case disposal times; correlation between lower-court judgments and Supreme Court reversals on Article 32 appeals; institutional capacity metrics (judge-to-case ratio, training in constitutional law, infrastructure for enforcement).',
    'If improving: scaffold sunset is real (lower courts will absorb remedy-seeking load). If stagnant or deteriorating: Article 32 direct access remains structurally necessary indefinitely — scaffold classification is aspirational rather than structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lower_court_capacity_maturation_trajectory, empirical, 'Whether lower-court capacity enables future independence from Article 32 scaffold').

omega_variable(
    suppression_mechanism_embedding,
    'Is suppression of rights-without-remedy embedded in procedural barriers that Article 32 bypasses, or does Article 32 itself create new suppression (through docket constraints, elite-access bias, linguistic barriers to petition drafting)?',
    'Comparative analysis: suppression experienced by petitioners before and after Article 32 filing; demographic mapping of successful petitioners (class, education, geographic access); disposal rate variance across petition types; rate at which procedural objections dispose of petitions vs substantive merits decisions.',
    'If Article 32 reduces net suppression: remedies reading validates — access improvement dominates any new bottlenecks. If Article 32 redistributes suppression (removes lower-court gatekeeping but adds apex gatekeeping): tangled_rope classification strengthens; extractiveness rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_embedding, empirical, 'Whether Article 32 reduces suppression or redistributes it').

omega_variable(
    kernel_reading_distinctness,
    'Does the remedies reading (Article 32 as right to remedies, direct access, Ambedkar''s heart-and-soul) constitute a structurally distinct claim from the equality reading (Articles 14-18 as connected equality scheme) and the freedoms reading (Article 19 as bundled freedoms with paired restrictions)?',
    'Doctrinal analysis: Does Article 32 stand independently as a remedial architecture, or is it merely the enforcement mechanism for the substantive rights in Articles 14-19? Can one read Article 32 coherently without resolving the equality-vs-freedoms tension in Articles 14-19? Is Article 32 ever invoked as a constraint-settlement mechanism when Articles 14 and 19 conflict?',
    'If substantively distinct: three separate constraint stories with distinct ε values are justified. If Article 32 is merely enforcement apparatus for Articles 14-19: the remedies reading collapses into network dependencies on the other two, and should be re-authorized as a network node rather than a standalone constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_distinctness, conceptual, 'Whether Article 32 is a distinct doctrinal constraint or enforcement apparatus for Articles 14-19').

omega_variable(
    ambedkar_heart_soul_metaphor_grounding,
    'Ambedkar''s statement that Article 32 is the Constitution''s ''heart and soul'' — is this grounded in a specific structural analysis of why remedy access is foundational to rights, or is it aspirational framing?',
    'Historical textual analysis of Ambedkar''s Constituent Assembly speeches on Article 32; comparison with other constitutional scholars'' accounts of remedy architecture; empirical test — do rights without remedy enforcement actually fail to exist as enforceable rights, or do they persist as declarative rights with uneven enforcement?',
    'If foundational analysis: Article 32 is logically prior to substantive rights (right-without-remedy is not a right). If aspirational: Article 32 is a preferred but not necessary condition for rights (substantive rights can be declared; Article 32 enables their enforcement).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ambedkar_heart_soul_metaphor_grounding, conceptual, 'Grounding of Ambedkar''s characterization of Article 32').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fundamental_rights_part_iii__remedies_article_32, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(article32_theater_1950_adoption, fundamental_rights_part_iii__remedies_article_32, theater_ratio, 0, 0.15).
narrative_ontology:measurement(article32_theater_2020_contemporary, fundamental_rights_part_iii__remedies_article_32, theater_ratio, 70, 0.25).

% Extraction over time
narrative_ontology:measurement(article32_extractiveness_1950_adoption, fundamental_rights_part_iii__remedies_article_32, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(article32_extractiveness_1975_during_emergency, fundamental_rights_part_iii__remedies_article_32, base_extractiveness, 25, 0.15).
narrative_ontology:measurement(article32_extractiveness_1995_pil_maturation, fundamental_rights_part_iii__remedies_article_32, base_extractiveness, 45, 0.18).
narrative_ontology:measurement(article32_extractiveness_2020_contemporary, fundamental_rights_part_iii__remedies_article_32, base_extractiveness, 70, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fundamental_rights_part_iii__remedies_article_32, enforcement_mechanism).
narrative_ontology:affects_constraint(fundamental_rights_part_iii__remedies_article_32, fundamental_rights_part_iii__equality_code).
narrative_ontology:affects_constraint(fundamental_rights_part_iii__remedies_article_32, fundamental_rights_part_iii__freedoms_article_19).

% DUAL FORMULATION NOTE:
% The remedies reading (Article 32) is a network node linking two sibling readings of the same kernel. The equality reading and freedoms reading are structurally dependent on Article 32 for enforcement, but Article 32's own ε-invariance is distinct. The three readings together form a constraint family modeling the Part III fundamental rights architecture. Article 32's low extractiveness (0.18) contrasts with potential higher extractiveness in the equality reading (contested: is equality enforcement a pure coordination, or does it enable state extraction through welfarism?) and the freedoms reading (contested: are paired restrictions on freedoms coordination or suppression?).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
