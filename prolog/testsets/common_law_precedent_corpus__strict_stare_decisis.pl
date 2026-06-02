% ============================================================================
% CONSTRAINT STORY: common_law_precedent_corpus__strict_stare_decisis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_law_precedent_corpus__strict_stare_decisis, []).

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
 *   constraint_id: common_law_precedent_corpus__strict_stare_decisis
 *   human_readable: Strict Stare Decisis: Precedent as Backward Constraint
 *   domain: legal_theory/jurisprudence/constitutional_law
 *
 * SUMMARY:
 *   Stare decisis — the doctrine that precedent is binding and should not be
 *   overruled without extraordinary justification — creates a structural
 *   constraint on jurisprudential change. This constraint is one reading of
 *   the common law precedent corpus kernel. The strict stare decisis reading
 *   emphasizes rigidity, stability, and the rarity of precedent overruling;
 *   litigants seeking to challenge established doctrine face suppressive
 *   barriers and must justify departure as extraordinary. This reading
 *   coexists with two alternative readings: the evolutionary framework (which
 *   treats precedent as guidance rather than binding authority and allows
 *   gradual doctrinal development) and the pluralist balancing approach
 *   (which weights stare decisis against other interpretive values and allows
 *   more fluid overruling). The strict reading has been more dominant in
 *   common law traditions and formalist jurisprudence, but the last 40 years
 *   show rising extractiveness and suppression as reform movements encounter
 *   stronger resistance and the 'extraordinary justification' threshold has
 *   become more contested. The theater_ratio remains moderate because stare
 *   decisis disputes are substantive legal debates with real content, not
 *   purely performative — the doctrine is not degraded (piton) but rather
 *   represents a genuine structural choice about how much backward constraint
 *   to enforce. Beneficiaries are those whose legal positions are secured by
 *   existing precedent; victims are litigants seeking doctrinal change and
 *   jurisprudential innovation communities.
 *
 * KEY AGENTS:
 *   - Litigants Challenging Precedent: Primary victims (powerless/trapped) — face near-absolute barriers to changing established law; burden of extraordinary justification falls entirely on them
 *   - Lower Court Judges: Secondary victims (moderate/constrained) — must follow binding precedent even when convinced it is wrong; face reversal and professional discipline for departing
 *   - Established Doctrine Holders: Primary beneficiaries (institutional/arbitrage) — their legal positions are secured and protected against disruption; exit from protection would mean losing precedential foundation
 *   - Constitutional Reform Movements: Secondary agents (organized/mobile) — can petition for overruling and have some collective agency, but face asymmetric burden of proof
 *   - Mid-Tier Appellate Courts: Institutional actors (institutional/constrained) — bound by higher precedent but create binding authority for lower courts; constrained exit through distinction and strategic narrow holdings
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing a doctrine-specific choice (strict stare decisis) as inherent to law itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_law_precedent_corpus__strict_stare_decisis, 0.52).
domain_priors:suppression_score(common_law_precedent_corpus__strict_stare_decisis, 0.68).
domain_priors:theater_ratio(common_law_precedent_corpus__strict_stare_decisis, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, extractiveness, 0.52).
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_law_precedent_corpus__strict_stare_decisis, tangled_rope).
narrative_ontology:human_readable(common_law_precedent_corpus__strict_stare_decisis, "Strict Stare Decisis: Precedent as Backward Constraint").
narrative_ontology:topic_domain(common_law_precedent_corpus__strict_stare_decisis, "legal_theory/jurisprudence/constitutional_law").

domain_priors:requires_active_enforcement(common_law_precedent_corpus__strict_stare_decisis).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_law_precedent_corpus__strict_stare_decisis, '6ece0f1e-2f04-4216-89ea-19a705f9289e').
narrative_ontology:cs_kernel_codification('6ece0f1e-2f04-4216-89ea-19a705f9289e', formalized).
narrative_ontology:cs_authority_grounding('6ece0f1e-2f04-4216-89ea-19a705f9289e', lineage).
narrative_ontology:cs_interpretation_layer_present('6ece0f1e-2f04-4216-89ea-19a705f9289e').
narrative_ontology:cs_reading_relation('6ece0f1e-2f04-4216-89ea-19a705f9289e', common_law_precedent_corpus__evolutionary_framework, coexists_with).
narrative_ontology:cs_reading_relation('6ece0f1e-2f04-4216-89ea-19a705f9289e', common_law_precedent_corpus__pluralist_balancing, coexists_with).
narrative_ontology:cs_axiom('6ece0f1e-2f04-4216-89ea-19a705f9289e', foundational, precedent_nearly_absolute_absent_extraordinary_justification).
narrative_ontology:cs_axiom_status(precedent_nearly_absolute_absent_extraordinary_justification, holdable).
narrative_ontology:cs_axiom_grounding('6ece0f1e-2f04-4216-89ea-19a705f9289e', precedent_nearly_absolute_absent_extraordinary_justification, deontological).
narrative_ontology:cs_axiom('6ece0f1e-2f04-4216-89ea-19a705f9289e', foundational, institutional_stability_primacy_over_doctrinal_evolution).
narrative_ontology:cs_axiom_status(institutional_stability_primacy_over_doctrinal_evolution, holdable).
narrative_ontology:cs_axiom_grounding('6ece0f1e-2f04-4216-89ea-19a705f9289e', institutional_stability_primacy_over_doctrinal_evolution, instrumental).
narrative_ontology:cs_reference_frame('6ece0f1e-2f04-4216-89ea-19a705f9289e', formalist_bounded_judicial_discretion).
narrative_ontology:cs_drift_state('6ece0f1e-2f04-4216-89ea-19a705f9289e', contemporary_constitutional_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6ece0f1e-2f04-4216-89ea-19a705f9289e', '').
narrative_ontology:cs_kernel_id(common_law_precedent_corpus__strict_stare_decisis, common_law_precedent_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__strict_stare_decisis, established_doctrine_holders).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__strict_stare_decisis, institutional_continuity_interests).
narrative_ontology:constraint_victim(common_law_precedent_corpus__strict_stare_decisis, litigants_challenging_precedent).
narrative_ontology:constraint_victim(common_law_precedent_corpus__strict_stare_decisis, jurisprudential_innovation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% A party seeking to overturn established precedent faces near-absolute barriers: precedent is binding authority, departure requires extraordinary justification, and the petitioner bears the burden of proving the prior holding is demonstrably erroneous. The litigant cannot exit the constraint without changing the law itself — a task beyond individual agency.
constraint_indexing:constraint_classification(common_law_precedent_corpus__strict_stare_decisis, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% A trial or appellate judge in a jurisdiction where stare decisis is strictly enforced must follow binding precedent even when individually convinced it is wrong. The judge faces professional consequences (reversal, bar discipline) for departing from precedent without extraordinary justification. The constraint is enforced through appellate oversight and institutional hierarchy.
constraint_indexing:constraint_classification(common_law_precedent_corpus__strict_stare_decisis, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% Beneficiaries of established precedent experience the constraint as stable coordination: stare decisis ensures their legal positions remain valid, contracts and rights retain predictable meaning, and their settled expectations are protected across generations. The constraint subsidizes this agent's position — exit would mean losing the precedential foundation of their advantages.
constraint_indexing:constraint_classification(common_law_precedent_corpus__strict_stare_decisis, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Organized agents (appellate bars, constitutional scholars, reform coalitions) see stare decisis as both coordination mechanism and constraint on jurisprudential evolution. They can challenge precedent through Supreme Court petition (mobile exit) and have some agency through collective action, but the burden of extraordinary justification creates asymmetric extraction. The constraint has genuine coordination function (it prevents constant re-litigation) but embeds significant asymmetric cost on those seeking change.
constraint_indexing:constraint_classification(common_law_precedent_corpus__strict_stare_decisis, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% A circuit court or intermediate appellate body experiences stare decisis as enforced coordination: it must follow precedent from higher courts (binding authority) but also creates binding authority for lower courts through its own decisions. The court has constrained exit — it cannot overturn Supreme Court precedent without extraordinary justification, but it can distinguish cases, create doctrine in areas of precedential silence, and influence future Supreme Court decisions through bold interpretation.
constraint_indexing:constraint_classification(common_law_precedent_corpus__strict_stare_decisis, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% From a civilizational perspective, some backward constraint is inherent to law itself: any legal system requires doctrinal stability, precedent provides predictability, and complete overruling of prior holdings would dissolve the rule of law. Stare decisis appears as a necessary structural feature of legality rather than a contingent institutional arrangement. However, this perspective risks naturalizing what is actually a doctrine-specific choice about how much constraint is appropriate.
constraint_indexing:constraint_classification(common_law_precedent_corpus__strict_stare_decisis, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_law_precedent_corpus__strict_stare_decisis_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(common_law_precedent_corpus__strict_stare_decisis, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(common_law_precedent_corpus__strict_stare_decisis, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_law_precedent_corpus__strict_stare_decisis, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(common_law_precedent_corpus__strict_stare_decisis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts significant cost from litigants seeking doctrinal change and from innovative jurisprudential communities. Those who benefit from established precedent receive protection subsidy. However, extractiveness is not maximal (≥0.66 snare threshold) because the 'extraordinary justification' standard is not absolute — overruling does occur, and litigants retain resort to Supreme Court petition (mobile exit). The metric reflects a 40-year trend of rising extractiveness as reform movements increasingly encounter resistance and the bar for overruling has been rhetorically heightened. Suppression (0.68): High. The barriers to precedent challenge are substantial: binding authority of prior holdings, burden of proof on petitioner, appellate oversight enforcing compliance, professional consequences for departure, institutional resistance to doctrinal instability. But suppression is not absolute (0.90+) because strategic distinction, circuit splits, and supreme court petition represent partial exits. Theater ratio (0.44): Moderate-low. Stare decisis disputes are substantive legal debates with real doctrinal content. The constraint does not rely primarily on performative compliance — judges genuinely are bound by obligation and appellate enforcement, not merely theater. The theater that exists is the 'extraordinary justification' rhetoric: the language of extraordinary burden may do some performative work normalizing the constraint beyond its actual functional necessity. Accessibility collapse (0.68): Courts and precedent are nominally accessible to litigants, but the practical barrier to changing precedent is high. Resistance (0.42): Moderate. Significant institutional interest in doctrinal stability exists, but legal actors retain capacity to resist through distinction, circuit-split creation, and philosophical critique.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits substantial perspectival disagreement. The beneficiary of established precedent (institutional) sees coordination and stability (rope) — the constraint solves the problem of constant re-litigation and provides predictable law. The litigant trapped by adverse precedent sees extraction and suppression (snare) — they face nearly insurmountable barriers to change. The lower court judge sees mixed coordination and constraint (tangled rope) — the stability is functional but the suppression is real. The reform movement sees a temporary obstacle with agency (tangled rope moving toward scaffold if political will develops). The analytical observer risks naturalizing institutional choice as legal necessity (false summit mountain). The perspectival gap reveals that strict stare decisis is not a natural feature of law but a doctrine-specific choice about institutional rigidity — other readings allow different equilibria.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position relative to the constraint. Litigants challenging precedent bear extraction (high d, ~0.80-0.90). Established doctrine holders benefit from it (low d, ~0.10-0.20). Lower court judges face constrained exit and enforcement overhead (d ~0.65-0.75). Reform movements have mobile exit through petition and public sphere advocacy (d ~0.55-0.65). The analytical observer at civilizational scope experiences maximum dereferencing from material interests (d ~0.72). The derived directionality feeds into the chi calculation through the sigmoid f(d), producing the effective extractiveness experienced by each perspective. Beneficiary institutional actors derive negative or very low effective extraction (rope perspective), while victims facing trapped/constrained exit experience maximal or high effective extraction (snare/tangled rope perspectives).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved through the kernel frame: strict stare decisis is ONE reading of the common law precedent corpus kernel, not the only reading. The doctrine's extractiveness (0.52) derives from its choice of HIGH constraint rigidity over evolutionary flexibility. Alternative readings (evolutionary framework, pluralist balancing) would produce different extractiveness values because they authorize different overruling thresholds. The constraint story models the strict reading's structural properties — what it extracts, who benefits, who pays, what barriers exist — without claiming this reading is the only legitimate jurisprudential position. The sibling readings coexist as live positions held by different judicial coalitions and jurisdictions. The analytical observer's risk of naturalizing the strict reading as inherent law is exactly what the kernel frame prevents: by naming the reading as one of multiple coexisting readings, the story reveals the choice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    precedent_overruling_threshold,
    'What level of doctrinal error or changed circumstances justifies overruling precedent under strict stare decisis? Is the threshold fixed, evolved, or contestable?',
    'Historical analysis of Supreme Court overrulings: frequency, stated justifications, correlation with ''extraordinary justification'' rhetoric; comparison of overruling rates across eras and doctrine areas',
    'If threshold is fixed: stare decisis operates as a genuine constraint on jurisprudential evolution with measurable suppression cost. If evolved: the doctrine''s rigidity has loosened over time, reducing suppression. If contestable: different justices apply different thresholds, producing perspectival disagreement about constraint severity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(precedent_overruling_threshold, empirical, 'Threshold for extraordinary justification in precedent overruling').

omega_variable(
    kernel_reading_distinction,
    'Is strict stare decisis (this reading) distinguishable from evolutionary framework and pluralist balancing readings, or do all three coexist as live jurisprudential positions held by different judicial coalitions?',
    'Doctrinal analysis: map each reading to specific Supreme Court justices or circuits; identify cases where one reading explicitly forecloses another vs. cases where both are defended simultaneously; test whether a single jurisdiction can coherently adopt multiple readings',
    'If distinct and coexisting: all three readings are live, justifying coexists_with relations. If one reading forecloses others within its own framework: justify forecloses relation. If readings are sequential (older tradition superseded by newer): justify overridden axiom status in the superseded reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_distinction, conceptual, 'Whether strict stare decisis forecloses or coexists with sibling readings').

omega_variable(
    extraordinary_justification_enforcement,
    'Is ''extraordinary justification'' for overruling precedent enforced as a genuine gate (making overruling rare), or has it become performative language that masks easier overruling in practice?',
    'Empirical: measure overruling frequency pre-post doctrine establishment; analyze whether stated justifications genuinely differ between overruled and affirmed precedents; compare overruling rates for precedents labeled ''wrong'' vs. those labeled ''outdated''',
    'If enforced: extractiveness and suppression values validated. If performative: the constraint functions at lower severity than claimed, theater_ratio should increase, and the classification should shift toward piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraordinary_justification_enforcement, empirical, 'Whether ''extraordinary justification'' requirement is genuinely enforced').

omega_variable(
    institutional_identity_lock,
    'Do judges internalize stare decisis as constitutive of judicial identity and rule-of-law commitment, or do they experience it primarily as an external constraint?',
    'Jurisprudential analysis: study judicial rhetoric in opinions defending stare decisis; interview data on judge perception of precedent obligation; analysis of whether judges who overturn precedent experience identity rupture or professional conflict',
    'If internalized identity: the constraint operates partly through identity_locked mechanism; judicial victims experience the constraint as binding on the self, not just externally. If external constraint: the binding mechanism is suppression/enforcement through appellate oversight.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_identity_lock, conceptual, 'Whether stare decisis is internalized judicial identity or external enforcement').

omega_variable(
    precedent_distinction_elasticity,
    'Is the ability to ''distinguish'' prior precedent (apply it narrowly, treat it as limited to its facts) a genuine exit option from stare decisis, or does strict stare decisis foreclose distinction and require direct overruling for any meaningful departure?',
    'Doctrine analysis: cases where distinction was used vs. overruled; study whether strict stare decisis jurisdictions allow broader distinction-based doctrinal evolution than they allow overruling; test whether distinction is treated as legitimate doctrinal development or as evasion of binding authority',
    'If distinction is robust exit: suppression is lower than 0.68, and litigants have more agency than trapped classification suggests (should be constrained). If distinction is foreclosed: snare classification validated, and precedent is nearly absolute.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(precedent_distinction_elasticity, empirical, 'Whether distinction provides meaningful exit from stare decisis constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_law_precedent_corpus__strict_stare_decisis, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(staredec_tr_t0, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 0, 0.38).
narrative_ontology:measurement(staredec_tr_t3, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 3, 0.41).
narrative_ontology:measurement(staredec_tr_t6, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 6, 0.44).

% Extraction over time
narrative_ontology:measurement(staredec_be_t0, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(staredec_be_t3, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(staredec_be_t6, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 6, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(staredec_su_t0, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(staredec_su_t3, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 3, 0.65).
narrative_ontology:measurement(staredec_su_t6, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 6, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_law_precedent_corpus__strict_stare_decisis, enforcement_mechanism).
narrative_ontology:affects_constraint(common_law_precedent_corpus__strict_stare_decisis, common_law_precedent_corpus__evolutionary_framework).
narrative_ontology:affects_constraint(common_law_precedent_corpus__strict_stare_decisis, common_law_precedent_corpus__pluralist_balancing).
narrative_ontology:affects_constraint(common_law_precedent_corpus__strict_stare_decisis, constitutional_amendment_difficulty).
narrative_ontology:affects_constraint(common_law_precedent_corpus__strict_stare_decisis, judicial_review_scope).

% DUAL FORMULATION NOTE:
% The common law precedent corpus decomposes into at least three constraint stories with different extractiveness values corresponding to different readings of how much rigidity to enforce. Strict stare decisis is the high-constraint reading (ε=0.52). Evolutionary framework would lower extractiveness (ε~0.35) by authorizing more fluid doctrinal development. Pluralist balancing would allow context-dependent extractiveness (ε~0.40) by weighing stare decisis against other values. The three stories are linked because they represent competing institutionalizations of the same kernel — different jurisdictions and eras enforce different thresholds. The network relation is coexist-and-compete rather than sequential replacement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(common_law_precedent_corpus__strict_stare_decisis, institutional, 0.15).
constraint_indexing:directionality_override(common_law_precedent_corpus__strict_stare_decisis, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
