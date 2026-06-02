% ============================================================================
% CONSTRAINT STORY: equal_protection_clause__formal_equality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_clause__formal_equality_reading, []).

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
 *   constraint_id: equal_protection_clause__formal_equality_reading
 *   human_readable: Equal Protection Clause: Formal Equality Reading (Facially Neutral Text Requirement)
 *   domain: constitutional_law/civil_rights
 *
 * SUMMARY:
 *   The formal equality reading of the Equal Protection Clause constrains
 *   constitutional protection to explicit facial classifications in statutory
 *   text. Under this reading, a law is unconstitutional only if it explicitly
 *   categorizes by protected status (race, gender, national origin) on the
 *   face of the statute. Facially neutral laws, regardless of their disparate
 *   impact on protected groups, satisfy the constitutional requirement. This
 *   reading has dominated federal equal protection doctrine for much of the
 *   post-Civil Rights era, particularly in Washington v. Davis (1976). The
 *   constraint exhibits structural tension between its beneficiaries
 *   (institutions wielding legislative power without textual constraint) and
 *   its victims (groups experiencing severe disparate impacts that the
 *   doctrine does not reach). The reading has become increasingly
 *   performative as originalist doctrine invokes historical fidelity while
 *   selectively limiting substantive protection in ways that contradict the
 *   original understanding of baseline equality that Reconstruction-era texts
 *   contemplated.
 *
 * KEY AGENTS:
 *   - Legislative Bodies and Status Quo Holders: Primary beneficiary (institutional/arbitrage) — can achieve discriminatory outcomes through facially neutral means; experiences the constraint as coordination enabling governance
 *   - Groups Experiencing Disparate Impact: Primary victim (powerless/trapped) — cannot challenge laws unless they prove facial discrimination; structural harms go unaddressed
 *   - Civil Rights Advocates and Reform Organizations: Secondary actor (moderate/constrained) — benefit from the clarity of the formal rule as a litigation target but suffer from its narrow scope
 *   - Substantive Equality Coalition: Organized actors (organized/constrained) — view formal equality as temporary doctrine subject to reframing through doctrine evolution and personnel shifts
 *   - Originalist Legal Community: Institutional actor (institutional/arbitrage) — maintains the formal reading through selective invocation of originalist method; benefits from doctrinal stability
 *   - Analytical Observer: Civilizational position (analytical/analytical) — risks naturalizing a contingent doctrinal choice as a logical necessity inherent in constitutional text
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_clause__formal_equality_reading, 0.38).
domain_priors:suppression_score(equal_protection_clause__formal_equality_reading, 0.45).
domain_priors:theater_ratio(equal_protection_clause__formal_equality_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_clause__formal_equality_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(equal_protection_clause__formal_equality_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(equal_protection_clause__formal_equality_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_clause__formal_equality_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_clause__formal_equality_reading, "Equal Protection Clause: Formal Equality Reading (Facially Neutral Text Requirement)").
narrative_ontology:topic_domain(equal_protection_clause__formal_equality_reading, "constitutional_law/civil_rights").

domain_priors:requires_active_enforcement(equal_protection_clause__formal_equality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_clause__formal_equality_reading, '43f626ce-441f-4685-964f-23ff43a9b988').
narrative_ontology:cs_kernel_codification('43f626ce-441f-4685-964f-23ff43a9b988', fixed_text).
narrative_ontology:cs_authority_grounding('43f626ce-441f-4685-964f-23ff43a9b988', lineage).
narrative_ontology:cs_interpretation_layer_present('43f626ce-441f-4685-964f-23ff43a9b988').
narrative_ontology:cs_reading_relation('43f626ce-441f-4685-964f-23ff43a9b988', equal_protection_clause__substantive_equality_reading, coexists_with).
narrative_ontology:cs_reading_relation('43f626ce-441f-4685-964f-23ff43a9b988', equal_protection_clause__anti_subordination_reading, coexists_with).
narrative_ontology:cs_axiom('43f626ce-441f-4685-964f-23ff43a9b988', foundational, statutory_form_determines_constitutionality).
narrative_ontology:cs_axiom_status(statutory_form_determines_constitutionality, holdable).
narrative_ontology:cs_axiom_grounding('43f626ce-441f-4685-964f-23ff43a9b988', statutory_form_determines_constitutionality, deontological).
narrative_ontology:cs_axiom('43f626ce-441f-4685-964f-23ff43a9b988', secondary, disparate_impact_insufficient_for_violation).
narrative_ontology:cs_axiom_status(disparate_impact_insufficient_for_violation, holdable).
narrative_ontology:cs_axiom_grounding('43f626ce-441f-4685-964f-23ff43a9b988', disparate_impact_insufficient_for_violation, conventional).
narrative_ontology:cs_reference_frame('43f626ce-441f-4685-964f-23ff43a9b988', formal_statutory_equality).
narrative_ontology:cs_drift_state('43f626ce-441f-4685-964f-23ff43a9b988', contemporary_divergence_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('43f626ce-441f-4685-964f-23ff43a9b988', '').
narrative_ontology:cs_kernel_id(equal_protection_clause__formal_equality_reading, equal_protection_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_clause__formal_equality_reading, institutional_status_quo_holders).
narrative_ontology:constraint_beneficiary(equal_protection_clause__formal_equality_reading, legislative_discretion_wielders).
narrative_ontology:constraint_victim(equal_protection_clause__formal_equality_reading, groups_experiencing_disparate_impact).
narrative_ontology:constraint_victim(equal_protection_clause__formal_equality_reading, substantive_equality_claimants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPARATE IMPACT GROUPS (SNARE) — Trapped within the formal equality framework that requires proving facial discrimination rather than structural harm. Cannot exit without changing the constitutional reading itself. High experienced extraction: the constraint allows facially neutral laws with severe disparate impact to survive constitutional challenge. Maximum suppression because alternative readings are judicially foreclosed under this framework.
constraint_indexing:constraint_classification(equal_protection_clause__formal_equality_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REFORM-SEEKING ADVOCATES (TANGLED ROPE) — Constrained by the burden of proving facial discrimination, but coordination value exists: the formal rule provides predictable doctrine and creates a clear litigation target. Some advocacy groups benefit from the clarity of the rule even as they fight against its outcomes. Mixed experience of coordination (predictable rule framework) and extraction (narrow scope for successful challenges).
constraint_indexing:constraint_classification(equal_protection_clause__formal_equality_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LEGISLATIVE/STATUS QUO HOLDERS (ROPE) — Primary beneficiary. The formal equality rule gives legislators maximal discretion: they can achieve discriminatory outcomes through facially neutral means without legal constraint. Experiences the constraint as pure coordination — communicating legislative intent via neutral statutory text enables governance. Zero experienced extraction for this agent; the rule directly benefits them.
constraint_indexing:constraint_classification(equal_protection_clause__formal_equality_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: SUBSTANTIVE EQUALITY COALITION (SCAFFOLD) — Organized actors (civil rights organizations, progressive jurists, some academic movements) see the formal equality constraint as temporary, subject to reframing through doctrine evolution and demographic shifts. The coalition views this reading as currently dominant but not durable — disparate impact doctrine, strict scrutiny calibration, and intersectionality frameworks represent exit pathways. Sunset logic: as the coalition builds institutional power (appointment of judges, shifting political coalitions), the formal reading's dominance wanes.
constraint_indexing:constraint_classification(equal_protection_clause__formal_equality_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ORIGINALIST JURISPRUDENCE (PITON) — The formal equality reading claims to be grounded in the original public meaning of the Fourteenth Amendment's text ('equal protection of the laws'). However, this reading has become substantially performative: originalists invoke it selectively (applying it strictly to facially discriminatory laws but loosely to facially neutral laws with disparate impact that align with original understanding). Theater ratio elevated by the selective invocation of originalist method. The reading persists through institutional inertia in law schools and appellate doctrine despite internal inconsistencies.
constraint_indexing:constraint_classification(equal_protection_clause__formal_equality_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a logical/textual perspective, formal equality appears as an inescapable requirement: the word 'equal' in 'equal protection' can only mean 'same treatment in statutory language,' and any deviation toward substance opens boundless judicial discretion. This perspective sees the formal equality rule as a logical necessity, not a contingent doctrine. However, structural data contradicts the mountain classification — identified beneficiaries and enforcement mechanisms reveal this as a false summit: the 'logical requirement' naturalizes a reading that benefits institutional status quo holders.
constraint_indexing:constraint_classification(equal_protection_clause__formal_equality_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_clause__formal_equality_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(equal_protection_clause__formal_equality_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(equal_protection_clause__formal_equality_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(equal_protection_clause__formal_equality_reading, TR),
    TR >= 0.70.

:- end_tests(equal_protection_clause__formal_equality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The formal equality reading permits facially neutral laws with disparate impact to survive challenge, extracting protection from groups experiencing structural harm. However, the extraction is not total because (1) facial discrimination claims remain available as a coordination mechanism for litigation; (2) other doctrinal frameworks (Title VII disparate impact doctrine, section 5 of the Voting Rights Act as initially understood) provide alternative paths, though narrower ones; (3) some states adopt more expansive equal protection readings. The extractiveness has increased over the interval (0.28 → 0.38) as the domain of facially neutral laws with known disparate impacts has grown and as alternative doctrinal protections have been narrowed. Suppression (0.45): Moderate. Barriers to challenging facially neutral laws are substantial — the burden of proving facial discrimination is difficult and time-consuming — but not absolute. Some litigation resources exist; some sympathetic judges interpret the rule expansively. Theater ratio (0.55): Moderate-high. The originalist invocation of the formal rule has become increasingly performative as doctrinal application diverges from stated originalist principles. The rule is justified by reference to original meaning, but application is selective — originating era equality baseline concepts are ignored when they would expand protection. Theater has increased (0.40 → 0.55) as the gap between originalist rhetoric and substantive doctrine has widened.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same constitutional text generates radically different classifications from different positions. Legislative bodies see coordination (Rope) — a clear rule enabling neutral statutory language. Disparate impact groups see pure extraction (Snare) — trapped within a framework that doesn't reach their harms. Reform advocates see mixed coordination and extraction (Tangled Rope) — they benefit from doctrinal clarity but suffer from doctrinal narrowness. The substantive equality coalition sees a temporary problem (Scaffold) — a reading they believe can be reframed as judges and political coalitions shift. Originalists see their own doctrine as performative (Piton) — formally grounded in historical meaning but selectively applied to maintain stability. The civilizational analytical observer risks seeing logical necessity (Mountain) — 'equal protection' must mean 'same statutory language' — but structural data reveals beneficiaries and enforcement mechanisms that contradict the false summit claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from the agent's structural position in the extraction flow. Legislative bodies and status quo holders are net beneficiaries with arbitrage options — they can craft facially neutral laws and depart the equal protection framework entirely through constitutional amendment or institutional change. Groups experiencing disparate impact are trapped victims with no structural exit — they cannot avoid the constraint's effects. Civil rights advocates are moderately constrained — they can litigate but face severe burden of proof. The substantive equality coalition is organized and constrained — they have institutional power (some academic positions, some judicial allies) but face a dominant reading that limits their leverage. The originalist community benefits as institutional custodians of the doctrine; they have arbitrage options (they can shift doctrine interpretation) but currently exercise discretion to maintain the formal reading because it stabilizes their intellectual project.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by clarifying that 'formal equality reading' is a specific institutional commitment to a particular interpretation of the equal protection kernel, not a discovery of what the clause logically requires. The constraint can be Tangled Rope (mixed coordination and extraction) without being Rope (pure coordination) because the reading benefits some actors while extracting from others. The false summit (Mountain perspective) is diagnostic: it reveals that the 'logical necessity' frame naturalizes a contingent doctrinal choice. The analytical observer risks committing the oracle gap (Theorem 4) — claiming that the text's logical form determines the reading — when in fact the reading is an institutional choice that different positions reveal differently.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    facial_discrimination_definition_boundary,
    'What counts as a ''facial'' classification for equal protection purposes? Does a law facially classify if its effect depends on an implicit categorization (e.g., criminal sentencing guidelines that are race-neutral on their face but apply disparately because of baseline inequality in arrest patterns)?',
    'Case law analysis tracing the boundary: compare cases in which courts found facial discrimination (e.g., Loving v. Virginia — explicit race language) vs. cases rejecting facial discrimination claims (e.g., Washington v. Davis — facially neutral law with disparate impact). Identify structural characteristics that determine facial vs. neutral across decided cases.',
    'If the boundary shifts toward implicit categorization: more laws become facially discriminatory; formal equality expands dramatically. If the boundary holds strict textual form: many laws with architecturally embedded disparate impact escape scrutiny; formal equality constrains only explicit language.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(facial_discrimination_definition_boundary, empirical, 'Definition boundary for facial vs. facially neutral classification').

omega_variable(
    original_public_meaning_underspecification,
    'What was the original public meaning of ''equal protection of the laws'' in 1868? Did it contemplate only explicit classifications or also structural inequalities in baseline conditions?',
    'Historical scholarship on Reconstruction-era debates; analysis of how contemporaries understood ''equal protection'' in light of existing institutional inequalities (slavery abolition, women''s disenfranchisement, property qualifications). Compare originalist interpretations offered by different schools.',
    'If original meaning was facially narrow (explicit language only): formal equality is the historically grounded reading. If original meaning contemplated structural baseline equality: formal equality misreads the original; substantive equality becomes the originalist position.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_public_meaning_underspecification, empirical, 'Original public meaning of equal protection clause in 1868').

omega_variable(
    empirical_distribution_of_disparate_impact,
    'How many facially neutral laws produce disparate impacts that would fail substantive equal protection scrutiny but survive formal equal protection scrutiny? What is the aggregate distribution of protection denial under formal vs. substantive readings?',
    'Systematic analysis of state and federal law corpus: identify all facially neutral laws, measure their disparate impacts across protected groups, estimate which would be invalidated under substantive vs. formal equal protection. Compute aggregate protection differential.',
    'If the distribution is large and systematic (many facially neutral laws, large disparate impacts): formal equality permits massive structural extraction; the Snare classification is empirically grounded. If the distribution is small and idiosyncratic: formal equality is a minor carve-out; the Tangled Rope classification may overestimate impact.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(empirical_distribution_of_disparate_impact, empirical, 'Empirical distribution of disparate impact under facially neutral laws').

omega_variable(
    kernel_vs_reading_distinction_application,
    'This constraint is one reading (formal equality) of a contested kernel (the equal protection clause itself). Can a single legal framework coherently hold multiple readings simultaneously, or does institutional authority force a monolithic interpretation?',
    'Doctrinal analysis: examine whether courts have applied formal equality to some categories (race) and substantive equality to others (gender, disability). Track doctrine evolution and jurisdictional variation. Assess whether the readings coexist in practice or mutually exclude each other.',
    'If readings coexist: formal equality is one legitimate reading among others; the coexists_with relation is empirically grounded. If readings are mutually exclusive in any single court: this reading forecloses the others within that jurisdiction; the relation is stronger than coexistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_vs_reading_distinction_application, conceptual, 'Whether equal protection doctrine permits simultaneous coexistence of formal and substantive readings or enforces monolithic interpretation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_clause__formal_equality_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epc_formal_tr_t0, equal_protection_clause__formal_equality_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(epc_formal_tr_t50, equal_protection_clause__formal_equality_reading, theater_ratio, 50, 0.48).
narrative_ontology:measurement(epc_formal_tr_t100, equal_protection_clause__formal_equality_reading, theater_ratio, 100, 0.55).

% Extraction over time
narrative_ontology:measurement(epc_formal_be_t0, equal_protection_clause__formal_equality_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(epc_formal_be_t50, equal_protection_clause__formal_equality_reading, base_extractiveness, 50, 0.35).
narrative_ontology:measurement(epc_formal_be_t100, equal_protection_clause__formal_equality_reading, base_extractiveness, 100, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_clause__formal_equality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_protection_clause__formal_equality_reading, equal_protection_clause__substantive_equality_reading).
narrative_ontology:affects_constraint(equal_protection_clause__formal_equality_reading, equal_protection_clause__anti_subordination_reading).
narrative_ontology:affects_constraint(equal_protection_clause__formal_equality_reading, disparate_impact_doctrine_title_vii).
narrative_ontology:affects_constraint(equal_protection_clause__formal_equality_reading, voting_rights_act_section_five_original).

% DUAL FORMULATION NOTE:
% The equal protection clause is a kernel with multiple readings. The formal equality reading is one constraint story; the substantive equality reading and anti-subordination reading are separate constraint stories instantiating the same kernel differently. Each reading has its own ε, its own beneficiary/victim structure, and its own perspectives. They are linked via network.affects_constraints because they compete for institutional authority over the same constitutional text. The formal reading affects (but does not foreclose) the substantive reading: if formal equality is judicially dominant, substantive equality advocates must work through legislative remedies and doctrine reframing. If substantive equality becomes dominant, it influences the formal reading's scope by narrowing the range of facially neutral laws treated as per se constitutional.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
