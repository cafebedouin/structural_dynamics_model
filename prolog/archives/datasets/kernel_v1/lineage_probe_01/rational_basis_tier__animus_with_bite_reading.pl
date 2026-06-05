% ============================================================================
% CONSTRAINT STORY: rational_basis_tier__animus_with_bite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rational_basis_tier__animus_with_bite_reading, []).

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
 *   constraint_id: rational_basis_tier__animus_with_bite_reading
 *   human_readable: Rational Basis Tier with Animus Doctrine: Harm-for-Its-Own-Sake as Irrationality
 *   domain: constitutional_law/equal_protection
 *
 * SUMMARY:
 *   The rational basis tier with animus doctrine reveals a structural
 *   contradiction in constitutional equal protection doctrine. Rational basis
 *   review is nominally the most deferential standard: legislatures receive
 *   presumptive deference, and their laws survive if any conceivable rational
 *   basis exists. Yet in three critical cases (Moreno, Cleburne, Romer),
 *   courts identified bare desire to harm a group as the sole explanation and
 *   invalidated the laws anyway. This reading models that doctrinal
 *   configuration: animus is a cognizable constitutional defect that rational
 *   basis review can detect and remedy, but the remedy remains within the
 *   tier system. The constraint's structure is snare-like: unpopular groups
 *   below suspect status have no exit from their doctrinal tier, yet the
 *   doctrinal tier itself prevents meaningful protection even when animus is
 *   obvious. The tier's suppression mechanism is activated against animus
 *   claimants — the burden of proving animus is high, the class must still
 *   occupy non-suspect status, and invalidation of one animus law does not
 *   change the tier that enabled the harm in the first place. The
 *   constraint's extractiveness (0.68) reflects the severity of harm
 *   inflicted on unpopular groups through doctrinal immunity, modulated by
 *   the recognition that animus cases occasionally provide relief. The
 *   theater ratio (0.55) reflects that rational basis review maintains a
 *   performative review function (courts must articulate a rational basis,
 *   courts do engage with evidence of animus) while losing its actual
 *   gatekeeping function for non-suspect tiers (the doctrine cannot prevent
 *   targeted harm).
 *
 * KEY AGENTS:
 *   - Targeted Unpopular Groups (below suspect status): Primary victims — face state action motivated by bare animus with no reliable doctrinal protection
 *   - Rational Basis Doctrine (as institution): Primary beneficiary — provides immunity for legislatures wanting to target non-suspect groups while performing judicial review
 *   - Equal Protection Advocacy Coalition: Secondary victim/organized claimant — benefits from animus doctrine openings but constrained by tier immunity from extending protection to whole classes
 *   - Judicial Hierarchy: Secondary beneficiary — tier provides clear rule, animus cases allow courts to maintain review appearance while preserving tier boundaries
 *   - Legislating Bodies (using tier as tool): Secondary beneficiary — coordinate behavior within clear doctrinal rules about what classifications will be invalidated
 *   - Analytical Observer: Perspective on whether tier is natural law (immutable) or contingent institution (revisable)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rational_basis_tier__animus_with_bite_reading, 0.68).
domain_priors:suppression_score(rational_basis_tier__animus_with_bite_reading, 0.72).
domain_priors:theater_ratio(rational_basis_tier__animus_with_bite_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rational_basis_tier__animus_with_bite_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(rational_basis_tier__animus_with_bite_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(rational_basis_tier__animus_with_bite_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rational_basis_tier__animus_with_bite_reading, snare).
narrative_ontology:human_readable(rational_basis_tier__animus_with_bite_reading, "Rational Basis Tier with Animus Doctrine: Harm-for-Its-Own-Sake as Irrationality").
narrative_ontology:topic_domain(rational_basis_tier__animus_with_bite_reading, "constitutional_law/equal_protection").

domain_priors:requires_active_enforcement(rational_basis_tier__animus_with_bite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rational_basis_tier__animus_with_bite_reading, '49ded07f-6411-4031-b8bc-ba5f0e90ade0').
narrative_ontology:cs_kernel_codification('49ded07f-6411-4031-b8bc-ba5f0e90ade0', formalized).
narrative_ontology:cs_authority_grounding('49ded07f-6411-4031-b8bc-ba5f0e90ade0', lineage).
narrative_ontology:cs_interpretation_layer_present('49ded07f-6411-4031-b8bc-ba5f0e90ade0').
narrative_ontology:cs_reading_relation('49ded07f-6411-4031-b8bc-ba5f0e90ade0', rational_basis_tier__pure_deference_reading, coexists_with).
narrative_ontology:cs_reading_relation('49ded07f-6411-4031-b8bc-ba5f0e90ade0', rational_basis_tier__class_of_one_reading, influences).
narrative_ontology:cs_axiom('49ded07f-6411-4031-b8bc-ba5f0e90ade0', foundational, animus_is_cognizable_defect).
narrative_ontology:cs_axiom_status(animus_is_cognizable_defect, holdable).
narrative_ontology:cs_axiom_grounding('49ded07f-6411-4031-b8bc-ba5f0e90ade0', animus_is_cognizable_defect, deontological).
narrative_ontology:cs_axiom('49ded07f-6411-4031-b8bc-ba5f0e90ade0', secondary, tier_immunity_survives_animus_detection).
narrative_ontology:cs_axiom_status(tier_immunity_survives_animus_detection, holdable).
narrative_ontology:cs_axiom_grounding('49ded07f-6411-4031-b8bc-ba5f0e90ade0', tier_immunity_survives_animus_detection, conventional).
narrative_ontology:cs_reference_frame('49ded07f-6411-4031-b8bc-ba5f0e90ade0', tier_system_with_animus_cognizable).
narrative_ontology:cs_drift_state('49ded07f-6411-4031-b8bc-ba5f0e90ade0', contemporary, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('49ded07f-6411-4031-b8bc-ba5f0e90ade0', '').
narrative_ontology:cs_kernel_id(rational_basis_tier__animus_with_bite_reading, rational_basis_tier).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rational_basis_tier__animus_with_bite_reading, targeted_unpopular_groups).
narrative_ontology:constraint_victim(rational_basis_tier__animus_with_bite_reading, rational_basis_doctrinal_integrity).
narrative_ontology:constraint_victim(rational_basis_tier__animus_with_bite_reading, equal_protection_claimants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TARGETED UNPOPULAR GROUP (SNARE) — A group beneath suspect classification (not race, not alienage, not fundamental right) cannot exit their classification status. They face state action motivated by bare animus — desire to harm for its own sake — yet rational basis review provides no meaningful protection because the tier itself is the cage. Even when courts identify animus as the sole explanation (Moreno, Cleburne, Romer), the tier's doctrinal floor means the group has no recourse except hoping to rise to suspect status. Maximum extraction — the harm is the mechanism, and the doctrinal tier prevents remedy.
constraint_indexing:constraint_classification(rational_basis_tier__animus_with_bite_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: EQUAL PROTECTION ADVOCACY COALITION (TANGLED ROPE) — Organized advocates benefit from the recognition that animus is a cognizable defect (Moreno, Cleburne, Romer created doctrinal openings), but face severe constraints: these victories did not change the tier itself, so future groups must litigate category by category. The constraint extracts work — constant legal challenge to tier immunity — while providing only partial coordination benefit (occasional relief for specific groups). Constrained exit because tier-based exemption logic forecloses class-based protection.
constraint_indexing:constraint_classification(rational_basis_tier__animus_with_bite_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RATIONAL BASIS DOCTRINE AS INSTITUTIONAL THEATER (PITON) — The doctrine maintains the performative function of 'review' (courts must articulate a purported rational basis, even when animus is obvious) while losing its actual screening function. The ritual persists through institutional precedent and the legitimacy claims of Lee Optical deference, but the function has atrophied: animus cases show the doctrine cannot achieve its stated purpose of distinguishing legitimate from illegitimate state action at the non-suspect tier. Theater ratio reflects this: significant performative content (courts still conduct the review), but minimal real gatekeeping. The doctrine sees itself as degraded but continues through inertia.
constraint_indexing:constraint_classification(rational_basis_tier__animus_with_bite_reading, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: JUDICIAL HIERARCHY ENFORCING TIER IMMUNITY (TANGLED ROPE) — Courts benefit from the tier structure (it provides a clear rule requiring minimal engagement with equal protection substance at the rational basis level) while being constrained by doctrinal precedent that limits their mobility. The cases (Moreno, Cleburne, Romer) required courts to acknowledge animus without remedying it — enforcing the tier's immunity while performing judicial review. The extraction is asymmetric: the rule protects judicial discretion at the cost of meaningful equal protection for disfavored groups. Constrained exit because revisiting the tier itself requires overruling foundational doctrine (Ferguson, substantive due process debate).
constraint_indexing:constraint_classification(rational_basis_tier__animus_with_bite_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGISLATING BODY USING TIER AS TOOL (ROPE) — A legislature that knows the rational basis tier provides immunity for non-suspect classifications experiences the constraint as pure coordination: the tier clarifies which groups can be targeted without risking judicial invalidation. Net beneficiary — the constraint subsidizes targeted harm by providing doctrinal cover. This is genuine coordination for legislative purposes (clear rules about what laws will be struck down and what will survive), but it is coordination in service of extraction.
constraint_indexing:constraint_classification(rational_basis_tier__animus_with_bite_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some doctrinal stratification in equal protection is inevitable: not all classifications can receive the same level of scrutiny, and drawing lines between suspect and non-suspect categories is a necessary feature of any classification system. This reading naturalizes the tier as an immutable feature of constitutional adjudication. However, the structural data contradicts the mountain classification: the animus cases show that the tier is contingent, revisable, and dependent on institutional choices. The engine's false summit detector will identify this as naturalization of a doctrinal institution.
constraint_indexing:constraint_classification(rational_basis_tier__animus_with_bite_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rational_basis_tier__animus_with_bite_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rational_basis_tier__animus_with_bite_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rational_basis_tier__animus_with_bite_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(rational_basis_tier__animus_with_bite_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(rational_basis_tier__animus_with_bite_reading, TR),
    TR >= 0.70.

:- end_tests(rational_basis_tier__animus_with_bite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts severe harm from unpopular groups: targeted groups below suspect status face state action motivated by bare animus (Moreno's targeting of food stamp recipients, Cleburne's targeting of people with intellectual disabilities, Romer's targeting of LGBTQ people), and the doctrinal tier prevents meaningful remedy even when courts identify animus as the sole motivation. The extraction is not higher (0.72+) because animus cases do occasionally invalidate laws, providing some remedy pathway. Suppression (0.72): High. Multiple barriers suppress alternatives: groups must prove animus (high evidentiary burden), must remain in non-suspect tier (cannot achieve suspect status through case-by-case litigation), and have no access to heightened scrutiny even when animus is proven. The tier itself is a suppression mechanism — it forecloses the possibility of meaningful review for non-suspect classifications even when illegitimate purposes are obvious. Theater ratio (0.55): Moderate. Rational basis review performs the review function (courts examine evidence, write opinions about rational bases), but this performance masks the loss of gatekeeping function. The doctrine maintains appearance of review while losing capacity to detect and remedy targeted harm against unpopular groups. Measurements show increasing theater ratio (0.42 → 0.55) as post-Romer doctrine accumulates animus cases that ostensibly protect groups but fail to change the tier system, increasing the performative gap.
 *
 * PERSPECTIVAL GAP:
 *   This constraint generates strong perspectival divergence. The targeted group sees snare — pure extraction with no exit. Equal protection advocates see tangled rope — mixed coordination (animus cases provide doctrinal openings, allow litigation victories) and extraction (tier immunity persists, victories are case-specific). Judicial institutions see piton — the doctrine has lost its gatekeeping function but persists through institutional inertia and precedent. Legislators see rope — the tier provides pure coordination, clarifying which groups can be safely targeted. The analytical observer risks seeing mountain — the tier as an immutable feature of any equal protection system — but this is a false summit: the animus cases show the tier is contingent and revisable. The largest gap is between the powerless victim perspective (snare) and the institutional beneficiary perspective (rope/piton), which reflects the fundamental asymmetry: the constraint provides coordination and efficiency for institutional actors while extracting from groups that cannot exit.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies by agent's structural position relative to this constraint. Targeted groups occupy maximum extraction position (d ≈ 0.95): they are trapped in their tier status, bear full cost of animus-motivated harm, and have no escape route even when animus is proven. Equal protection advocates occupy constrained position (d ≈ 0.55): they benefit from doctrinal openings (animus is cognizable) but face severe constraints (tier immunity persists). Judicial institutions occupy low/negative extraction position (d ≈ 0.15): the tier benefits courts by providing clear rules and allowing them to maintain review appearance without changing tier boundaries. Legislatures occupy arbitrage position (d ≈ 0.10): the tier provides them with clear doctrinal boundaries for permissible targeting. The engine derives these d values from power level + exit options + beneficiary/victim declarations, producing f(d) values that reflect each agent's experienced extractiveness chi.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by showing that rational basis review is correctly classified as snare from the powerless victim perspective (targeted groups with no exit, no meaningful protection even when animus is proven) while simultaneously appearing as rope/piton from institutional perspectives (provides coordination for legislatures, maintains review appearance for courts). The animus doctrine does NOT resolve mandatrophy — it manages it by patching the snare with occasional victories while preserving the tier system that enables the snare. True mandatrophy resolution would require either: (1) eliminating the tier system and applying single-standard rational review to all classifications (converting snare to tangled rope or rope); (2) extending suspect/heightened scrutiny to previously non-suspect groups; or (3) recognizing animus as sufficient to trigger heightened scrutiny even for non-suspect classifications (category-switching at the point of animus detection). The animus doctrine does none of these — it provides relief in egregious cases while reinforcing tier immunity. This is the constraint's strategic function: managing the contradiction without resolving it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    animus_operationalization_threshold,
    'What standard of evidence constitutes ''bare desire to harm'' sufficient to trigger rational basis bite? How do Moreno (legislative history), Cleburne (logical inconsistency), and Romer (discriminatory effect + discriminatory purpose) operationalize animus differently?',
    'Doctrinal mapping: categorize post-Romer animus cases by the evidence type that triggered intervention (legislative history, logical gaps, disparate impact patterns, intra-systemic inconsistency); identify systematic differences in court willingness to infer animus from different evidence types',
    'If operationalization is stringent: animus doctrine applies only in rare clear cases, leaving most unpopular groups unprotected (snare classification holds). If permissive: animus doctrine could extend rational basis review across non-suspect tiers, degrading the tier system itself (Tangled Rope classification would apply more broadly).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(animus_operationalization_threshold, empirical, 'What counts as sufficient evidence of bare animus to trigger rational basis intervention').

omega_variable(
    tier_contingency_or_necessity,
    'Is the suspect/non-suspect tier system a necessary feature of any equal protection doctrine, or a contingent institutional choice that could be replaced by a single-standard rational review?',
    'Comparative constitutional law: how do other democracies structure equal protection review without a formal tier system?; doctrinal analysis of whether Moreno/Cleburne/Romer could be reframed as single-standard rather than tier-bound',
    'If contingent: the animus doctrine is a patching mechanism indicating tier failure (Snare reading holds, with strong pressure for systemic reform). If necessary: the tier is justified but needs refinement to handle animus cases (Piton reading holds — the theatre persists because no true alternative exists).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tier_contingency_or_necessity, conceptual, 'Whether the tier system is constitutionally necessary or a contingent institutional choice').

omega_variable(
    reading_contest_foreclosure,
    'Does the animus-with-bite reading foreclose the pure-deference reading, or do they coexist as live alternatives held by different coalitions?',
    'Doctrinal mapping: identify which judges, circuits, and theoretical traditions maintain pure deference vs. animus-as-cognizable-defect; assess whether any judge or scholar maintains both positions simultaneously',
    'If foreclosed: the animus doctrine has logically defeated pure deference within the evolving constitutional tradition (rare). If coexist: Moreno/Cleburne/Romer remain contested, and different courts/commentators accept both readings (most likely).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_foreclosure, conceptual, 'Structural relationship between animus-with-bite and pure-deference readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rational_basis_tier__animus_with_bite_reading, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rati_tr_t0, rational_basis_tier__animus_with_bite_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(rati_tr_t8, rational_basis_tier__animus_with_bite_reading, theater_ratio, 8, 0.48).
narrative_ontology:measurement(rati_tr_t16, rational_basis_tier__animus_with_bite_reading, theater_ratio, 16, 0.55).

% Extraction over time
narrative_ontology:measurement(rati_be_t0, rational_basis_tier__animus_with_bite_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(rati_be_t8, rational_basis_tier__animus_with_bite_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(rati_be_t16, rational_basis_tier__animus_with_bite_reading, base_extractiveness, 16, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(rati_su_t0, rational_basis_tier__animus_with_bite_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(rati_su_t8, rational_basis_tier__animus_with_bite_reading, suppression_requirement, 8, 0.7).
narrative_ontology:measurement(rati_su_t16, rational_basis_tier__animus_with_bite_reading, suppression_requirement, 16, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rational_basis_tier__animus_with_bite_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(rational_basis_tier__animus_with_bite_reading, rational_basis_tier__class_of_one_reading).
narrative_ontology:affects_constraint(rational_basis_tier__animus_with_bite_reading, rational_basis_tier__pure_deference_reading).
narrative_ontology:affects_constraint(rational_basis_tier__animus_with_bite_reading, suspect_classification_gatekeeping).

% DUAL FORMULATION NOTE:
% The animus-with-bite reading is one decomposition of the rational_basis_tier kernel. The pure-deference reading and class-of-one reading are structurally distinct readings of the same kernel authority structure (formalized tier system). All three readings share the kernel (the tier structure codified in constitutional doctrine) but diverge on what the tier permits and what defects can be recognized within each tier level. This story focuses exclusively on the animus reading's structure (extractiveness, suppression, theater); the sibling readings have their own constraint stories with different ε values reflecting different doctrinal architectures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rational_basis_tier__animus_with_bite_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
