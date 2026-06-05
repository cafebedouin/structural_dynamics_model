% ============================================================================
% CONSTRAINT STORY: magna_carta_1215__common_law_foundation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_1215__common_law_foundation_reading, []).

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
 *   constraint_id: magna_carta_1215__common_law_foundation_reading
 *   human_readable: Magna Carta 1215: Common Law Foundation Reading
 *   domain: legal/constitutional/doctrinal
 *
 * SUMMARY:
 *   This constraint instantiates the common-law foundation reading of Magna
 *   Carta 1215: the document's clauses 39 and 40 (prohibiting arbitrary
 *   disseisement and establishing the right to due process) seeded a
 *   generalizable principle — that the Crown itself is subject to law — which
 *   successive reconfirmations transformed into a foundational commitment of
 *   English constitutional order. The constraint's core mechanism is the
 *   repeated political act of reconfirming Magna Carta, which operationalizes
 *   the principle that legality is the Crown's permanent condition. This
 *   reading privileges the long-term institutional development of common-law
 *   courts as independent arbiters of the Crown's compliance with a fixed
 *   standard. The constraint coordinates a stable legal order (beneficiary:
 *   political community, common-law courts) while suppressing prerogative
 *   justice (victim set: arbitrary rule, unaccountable power). The
 *   extractiveness trajectory reflects this: high extraction cost to the
 *   Crown initially (constraining prerogative is a significant loss),
 *   declining over centuries as legality becomes internalized and the courts'
 *   role becomes institutionalized, reaching a stable moderate level once the
 *   constraint is fully embedded in parliamentary tradition and judicial
 *   practice.
 *
 * KEY AGENTS:
 *   - Subjects claiming due process (powerless/trapped initially, then moderate/constrained): primary beneficiaries of the constraint's enforcement; primary victims of prerogative justice
 *   - Common-law courts (institutional/arbitrage): institutional beneficiary and enforcement mechanism; gain authority and autonomy through the constraint
 *   - The Crown (powerful/constrained): victim of prerogative suppression; beneficiary of legal order's stability; experiences mixed extraction
 *   - Political community / parliamentary tradition (institutional/arbitrage): long-term beneficiary; develops constitutional identity around legality as permanent condition
 *   - Prerogative justice (structural antagonist, not an agent): the mechanism the constraint suppresses; victim in the structural sense
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_1215__common_law_foundation_reading, 0.18).
domain_priors:suppression_score(magna_carta_1215__common_law_foundation_reading, 0.42).
domain_priors:theater_ratio(magna_carta_1215__common_law_foundation_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_1215__common_law_foundation_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(magna_carta_1215__common_law_foundation_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(magna_carta_1215__common_law_foundation_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_1215__common_law_foundation_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_1215__common_law_foundation_reading, "Magna Carta 1215: Common Law Foundation Reading").
narrative_ontology:topic_domain(magna_carta_1215__common_law_foundation_reading, "legal/constitutional/doctrinal").

domain_priors:requires_active_enforcement(magna_carta_1215__common_law_foundation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_1215__common_law_foundation_reading, 'd0c125ef-9b94-4d89-98c8-9229e3d81952').
narrative_ontology:cs_kernel_codification('d0c125ef-9b94-4d89-98c8-9229e3d81952', fixed_text).
narrative_ontology:cs_authority_grounding('d0c125ef-9b94-4d89-98c8-9229e3d81952', lineage).
narrative_ontology:cs_interpretation_layer_present('d0c125ef-9b94-4d89-98c8-9229e3d81952').
narrative_ontology:cs_reading_relation('d0c125ef-9b94-4d89-98c8-9229e3d81952', magna_carta_1215__feudal_bargain_reading, influences).
narrative_ontology:cs_reading_relation('d0c125ef-9b94-4d89-98c8-9229e3d81952', magna_carta_1215__symbolic_myth_reading, coexists_with).
narrative_ontology:cs_axiom('d0c125ef-9b94-4d89-98c8-9229e3d81952', foundational, legality_as_permanent_crown_condition).
narrative_ontology:cs_axiom_status(legality_as_permanent_crown_condition, holdable).
narrative_ontology:cs_axiom_grounding('d0c125ef-9b94-4d89-98c8-9229e3d81952', legality_as_permanent_crown_condition, conventional).
narrative_ontology:cs_axiom('d0c125ef-9b94-4d89-98c8-9229e3d81952', foundational, repeated_reconfirmation_establishes_constitutionality).
narrative_ontology:cs_axiom_status(repeated_reconfirmation_establishes_constitutionality, holdable).
narrative_ontology:cs_axiom_grounding('d0c125ef-9b94-4d89-98c8-9229e3d81952', repeated_reconfirmation_establishes_constitutionality, conventional).
narrative_ontology:cs_reference_frame('d0c125ef-9b94-4d89-98c8-9229e3d81952', legality_binding_the_crown).
narrative_ontology:cs_drift_state('d0c125ef-9b94-4d89-98c8-9229e3d81952', contemporary_constitutional_law, gap(authority_erosion, minor, true)).
narrative_ontology:cs_created_at('d0c125ef-9b94-4d89-98c8-9229e3d81952', '').
narrative_ontology:cs_kernel_id(magna_carta_1215__common_law_foundation_reading, magna_carta_1215).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_1215__common_law_foundation_reading, subjects_claiming_due_process).
narrative_ontology:constraint_beneficiary(magna_carta_1215__common_law_foundation_reading, common_law_courts).
narrative_ontology:constraint_beneficiary(magna_carta_1215__common_law_foundation_reading, political_community_against_prerogative).
narrative_ontology:constraint_victim(magna_carta_1215__common_law_foundation_reading, prerogative_justice).
narrative_ontology:constraint_victim(magna_carta_1215__common_law_foundation_reading, arbitrary_rule).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COMMONER SUBJECT (SNARE) — Before and without the constraint's enforcement, the subject has no exit from arbitrary rule. The Crown wields justice as an instrument of power, not law. Maximum suppression, maximum extraction. The subject cannot exit the jurisdiction or appeal to a superior law.
constraint_indexing:constraint_classification(magna_carta_1215__common_law_foundation_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SUBJECT CLAIMING PROCESS (TANGLED ROPE) — Over generations, the subject who invokes clause 39 ('no free man shall be disseised') gains access to common-law courts, establishing a procedural constraint on prerogative. The constraint coordinates (creates due-process mechanism) while extracting (enforcement of that mechanism requires courts and judges whose authority the Crown must recognize). Moderate suppression remains — the subject must know the right exists, must afford litigation, must navigate court procedure. But exit cost is reduced: the subject can now appeal to law itself as a superior authority.
constraint_indexing:constraint_classification(magna_carta_1215__common_law_foundation_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: COMMON-LAW COURTS (ROPE) — The courts experience the constraint as coordination: repeated reconfirmations of Magna Carta establish the courts' authority to review Crown action against a fixed standard. The courts gain institutional autonomy and prestige through this arrangement. The constraint is pure coordination from their perspective — it enables their function and legitimates their role. Very low experienced extraction; the courts are net beneficiaries through institutional authority expansion.
constraint_indexing:constraint_classification(magna_carta_1215__common_law_foundation_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: THE CROWN (TANGLED ROPE) — The Crown experiences the constraint as both coordination and extraction. The constraint coordinates a stable legal order that benefits Crown governance (predictable subjects, enforceable contracts, institutionalized justice). But it also extracts — the Crown accepts a permanent limitation on its prerogative, subjecting itself to a standard it cannot unilaterally override. The suppression requirement (enforcement of court authority) is high; the Crown must sustain courts and accept their rulings. Moderate experienced extraction because coordination benefits (stable realm) partially offset legality costs.
constraint_indexing:constraint_classification(magna_carta_1215__common_law_foundation_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: POLITICAL COMMUNITY / PARLIAMENTARY TRADITION (ROPE) — Over centuries, repeated reconfirmations of Magna Carta build a political tradition where legality is recognized as the Crown's permanent condition. This perspective sees pure coordination: the document enables collective political identity and a shared commitment to rule-bound governance. The constraint becomes a foundation for parliamentary claims against Crown prerogative. Pure coordination benefit; no experienced extraction from this perspective.
constraint_indexing:constraint_classification(magna_carta_1215__common_law_foundation_reading, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: MODERN MYTH-MAKING (PITON) — In contemporary discourse, Magna Carta is invoked as an originary founding document of constitutional liberty despite the historical fact that most of its clauses have been repealed and the original 1215 version was annulled. The invocation of 'Magna Carta' performs legitimacy work rather than functional constraint — it justifies contemporary constitutional claims through ancestral authority. Theater ratio is high; the actual operative constraint is modern common law and parliamentary tradition, not the medieval document. The Piton classification reflects degradation of the document's functional force combined with sustained performative authority.
constraint_indexing:constraint_classification(magna_carta_1215__common_law_foundation_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW READING (MOUNTAIN) — From a civilizational/universal view, the constraint appears as an immutable discovery about the conditions for political order: any stable realm requires subjection of power to law. The analytical observer risks reading legality as a natural law rather than a contingent institutional arrangement. However, the structural data reveals this as a false summit — the constraint has identifiable beneficiaries (subjects, courts, political community), explicit enforcement mechanisms, and empirical contingency on repeated reconfirmation acts. What appears to be natural law is an institutionalized practice kept alive through political commitment.
constraint_indexing:constraint_classification(magna_carta_1215__common_law_foundation_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_1215__common_law_foundation_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(magna_carta_1215__common_law_foundation_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(magna_carta_1215__common_law_foundation_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(magna_carta_1215__common_law_foundation_reading, TR),
    TR >= 0.70.

:- end_tests(magna_carta_1215__common_law_foundation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low but non-zero. The common-law foundation reading emphasizes that Magna Carta establishes a constraint on arbitrary rule, not a complete elimination of Crown power. The extraction cost to the Crown is real (loss of prerogative flexibility) but structured and bounded by the common-law framework. Unlike a snare (high extraction) or pure coordination (zero extraction), this constraint coordinates a legal order while requiring the Crown to pay the cost of accepting an external standard. The value reflects that extractiveness declines over time as legality becomes normal and the Crown's power to constrain itself institutionally increases. Suppression (0.42): Moderate but substantial. The constraint requires sustained suppression of prerogative justice — the Crown must refrain from overriding courts, accept adverse judgments, and maintain the institutional framework that empowers courts to review Crown action. This suppression is high enough to be real (the Crown has incentive to escape it) but not total (the Crown can and does influence common law through appointment of judges, legislation, and strategic use of its authority). Theater ratio (0.35): Moderate-low. The common-law foundation reading emphasizes the functional operation of courts as genuine arbiters of legality, not performative ritual. However, some theater is present — the mythic invocation of Magna Carta's authority often exceeds the document's actual legal force, and modern constitutional claims frequently misread the original clauses. The theater increases over time (perspectives 6-7) as the document's actual operative force diminishes relative to its symbolic authority.
 *
 * PERSPECTIVAL GAP:
 *   The common-law foundation reading generates a wide perspectival gap. The Crown experiences the constraint as a meaningful loss of prerogative (tangled rope — coordination + extraction). The subjects experience it as liberation from arbitrary rule (tangled rope — mixed protection and cost). The common-law courts experience it as pure institutional gain (rope — legitimacy expansion). The political community experiences it as constitutional foundation (rope — shared commitment). The modern myth-maker sees degraded function (piton — performative ritual). The analytical observer risks naturalizing it as law (false-summit mountain). Each perspective reflects a genuine structural aspect of the constraint; no single perspective is complete.
 *
 * DIRECTIONALITY LOGIC:
 *   The derivation of directionality (d) for each perspective follows from the agent's power level, exit options, and relationship to the constraint's extraction flow. Powerless subjects facing arbitrary rule have high d (0.95) — they are full victims with no exit. Moderate subjects claiming process have medium d (0.55) — they gain protection but must navigate procedural barriers and funding constraints. Powerful institutional actors (Crown, courts) have medium d (0.45-0.50) — they are both constrained and beneficiaries depending on whether they are enforcing or subject to the constraint. The analytical observer has high d (0.72) — they can measure the constraint's structure but are external to its enforcement. The sigmoid f(d) applies these d values to produce effective extractiveness chi, which is the experienced magnitude of constraint from each perspective. Beneficiaries with arbitrage exit (courts, political community) show negative f(d) (institutional subsidy effect), experiencing the constraint as enabling rather than extractive. Victims with trapped exit (subjects under prerogative) show maximum f(d) (powerless effect), experiencing maximal extraction. The common-law reading's moderate extractiveness (0.18) reflects that the constraint's net effect is to reduce extraction (compared to pure prerogative) while establishing a binding framework that requires Crown compliance.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    repeated_reconfirmation_necessity,
    'Does the constraint''s force depend on continuous political reconfirmation, or does Magna Carta''s authority persist independently once established?',
    'Historical analysis of reconfirmation frequency and political context; comparison of periods when reconfirmation was activated vs. dormant; institutional authority decay curves',
    'If reconfirmation is necessary: the constraint is contingent and political (requires active enforcement). If authority persists independently: the constraint approaches natural-law status (emerges from institutional maturation). This determines whether the mountain perspective is false-summit or genuinely grounded.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(repeated_reconfirmation_necessity, empirical, 'Whether repeated reconfirmation is structurally necessary for constraint persistence').

omega_variable(
    prerogative_justice_alternative,
    'Is prerogative justice (arbitrary rule by the Crown) a genuine alternative that Magna Carta suppresses, or is it a historically unavailable option that the constraint merely formalizes?',
    'Comparative study of realms without common-law constraint; analysis of Crown capacity to override courts during crisis periods; determination of whether prerogative is ever truly exercised or merely threatened',
    'If prerogative is a real suppressed alternative: victim set (prerogative justice) is genuine; suppression value is justified. If prerogative is historically unavailable: the constraint is largely performative, and suppression value is overstated; extractiveness is lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prerogative_justice_alternative, empirical, 'Whether prerogative justice is a suppressed alternative or a historically unavailable option').

omega_variable(
    reading_kernel_ambiguity,
    'Is this constraint a reading of a single kernel (Magna Carta 1215), or do the sibling readings (feudal bargain, symbolic myth) describe fundamentally different kernels that happen to invoke the same document?',
    'Doctrinal analysis: examine whether each reading can coherently interpret the same legal-historical facts, or whether each reading''s foundational claims are empirically incompatible',
    'If one kernel: the readings represent legitimate perspectival pluralism about a single constraint. If different kernels: decomposing into separate constraint stories is more analytically precise (ε-invariance principle). This determines whether the committer-frame architecture applies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_ambiguity, conceptual, 'Whether the three readings constitute perspectival readings of one kernel or separate constraints').

omega_variable(
    natural_law_vs_constructed_constraint,
    'Does legality itself (the requirement that power be subject to law) represent a natural law of political order, or is it a contingent institutional achievement that the constraint formalizes?',
    'Comparative constitutional analysis; examination of realms that reject legality as a governing principle; determination of whether legality is empirically necessary for political stability or contingent on specific cultural/institutional contexts',
    'If natural law: the mountain classification is justified (analyti. If constructed: the mountain is a false summit (naturalization of contingent arrangement). This determines whether the analytical observer''s perspective is structurally accurate or misrepresents institutional contingency as law.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_constraint, conceptual, 'Whether legality is a natural law or contingent institutional achievement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_1215__common_law_foundation_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mc_clf_tr_t0, magna_carta_1215__common_law_foundation_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(mc_clf_tr_t100, magna_carta_1215__common_law_foundation_reading, theater_ratio, 100, 0.28).
narrative_ontology:measurement(mc_clf_tr_t200, magna_carta_1215__common_law_foundation_reading, theater_ratio, 200, 0.35).

% Extraction over time
narrative_ontology:measurement(mc_clf_be_t0, magna_carta_1215__common_law_foundation_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(mc_clf_be_t100, magna_carta_1215__common_law_foundation_reading, base_extractiveness, 100, 0.28).
narrative_ontology:measurement(mc_clf_be_t200, magna_carta_1215__common_law_foundation_reading, base_extractiveness, 200, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(mc_clf_su_t0, magna_carta_1215__common_law_foundation_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(mc_clf_su_t100, magna_carta_1215__common_law_foundation_reading, suppression_requirement, 100, 0.52).
narrative_ontology:measurement(mc_clf_su_t200, magna_carta_1215__common_law_foundation_reading, suppression_requirement, 200, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_1215__common_law_foundation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_1215__common_law_foundation_reading, magna_carta_1215__feudal_bargain_reading).
narrative_ontology:affects_constraint(magna_carta_1215__common_law_foundation_reading, magna_carta_1215__symbolic_myth_reading).

% DUAL FORMULATION NOTE:
% The three constraint stories (common_law_foundation_reading, feudal_bargain_reading, symbolic_myth_reading) are sibling readings of the same kernel (magna_carta_1215). Each story represents a distinct epistemic and doctrinal interpretation of the same historical document and political event. The three readings do not have independent ε values from different observables (ε-invariance principle) — rather, they represent different frameworks for interpreting what the constraint IS. The network relationship is bidirectional: each reading affects how the others are interpreted, and each functions as an alternative framing that limits the others' explanatory scope. The common-law reading emphasizes institutional continuity and functional constraint; the feudal reading emphasizes historical contingency and immediate failure; the myth reading emphasizes narrative authority and invocational practice. These are committer-axis readings in the kernel contest, not observable-dependent decompositions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
