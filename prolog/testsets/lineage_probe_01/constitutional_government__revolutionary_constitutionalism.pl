% ============================================================================
% CONSTRAINT STORY: constitutional_government__revolutionary_constitutionalism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_government__revolutionary_constitutionalism, []).

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
 *   constraint_id: constitutional_government__revolutionary_constitutionalism
 *   human_readable: Revolutionary Constitutionalism: The Founding as Deliberate Act
 *   domain: political/legal/constitutional_theory
 *
 * SUMMARY:
 *   Revolutionary constitutionalism instantiates the reading of
 *   constitutional government as a single, deliberate founding act in which a
 *   people constitutes legitimate government from first principles through an
 *   authoritative written instrument. This reading forecloses the
 *   pre-revolutionary form (ancient balanced orders, inherited monarchy,
 *   customary law) by treating the written document as supreme and
 *   self-contained. The structural tension is acute: the founding moment
 *   produces maximal suppression (it forecloses alternative forms, locks
 *   later generations into the founding bargain, and silences those excluded
 *   from the original covenant) while simultaneously being narrated as the
 *   people's voluntary self-constitution. The beneficiary is the founding
 *   coalition and its successors-in-interpretation (canonical judges, legal
 *   scholars, statesmen who control the document's meaning). The victims are
 *   those excluded at the founding and those subject to the constraint's
 *   suppression over time. Extractiveness is not static: it is highest at the
 *   founding moment (when the document's authority is most uncontested and
 *   exclusions most stark) and decays over time as later amendments expand
 *   the founding coalition's identity and doctrinal reinterpretation erodes
 *   the original document's centrality. This constraint is one reading of the
 *   contested kernel 'constitutional government'; its sibling
 *   readings—ancient constitutionalism, postwar constitutionalism, and
 *   Westminster evolution—offer alternative framings of what makes government
 *   constitutional.
 *
 * KEY AGENTS:
 *   - Founding Coalition: Primary beneficiary (institutional/arbitrage) — captures first-mover advantage and the power to define the constitutional frame; experiences constraint as pure coordination
 *   - Canonical Interpreters (Judiciary): Secondary beneficiary and constraint-bearer (institutional/constrained) — inheritors of authority to interpret; extract legitimacy while constrained by the founding document's authority
 *   - Excluded Populations (enslaved persons, women, non-property owners, indigenous peoples, religious minorities): Primary victims (powerless/trapped) — locked into founding bargain they did not negotiate; suppression of alternative claims for recognition
 *   - Pre-Revolutionary Authorities (monarchs, aristocracies, customary law holders): Victims of foreclosure (institutional/trapped) — the founding document explicitly denies their legitimacy; constrained to exit or capitulate
 *   - Reform Movements (later generations seeking amendment): Secondary victims (moderate/constrained) — face both coordination benefits (stable touchstone for claims) and extraction costs (gatekeeping by canonical interpreters); generational exit possibilities
 *   - Analytical Observer / Constitutionalist Tradition: Identity-locked analyst (analytical/identity_locked at biographical, rope at generational) — professional identity constituted through the constitutional tradition; can perceive change in principle but cannot exit in practice
 *   - Essentialist Observer: Risks naturalizing contested institutional choice (analytical/analytical) — mountain perspective reveals false summit through structural data
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_government__revolutionary_constitutionalism, 0.52).
domain_priors:suppression_score(constitutional_government__revolutionary_constitutionalism, 0.68).
domain_priors:theater_ratio(constitutional_government__revolutionary_constitutionalism, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_government__revolutionary_constitutionalism, extractiveness, 0.52).
narrative_ontology:constraint_metric(constitutional_government__revolutionary_constitutionalism, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(constitutional_government__revolutionary_constitutionalism, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_government__revolutionary_constitutionalism, tangled_rope).
narrative_ontology:human_readable(constitutional_government__revolutionary_constitutionalism, "Revolutionary Constitutionalism: The Founding as Deliberate Act").
narrative_ontology:topic_domain(constitutional_government__revolutionary_constitutionalism, "political/legal/constitutional_theory").

domain_priors:requires_active_enforcement(constitutional_government__revolutionary_constitutionalism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_government__revolutionary_constitutionalism, '2968988c-4b89-4870-9827-898e491211b3').
narrative_ontology:cs_kernel_codification('2968988c-4b89-4870-9827-898e491211b3', formalized).
narrative_ontology:cs_authority_grounding('2968988c-4b89-4870-9827-898e491211b3', lineage).
narrative_ontology:cs_interpretation_layer_present('2968988c-4b89-4870-9827-898e491211b3').
narrative_ontology:cs_reading_relation('2968988c-4b89-4870-9827-898e491211b3', constitutional_government__ancient_constitutionalism, forecloses).
narrative_ontology:cs_reading_relation('2968988c-4b89-4870-9827-898e491211b3', constitutional_government__postwar_constitutionalism, coexists_with).
narrative_ontology:cs_reading_relation('2968988c-4b89-4870-9827-898e491211b3', constitutional_government__westminster_evolution, influences).
narrative_ontology:cs_axiom('2968988c-4b89-4870-9827-898e491211b3', foundational, foundational_written_supremacy).
narrative_ontology:cs_axiom_status(foundational_written_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('2968988c-4b89-4870-9827-898e491211b3', foundational_written_supremacy, deontological).
narrative_ontology:cs_axiom('2968988c-4b89-4870-9827-898e491211b3', foundational, revolutionary_displacement_of_prior_form).
narrative_ontology:cs_axiom_status(revolutionary_displacement_of_prior_form, holdable).
narrative_ontology:cs_axiom_grounding('2968988c-4b89-4870-9827-898e491211b3', revolutionary_displacement_of_prior_form, empirically_contingent).
narrative_ontology:cs_reference_frame('2968988c-4b89-4870-9827-898e491211b3', revolutionary_founding_supremacy).
narrative_ontology:cs_drift_state('2968988c-4b89-4870-9827-898e491211b3', contemporary_democratic_expansion, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('2968988c-4b89-4870-9827-898e491211b3', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(constitutional_government__revolutionary_constitutionalism, constitutional_government).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_government__revolutionary_constitutionalism, founding_coalition).
narrative_ontology:constraint_beneficiary(constitutional_government__revolutionary_constitutionalism, canonical_interpreters).
narrative_ontology:constraint_victim(constitutional_government__revolutionary_constitutionalism, excluded_populations).
narrative_ontology:constraint_victim(constitutional_government__revolutionary_constitutionalism, pre_revolutionary_authorities).
narrative_ontology:constraint_victim(constitutional_government__revolutionary_constitutionalism, competing_foundational_claims).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED POPULATION (SNARE) — Those not party to the founding (enslaved persons, women, non-property owners, indigenous peoples, religious minorities) are locked into a constitutional order that explicitly or implicitly excludes them. They cannot exit or renegotiate the founding bargain. The constraint suppresses pre-constitutional claims for recognition and restricts appeal to any authority above or outside the document. Maximum extraction: they bear the costs of the constitutional order without having been consulted on its terms.
constraint_indexing:constraint_classification(constitutional_government__revolutionary_constitutionalism, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REFORM MOVEMENT (TANGLED ROPE) — Later generations seeking constitutional amendment or interpretation to expand the founding coalition face both coordination benefits (the written document provides a stable, shared touchstone for reform claims) and extraction costs (the document's entrenchment and the canonical interpreters' gatekeeping power restrict the pace and scope of change). Significant agency exists through amendment procedures and reinterpretation, but the original founding's suppression cascades across time.
constraint_indexing:constraint_classification(constitutional_government__revolutionary_constitutionalism, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FOUNDING COALITION (ROPE) — The agents who author and ratify the constitution experience it as pure coordination: solving the collective action problem of founding legitimate government. They capture first-mover advantage and the power to define the constitutional frame itself. The constraint functions as their solution mechanism — extractiveness is experienced as the legitimate reward for risk and leadership in the founding moment. Net beneficiary; minimal experienced extraction.
constraint_indexing:constraint_classification(constitutional_government__revolutionary_constitutionalism, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CANONICAL INTERPRETERS / JUDICIARY (TANGLED ROPE) — Inheritors of the founding's authority to interpret the constitution face a hybrid structure. They coordinate the legal system through stable doctrinal frameworks (coordination function), but they also extract legitimacy by gatekeeping what counts as valid constitutional interpretation. Later interpreters cannot escape the founding document's authority without destroying their own legitimacy — constrained exit. The constraint benefits them (institutional authority) while also binding them.
constraint_indexing:constraint_classification(constitutional_government__revolutionary_constitutionalism, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: VESTIGIAL ORIGINALIST MYTH (PITON) — The claim that the written constitution captures a single, discoverable 'original meaning' that constrains interpretation has become largely theatrical in actual judicial practice. Courts regularly deviate from strict originalism while invoking it; the performance of fidelity to original meaning persists as institutional theater even as practitioners acknowledge that meaning is contestable and historically reconstructed. Theater ratio high (0.70+) — the constraint persists through inertia and myth rather than through actual functional constraint on interpretation.
constraint_indexing:constraint_classification(constitutional_government__revolutionary_constitutionalism, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / ESSENTIALIST VIEW (MOUNTAIN) — From a civilizational perspective, the written constitution might appear as an immutable, naturally emergent solution to the foundational problem of political legitimacy. Every stable state requires *some* founding moment and *some* supreme law; the revolutionary constitutionalist reading naturalizes this as the inevitable form. However, the constraint's structural data (identifiable beneficiaries, active suppression, enforcement requirements) contradicts the mountain classification and reveals the essentialist view as false summit — naturalizing a contested institutional choice as inevitable law.
constraint_indexing:constraint_classification(constitutional_government__revolutionary_constitutionalism, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / CONSTITUTIONALIST TRADITION (TANGLED ROPE w/ identity_locked) — The scholar or jurist trained in constitutional law carries the founding document's cognitive frame as part of professional identity. They can perceive the constraint as changeable in principle (generational time, analytical power, mobile exit in most structural senses), but their professional identity is constituted through the constitutional tradition itself — abandoning the document's authority would require abandoning the disciplinary frame that makes them a constitutional lawyer. Identity-locked at biographical time would produce mountain; at generational time produces rope. This gap (biographical mountain vs. generational rope) reveals the identity lock: the constraint is structurally changeable but cognitively unfixable from within the professional frame.
constraint_indexing:constraint_classification(constitutional_government__revolutionary_constitutionalism, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_government__revolutionary_constitutionalism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(constitutional_government__revolutionary_constitutionalism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(constitutional_government__revolutionary_constitutionalism, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_government__revolutionary_constitutionalism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(constitutional_government__revolutionary_constitutionalism, TR),
    TR >= 0.70.

:- end_tests(constitutional_government__revolutionary_constitutionalism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The founding moment locks later generations into a compromise that benefits the founding coalition disproportionately. However, extractiveness is not maximal (0.72+) because: (a) the founding is narrated as self-constituting and voluntary (reducing experienced coercion), (b) later generations can and do amend the document (reducing felt permanence), and (c) doctrinal reinterpretation shifts meaning over time without formal amendment (functional flexibility that reduces rigid extraction). The 0.52 reflects the entrenched compromise — high enough to merit Tangled Rope classification but not Snare. Suppression (0.68): High. The written constitution's supremacy forecloses appeals to any authority above or outside it. Pre-revolutionary forms are locked out; those excluded from the original covenant cannot appeal to competing legitimacy sources. Suppression concentrates at the founding moment (0.85) and decays as later amendments expand the coalition (0.68 contemporary), but it remains substantial. Theater ratio (0.38): Moderate-low. At the founding, the revolutionary constitutionalist moment is minimally theatrical — it is a genuinely deliberate act to replace one form of government with another. But over time, theater increases as the idealization of the founding diverges from the actual practice of amendment and reinterpretation. The vestigial originalist myth (Piton perspective, 0.70+ theater) shows how contemporary constitutional interpretation uses founding rhetoric while substantially revising its meaning.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how a single structural arrangement produces dramatically different classifications from different positions. The founding coalition sees Rope (pure coordination of a foundational problem). The excluded population sees Snare (complete extraction with no exit). The reform movement sees Tangled Rope (mixed coordination and extraction across generations). The judiciary sees Tangled Rope (authority granted but also constrained). The originalist myth sees Piton (theater masking doctrinal evolution). The essentialist analyst sees Mountain (falsely naturalizing a contested institutional choice). The identity-locked constitutionalist scholar experiences a biographical gap: at immediate time, they see the constraint as unchangeable (identity fused with the tradition); at generational time, they perceive agency to reinterpret; at civilizational time, they see the founding as a contingent historical moment. This perspectival range reveals that revolutionary constitutionalism is not a natural law but a powerful institutional arrangement that benefits specific agents (the founding coalition, canonical interpreters) while constraining others (excluded populations, reform movements).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim status and exit options. The founding coalition has low directionality (d ≈ 0.15, beneficiary + arbitrage exit) → negative f(d) → they experience negative chi (constraint subsidizes them). Excluded populations have high directionality (d ≈ 0.95, victim + trapped exit) → f(d) ≈ 1.42 → they experience maximum chi. Reform movements have moderate directionality (d ≈ 0.60, victim + constrained exit) → f(d) ≈ 0.75 → moderate experienced extraction. The judiciary (institutional, constrained, mixed beneficiary/victim) has moderate directionality reflecting the hybrid position. The identity-locked constitutionalist analyst has high directionality (d ≈ 0.89) because their identity is fused with the constraint — they are structurally mobile (analytical exit options) but cognitively trapped within the constitutional frame. No override needed — the structural derivation captures the asymmetries.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not exhibit mandatrophy (extractiveness = 0.52, below 0.70 threshold). However, the tension between narrative and structure deserves note. The revolutionary constitutionalist reading narrates the constraint as pure coordination (the people founding legitimate government) while the structural data reveals significant extraction (beneficiary concentration, victim exclusion, entrenched suppression). The mandatrophy resolution would require acknowledging that the founding bargain produces asymmetric outcomes despite the voluntary self-constitution narrative. Later amendments (suffrage expansion, civil rights, democratic reforms) partially resolve this by expanding who counts as 'the people,' but each wave of expansion reveals that the prior founding's legitimacy was contingent on hidden exclusions. The constraint's ethical claim (we the people freely constitute our government) coexists with its structural claim (specific coalitions benefit, specific populations bear costs). This is not mandatrophy — the Tangled Rope classification accurately captures the mix — but it reveals why revolutionary constitutionalism is a contested reading: different stakeholders assess the balance between coordination and extraction differently.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_legitimacy_source,
    'Is the founding moment''s legitimacy grounded in the people''s actual consent or in a post-hoc narrative that converts power into authority?',
    'Historical analysis of ratification processes, dissent records, and elite vs. popular participation. Comparison of founding rhetoric vs. actual inclusion/exclusion patterns.',
    'If actual consent: revolutionary constitutionalism is legitimate Rope. If post-hoc narrative: it is extractive Snare disguised as foundational Rope. Classification can shift with evidence about whether the founding was genuinely deliberative or elite-manufactured.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_legitimacy_source, empirical, 'Source of founding moment''s legitimacy: consent vs. post-hoc narrative').

omega_variable(
    permanence_vs_amendment_contestation,
    'Does the written constitution function as a permanent frame that constrains later generations, or as a living document whose meaning shifts sufficiently that it functions more like the Westminster unwritten evolution reading?',
    'Longitudinal textual analysis of constitutional interpretation across generations. Measurement of doctrinal drift vs. textual fidelity. Comparison of explicit amendment rates vs. effective doctrinal change rates.',
    'If genuinely permanent: revolutionary constitutionalism is a coherent reading with stable suppression at the founding. If doctrinal drift is substantial enough to match unwritten evolution: the two readings become functionally similar, and the revolutionary reading is more theatrical than structural. This would raise the theater_ratio and potentially shift classification toward Piton.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(permanence_vs_amendment_contestation, empirical, 'Actual permanence vs. doctrinal evolution of the written constitution').

omega_variable(
    identity_of_the_people,
    'Who is ''the people'' that constitute the government? Is the founding coalition''s identity static or does it evolve through later expansions of suffrage and citizenship?',
    'Historical tracking of who had voice in the founding vs. who came to be included through amendment and reinterpretation. Analysis of whether later expansions are seen as completing an original intent or revising the original compromise.',
    'If ''the people'' identity is fixed at the founding moment: excluded populations remain permanently outside the original bargain, and suppression remains high. If ''the people'' evolves through amendment: the founding coalition''s exclusive benefit diminishes over time, extractiveness decays, and the constraint shifts toward Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_of_the_people, conceptual, 'Identity stability of ''the people'' across founding and amendment cycles').

omega_variable(
    sibling_reading_empirical_undecidability,
    'Can the revolutionary constitutionalist reading be empirically distinguished from the Westminster unwritten evolution reading in terms of actual governmental behavior and institutional change?',
    'Cross-national comparison: cases where one jurisdiction claims revolutionary constitutional founding vs. Westminster unwritten tradition. Measurement of amendment frequency, doctrinal stability, and institutional change mechanisms. Identification of whether difference is substantive or rhetorical.',
    'If empirically indistinguishable: the two readings coexist as framing choices rather than structural claims. If distinguishable: revolutionary constitutionalism has a coherent structural claim independent of Westminster evolution. This affects the reading_relations classification (currently ''influences''; may need to shift to ''coexists_with'' if empirical difference is marginal).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_empirical_undecidability, empirical, 'Empirical distinctiveness of revolutionary vs. Westminster constitutional forms').

omega_variable(
    reading_is_contingent_institutional_choice,
    'Is revolutionary constitutionalism (the reading that this constraint instantiates) a viable description of actual constitutional government, or is it a theoretical construct that no real constitutional system fully instantiates?',
    'Case study of US, French, and other revolutionary constitutional moments: measure the degree to which their actual practice matches the revolutionary constitutionalist ideal (single authoritative written founding act) vs. the degree to which historical revisions, unwritten conventions, and evolutionary doctrinal change have eroded the founding''s centrality.',
    'If viable: this constraint story correctly describes a real structural feature of revolutionary constitutional regimes. If mostly theoretical: the constraint is a normative claim about what revolutionary constitutionalism *should* be rather than what it *is*, and the classification should be downgraded to reflect the gap between ideal and practice. This affects omegas about false summits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_is_contingent_institutional_choice, conceptual, 'Revolutionary constitutionalism as viable institutional form vs. theoretical ideal').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_government__revolutionary_constitutionalism, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(constrev_theater_founding, constitutional_government__revolutionary_constitutionalism, theater_ratio, 0, 0.15).
narrative_ontology:measurement(constrev_theater_generation1, constitutional_government__revolutionary_constitutionalism, theater_ratio, 5, 0.28).
narrative_ontology:measurement(constrev_theater_contemporary, constitutional_government__revolutionary_constitutionalism, theater_ratio, 10, 0.38).

% Extraction over time
narrative_ontology:measurement(constrev_extract_founding, constitutional_government__revolutionary_constitutionalism, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(constrev_extract_generation1, constitutional_government__revolutionary_constitutionalism, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(constrev_extract_contemporary, constitutional_government__revolutionary_constitutionalism, base_extractiveness, 10, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(constrev_suppression_founding, constitutional_government__revolutionary_constitutionalism, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(constrev_suppression_generation1, constitutional_government__revolutionary_constitutionalism, suppression_requirement, 5, 0.72).
narrative_ontology:measurement(constrev_suppression_contemporary, constitutional_government__revolutionary_constitutionalism, suppression_requirement, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_government__revolutionary_constitutionalism, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_government__revolutionary_constitutionalism, constitutional_government__ancient_constitutionalism).
narrative_ontology:affects_constraint(constitutional_government__revolutionary_constitutionalism, constitutional_government__postwar_constitutionalism).
narrative_ontology:affects_constraint(constitutional_government__revolutionary_constitutionalism, constitutional_government__westminster_evolution).

% DUAL FORMULATION NOTE:
% Constitutional government is a contested kernel with four structurally distinct readings. This story instantiates the revolutionary_constitutionalism reading (written, deliberate founding act). Sibling stories (ancient, postwar, westminster) each have different epsilon values reflecting their different mechanisms: ancient balances orders (ε ≈ 0.35, Rope); postwar grounds legitimacy in rights protection after catastrophe (ε ≈ 0.38, Tangled Rope); Westminster evolves through unwritten convention (ε ≈ 0.42, Piton). The revolutionary reading has higher extractiveness (0.52) because it concentrates legitimacy in a founding moment and forecloses competing sources. Each reading is a separate constraint story linked through network.affects_constraints and the shared kernel_id in cs_structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
