% ============================================================================
% CONSTRAINT STORY: notability_guidelines__deliberative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_notability_guidelines__deliberative_reading, []).

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
 *   constraint_id: notability_guidelines__deliberative_reading
 *   human_readable: Wikipedia Notability Guidelines as Deliberative Governance Process
 *   domain: digital_commons/platform_constitutionalism/knowledge_infrastructure
 *
 * SUMMARY:
 *   Wikipedia's notability guidelines represent a distributed deliberative
 *   governance mechanism for maintaining encyclopedia scope and quality in
 *   the face of continuous information influx. Rather than treating
 *   notability as a fixed rule or a natural boundary, the deliberative
 *   reading frames it as an ongoing process where AfD (Articles for Deletion)
 *   discussions generate situated closure on boundary questions. This reading
 *   emphasizes that notability criteria (GNG, SNG) function as invocation
 *   hooks for community deliberation rather than automatic decision rules.
 *   The constraint exhibits scaffold characteristics: it has active
 *   enforcement (deletion via community consensus), bounded suppression
 *   (gatekeeping effort with appeal mechanisms), and a structured sunset
 *   logic (notability can be renegotiated as evidence or community judgment
 *   shifts). The theater ratio (0.58) reflects tension between the formal
 *   policy infrastructure (WP:N page, stated criteria) and the distributed
 *   decision-making process (AfD consensus). Extractiveness (0.32) is
 *   moderate because the constraint primarily coordinates scope but creates
 *   asymmetric burden on communities with limited editorial representation.
 *   The deliberative reading contrasts with the deletionist reading (which
 *   treats notability as fixed rules to be applied) and the inclusionist
 *   reading (which treats the burden of proof as illegitimately skewed toward
 *   deletion).
 *
 * KEY AGENTS:
 *   - Deliberative Governance Coalition: Organized participants (editors, admins, policy makers) who see notability as ongoing negotiation; constrained but mobile within the system; beneficiaries of shared governance mechanism
 *   - Active Editor Community: Editors with sustained engagement; experience rope coordination; primarily solve the collective action problem of scope curation
 *   - Marginalized Subject Communities: Communities with limited resources (indigenous knowledge, grassroots history, niche fields); experience tangled rope (mixed coordination and asymmetric burden); constrained exit
 *   - Subjects Lacking Advocates: Topics without organized defenders (deceased local figures, small businesses); experience snare classification (trapped, zero voice in process)
 *   - Notability Policy Institution: The formal policy machinery (WP:N page, templates, guidelines); institutional actor maintaining appearance of objectivity; piton status (increasingly performative)
 *   - Analytical Observer: Meta-governance perspective; sees Wikipedia notability as functional prototype for distributed epistemic commons governance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(notability_guidelines__deliberative_reading, 0.32).
domain_priors:suppression_score(notability_guidelines__deliberative_reading, 0.45).
domain_priors:theater_ratio(notability_guidelines__deliberative_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(notability_guidelines__deliberative_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(notability_guidelines__deliberative_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(notability_guidelines__deliberative_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(notability_guidelines__deliberative_reading, scaffold).
narrative_ontology:human_readable(notability_guidelines__deliberative_reading, "Wikipedia Notability Guidelines as Deliberative Governance Process").
narrative_ontology:topic_domain(notability_guidelines__deliberative_reading, "digital_commons/platform_constitutionalism/knowledge_infrastructure").

domain_priors:requires_active_enforcement(notability_guidelines__deliberative_reading).
narrative_ontology:has_sunset_clause(notability_guidelines__deliberative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(notability_guidelines__deliberative_reading, '6dc462f6-c764-4a1c-8a95-0e9efb2f1621').
narrative_ontology:cs_kernel_codification('6dc462f6-c764-4a1c-8a95-0e9efb2f1621', formalized).
narrative_ontology:cs_authority_grounding('6dc462f6-c764-4a1c-8a95-0e9efb2f1621', distributed).
narrative_ontology:cs_reading_relation('6dc462f6-c764-4a1c-8a95-0e9efb2f1621', notability_guidelines__deletionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('6dc462f6-c764-4a1c-8a95-0e9efb2f1621', notability_guidelines__inclusionist_reading, coexists_with).
narrative_ontology:cs_axiom('6dc462f6-c764-4a1c-8a95-0e9efb2f1621', foundational, notability_as_process_output).
narrative_ontology:cs_axiom_status(notability_as_process_output, holdable).
narrative_ontology:cs_axiom_grounding('6dc462f6-c764-4a1c-8a95-0e9efb2f1621', notability_as_process_output, conventional).
narrative_ontology:cs_axiom('6dc462f6-c764-4a1c-8a95-0e9efb2f1621', foundational, deliberative_legitimacy_principle).
narrative_ontology:cs_axiom_status(deliberative_legitimacy_principle, holdable).
narrative_ontology:cs_axiom_grounding('6dc462f6-c764-4a1c-8a95-0e9efb2f1621', deliberative_legitimacy_principle, conventional).
narrative_ontology:cs_reference_frame('6dc462f6-c764-4a1c-8a95-0e9efb2f1621', encyclopedia_as_curated_commons).
narrative_ontology:cs_drift_state('6dc462f6-c764-4a1c-8a95-0e9efb2f1621', contemporary_mass_deletion_pressure, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('6dc462f6-c764-4a1c-8a95-0e9efb2f1621', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(notability_guidelines__deliberative_reading, notability_guidelines).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(notability_guidelines__deliberative_reading, deliberative_contributors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DELIBERATIVE GOVERNANCE COALITION (SCAFFOLD) — Organized participants (editors, admins, ArbCom, policy makers) see notability as output of an ongoing AfD (Articles for Deletion) deliberation process. Suppression exists (gatekeeping effort, debate costs) but is bounded by the sunset logic: the policy is maintained as a living negotiation with periodic recalibration. The constraint has active enforcement (deletion via community consensus) but is temporary and revisable — new evidence or arguments can shift boundaries.
constraint_indexing:constraint_classification(notability_guidelines__deliberative_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 2: ACTIVE EDITOR COMMUNITY (ROPE) — Editors with sustained engagement experience notability as a coordination mechanism: shared criteria enable collaborative curation, reduce redundant adjudication, and align the encyclopedia's scope. Exit options exist (fork to Inclusionpedia, edit Wikivoyage, start a specialized wiki) but are costly. The constraint primarily solves a coordination problem (how do we decide together what to keep?) rather than extracting asymmetric benefit. Low theater — editorial decisions are transparent and appealable.
constraint_indexing:constraint_classification(notability_guidelines__deliberative_reading, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: MARGINALIZED SUBJECT COMMUNITIES (TANGLED ROPE) — For communities with limited resources or visibility (indigenous knowledge systems, grassroots history, niche academic fields), notability gatekeeping creates asymmetric burden: they must present evidence in forms the consensus recognizes as legitimate, while mainstream subjects benefit from existing infrastructure. The constraint coordinates encyclopedia scope (coordination function) but also extracts a disproportionate evidentiary burden. Constrained exit: moving to specialized wikis fractures knowledge commons; staying means submitting to gatekeeping.
constraint_indexing:constraint_classification(notability_guidelines__deliberative_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: NOTABILITY POLICY INSTITUTION (PITON) — The formal policy machinery (WP:N page, guidelines documents, deletion templates) has become substantially performative. Most deletion decisions are made through local consensus on AfD pages, not by rigid application of stated criteria. The institutional policy persists through inertia (it provides a veneer of objectivity) while the actual mechanism is distributed deliberation. Theater ratio (0.58) reflects this gap: stated rules vs. actual practice diverge increasingly.
constraint_indexing:constraint_classification(notability_guidelines__deliberative_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: SUBJECTS LACKING ORGANIZED ADVOCATES (SNARE) — For topics without community advocates (deceased local figures, small businesses, marginalized cultural artifacts), notability becomes pure extraction: deletion risk with no voice in AfD process. Suppression is total — the subject cannot defend itself. Theater is high (formal criteria, citation standards) but purely performative for undefended topics. This perspective reveals the constraint's failure mode: when the deliberative mechanism assumes organized participants, powerless subjects face extractive gatekeeping.
constraint_indexing:constraint_classification(notability_guidelines__deliberative_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / META-GOVERNANCE (ROPE) — From a civilizational perspective, Wikipedia's notability negotiation is a functional prototype for distributed epistemic commons governance. The constraint coordinates diverse communities around shared knowledge standards without centralized authority. Scalability and emergent legitimacy demonstrate the viability of deliberative boundary-setting in large-scale knowledge systems. Pure coordination function — the debate itself IS the governance mechanism.
constraint_indexing:constraint_classification(notability_guidelines__deliberative_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(notability_guidelines__deliberative_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(notability_guidelines__deliberative_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(notability_guidelines__deliberative_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(notability_guidelines__deliberative_reading, TR),
    TR >= 0.70.

:- end_tests(notability_guidelines__deliberative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32): Moderate-low. The deliberative reading treats notability as a governance coordination mechanism whose primary function is to align distributed editorial decisions without centralized authority. Moderate extractiveness reflects that the gatekeeping effort is real (marginalized communities face higher evidentiary burden) but is not the mechanism's dominant function. The scaffold classification holds because the constraint has a sunset logic: notability boundaries are legitimately revisable through deliberation. If the constraint were pure extraction (snare/tangled_rope dominant), sunset would not apply — the boundaries would be rigid extraction apparatus. Suppression (0.45): Moderate. Gatekeeping effort and citation standards create barriers to inclusion, but suppression is bounded by appeal mechanisms, community discussion forums, and the ability to contest decisions. Theater ratio (0.58): Moderate-high. The gap between formal criteria (WP:N page) and actual AfD decision logic is substantial. Many deletion decisions invoke criteria post-hoc or rely on implicit consensus signals rather than explicit criterion-by-criterion analysis. The theater has increased over time (0.48 → 0.58) as the community has shifted from rule application toward consensus deliberation, making stated policies increasingly performative.
 *
 * PERSPECTIVAL GAP:
 *   The deliberative reading generates perspectival gaps across multiple axes. The governance coalition sees coordination (rope/scaffold) — shared scope criteria enable collective curation. Marginalized communities see mixed coordination and extraction (tangled_rope) — they benefit from the epistemic commons but bear asymmetric evidentiary burden. Powerless subjects see pure extraction (snare) — they have zero voice in the deliberative process. The piton perspective (institutional policy machinery) reveals the constraint's degradation: formal criteria are increasingly performative as actual decisions are made through consensus negotiation. The analytical observer sees meta-governance (rope at civilizational scale) — Wikipedia's notability mechanism demonstrates a functional model for distributed epistemic commons governance. These gaps reveal that the deliberative reading is valid only when deliberative participation is possible; it collapses into snare/tangled_rope/piton for agents without access to or standing in AfD processes.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for the deliberative reading is derived from the agent's structural relationship to the gatekeeping mechanism. Organized contributors (editors, policy makers) with mobile exit options experience low d (beneficiaries of the coordination function). Marginalized communities with constrained exit experience higher d (face asymmetric evidentiary burden). Powerless subjects with no organized voice experience maximal d (trapped, full target of gatekeeping). The analytical observer experiences d ≈ 0.72 (typical for knowledge-infrastructure assessment). The deliberative reading emphasizes that d is not fixed by the constraint structure but is negotiable through participation in AfD: an agent who enters the process and mobilizes community support can shift their structural position from target toward beneficiary. This negotiability is central to why the constraint is scaffold rather than snare.
 *
 * MANDATROPHY ANALYSIS:
 *   The deliberative reading resolves mandatrophy by distinguishing the constraint's coordination function (setting shared scope for collective action) from its gatekeeping function (excluding topics from the commons). The two functions coexist in the constraint, making it tangled_rope from certain perspectives and scaffold from others. The mandatrophy is resolved by recognizing that the classification depends on whether the agent has standing in the deliberative process: agents with genuine participation rights and the ability to organize support experience scaffold (temporary, revisable boundaries); agents excluded from deliberation or lacking advocacy capacity experience snare (permanent gatekeeping with no voice). The deliberative reading itself is mandatrophy-resolving because it foregrounds the process as the mechanism of legitimacy — closure is not imposed by rule but generated through distributed deliberation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deliberative_closure_adequacy,
    'Does the AfD deliberative process generate adequate closure (legitimate boundary decisions) or does it perpetually defer hard cases to informal consensus that varies by editorial clique?',
    'Audit of deletion decision consistency: track how similar articles are treated across time and editor communities; measure variance in citation standard application and GNG interpretation across ArbCom decisions',
    'If adequate closure: scaffold classification holds — sunset logic is real because boundaries can be negotiated and reset. If inadequate: constraint drifts toward piton (performative) or tangled_rope (clique extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deliberative_closure_adequacy, empirical, 'Whether AfD deliberation produces legitimate and consistent boundary decisions').

omega_variable(
    systemic_bias_in_gatekeeping,
    'Does notability enforcement distribute suppression equally across communities or does it systemically disadvantage subjects that lack organized editorial advocates?',
    'Comparative deletion rates by subject domain (STEM vs. humanities vs. local history); survival analysis of biographical articles by subject prominence; language-edition analysis (Wikipedia-EN vs. Wikipedia-FR vs. Wikipedias in languages serving marginalized communities)',
    'If equal distribution: scaffold maintains deliberative legitimacy. If systemic bias: snare classification becomes dominant for powerless subjects; constraint requires active bias correction (affirmative inclusion policies) to prevent pure extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(systemic_bias_in_gatekeeping, empirical, 'Whether notability gatekeeping has systematic biases against marginalized communities').

omega_variable(
    afd_vs_stated_criteria_divergence,
    'How large is the gap between stated notability criteria (WP:N page) and actual deletion decisions in AfD deliberation? Are deletions justified by criteria logic or by editorial consensus that invokes criteria post-hoc?',
    'Content analysis of AfD closing statements: extract explicit criteria citations vs. implicit consensus signals; compare invoked criteria to stated GNG/SNG definitions; track ''snowball'' closures vs. detailed criterion-by-criterion analysis',
    'Large divergence confirms piton status (theater) — the policy persists for appearance of legitimacy while actual mechanism is consensus negotiation. Small divergence supports scaffold status (deliberative output legitimates boundary).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(afd_vs_stated_criteria_divergence, empirical, 'Gap between stated notability criteria and actual AfD decision logic').

omega_variable(
    reading_identity_ambiguity,
    'Is notability fundamentally a deliberative governance process (this reading) or is it fundamentally a fixed set of rules administered through deliberation (deletionist reading) or fundamentally an inclusionist principle constrained by practical limits (inclusionist reading)?',
    'Examine Wikipedia policy history: which reading was anchored first? How do policy makers justify amendments — as clarifying rules, as recalibrating consensus, or as defending inclusion against deletion pressure? Which institutional layer (ArbCom, Jimbo, community consensus) has final authority to revise boundaries?',
    'If deliberative: scaffold reading is correct — boundaries are outputs of process. If deletionist: the reading is miscategorized as scaffold when it should be piton or snare (rules with appearance of deliberation). If inclusionist: the reading overstates the legitimacy of gatekeeping.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_identity_ambiguity, conceptual, 'Whether notability is fundamentally deliberative (this reading), rule-based (deletionist), or inclusion-presumptive (inclusionist)').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(notability_guidelines__deliberative_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(notab_delib_tr_t0, notability_guidelines__deliberative_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(notab_delib_tr_t5, notability_guidelines__deliberative_reading, theater_ratio, 5, 0.52).
narrative_ontology:measurement(notab_delib_tr_t10, notability_guidelines__deliberative_reading, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(notab_delib_be_t0, notability_guidelines__deliberative_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(notab_delib_be_t5, notability_guidelines__deliberative_reading, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(notab_delib_be_t10, notability_guidelines__deliberative_reading, base_extractiveness, 10, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(notability_guidelines__deliberative_reading, identity_coordination).
narrative_ontology:affects_constraint(notability_guidelines__deliberative_reading, wikipedia_deletion_process).
narrative_ontology:affects_constraint(notability_guidelines__deliberative_reading, encyclopedic_inclusionism_bias).
narrative_ontology:affects_constraint(notability_guidelines__deliberative_reading, editorial_gatekeeping_asymmetry).

% DUAL FORMULATION NOTE:
% Notability guidelines decompose into three constraint stories corresponding to three readings of the same kernel. Deliberative reading (this file): ε=0.32, scaffold, emphasizes process-based legitimacy. Deletionist reading: ε≈0.15–0.20, rope, emphasizes rule consistency and predictability. Inclusionist reading: ε≈0.45–0.55, tangled_rope, emphasizes asymmetric burden and gatekeeping extraction. All three stories have the same base properties kernel but different classification chains reflecting different commitments about what grounds notability. Network links establish that changes in one reading's implementation create pressure on siblings (e.g., if AfD deliberation becomes more inclusive, deletionist reading's rule consistency suffers; if exclusionary bias emerges, inclusionist reading's framing becomes more salient).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
