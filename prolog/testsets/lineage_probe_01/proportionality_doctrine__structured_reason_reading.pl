% ============================================================================
% CONSTRAINT STORY: proportionality_doctrine__structured_reason_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_proportionality_doctrine__structured_reason_reading, []).

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
 *   constraint_id: proportionality_doctrine__structured_reason_reading
 *   human_readable: Proportionality Doctrine: Structured Public Reason Reading
 *   domain: legal/constitutional_doctrine
 *
 * SUMMARY:
 *   Proportionality doctrine — specifically, the four-step test: (1)
 *   legitimate aim, (2) suitable means, (3) necessary means, (4)
 *   proportionate balance — is one of the most consequential innovations in
 *   post-WWII constitutional law. Beginning in Germany
 *   (Bundesverfassungsgericht), it has been adopted across the European
 *   Union, Canada, South Africa, and many other jurisdictions. This
 *   constraint story instantiates ONE reading of proportionality: the
 *   'structured public reason' reading. This reading claims that
 *   proportionality works by converting state action into structured,
 *   auditable reasoning — four fixed questions in fixed order that force the
 *   state to justify any burden it imposes, subject to public scrutiny and
 *   judicial review. The reading stands opposed to two sibling readings: (1)
 *   the balancing_critique_reading, which argues that the final
 *   'proportionate in the narrow sense' step reduces to naked judicial
 *   preference dressed as measurement, and (2) the global_export_reading,
 *   which treats proportionality primarily as a tool for constitutional
 *   diffusion and global institutional alignment, bracketing the question of
 *   whether the mechanism actually constrains state action or merely
 *   rationalizes it. The structured_reason_reading is itself a contested
 *   doctrinal claim, not a neutral description. It carries normative
 *   commitments: that reasoning can be structured, that public justification
 *   constrains discretion, that the four steps have inherent logical force
 *   independent of the judge applying them. These commitments are live in
 *   contemporary constitutional theory, but they are challenged by critics
 *   who see the balancing step as an escape hatch for preference.
 *
 * KEY AGENTS:
 *   - Burdened Subjects: Primary beneficiary (powerless/constrained) — gain a structured forum to demand state justification, reducing arbitrary suffering and enabling contestation through reasoned argument
 *   - Constrained Administration: Secondary beneficiary (institutional/constrained) — obtains legitimacy and legal predictability; the four-step framework solves the coordination problem of justifying state action without falling into pure discretion or paralysis
 *   - Judicial Interpreters: Mixed (institutional/constrained) — experience both coordination benefit (transparent framework for review, legitimate authority) and extraction benefit (the balancing step allows discretion to masquerade as measurement)
 *   - Balancing Critics (Schmitt, Habermas): Analytical observer — see the doctrine's final step as a mask for will; deny that proportionality actually constrains discretion
 *   - Global Export Advocates: Analytical observer — emphasize the doctrine's adoptability and global institutional influence, treating it as a successful constitutional technology independent of whether it constrains state action
 *   - Proportionality's Doctrinal Tradition: Authority structure (institutional) — the corpus of constitutional court decisions, academic commentary, and cross-jurisdictional adoption that treats proportionality as established, binding doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(proportionality_doctrine__structured_reason_reading, 0.22).
domain_priors:suppression_score(proportionality_doctrine__structured_reason_reading, 0.35).
domain_priors:theater_ratio(proportionality_doctrine__structured_reason_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(proportionality_doctrine__structured_reason_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(proportionality_doctrine__structured_reason_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(proportionality_doctrine__structured_reason_reading, theater_ratio, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(proportionality_doctrine__structured_reason_reading, rope).
narrative_ontology:human_readable(proportionality_doctrine__structured_reason_reading, "Proportionality Doctrine: Structured Public Reason Reading").
narrative_ontology:topic_domain(proportionality_doctrine__structured_reason_reading, "legal/constitutional_doctrine").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(proportionality_doctrine__structured_reason_reading, '8b1d1317-0611-4978-9fb8-a48519a7b09d').
narrative_ontology:cs_kernel_codification('8b1d1317-0611-4978-9fb8-a48519a7b09d', fixed_text).
narrative_ontology:cs_authority_grounding('8b1d1317-0611-4978-9fb8-a48519a7b09d', lineage).
narrative_ontology:cs_interpretation_layer_present('8b1d1317-0611-4978-9fb8-a48519a7b09d').
narrative_ontology:cs_reading_relation('8b1d1317-0611-4978-9fb8-a48519a7b09d', proportionality_doctrine__balancing_critique_reading, coexists_with).
narrative_ontology:cs_reading_relation('8b1d1317-0611-4978-9fb8-a48519a7b09d', proportionality_doctrine__global_export_reading, influences).
narrative_ontology:cs_axiom('8b1d1317-0611-4978-9fb8-a48519a7b09d', foundational, reasoning_constrains_discretion).
narrative_ontology:cs_axiom_status(reasoning_constrains_discretion, holdable).
narrative_ontology:cs_axiom_grounding('8b1d1317-0611-4978-9fb8-a48519a7b09d', reasoning_constrains_discretion, instrumental).
narrative_ontology:cs_axiom('8b1d1317-0611-4978-9fb8-a48519a7b09d', foundational, justification_must_be_public_and_structured).
narrative_ontology:cs_axiom_status(justification_must_be_public_and_structured, holdable).
narrative_ontology:cs_axiom_grounding('8b1d1317-0611-4978-9fb8-a48519a7b09d', justification_must_be_public_and_structured, deontological).
narrative_ontology:cs_reference_frame('8b1d1317-0611-4978-9fb8-a48519a7b09d', structured_liberal_justification).
narrative_ontology:cs_drift_state('8b1d1317-0611-4978-9fb8-a48519a7b09d', contemporary_balancing_practice, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('8b1d1317-0611-4978-9fb8-a48519a7b09d', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(proportionality_doctrine__structured_reason_reading, proportionality_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(proportionality_doctrine__structured_reason_reading, burdened_subjects).
narrative_ontology:constraint_beneficiary(proportionality_doctrine__structured_reason_reading, constrained_administration).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE BURDENED SUBJECT (ROPE) — Gains a structured forum to demand state justification. The four-step test (legitimate aim, suitable means, necessary means, proportionate balance) creates an obligation on the state to show its work. The constraint is genuine coordination: the subject can now contest arbitrary burdens through reasoned argument. No exit option because the burden is imposed by authority, but the forum reduces suppression. Low experienced extractiveness because the mechanism is non-extractive — the state gains no asymmetric benefit from the four-step structure.
constraint_indexing:constraint_classification(proportionality_doctrine__structured_reason_reading, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: THE CONSTRAINED ADMINISTRATION (ROPE) — Must now justify burdens through the four-step framework. This is coordination cost, not extraction: the administration benefits from legitimacy and legal certainty (predictability across cases). The framework solves a collective action problem in the state itself — preventing ad hoc, unreasoned burdens that undermine rule of law. The constraint is binding (constrained exit: violating it carries legal consequences), but the coordination function is genuine. The four-step structure enables better administration, not asymmetric extraction from the state.
constraint_indexing:constraint_classification(proportionality_doctrine__structured_reason_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (ROPE) — This reading instantiates proportionality as a coordination mechanism that solves the enduring problem: how does a liberal state justify coercive action without falling into pure discretion (Schmittean decisionism) or hollow ritualism (Habermas's critique)? The four-step structure is a coordination solution — it makes reasoning transparent, auditable, and falsifiable. Low extractiveness because the mechanism serves the mutual interest of both the burdened and the burdening authority. High theater? No — the steps are functional. The constraint is pure coordination: both sides benefit from reduced arbitrariness.
constraint_indexing:constraint_classification(proportionality_doctrine__structured_reason_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: THE JUDICIAL INTERPRETER (TANGLED ROPE) — The judge who applies the four-step test experiences both coordination and extraction. Coordination: the framework gives judges a transparent tool for review, solving the legitimacy crisis of judicial review (why judges get to second-guess legislatures). Extraction: the final 'proportionate in the narrow sense' step (balancing necessity against burden) has no fixed weights — judges necessarily exercise discretion here, creating space for judicial preference to masquerade as measurement. The judicial role is elevated (gains authority), but the constraint also disciplines and constrains judges. Mixed: genuine coordination on legitimacy + real extraction through the balancing step.
constraint_indexing:constraint_classification(proportionality_doctrine__structured_reason_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(proportionality_doctrine__structured_reason_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(proportionality_doctrine__structured_reason_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(proportionality_doctrine__structured_reason_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(proportionality_doctrine__structured_reason_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.22): Low. This reading instantiates proportionality as a coordination solution to the enduring problem: how does a liberal state justify coercive action without falling into pure discretion (decisionism) or hollow ritualism? The four-step structure constrains state action by requiring auditable reasons. No agent systematically benefits at another's expense; instead, both the burdened and the burdening authority benefit from reduced arbitrariness. The extracted value (if any) is minimal — the mechanism is not set up to extract from anyone, and historical adoption patterns show that proportionality generally reduces rather than increases burden. The slight upward drift (0.08 → 0.22 over 50 years) reflects growing judicial confidence in the final balancing step, which does create interpretive space — but this is a minimal effect. Suppression (0.35): Moderate. The four-step framework genuinely suppresses unreasoned burden — states can no longer impose burdens without justification. However, suppression is not complete; the balancing step leaves room for justifications that masquerade as weights. The mechanism constrains but does not eliminate state discretion. The suppression metric reflects this: genuine constraint without totalizing control. Theater ratio (0.28): Low. The four-step structure is functional, not performative. The framework has real logical force: a burden that fails step 1 (no legitimate aim) or step 2 (not suitable) or step 3 (not necessary) must be withdrawn. The final balancing step does introduce some theater, but the first three steps are rigid gates. The low theater ratio reflects that the constraint is mostly composed of material constraint rather than performance.
 *
 * PERSPECTIVAL GAP:
 *   The key perspectival gap is between the burdened subject's view (rope: gain a genuine forum for contestation) and the judicial interpreter's view (tangled_rope: the framework coordinates legitimate review, but the balancing step allows preference to masquerade as measurement). The analytical observer, across the other three perspectives, sees pure coordination — but this perspective lacks the internal experience of applying the final step, where judges navigate the space between reasoning and will. The analytical view risks underestimating the extraction risk that the judicial perspective reveals. The gap is instructive: the structured_reason_reading appears robust from a distance (analytical) and to subjects who use the forum (burdened), but fragments when examined from inside the judicial role that applies it. The balancing_critique_reading (sibling) makes this gap its entire argument.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary derivation: Burdened subjects (powerless/constrained) benefit from the forum — low directionality d. Constrained administration (institutional/constrained) benefits from legitimacy and predictability — also low d. No victimization structure; the constraint does not extract from anyone systematically. The beneficiary list consists of agents who gain from reduced arbitrariness. This is why the classification is rope: low base extraction (0.22), moderate suppression (0.35), no asymmetric beneficiary set. Both beneficiary and burdened are the same coalition (anyone subject to state authority who wants it reasoned rather than arbitrary). Canonical d for institutional beneficiary with constrained exit is approximately 0.25; for powerless beneficiary with constrained exit, approximately 0.65. However, because both are beneficiaries (not victims), the directionality flow is cooperative, not extractive. The engine will compute low f(d) values for both, confirming the rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   N/A — Extractiveness (0.22) is below the 0.46 threshold. Mandatrophy does not apply to rope constraints. However, the perspectival gap between the analytical/burdened view (rope) and the judicial view (tangled_rope) signals potential misclassification. If the balancing step is truly discretionary, the constraint should be classified as tangled_rope across all perspectives (mixing genuine coordination legitimacy with real extraction through unmeasured balancing). The structured_reason_reading asserts that the first three steps sufficiently constrain the balancing step that net extraction remains low. The balancing_critique_reading denies this. The empirical omega variables (particularly 'balancing_step_discretion') are designed to adjudicate this claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    balancing_step_discretion,
    'Does the final balancing step reduce to naked judicial preference, or does the structured process constrain discretion sufficiently to constitute reasoned judgment?',
    'Cross-jurisdictional comparative analysis of outcomes under identical fact patterns; correlation between stated reasons and final judgments; detection of systematic judicial ideology in balancing outcomes. Also: meta-analysis of whether different judges following the four-step protocol reach different conclusions from identical briefs.',
    'If balancing is covert preference: the rope classification collapses toward snare or tangled_rope (extractiveness increases to 0.45+, suppression to 0.55+). The constraint becomes a mask for judicial will. If balancing respects the prior steps: rope classification confirmed. The structured process does constrain discretion, and the remaining discretion is legitimate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(balancing_step_discretion, empirical, 'Whether the final balancing step constrains discretion or masks preference').

omega_variable(
    kernel_vs_reading_identity,
    'Is this reading (structured public reason) a genuine alternative interpretation of proportionality doctrine, or is it the reading that the doctrine''s own logic demands?',
    'Historical analysis of proportionality''s development: did it emerge as structured reason (the reading position) or as something else? Examine whether the doctrine''s architecture (fixed four steps) was designed to instantiate this reading or whether the reading is imposed retroactively. Check whether practitioners and doctrinal authorities self-describe proportionality as ''structured public reason'' or whether this is an external characterization.',
    'If the reading is exogenous (imposed retroactively): this may be a conceptual framing rather than a true kernel reading; reclassify as ordinary constraint with cs_structure. If the reading is native (the doctrine was designed around this idea): the kernel-reading structure is warranted. If the doctrine emerged from different roots: this reading coexists_with rival readings from shared authority, not as a foreclosing alternative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_vs_reading_identity, conceptual, 'Whether ''structured public reason'' is the doctrine''s native logic or an imposed reading').

omega_variable(
    suppression_mechanism_identity,
    'Does the four-step framework actually suppress unreasoned burden, or does it merely move the point of suppression downstream (from the burdens themselves to the justifications)?',
    'Track empirical outcomes: do states adopt proportionality doctrine and then cease to impose arbitrary burdens? Or do they continue the same burdens with better justifications? Examine jurisdictions pre- and post-adoption of proportionality review. Also: comparative analysis of burden distributions in proportionality regimes vs. regimes without structured review (if any remain).',
    'If burdens are actually suppressed: suppression metric confirmed at 0.35 (state discretion is genuinely constrained). If burdens continue with better cover: suppression is really ~0.55, extractiveness rises to 0.40+, and the constraint reclassifies as tangled_rope or snare (the framework becomes a mask for continued extraction). The four-step structure itself would then be the extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_identity, empirical, 'Whether proportionality suppresses unreasoned burdens or merely rationalizes them').

omega_variable(
    structured_reason_exportability,
    'Can the four-step test be transplanted across constitutional traditions, or is its function dependent on institutional context (judge appointments, legislative supremacy, rule-of-law infrastructure)?',
    'Comparative constitutional law study: proportionality''s effectiveness in different regimes (strong judiciaries vs. weak, separated powers vs. parliamentary, established rule of law vs. fragile). Measure by outcome distribution: does the test produce stable, predictable, reasoned burden justification across contexts? Or does it degrade in fragile institutions?',
    'If context-independent: the reading is robust; structured reason is a real institutional solution. If context-dependent: the reading''s universality claim (implicit in the ''structured public reason'' label) is overstated. The constraint may be rope in strong liberal-democratic contexts (genuine coordination) but tangled_rope or snare in weak ones (structured justification becomes cover for discretion). This would suggest the constraint needs decomposition (separate stories for different institutional contexts), not unification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structured_reason_exportability, empirical, 'Whether proportionality''s four-step structure is institution-independent').

omega_variable(
    reading_kernel_distinction,
    'Which elements of proportionality doctrine belong to the kernel (the fixed, contested common text), and which belong to the reading (this interpretation of what the kernel means)?',
    'Doctrinal archaeology: identify the stable elements claimed by all sibling readings (the kernel proper), then identify the elements this reading adds, interprets, or prioritizes. Example: all siblings likely accept that proportionality involves some testing of burden and justification; this reading adds that the test must be STRUCTURED and PUBLIC. Are structure and publicity part of the kernel or part of the reading?',
    'If structure/publicity are kernel properties: this reading is not a true alternative but a clarification of what all readings assume. Reclassify as ordinary constraint (not a kernel reading). If they are reading-specific: the kernel-reading structure is warranted. The distinction changes how sibling readings relate: coexistence vs. foreclosure vs. influence depend on whether the disputed element is in the kernel or in the interpretation layer.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kernel_distinction, conceptual, 'Boundary between the kernel and the reading''s interpretation of it').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(proportionality_doctrine__structured_reason_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prop_struct_theater_t0, proportionality_doctrine__structured_reason_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(prop_struct_theater_t25, proportionality_doctrine__structured_reason_reading, theater_ratio, 25, 0.24).
narrative_ontology:measurement(prop_struct_theater_t50, proportionality_doctrine__structured_reason_reading, theater_ratio, 50, 0.28).

% Extraction over time
narrative_ontology:measurement(prop_struct_extractiveness_t0, proportionality_doctrine__structured_reason_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(prop_struct_extractiveness_t25, proportionality_doctrine__structured_reason_reading, base_extractiveness, 25, 0.15).
narrative_ontology:measurement(prop_struct_extractiveness_t50, proportionality_doctrine__structured_reason_reading, base_extractiveness, 50, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(proportionality_doctrine__structured_reason_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(proportionality_doctrine__structured_reason_reading, proportionality_doctrine__balancing_critique_reading).
narrative_ontology:affects_constraint(proportionality_doctrine__structured_reason_reading, proportionality_doctrine__global_export_reading).

% DUAL FORMULATION NOTE:
% The proportionality_doctrine kernel is contested across three readings, each instantiating a different constraint with different extractiveness and suppression values. The structured_reason_reading (this story, ε=0.22, Rope) claims proportionality is genuine coordination. The balancing_critique_reading (ε≈0.45, Tangled Rope or Snare) claims the balancing step undoes the constraint's structure. The global_export_reading (ε≈0.30, Rope or Scaffold) treats proportionality as an institutional solution transplantable across contexts. Each reading shares the kernel (the four-step test) but produces different classifications because it emphasizes different elements. The stories are linked by network dependencies: this story's success (rope classification confirmed) would influence the balancing critique (more people would adopt the doctrine as constraint), which would then face pressure from critics. The network structure captures this interdependence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
