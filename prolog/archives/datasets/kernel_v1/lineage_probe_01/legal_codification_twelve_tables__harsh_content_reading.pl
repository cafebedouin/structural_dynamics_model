% ============================================================================
% CONSTRAINT STORY: legal_codification_twelve_tables__harsh_content_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legal_codification_twelve_tables__harsh_content_reading, []).

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
 *   constraint_id: legal_codification_twelve_tables__harsh_content_reading
 *   human_readable: The Twelve Tables: Harsh Content Reading (Codified Debt Bondage and Status Hierarchy)
 *   domain: legal/doctrinal/roman_law
 *
 * SUMMARY:
 *   The harsh_content_reading focuses on what the Twelve Tables actually
 *   codified: debt bondage (nexum), the creditor's legal right to seize
 *   debtors' persons, intermarriage bans between patricians and plebeians,
 *   and explicit status hierarchy. This reading treats codification not as
 *   liberation (publication victory reading) or as ancestor myth (foundation
 *   myth reading), but as institutional escalation — the fixing in writing of
 *   extractive relationships that had been ambiguous or customary. The
 *   harshness was not new, but codification made it explicit, enforceable,
 *   and immutable. Creditors benefited from clarity; debtors were trapped by
 *   explicitness. The suppression metrics reflect this: pre-codification, the
 *   law was ambiguous (known only to pontifical interpreters, susceptible to
 *   reinterpretation); post-codification, the law was transparent and
 *   unambiguous, which paradoxically increased suppression — there was no
 *   longer any room for ambiguity or merciful reinterpretation. Theater ratio
 *   dropped because the harsh rules are stated plainly, not shrouded in
 *   ritual. The constraint is Tangled Rope because genuine coordination
 *   occurs (credit relationships require clarity) alongside asymmetric
 *   extraction (clarity benefits creditors and harms debtors). The analytics
 *   at civilizational distance risks false summitry by treating the Tables'
 *   harshness as an unchangeable natural law of social order.
 *
 * KEY AGENTS:
 *   - Creditors and Patrician Order: Primary beneficiary (institutional/arbitrage) — capture explicit legal enforcement of debt and status hierarchy
 *   - Debtors Under Nexum: Primary victim (powerless/trapped) — bound by explicit codification of debt bondage with no exit except creditor release
 *   - Plebeian Underclass: Secondary victim (powerless/constrained) — benefit from publication of law but harmed by codified restrictions on intermarriage and status
 *   - Plebeian Assembly: Organized agent (organized/constrained) — achieved publication victory but constrained by the written law they demanded
 *   - Pontifical Authority: Institutional actor (institutional/constrained) — loses monopoly knowledge but maintains interpretive role and ritual gatekeeping
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the Tables' harshness as immutable law rather than contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legal_codification_twelve_tables__harsh_content_reading, 0.58).
domain_priors:suppression_score(legal_codification_twelve_tables__harsh_content_reading, 0.72).
domain_priors:theater_ratio(legal_codification_twelve_tables__harsh_content_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legal_codification_twelve_tables__harsh_content_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(legal_codification_twelve_tables__harsh_content_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(legal_codification_twelve_tables__harsh_content_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legal_codification_twelve_tables__harsh_content_reading, tangled_rope).
narrative_ontology:human_readable(legal_codification_twelve_tables__harsh_content_reading, "The Twelve Tables: Harsh Content Reading (Codified Debt Bondage and Status Hierarchy)").
narrative_ontology:topic_domain(legal_codification_twelve_tables__harsh_content_reading, "legal/doctrinal/roman_law").

domain_priors:requires_active_enforcement(legal_codification_twelve_tables__harsh_content_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legal_codification_twelve_tables__harsh_content_reading, 'e3e474f5-ddb9-4a8e-9836-b0a924cc5bb8').
narrative_ontology:cs_kernel_codification('e3e474f5-ddb9-4a8e-9836-b0a924cc5bb8', formalized).
narrative_ontology:cs_authority_grounding('e3e474f5-ddb9-4a8e-9836-b0a924cc5bb8', extraction).
narrative_ontology:cs_interpretation_layer_present('e3e474f5-ddb9-4a8e-9836-b0a924cc5bb8').
narrative_ontology:cs_reading_relation('e3e474f5-ddb9-4a8e-9836-b0a924cc5bb8', legal_codification_twelve_tables__foundation_myth_reading, coexists_with).
narrative_ontology:cs_reading_relation('e3e474f5-ddb9-4a8e-9836-b0a924cc5bb8', legal_codification_twelve_tables__publication_victory_reading, coexists_with).
narrative_ontology:cs_axiom('e3e474f5-ddb9-4a8e-9836-b0a924cc5bb8', foundational, codification_fixes_extractive_hierarchy).
narrative_ontology:cs_axiom_status(codification_fixes_extractive_hierarchy, holdable).
narrative_ontology:cs_axiom_grounding('e3e474f5-ddb9-4a8e-9836-b0a924cc5bb8', codification_fixes_extractive_hierarchy, empirically_contingent).
narrative_ontology:cs_axiom('e3e474f5-ddb9-4a8e-9836-b0a924cc5bb8', foundational, clarity_suppresses_alternative_readings).
narrative_ontology:cs_axiom_status(clarity_suppresses_alternative_readings, holdable).
narrative_ontology:cs_axiom_grounding('e3e474f5-ddb9-4a8e-9836-b0a924cc5bb8', clarity_suppresses_alternative_readings, deontological).
narrative_ontology:cs_reference_frame('e3e474f5-ddb9-4a8e-9836-b0a924cc5bb8', customary_law_ambiguity_frame).
narrative_ontology:cs_drift_state('e3e474f5-ddb9-4a8e-9836-b0a924cc5bb8', post_codification_enforcement_era, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('e3e474f5-ddb9-4a8e-9836-b0a924cc5bb8', '').
narrative_ontology:cs_kernel_id(legal_codification_twelve_tables__harsh_content_reading, legal_codification_twelve_tables).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legal_codification_twelve_tables__harsh_content_reading, creditors_and_patrician_order).
narrative_ontology:constraint_victim(legal_codification_twelve_tables__harsh_content_reading, debtors_under_nexum).
narrative_ontology:constraint_victim(legal_codification_twelve_tables__harsh_content_reading, plebeian_underclass).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEBT-BONDED DEBTOR (SNARE) — Trapped by nexum, the self-sale into bondage. Codification makes the debtor's status explicit and enforceable. No exit except through manumission by creditor. The harshness is now written law, not hidden custom — extraction is transparent and absolute.
constraint_indexing:constraint_classification(legal_codification_twelve_tables__harsh_content_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: PATRICIAN CREDITOR (TANGLED ROPE) — Benefits from explicit nexum law that guarantees debtor bondage and transfer rights. Also coordinates underlying credit relationships and debt enforcement across the community. Genuine coordination function (enabling credit system) coexists with asymmetric extraction (creditor captures debtor's labor and legal status). Arbitrage exit: creditor can shift assets, relocate, or leverage political power.
constraint_indexing:constraint_classification(legal_codification_twelve_tables__harsh_content_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: PLEBEIAN ASSEMBLY (ROPE) — Organized political body. Demanded publication of the law to end pontiff monopoly on legal knowledge. Codification delivers that coordination benefit: law written where plebs can read it is a genuine coordination victory. But the Tables' harsh content (debt bondage, status hierarchy) extracts from plebeian debtors. Constrained exit: plebs cannot overturn the written law without organized political struggle.
constraint_indexing:constraint_classification(legal_codification_twelve_tables__harsh_content_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: PONTIFICAL AUTHORITY (TANGLED ROPE) — Loses monopoly on legal knowledge (coordination loss) but maintains interpretive authority over law's meaning. The written code requires priests to explain ambiguities, consult omens on procedure, and certify correct ritual. Codification reduces their gate-keeping but creates new enforcement role. Mixed extraction and coordination in the institutional relationship.
constraint_indexing:constraint_classification(legal_codification_twelve_tables__harsh_content_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational distance, the Tables appear as a fixed codification of unchangeable social orders: debt, status, hierarchy — these are presented as immutable rules of community life. The harshness is portrayed as natural to the structure itself, not as a choice. This perspective risks false summitry: it naturalizes what is actually a contingent institutional arrangement benefiting creditors and patricians.
constraint_indexing:constraint_classification(legal_codification_twelve_tables__harsh_content_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legal_codification_twelve_tables__harsh_content_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(legal_codification_twelve_tables__harsh_content_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(legal_codification_twelve_tables__harsh_content_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(legal_codification_twelve_tables__harsh_content_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legal_codification_twelve_tables__harsh_content_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The harsh_content_reading emphasizes that codification fixed extractive relationships (debt bondage, status hierarchy) in permanent form. The extractiveness value reflects the actual economic harm to debtors, not merely the institutional change. The increase from pre-codification ambiguity (0.48) to post-codification clarity (0.58) reflects the escalation from custom to law — what was once negotiable became rigid. Theater ratio (0.35): Low. Unlike piton constraints that rely on performative justification, the harsh reading emphasizes direct, plainly stated enforcement. The Tables' code states rules explicitly: 'If a creditor has bound a debtor with nexum, the debtor becomes the creditor's property' (paraphrased). This is not theater — it is direct statement of harsh terms. Suppression (0.72): High. Pre-codification, customary law allowed ambiguity and reinterpretation (suppression 0.55). Post-codification, the written word suppressed alternative readings. Debtors could no longer claim custom was different; the law was now fixed. Suppression increased because clarity removed interpretive escape routes.
 *
 * PERSPECTIVAL GAP:
 *   The harsh_content_reading generates maximum perspectival gap. Creditors see coordination and benefit (Tangled Rope from their view, potentially Rope if coordination benefit is emphasized). Debtors see pure extraction with no exit (Snare). The plebeian assembly sees coordination victory in the act of publication (Rope) but the plebeian underclass sees harm from the content (Snare). The pontifical authority sees mixed institutional change — loss of monopoly, gain of interpretive role (Tangled Rope). The analytical observer risks false summitry by naturalizing the harshness. This perspectival divergence is diagnostic: the same code produces opposite classification outcomes depending on structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   The harsh_content_reading computes directionality from beneficiary and victim declarations plus exit options. Creditors (institutional/arbitrage) have low d because they benefit and can exit via asset transfer or political leverage — they experience negative effective extraction (they are the extractors, not the extracted-from). Debtors (powerless/trapped) have high d because they bear costs with no exit — they experience maximum effective extraction. The tangled_rope classification results from the coexistence of coordination (credit system requires clarity) and extraction (clarity benefits creditors asymmetrically). The snare perspectives (debtors, plebeian underclass) emerge because those agents have zero agency and zero benefit from the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The harsh reading resolves the mandatrophy by showing that the Tables contain genuine coordination (credit system enablement) alongside genuine extraction (creditor-favorable enforcement and status hierarchy). The constraint is not purely extractive (that would be Snare) because the written law does solve a real coordination problem: debtors and creditors both benefit from clarity about debt obligations, even if the written clarity happens to favor creditors asymmetrically. The constraint is not purely coordinating (that would be Rope) because the clarity is not neutral — it codifies and makes permanent what might have been more flexible customarily. Tangled Rope is the correct type because both functions are real and neither can be removed without destroying the constraint entirely. Remove the extraction and you have a beneficiary-neutral coordination mechanism; remove the coordination and you have pure debt slavery. The harshness and the coordination are structurally inseparable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nexum_as_consensual_sale_vs_coerced_bondage,
    'Is nexum a consensual self-sale (coordination mechanism enabling credit) or a coerced debt trap (extraction mechanism exploiting power asymmetry)?',
    'Analysis of actual nexum contracts and release patterns: Do debtors genuinely consent or are they forced by hunger/debt? Are releases routine or rare? Do creditors use bondage as a labor extraction tool or as a last-resort collection mechanism?',
    'If consensual coordination: extractiveness drops to 0.35, constraint becomes Rope. If coerced: extractiveness rises to 0.75, constraint becomes pure Snare. Current estimate (0.58, Tangled Rope) assumes mixed motives and varying outcomes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nexum_as_consensual_sale_vs_coerced_bondage, empirical, 'Whether nexum is consensual credit mechanism or coerced labor extraction').

omega_variable(
    codification_as_suppression_clarification_or_escalation,
    'Does codifying harsh terms suppress existing ambiguity (making extraction explicit) or escalate existing cruelties (making them enforceable)?',
    'Comparative legal history: Did written debt law create new obligations or clarify existing customs? Were bondage practices more or less severe before codification? Did enforcement mechanisms intensify after writing?',
    'If clarification: suppression (0.72) is accurate — ambiguity suppressed, extraction made plain. If escalation: suppression understates the shift — new enforcement machinery creates new extraction vectors. Current assessment assumes suppression of pre-existing ambiguity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(codification_as_suppression_clarification_or_escalation, empirical, 'Whether codification clarified or escalated harsh debt practices').

omega_variable(
    plebeian_benefit_vs_plebeian_victimhood_same_code,
    'Did the same published code benefit plebeians (by ending pontiff monopoly) and harm plebeians (by codifying debt bondage against them)?',
    'Disaggregated analysis: separate plebeian landowners/creditors (beneficiaries) from plebeian debtors (victims). Measure representation in each group over time.',
    'If plebeians were internally stratified, the rope perspective (assembly benefits from publication) coexists with snare perspective (debtor victims harmed by content). If plebeians were mostly debtors, rope perspective overstates coordination benefit. Current reading assumes internal stratification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(plebeian_benefit_vs_plebeian_victimhood_same_code, empirical, 'Disaggregation of plebeian benefit vs. harm from same codification').

omega_variable(
    kernel_reading_contest_harsh_vs_foundation_vs_publication,
    'Which reading of the Twelve Tables kernel is historically dominant: harsh_content (what was written was cruel and extractive), foundation_myth (the Tables became ancestor cult divorced from practical law), or publication_victory (the act of writing ended monopoly knowledge)?',
    'This is a committer-axis question, not an empirical one. The three readings coexist as live positions held by different historians and legal traditions. Harsh reading emphasizes actual economic harm and status hierarchy. Foundation myth emphasizes later reception and textual awe. Publication victory emphasizes epistemological liberation. Each reading highlights real features of the historical event.',
    'The readings do not foreclose each other within any single framework. All three can be true simultaneously: the Tables were harsh (content), became mythologized (reception), and did shift knowledge access (publication effect). The harsh_content_reading is one legitimate extraction of the kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_harsh_vs_foundation_vs_publication, conceptual, 'The kernel ''Twelve Tables'' admits multiple structurally distinct readings with different focal points').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legal_codification_twelve_tables__harsh_content_reading, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(harsh_theater_t0_customary_ritual, legal_codification_twelve_tables__harsh_content_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(harsh_theater_t1_explicit_rules, legal_codification_twelve_tables__harsh_content_reading, theater_ratio, 1, 0.35).
narrative_ontology:measurement(harsh_theater_t5_interpretive_ritual_revival, legal_codification_twelve_tables__harsh_content_reading, theater_ratio, 5, 0.38).

% Extraction over time
narrative_ontology:measurement(harsh_extractiveness_t0_precodification, legal_codification_twelve_tables__harsh_content_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(harsh_extractiveness_t1_postcodification, legal_codification_twelve_tables__harsh_content_reading, base_extractiveness, 1, 0.58).
narrative_ontology:measurement(harsh_extractiveness_t5_enforcement_ratchet, legal_codification_twelve_tables__harsh_content_reading, base_extractiveness, 5, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(harsh_suppression_t0_customary_ambiguity, legal_codification_twelve_tables__harsh_content_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(harsh_suppression_t1_codified_clarity, legal_codification_twelve_tables__harsh_content_reading, suppression_requirement, 1, 0.72).
narrative_ontology:measurement(harsh_suppression_t5_interpretive_lock, legal_codification_twelve_tables__harsh_content_reading, suppression_requirement, 5, 0.74).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legal_codification_twelve_tables__harsh_content_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(legal_codification_twelve_tables__harsh_content_reading, 0.15).
narrative_ontology:affects_constraint(legal_codification_twelve_tables__harsh_content_reading, legal_codification_twelve_tables__foundation_myth_reading).
narrative_ontology:affects_constraint(legal_codification_twelve_tables__harsh_content_reading, legal_codification_twelve_tables__publication_victory_reading).
narrative_ontology:affects_constraint(legal_codification_twelve_tables__harsh_content_reading, roman_debt_slavery_nexum_mechanism).
narrative_ontology:affects_constraint(legal_codification_twelve_tables__harsh_content_reading, patrician_plebeian_status_hierarchy_codification).

% DUAL FORMULATION NOTE:
% The Twelve Tables kernel decomposes into three structurally distinct constraint stories: harsh_content_reading emphasizes what was written and who bore costs; foundation_myth_reading emphasizes later mythologization divorced from practical function; publication_victory_reading emphasizes the epistemological shift (end of pontiff monopoly). These are not different perspectives on one constraint — they are different constraints with different ε values and different historical causality. The harsh_content_reading (this story, ε=0.58) is upstream of the other two in causal order: the content had to exist before it could be mythologized or its publication effect could register. All three readings are linked through the same kernel and must be cross-referenced.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
