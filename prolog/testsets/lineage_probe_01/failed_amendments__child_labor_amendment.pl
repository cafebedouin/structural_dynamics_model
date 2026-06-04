% ============================================================================
% CONSTRAINT STORY: failed_amendments__child_labor_amendment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_failed_amendments__child_labor_amendment, []).

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
 *   constraint_id: failed_amendments__child_labor_amendment
 *   human_readable: Child Labor Amendment (1924–1938): Blocked Constitutional Grant to Suppress Child Exploitation
 *   domain: political/legal/labor_rights
 *
 * SUMMARY:
 *   The Child Labor Amendment (1924) represents a constitutional attempt to
 *   grant Congress plenary authority to regulate the labor of persons under
 *   eighteen. Proposed in response to Hammer v. Dagenhart (1918), which
 *   struck down the federal Child Labor Tax Act as exceeding Congress's
 *   commerce clause authority, the amendment passed both houses and was sent
 *   to the states for ratification. It stalled: manufacturing states
 *   (particularly in textiles and mining) refused ratification, and the
 *   amendment languished without reaching three-fourths ratification. The
 *   constraint captures this structural deadlock: a clear normative objective
 *   (protecting children from exploitation) required constitutional authority
 *   (federal power), but the ratification mechanism distributed veto power to
 *   the interests that benefited from child labor wage suppression. The
 *   constraint was eventually mooted — not resolved, but bypassed — when the
 *   New Deal's Fair Labor Standards Act (1938) and the Supreme Court's
 *   expansion of commerce clause authority in West Coast Hotel Co. v. Parrish
 *   (1937) and subsequent cases made the constitutional amendment
 *   unnecessary. Yet the amendment remained formally pending in the states
 *   until 1938. The extractiveness trajectory shows a slight decline over the
 *   period as the New Deal coalition built alternative pathways, but
 *   suppression remained high throughout: manufacturing states maintained
 *   legislative opposition, and children in exploitative labor continued to
 *   bear the full cost of the constraint's failure.
 *
 * KEY AGENTS:
 *   - Working children: Primary victims (powerless/trapped) — bear the extraction through lost education, physical harm, wage suppression, and dangerous conditions; have zero power to ratify
 *   - Federal government (pre-1937): Secondary victim (institutional/constrained) — lacks constitutional authority to regulate child labor until commerce clause is reinterpreted
 *   - Reform coalition (unions, child welfare advocates, progressive legislators): Organized opponents (organized/constrained) — push for amendment but face veto power held by beneficiaries
 *   - Manufacturing-dependent state legislatures: Primary beneficiaries (institutional/arbitrage) — maintain competitive labor cost advantages through refusal to ratify; coordinate through state political systems
 *   - Interstate manufacturing coalition (textile, mining, agricultural processors): Secondary beneficiaries (powerful/mobile) — extract wage suppression through political opposition; could exit without existential threat but choose not to
 *   - New Deal coalition (1933–1938): Alternative mechanism (organized/constrained) — eventually moots the constraint by achieving child labor suppression through statute rather than constitutional amendment
 *   - Analytical observer: Structural analyst (analytical/analytical) — sees the constitutional deadlock as a snare whose beneficiaries have veto power over their own constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(failed_amendments__child_labor_amendment, 0.68).
domain_priors:suppression_score(failed_amendments__child_labor_amendment, 0.82).
domain_priors:theater_ratio(failed_amendments__child_labor_amendment, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(failed_amendments__child_labor_amendment, extractiveness, 0.68).
narrative_ontology:constraint_metric(failed_amendments__child_labor_amendment, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(failed_amendments__child_labor_amendment, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(failed_amendments__child_labor_amendment, snare).
narrative_ontology:human_readable(failed_amendments__child_labor_amendment, "Child Labor Amendment (1924–1938): Blocked Constitutional Grant to Suppress Child Exploitation").
narrative_ontology:topic_domain(failed_amendments__child_labor_amendment, "political/legal/labor_rights").

domain_priors:requires_active_enforcement(failed_amendments__child_labor_amendment).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(failed_amendments__child_labor_amendment, 'bcc353a8-cb3e-4e57-b02f-17a9e417c3c3').
narrative_ontology:cs_kernel_codification('bcc353a8-cb3e-4e57-b02f-17a9e417c3c3', formalized).
narrative_ontology:cs_authority_grounding('bcc353a8-cb3e-4e57-b02f-17a9e417c3c3', lineage).
narrative_ontology:cs_interpretation_layer_present('bcc353a8-cb3e-4e57-b02f-17a9e417c3c3').
narrative_ontology:cs_reading_relation('bcc353a8-cb3e-4e57-b02f-17a9e417c3c3', failed_amendments__balanced_budget_amendment, coexists_with).
narrative_ontology:cs_reading_relation('bcc353a8-cb3e-4e57-b02f-17a9e417c3c3', failed_amendments__dc_voting_rights_amendment, coexists_with).
narrative_ontology:cs_reading_relation('bcc353a8-cb3e-4e57-b02f-17a9e417c3c3', failed_amendments__equal_rights_amendment, coexists_with).
narrative_ontology:cs_axiom('bcc353a8-cb3e-4e57-b02f-17a9e417c3c3', foundational, federal_child_labor_protection_constitutionally_necessary).
narrative_ontology:cs_axiom_status(federal_child_labor_protection_constitutionally_necessary, holdable).
narrative_ontology:cs_axiom_grounding('bcc353a8-cb3e-4e57-b02f-17a9e417c3c3', federal_child_labor_protection_constitutionally_necessary, deontological).
narrative_ontology:cs_axiom('bcc353a8-cb3e-4e57-b02f-17a9e417c3c3', foundational, constitutional_amendment_requires_three_fourths_state_consent).
narrative_ontology:cs_axiom_status(constitutional_amendment_requires_three_fourths_state_consent, holdable).
narrative_ontology:cs_axiom_grounding('bcc353a8-cb3e-4e57-b02f-17a9e417c3c3', constitutional_amendment_requires_three_fourths_state_consent, conventional).
narrative_ontology:cs_reference_frame('bcc353a8-cb3e-4e57-b02f-17a9e417c3c3', federal_constitutional_authority_required).
narrative_ontology:cs_drift_state('bcc353a8-cb3e-4e57-b02f-17a9e417c3c3', post_west_coast_hotel_1937, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('bcc353a8-cb3e-4e57-b02f-17a9e417c3c3', '2026-02-26T14:32:15Z').
narrative_ontology:cs_kernel_id(failed_amendments__child_labor_amendment, failed_amendments).

% --- Structural relationships ---
narrative_ontology:constraint_victim(failed_amendments__child_labor_amendment, working_children).
narrative_ontology:constraint_victim(failed_amendments__child_labor_amendment, federal_regulatory_power).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WORKING CHILD (SNARE) — Trapped in exploitative labor with no legal protection. The amendment's failure means no federal regulatory power exists to constrain state-level permissiveness on working hours, minimum age, or hazardous conditions. The child bears full cost of extraction through lost education, physical harm, and wage suppression. Suppression is maximal: state laws often explicitly permit or facilitate child labor; alternatives (schooling, parental support) are economically unavailable to poor families.
constraint_indexing:constraint_classification(failed_amendments__child_labor_amendment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REFORM COALITION (SNARE) — Child welfare advocates, union organizers, and progressive legislators see the amendment as the only viable path to federal authority. State ratification is constrained by political economy: state legislatures are dominated by manufacturing interests who benefit from child labor's wage suppression. The reform coalition faces suppression through legislative stalling, false ratifications, and political isolation. They can exit only by accepting state-level compromise (insufficient) or shifting strategy (which occurs with the New Deal).
constraint_indexing:constraint_classification(failed_amendments__child_labor_amendment, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MANUFACTURING-DEPENDENT STATE LEGISLATURE (ROPE) — From the state legislature's view, the amendment represents a coordination problem it solves by refusal: preserving state labor cost advantage. However, this perspective classifies as Rope rather than pure Snare because the mechanism involves genuine coordination of state-level manufacturing interests — textile mills, mining operations, agricultural processors coordinate through state political systems to prevent federal preemption. The extraction is a side effect of coordination, not its primary function. This perspective experiences the constraint as preserving inter-state competitive order.
constraint_indexing:constraint_classification(failed_amendments__child_labor_amendment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: FEDERAL GOVERNMENT PRE-NEW DEAL (PITON) — The amendment process itself becomes a degraded theater: Congress passes the amendment (1924), it gets sent to the states, and there it stalls — formally alive but functionally inert. The constraint persists through the amendment's institutional form (it remains technically pending until 1938) despite the underlying problem being solved through statutory commerce clause authority post-1937. Theater ratio is high because the constraint exists as a constitutional proposal that everyone acknowledges is 'the right answer' while the real mechanism (New Deal Fair Labor Standards Act) operates in parallel. The constraint has become vestigial institutional performance.
constraint_indexing:constraint_classification(failed_amendments__child_labor_amendment, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERSTATE MANUFACTURING COALITION (ROPE) — Textile manufacturers, mining companies, agricultural processors coordinate interstate labor cost suppression through political opposition to the amendment. From their perspective, the constraint is pure coordination: they use the federal ratification mechanism (requiring three-fourths of states) as a coordination solution to maintain labor cost advantages. Exit for them is high (they could accept federal regulation without existential threat), so this classifies as Rope despite the extraction others bear — their experience is coordination benefit.
constraint_indexing:constraint_classification(failed_amendments__child_labor_amendment, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: NEW DEAL COALITION (TANGLED ROPE) — Roosevelt's coalition coordinates labor protection AND economic recovery through the Fair Labor Standards Act (1938), which solves the child labor problem through commerce clause authority rather than constitutional amendment. This perspective sees the constraint as having both coordination function (building interstate agreement on minimum labor standards to prevent destructive wage competition) and extraction (manufacturers must accept higher labor costs). The New Deal eventually moots the amendment by achieving its outcome through statute, making the constitutional route unnecessary. Requires active enforcement through federal inspection.
constraint_indexing:constraint_classification(failed_amendments__child_labor_amendment, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From a civilizational view, the constraint exhibits the structural logic of constitutional deadlock: a clear normative claim (children should not be exploited in labor) requires constitutional authority (federal power to regulate child labor), but the amendment process distributes veto power to the interests it would constrain (states benefiting from child labor wage suppression). The extraction is built into the constitutional structure itself: the victims (working children) have zero power to ratify; the beneficiaries (labor-cost-suppressing manufacturers) have veto power in state legislatures. This is a snare masquerading as a coordination problem (the amendment framing) that gets resolved only when a parallel mechanism (commerce clause) becomes available.
constraint_indexing:constraint_classification(failed_amendments__child_labor_amendment, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(failed_amendments__child_labor_amendment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(failed_amendments__child_labor_amendment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(failed_amendments__child_labor_amendment, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(failed_amendments__child_labor_amendment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(failed_amendments__child_labor_amendment, TR),
    TR >= 0.70.

:- end_tests(failed_amendments__child_labor_amendment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The child labor system extracts from working children through suppressed wages, lost education, and physical harm. The constraint's failure to grant federal regulatory authority perpetuates this extraction. The value reflects that the extraction is substantial but not maximal — the system is not perfectly sealed; some children escape into schooling, some states provide partial protection, and the New Deal eventually creates an exit path. The measurement trajectory (0.78→0.72→0.58) shows declining extractiveness as the FLSA and commerce clause authority create alternative mechanisms, though the decline is modest because suppression remains high even as statutory authority substitutes for constitutional authority. Suppression (0.82): Very high. The structural barriers to ending child labor are severe: state legislatures dominated by manufacturing interests maintain legal permission for child labor; working families face economic necessity that makes schooling alternatives unavailable; political opposition from business interests blocks federal action; constitutional amendment process distributes veto power to the constrained interests. Theater ratio (0.55): Moderate. The amendment itself is not highly theatrical — it is a straightforward constitutional grant of power. However, the constraint as a whole involves theater: the amendment remains formally pending for 14 years despite universal acknowledgment that it is 'the right answer,' while real child labor suppression occurs through statutory FLSA authority. The rising theater ratio (0.35→0.62) reflects this divergence: as the New Deal provides alternative paths, the constitutional amendment becomes increasingly performative, a symbolic commitment divorced from actual mechanism.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a fundamental perspectival inversion. The working child and the reform coalition see a snare (blocked protective power). Manufacturing states see a rope (coordinate labor cost coordination through constitutional veto). The federal government before 1937 sees itself as constrained (lacks authority). The New Deal coalition sees a tangled rope (coordinating labor protection through an alternative mechanism). The piton perspective (post-1938 institutional survival) sees the amendment as a vestigial form that everyone agrees is 'correct' but that nobody needs anymore — institutional theater. The analytical observer sees a snare masquerading as a coordination problem, with veto power distributed to the interests the constraint would constrain. The gap reveals that what appears as a 'neutral' constitutional amendment process is actually a mechanism for distributing veto power, and that distribution is not neutral.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position. Working children have zero power and zero exit options (trapped) — they experience maximum extraction. Manufacturing interests have high power and exit options (they could accept federal regulation without existential threat, hence arbitrage classification) — they experience low or negative extraction. The reform coalition has moderate power (organized institutions can mobilize) but constrained exit (they cannot ratify the amendment unilaterally) — they experience high extraction. The analytical observer is positioned outside the constraint (analytical context) and sees the structure clearly: veto power is distributed to beneficiaries, creating a snare. No directionality overrides are needed; the structural data produces the correct d values through the derivation chain.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by distinguishing extraction (child labor wage suppression) from coordination (interstate labor cost alignment). Manufacturing states genuinely coordinate through their opposition to the amendment — they align their ratification votes to preserve labor cost advantages. But this coordination is asymmetric and extractive: it serves the manufacturers' interests while devastating working children. The constraint is not a pure coordination problem (which would suggest a Rope classification) because the beneficiaries' coordination actively prevents the protective mechanism. This is the defining feature of a Tangled Rope that looks like a Snare: both coordination and extraction are structurally present, but extraction dominates because the coordination is asymmetric. In this case, the manufacturing coalition's perfect coordination on refusal creates the snare for children. The New Deal moots the constraint not by achieving consensus on the amendment, but by creating an alternative mechanism (statutory authority) that bypasses the deadlocked constitutional process. The piton perspective (post-1938) sees the amendment as vestigial theater because both mechanisms (constitutional and statutory) would achieve the same outcome, but only the statutory route ever gets implemented.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    alternative_path_mooting,
    'Did the New Deal''s commerce clause authority genuinely moot the Child Labor Amendment, or did the Fair Labor Standards Act depend on the amendment''s existence as a legitimating precedent?',
    'Doctrinal analysis of FLSA judicial rationale; examination of whether courts cited the proposed amendment as evidence of constitutional intent; counterfactual: would FLSA have passed without the amendment raising child labor as a constitutional question?',
    'If FLSA was independent: the amendment was genuinely redundant and extraction ended through an alternative mechanism. If FLSA depended on amendment legitimacy: the amendment continued to extract through a delegitimizing mechanism even after statutory mooting.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_path_mooting, conceptual, 'Whether the amendment was truly mooted or continued to extract through constitutional legitimation').

omega_variable(
    ratification_counterfactual,
    'Would the amendment have been ratified if the New Deal had not occurred? What was the actual trajectory of state ratifications before and after 1933?',
    'Historical record of state ratification votes; comparison of ratification momentum pre-1933 vs. 1933-1938 vs. post-1938; analysis of whether manufacturing states ever showed movement toward ratification or maintained constant opposition',
    'If trajectory was toward ratification: the extraction was temporary and structural factors were shifting. If trajectory was static/declining: the extraction was structurally locked and only mooted, not resolved, by the New Deal.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ratification_counterfactual, empirical, 'Historical trajectory of state ratification attempts').

omega_variable(
    kernel_reading_ambiguity,
    'Is the Child Labor Amendment a failed attempt to grant federal power (reading: victim is federal authority), or a blocked protection of children (reading: victim is working children), or a stalled constitutional process (reading: victim is constitutional law itself)?',
    'Interpretive: depends on which normative commitment grounds the amendment''s legitimacy. If grounded in child welfare rights, victim is children. If grounded in federalism, victim is federal power. If grounded in constitutional amendment process integrity, victim is the process. These framings are not resolvable through empirical data alone — they are readings of the contested kernel.',
    'Different victim framings change the snare classification: if victim is children, extractiveness is 0.68 (high). If victim is federal authority, extractiveness is 0.54 (moderate). If victim is constitutional process, extractiveness is 0.45 (moderate).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Which normative framing defines the victim set and thus the snare''s severity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(failed_amendments__child_labor_amendment, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cla_theater_1924, failed_amendments__child_labor_amendment, theater_ratio, 0, 0.35).
narrative_ontology:measurement(cla_theater_1931, failed_amendments__child_labor_amendment, theater_ratio, 7, 0.48).
narrative_ontology:measurement(cla_theater_1938, failed_amendments__child_labor_amendment, theater_ratio, 14, 0.62).

% Extraction over time
narrative_ontology:measurement(cla_extract_1924, failed_amendments__child_labor_amendment, base_extractiveness, 0, 0.78).
narrative_ontology:measurement(cla_extract_1931, failed_amendments__child_labor_amendment, base_extractiveness, 7, 0.72).
narrative_ontology:measurement(cla_extract_1938, failed_amendments__child_labor_amendment, base_extractiveness, 14, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(cla_supp_1924, failed_amendments__child_labor_amendment, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(cla_supp_1931, failed_amendments__child_labor_amendment, suppression_requirement, 7, 0.82).
narrative_ontology:measurement(cla_supp_1938, failed_amendments__child_labor_amendment, suppression_requirement, 14, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(failed_amendments__child_labor_amendment, enforcement_mechanism).
narrative_ontology:affects_constraint(failed_amendments__child_labor_amendment, failed_amendments__balanced_budget_amendment).
narrative_ontology:affects_constraint(failed_amendments__child_labor_amendment, failed_amendments__dc_voting_rights_amendment).
narrative_ontology:affects_constraint(failed_amendments__child_labor_amendment, failed_amendments__equal_rights_amendment).
narrative_ontology:affects_constraint(failed_amendments__child_labor_amendment, fair_labor_standards_act_1938).
narrative_ontology:affects_constraint(failed_amendments__child_labor_amendment, commerce_clause_reinterpretation_1937).

% DUAL FORMULATION NOTE:
% The Child Labor Amendment constraint family contains two structurally distinct claims: (1) the constitutional mechanism for suppressing child labor through direct federal grant of power (ε=0.68, Snare in failed state), and (2) the statutory mechanism for suppressing child labor through commerce clause interpretation (ε=0.25, Rope via FLSA). These are not the same constraint viewed from two angles — they have different extractiveness values, different beneficiary/victim structures, and different terminal states. The amendment story (this file) focuses on the constitutional deadlock. The FLSA story (separate file) focuses on the alternative statutory pathway that eventually renders the amendment moot.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
