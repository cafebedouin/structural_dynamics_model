% ============================================================================
% CONSTRAINT STORY: citizenship_clause__birthright_territorial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_citizenship_clause__birthright_territorial_reading, []).

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
 *   constraint_id: citizenship_clause__birthright_territorial_reading
 *   human_readable: Birthright Citizenship: Territorial Reading of the Citizenship Clause
 *   domain: legal/constitutional/immigration
 *
 * SUMMARY:
 *   The 14th Amendment's Citizenship Clause ('All persons born or naturalized
 *   in the United States, and subject to the jurisdiction thereof, are
 *   citizens of the United States and of the State wherein they reside') is a
 *   contested kernel with at least three distinct doctrinal readings. This
 *   constraint instantiates the TERRITORIAL READING: birth on the territory
 *   is the constitutive fact of membership; 'subject to the jurisdiction'
 *   excludes only diplomats and invading armies, and everyone else born here
 *   is born a citizen. This reading suppresses the alternative that children
 *   can inherit their parents' outsider status (hereditary non-membership),
 *   coordinates automatic membership recognition regardless of parental legal
 *   status (benefiting children of undocumented immigrants), and extracts
 *   from the nation-state's discretionary gatekeeping power (the state cannot
 *   condition citizenship on parental loyalty or consent). The constraint
 *   exhibits genuine coordination (automatic mass membership determination)
 *   coupled with asymmetric power loss (the state loses gatekeeping
 *   discretion) — classic tangled rope structure. The suppression_requirement
 *   has increased over time as undocumented immigration numbers have grown,
 *   raising the stakes of the territorial rule's foreclosure of hereditary
 *   outsider status. The theater_ratio is moderate (0.48): the rule-as-stated
 *   is clear and simple ('birth on territory = citizenship'), but enforcement
 *   complexity around establishing jurisdiction and birth location introduces
 *   some performative overhead.
 *
 * KEY AGENTS:
 *   - Children of undocumented immigrants: Primary beneficiaries (powerless/trapped) — receive automatic citizenship status that their parents cannot provide; coordination benefit is maximal because no alternative mechanism exists for these children
 *   - Undocumented immigrant parents: Secondary beneficiaries but also victims (moderate/constrained) — benefit from child citizenship anchor but face vulnerability-extraction through family separation risk and legal status asymmetry
 *   - The nation-state authority: Powerful institutional actor (institutional/constrained) — benefits from administrative simplicity and automatic population incorporation, loses discretionary gatekeeping power and cannot condition membership on assimilation or allegiance
 *   - Alternative hereditary membership frameworks: Suppressed subject (powerful/mobile) — the principle that membership can be inherited or that outsider status can persist across generations is completely foreclosed by the territorial reading
 *   - Allegiance-qualified reading proponents: Competing doctrinal position (institutional/constrained) — their framework requires allegiance or consent and loses foreclosure battle when territorial rule is applied
 *   - Wong Kim Ark settlement proponents: Competing doctrinal position (institutional/constrained) — their framework emphasizes accumulated reliance on 1898 precedent rather than territorial reading's own logical grounds
 *   - Analytical observer: Civilizational perspective (analytical/analytical) — risks treating contested doctrinal choice as self-evident natural law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(citizenship_clause__birthright_territorial_reading, 0.35).
domain_priors:suppression_score(citizenship_clause__birthright_territorial_reading, 0.62).
domain_priors:theater_ratio(citizenship_clause__birthright_territorial_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(citizenship_clause__birthright_territorial_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(citizenship_clause__birthright_territorial_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(citizenship_clause__birthright_territorial_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(citizenship_clause__birthright_territorial_reading, tangled_rope).
narrative_ontology:human_readable(citizenship_clause__birthright_territorial_reading, "Birthright Citizenship: Territorial Reading of the Citizenship Clause").
narrative_ontology:topic_domain(citizenship_clause__birthright_territorial_reading, "legal/constitutional/immigration").

domain_priors:requires_active_enforcement(citizenship_clause__birthright_territorial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(citizenship_clause__birthright_territorial_reading, 'f28a9434-0d19-47c6-b049-e2ef3de646fe').
narrative_ontology:cs_kernel_codification('f28a9434-0d19-47c6-b049-e2ef3de646fe', fixed_text).
narrative_ontology:cs_authority_grounding('f28a9434-0d19-47c6-b049-e2ef3de646fe', lineage).
narrative_ontology:cs_interpretation_layer_present('f28a9434-0d19-47c6-b049-e2ef3de646fe').
narrative_ontology:cs_reading_relation('f28a9434-0d19-47c6-b049-e2ef3de646fe', citizenship_clause__allegiance_qualified_reading, forecloses).
narrative_ontology:cs_reading_relation('f28a9434-0d19-47c6-b049-e2ef3de646fe', citizenship_clause__wong_kim_ark_settlement_reading, influences).
narrative_ontology:cs_axiom('f28a9434-0d19-47c6-b049-e2ef3de646fe', foundational, territorial_birth_sufficient_for_membership).
narrative_ontology:cs_axiom_status(territorial_birth_sufficient_for_membership, holdable).
narrative_ontology:cs_axiom_grounding('f28a9434-0d19-47c6-b049-e2ef3de646fe', territorial_birth_sufficient_for_membership, deontological).
narrative_ontology:cs_axiom('f28a9434-0d19-47c6-b049-e2ef3de646fe', foundational, hereditary_outsider_status_foreclosed).
narrative_ontology:cs_axiom_status(hereditary_outsider_status_foreclosed, holdable).
narrative_ontology:cs_axiom_grounding('f28a9434-0d19-47c6-b049-e2ef3de646fe', hereditary_outsider_status_foreclosed, deontological).
narrative_ontology:cs_reference_frame('f28a9434-0d19-47c6-b049-e2ef3de646fe', territorial_membership_by_birth).
narrative_ontology:cs_drift_state('f28a9434-0d19-47c6-b049-e2ef3de646fe', contemporary_undocumented_immigration_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f28a9434-0d19-47c6-b049-e2ef3de646fe', '').
narrative_ontology:cs_kernel_id(citizenship_clause__birthright_territorial_reading, citizenship_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(citizenship_clause__birthright_territorial_reading, children_of_all_residents).
narrative_ontology:constraint_beneficiary(citizenship_clause__birthright_territorial_reading, undocumented_immigrant_families).
narrative_ontology:constraint_victim(citizenship_clause__birthright_territorial_reading, consent_based_membership_theory).
narrative_ontology:constraint_victim(citizenship_clause__birthright_territorial_reading, hereditary_status_gatekeeping).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNDOCUMENTED CHILD (ROPE) — Experiences the constraint as pure coordination: territorial birth automatically grants citizenship status, eliminating the need for parental consent-negotiation or status-dependent gatekeeping. No extraction — the constraint solves the child's membership problem directly. The child cannot exit the territory during gestation or immediately after birth; the territorial rule's coordination function is to resolve that impossibility by granting membership regardless of parental legal status.
constraint_indexing:constraint_classification(citizenship_clause__birthright_territorial_reading, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: UNDOCUMENTED PARENT (TANGLED ROPE) — Faces constrained exit (deportation risk, legal vulnerability). The constraint provides coordination benefit (child citizenship stabilizes family membership) but also enforces extraction: the parent remains deportable while the child is not, creating asymmetric legal status within the family unit. The parent cannot fully exit the constraint without abandoning the child's citizenship anchor. Genuine coordination (family cohesion) coupled with asymmetric extraction (parental vulnerability).
constraint_indexing:constraint_classification(citizenship_clause__birthright_territorial_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INHERITED OUTSIDER STATUS (SNARE) — The territorial reading's core victim is the principle that membership can be hereditary — that children can inherit their parents' legal outsider status across generations. This principle has no agent to advocate for it but exists as a structural alternative. The territorial rule suppresses it entirely: no child born here inherits outsider status, period. Maximum suppression of the alternative framework.
constraint_indexing:constraint_classification(citizenship_clause__birthright_territorial_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 4: NATION-STATE AUTHORITY (TANGLED ROPE) — The state benefits from territorial birthright (automatic mass naturalization, reduced administrative burden of parentage-investigation, predictable citizen recruitment). But the constraint also enforces extraction: the state loses discretionary gatekeeping power — it cannot condition citizenship on parental loyalty, wealth, or cultural assimilation. The rule coordinates membership recognition with administrative simplicity while extracting the state's power to make membership conditional.
constraint_indexing:constraint_classification(citizenship_clause__birthright_territorial_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: SIBLING READINGS / ALLEGIANCE FRAMEWORK (SNARE) — The allegiance-qualified reading and wong_kim_ark_settlement_reading both represent frameworks where membership can be conditioned. From the territorial reading's perspective, these alternatives are trapped subjects: they lack the exit option of purely territorial determination. Their frameworks face complete suppression in this reading's jurisprudence (Wong Kim Ark is read as confirming territorialism, not qualifying it). They experience the territorial rule as a snare that has foreclosed their core premise.
constraint_indexing:constraint_classification(citizenship_clause__birthright_territorial_reading, snare,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW FRAMING (MOUNTAIN) — From a civilizational/universal perspective, territorial birth is presented as self-evident: 'the rule entire,' simple application of geography. This perspective risks mountain classification — naturalizing the territorial reading as inherent to citizenship itself rather than as one specific normative choice among competing readings. However, the kernel contest reveals this as a false summit: the alternative readings (allegiance, settlement-based) are live jurisprudential positions that contradict the 'inherent and obvious' framing. The engine's false summit detector identifies naturalization of a contested doctrine.
constraint_indexing:constraint_classification(citizenship_clause__birthright_territorial_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(citizenship_clause__birthright_territorial_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(citizenship_clause__birthright_territorial_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(citizenship_clause__birthright_territorial_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(citizenship_clause__birthright_territorial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The territorial reading provides genuine coordination benefit (automatic citizenship for all children born here, eliminating gatekeeping burden), but it also extracts discretionary power from the state and from consent-based membership theory. The state loses the ability to condition membership on assimilation, loyalty, or parental consent — significant loss of authority. However, the extraction is not severe (not snare-level) because the alternative (hereditary outsider status, parental-condition gatekeeping) would itself impose significant costs on the state and on the children excluded. The extractiveness is weighted by the fact that automatic territorial membership solves a genuine coordination problem (how do you determine citizenship for infants who cannot consent and whose parents may have complex legal status). The measurement trajectory (0.25 → 0.35) reflects increasing extractiveness as undocumented immigration numbers have grown, raising the stakes of the gatekeeping power loss and the state's pressure to narrow the rule via alternative interpretations. Suppression (0.62): Moderate-high. The territorial reading achieves this suppression through complete foreclosure: the alternative principle (hereditary outsider status, allegiance-conditioned membership) is not merely disfavored but logically ruled out under this reading. Suppression also includes legal barriers to challenging the rule (constitutional amendment difficulty, stare decisis weight of Wong Kim Ark precedent). Undocumented parents face suppression of their own exit options (deportation risk despite child citizenship). The measurement trajectory (0.45 → 0.62) reflects increasing suppression as immigration enforcement has intensified and as the rule's potential scope (how many children it grants citizenship to) has become more visible and more contested. Theater ratio (0.48): Moderate-low. The rule-as-stated is explicit and clear ('born in the United States and subject to the jurisdiction thereof'). However, enforcement of jurisdiction determination and proof of birth location introduces some performative complexity; the rule is not as simple as bare territoriality but requires establishing U.S. jurisdiction and documented birth.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits the full indexical range from a single base properties set. The undocumented child sees pure coordination (rope) — no extraction because the rule solves their membership by geography alone. The undocumented parent sees tangled rope — genuine coordination benefit (child membership anchor) with extracted vulnerability (family separation risk, legal status asymmetry). The principle of hereditary outsider status sees snare — complete suppression by foreclosure. The nation-state sees tangled rope — administrative benefit and automatic population incorporation extracted alongside loss of gatekeeping discretion. The sibling readings see snare — their frameworks are foreclosed and suppressed by the territorial reading's dominance. The analytical observer risks mountain classification — treating the territorial rule as self-evident ('the rule entire') rather than as a specific normative choice. The kernel contest reveals that all three readings (territorial, allegiance-qualified, settlement-based) remain live doctrinal positions despite the territorial reading's institutional dominance in contemporary U.S. law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for each perspective is computed from the agent's power level, exit options, and structural relationship to the rule. Children of undocumented immigrants are powerless/trapped with no exit option during fetal development and early infancy; they derive maximum benefit from the rule (d ≈ 0.05, nearly full beneficiary, negative or zero extracted value). Undocumented parents are moderate/constrained (high exit cost due to deportation risk); they derive mixed benefit (child citizenship) and mixed harm (family separation risk); d ≈ 0.50–0.60 (partial victim, partial beneficiary). The nation-state is institutional/constrained (high cost to abandon the rule due to precedent and population expectations); it derives benefit (administrative simplicity) and harm (lost gatekeeping); d ≈ 0.45–0.55 (rough balance). The sibling readings are powerful/mobile institutional actors, but relative to the territorial reading's institutional dominance, they are trapped (their framework is foreclosed); d ≈ 0.80–0.90 (nearly full victims).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy through kernel decomposition: the citizenship clause is not a single constraint but a kernel with three readings, each instantiating a different constraint with different ε, beneficiary/victim sets, and classification types. The territorial reading (ε ≈ 0.35, tangled rope) coordinates automatic membership while extracting gatekeeping discretion. The allegiance-qualified reading would have different ε (≈ 0.55–0.65, snare for undocumented children) and different victims (children of foreign-loyal parents). The settlement reading would focus on institutional reliance and precedent weight rather than doctrinal logic. The mandatrophy is resolved by recognizing that the apparent contradiction (is the clause naturalized law or contested doctrine?) stems from conflating multiple structurally distinct readings. No single type is 'correct' — the presheaf of readings over the kernel IS the answer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    territorial_rule_vs_consent_boundary,
    'Does ''subject to the jurisdiction'' exclude children whose parents have never consented to membership, or does it exclude only those with active allegiance elsewhere (diplomats, invaders)?',
    'Historical legislative intent analysis, case law evolution post-Wong Kim Ark, international comparative law examining which states accept territorial birthright and which condition it on parental status',
    'If ''subject to jurisdiction'' means geographic jurisdiction alone: territorial reading holds, extractiveness ≈ 0.35 (coordination with gatekeeping loss). If it means allegiance or consent: allegiance_qualified_reading holds, extractiveness ≈ 0.60 (extraction of membership status by condition-setting).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(territorial_rule_vs_consent_boundary, conceptual, 'Scope of ''subject to the jurisdiction'' clause: geography vs. allegiance/consent').

omega_variable(
    wong_kim_ark_settlement_stability,
    'Did Wong Kim Ark (1898) settle the territorial reading as binding precedent, or did it merely record one moment in ongoing doctrinal contest?',
    'Post-1898 jurisprudence: frequency of courts reaffirming territorial birthright without qualification; presence of ongoing allegiance-based challenges; legislative attempts to narrow birthright eligibility; degree of reliance invested in Wong''s settlement',
    'If settled: wong_kim_ark_settlement_reading holds independently, and the territorial reading is justified by accumulated reliance rather than by its own doctrinal logic. If unsettled: the contest remains live, and the territorial reading stands or falls on its own normative grounds, not on precedent inertia.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wong_kim_ark_settlement_stability, empirical, 'Whether Wong Kim Ark closed the doctrinal dispute or merely captured one moment').

omega_variable(
    undocumented_child_membership_extraction,
    'Is the territorial rule extracting undocumented parents'' vulnerability (family separation risk) in exchange for child citizenship, or is it pure coordination with no extraction?',
    'Empirical analysis of enforcement patterns: are undocumented parents with citizen children deported at different rates than those without citizen children? Do citizen children serve as migration anchors or do they provide no deportation protection? Are family separation policies applied uniformly or do they track parental status despite child citizenship?',
    'If extraction: the tangled_rope classification (genuine coordination benefit coupled with vulnerability exploitation) holds; suppressiveness ≈ 0.62. If pure coordination: the constraint is closer to rope; extractiveness ≈ 0.15; suppressiveness reflects only the legal barriers to undocumented status, not the rule itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(undocumented_child_membership_extraction, empirical, 'Whether territorial birthright extracts from undocumented parents despite coordinating for children').

omega_variable(
    kernel_reading_foreclosure_status,
    'Does the territorial reading logically foreclose the allegiance-qualified reading, or do they represent genuinely coexisting but incompatible jurisprudential positions?',
    'Philosophical analysis of each reading''s foundational premise. If the territorial reading''s axiom ''birth on territory grants membership'' strictly contradicts the allegiance reading''s axiom ''membership requires allegiance or consent,'' then foreclosure holds (no single authority framework can hold both). If both readings can coexist as different parties'' doctrinal commitments, then coexistence holds.',
    'If foreclosure: this reading has eliminated the alternative as logically impossible within constitutional framework. If coexistence: both remain live options in ongoing jurisprudential contest, and the engine computes winner via institutional power rather than logical derivation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_status, conceptual, 'Logical status of the relationship between territorial and allegiance readings').

omega_variable(
    theater_ratio_interpretation,
    'Is the territorial reading''s low theater ratio (0.48) evidence that doctrinal clarity matches actual practice, or evidence that performative simplicity (''born here = citizen'') masks complex gatekeeping enforcement?',
    'Analysis of gap between rule-as-stated and rule-as-enforced: Do citizenship determinations based on territorial birth encounter significant investigative or documentary barriers? Are there de facto conditions (proof of birth location, parental relationship to establish jurisdiction) that complicate the simple rule? Are there enforcement exceptions in practice?',
    'If clarity matches practice: theater_ratio ≈ 0.48 (low performative overhead) is accurate. If complexity is masked: actual theater_ratio is higher (≈ 0.65–0.75); the rule is more piton-like (performative simplicity masking complex enforcement).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_interpretation, empirical, 'Gap between rule-as-stated and rule-as-enforced in birthright citizenship determinations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(citizenship_clause__birthright_territorial_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(citi_be_t0, citizenship_clause__birthright_territorial_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(citi_be_t50, citizenship_clause__birthright_territorial_reading, base_extractiveness, 50, 0.32).
narrative_ontology:measurement(citi_be_t100, citizenship_clause__birthright_territorial_reading, base_extractiveness, 100, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(citi_su_t0, citizenship_clause__birthright_territorial_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(citi_su_t50, citizenship_clause__birthright_territorial_reading, suppression_requirement, 50, 0.58).
narrative_ontology:measurement(citi_su_t100, citizenship_clause__birthright_territorial_reading, suppression_requirement, 100, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(citizenship_clause__birthright_territorial_reading, identity_coordination).
narrative_ontology:affects_constraint(citizenship_clause__birthright_territorial_reading, citizenship_clause__allegiance_qualified_reading).
narrative_ontology:affects_constraint(citizenship_clause__birthright_territorial_reading, citizenship_clause__wong_kim_ark_settlement_reading).
narrative_ontology:affects_constraint(citizenship_clause__birthright_territorial_reading, undocumented_immigration_family_separation).

% DUAL FORMULATION NOTE:
% The citizenship clause kernel has three structurally distinct readings with different extractiveness values and victim sets. The territorial reading (this file) has ε ≈ 0.35 and forecloses hereditary membership and allegiance-conditioning. The allegiance reading has ε ≈ 0.60 and suppresses territorial birth as a sufficient condition. The settlement reading has ε ≈ 0.25–0.35 and emphasizes reliance rather than logical foundations. All three are live doctrinal positions; the territorial reading's institutional dominance does not eliminate the alternatives as logical or normative possibilities.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(citizenship_clause__birthright_territorial_reading, institutional, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
