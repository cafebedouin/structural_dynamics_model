% ============================================================================
% CONSTRAINT STORY: hammurabi
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hammurabi, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hammurabi
 *   human_readable: The Law of Retaliation (Lex Talionis) in Hammurabi's Code
 *   domain: political/social
 *
 * SUMMARY:
 *   Hammurabi's Code (c. 1754 BCE) represents a foundational constraint on
 *   Mesopotamian legal order: the formalization of reciprocal justice within
 *   a rigidly stratified social hierarchy. The constraint appears to solve a
 *   coordination problem — replacing vendetta cycles and arbitrary punishment
 *   with codified, proportional consequences — but the benefits are captured
 *   asymmetrically by the ruling elite and the priesthood, while the costs
 *   are borne primarily by slaves, commoners, and the economically
 *   vulnerable. The class-stratified penalty structure is the constraint's
 *   defining feature: theft by a slave results in amputation, theft by a
 *   commoner in a fine, theft by a noble in even lesser penalty. This is not
 *   an accident of cultural practice but a deliberate legal architecture that
 *   preserves elite privilege while formalizing lower-class subordination.
 *   The constraint exhibits high suppression (0.75) because subjects have no
 *   legal recourse outside the system and limited exit options; it exhibits
 *   significant extractiveness (0.68) because the system transfers wealth and
 *   bodily security from lower classes to elites under the appearance of
 *   justice. The theater ratio (0.55) reflects that Hammurabi's Code performs
 *   civilization and justice — it is a landmark in legal history, celebrated
 *   as a step toward fairness — while the underlying asymmetry remains
 *   structural.
 *
 * KEY AGENTS:
 *   - Ruling Elite and Priesthood: Primary beneficiary (institutional/arbitrage) — design the system to protect their persons and property while extracting from lower classes; experience codification as coordination
 *   - Lower Classes and Slaves: Primary victims (powerless/trapped) — bear asymmetric penalties and have no exit or alternative recourse
 *   - Victims Seeking Justice: Secondary victims (powerless/trapped) — appear to be protected by the code but receive unequal retaliation depending on perpetrator's class
 *   - Merchants and Skilled Artisans: Secondary actors (powerful/mobile) — benefit from legal predictability but remain vulnerable to class-stratified extraction
 *   - Enforcement Apparatus (Judges, Guards): Institutional actors (institutional/arbitrage) — execute the constraint; derive legitimacy and resources from the system
 *   - Historical Narrative Observer: Analytical observer (analytical/analytical) — risks naturalizing the constraint as inevitable progress in legal evolution
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hammurabi, 0.68).
domain_priors:suppression_score(hammurabi, 0.75).
domain_priors:theater_ratio(hammurabi, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hammurabi, extractiveness, 0.68).
narrative_ontology:constraint_metric(hammurabi, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(hammurabi, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hammurabi, snare).
narrative_ontology:human_readable(hammurabi, "The Law of Retaliation (Lex Talionis) in Hammurabi's Code").
narrative_ontology:topic_domain(hammurabi, "political/social").

domain_priors:requires_active_enforcement(hammurabi).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hammurabi, ruling_elite).
narrative_ontology:constraint_beneficiary(hammurabi, enforcement_apparatus).
narrative_ontology:constraint_victim(hammurabi, lower_classes).
narrative_ontology:constraint_victim(hammurabi, slaves).
narrative_ontology:constraint_victim(hammurabi, commoners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SLAVE OR COMMONER (SNARE) — Cannot exit Hammurabi's jurisdiction; bears asymmetric penalties (loss of hand for theft vs. fine for noble). Suppression is total: no alternative legal recourse, no mobility. d≈0.96, f(d)≈1.42, σ=0.9 → χ≈0.87. Pure extraction.
constraint_indexing:constraint_classification(hammurabi, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: VICTIM SEEKING JUSTICE (SNARE) — Constrained by class: a commoner victim of a noble receives lesser retaliation than the commoner would if roles reversed. System appears to offer justice but withholds it based on perpetrator's class. d≈0.92, f(d)≈1.38, σ=0.9 → χ≈0.83. Extraction disguised as fairness.
constraint_indexing:constraint_classification(hammurabi, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: RULING ELITE AND PRIESTHOOD (ROPE) — Benefits from system that protects their persons and property while enabling controlled extraction from lower classes. Experiences lex talionis as a coordination mechanism: predictable, codified law that stabilizes social order and elite privilege. d≈0.08, f(d)≈-0.10, σ=0.9 → χ≈-0.06. Net beneficiary; negative extraction means subsidy.
constraint_indexing:constraint_classification(hammurabi, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: MERCHANT OR SKILLED ARTISAN (TANGLED ROPE) — Occupies intermediate position: benefits from legal predictability (can transact with confidence) and from protection of property and reputation, but also vulnerable to extraction through asymmetric penalties. Can migrate between city-states (mobile exit), creating a perspectival gap. d≈0.52, f(d)≈0.67, σ=0.9 → χ≈0.41. Mixed coordination and extraction.
constraint_indexing:constraint_classification(hammurabi, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: HISTORICAL OBSERVER / CIVILIZATION NARRATIVE (PITON) — From a civilizational perspective, lex talionis appears as a major advance: moving from vendetta cycles to codified law, from arbitrary punishment to predictable consequences. This narrative naturalizes the constraint as a necessary stage in legal evolution (theater_ratio=0.55). The 'progress' framing masks the continued extraction from lower classes — the system's functional innovation (codification) is performed as civilization advance, but the underlying asymmetry persists.
constraint_indexing:constraint_classification(hammurabi, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Risk: analytical perspective might naturalize lex talionis as an inevitable law of social justice ('proportional punishment is inherent to justice'). However, the structural data (extractiveness=0.68, suppression=0.75, class stratification) contradicts mountain classification. This represents a false summit: the appearance of immutable justice principle masking contingent extraction. The engine's false natural law detector will flag this.
constraint_indexing:constraint_classification(hammurabi, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hammurabi_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hammurabi, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hammurabi, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hammurabi, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hammurabi, TR),
    TR >= 0.70.

:- end_tests(hammurabi_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High but not maximal. The constraint extracts significantly from lower classes through asymmetric penalties and property seizure, but it also provides coordination benefits (reduced unpredictable vendetta cycles) that partially offset pure extraction. The extractiveness increases over the 100-year interval (0.55 → 0.72) as the state apparatus consolidates enforcement capacity and elites learn to exploit the system's loopholes. Suppression (0.75): High. Subjects have no legal recourse outside Hammurabi's code; mobility across jurisdictions is limited; the only path to status change (manumission, military service) is controlled by elites. This is not merely cultural suppression but structural — the code itself eliminates alternatives. Theater ratio (0.55): Moderate. The code is functional — it does reduce vendetta cycles and provides genuine legal predictability — but much of its significance is performative. Hammurabi's stele (where the code is inscribed) serves as a monument to justice and civilization, broadcasting the constraint's legitimacy. Over time (0.40 → 0.55), the theater increases as the code's historical reputation grows, generating a narrative of progress that masks the persistence of underlying asymmetry.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a dramatic perspectival gap between the ruling elite and the lower classes. The elite (institutional/arbitrage) experience lex talionis as a Rope — a coordination mechanism that stabilizes their privilege and enables confident property transactions. The powerless (powerless/trapped) experience it as a Snare — rigid, inescapable extraction with no alternative. The merchant (powerful/mobile) experiences it as Tangled Rope — mixed coordination and extraction, with the option to migrate if the extraction becomes unbearable. The historical observer (analytical/global) risks a false summit by naturalizing it as Mountain — seeing proportional justice as an immutable principle of civilization. The perspectival gap is not driven by disagreement about facts but by structural position: who controls the code, who bears the penalties, and who can exit.
 *
 * DIRECTIONALITY LOGIC:
 *   Lower classes and slaves: Victim + trapped → d≈0.96, f(d)≈1.42. Maximum extraction. No exit, no recourse, bear full asymmetric penalty burden. Victims seeking justice (commoner): Victim + trapped → d≈0.92, f(d)≈1.38. Asymmetric extraction: lesser retaliation when victimized by nobles. Ruling elite and priesthood: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary. Design and control the system; can exit or evade through patronage networks. Merchant or skilled artisan: Both beneficiary (legal predictability) and victim (class-stratified penalties) + mobile → d≈0.52, f(d)≈0.67. Mixed but closer to balanced than powerless agents. Enforcement apparatus: Institutional + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; execute the constraint and derive legitimacy and resources. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Risk of naturalizing constraint; false summit gate applies.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE WITH COORDINATION THEATER: Hammurabi's Code resolves the mandatrophy by exposing how a constraint can appear as pure coordination (moving from vendetta to codified law) while functioning as pure extraction (formalizing elite privilege). The mandatrophy is: 'Is lex talionis a coordination mechanism or extraction?' The resolution requires acknowledging that it is structurally both — it solves the problem (reduce vendetta cycles) that benefits primarily the elite, while imposing the solution (rigid stratification) on those with the least power to negotiate. The perspectival gap is the answer: from the elite's view, it is coordination (and it is, for them); from the lower classes' view, it is extraction (and it is, for them). The constraint's historical reputation as a landmark in justice is not false, but it naturalizes the solution in a way that masks who benefits and who pays. The theater ratio of 0.55 reflects this: the code genuinely functions (coordination), but its function is performed as universal justice (theater) when it is actually stratified extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    class_stratification_function,
    'Does the class-stratified penalty system function primarily as a coordination mechanism (stabilizing social order) or as an extraction mechanism (formalizing elite privilege)?',
    'Historical analysis of elite compliance vs. lower-class violation rates; assessment of whether stratification reduces violence (coordination) or merely transfers it to lower classes (extraction); examination of alternative legal systems from contemporary societies',
    'If coordination-primary: classification shifts toward Rope from elite perspective. If extraction-primary: remains Snare for commoners and lower classes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(class_stratification_function, conceptual, 'Whether class stratification serves coordination or extraction').

omega_variable(
    vendetta_cycle_prevention,
    'Does codified retaliation actually reduce cycles of vendetta and blood feud compared to the pre-codification baseline?',
    'Archaeological and textual evidence of vendetta frequency before and after Hammurabi''s reign; comparison with societies that lacked written law; assessment of whether codification reduced violence or merely regularized it',
    'If reduction is significant: coordination hypothesis strengthened. If vendetta persists unchanged: codification is theater (performance of justice without real violence reduction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vendetta_cycle_prevention, empirical, 'Whether codified retaliation reduces vendetta cycles').

omega_variable(
    exit_availability_slavery,
    'What fraction of the population under Hammurabi''s code could realistically exit the jurisdiction or the social class hierarchy?',
    'Textual analysis of manumission, mobility, and rebellion rates; assessment of whether exit options existed or were illusory; comparison of internal mobility with other ancient Near Eastern societies',
    'If exit is rare (< 5%): suppression gate (≥0.60) is fully satisfied; constraint is Snare. If exit is available (> 20%): suppression drops, classification may shift to Tangled Rope or Scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_availability_slavery, empirical, 'Fraction of population with realistic exit options').

omega_variable(
    mandate_enforcement_capacity,
    'What is the actual enforcement capacity of Hammurabi''s state apparatus — can it monitor and punish violations across all classes with equal rigor, or does enforcement itself embed asymmetry?',
    'Textual analysis of judge selection and accountability; assessment of enforcement mechanisms for crimes by elites vs. commoners; historical examples of elite non-compliance',
    'If enforcement is symmetric: suppression is structural (law itself is the constraint). If enforcement is asymmetric: suppression is behavioral (elites evade punishment), which may lower effective ε in elite perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandate_enforcement_capacity, empirical, 'Enforcement capacity and symmetry across classes').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hammurabi, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ham_tr_t0, hammurabi, theater_ratio, 0, 0.4).
narrative_ontology:measurement(ham_tr_t50, hammurabi, theater_ratio, 50, 0.48).
narrative_ontology:measurement(ham_tr_t100, hammurabi, theater_ratio, 100, 0.55).

% Extraction over time
narrative_ontology:measurement(ham_be_t0, hammurabi, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(ham_be_t50, hammurabi, base_extractiveness, 50, 0.68).
narrative_ontology:measurement(ham_be_t100, hammurabi, base_extractiveness, 100, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hammurabi, enforcement_mechanism).
narrative_ontology:affects_constraint(hammurabi, mesopotamian_class_hierarchy).
narrative_ontology:affects_constraint(hammurabi, ancient_slavery_systems).

% DUAL FORMULATION NOTE:
% Hammurabi's Code can be decomposed into two structurally distinct constraints: (1) the vendetta-reduction mechanism (ε≈0.15, primarily coordination/rope), and (2) the class-stratified penalty system (ε≈0.68, snare). This story focuses on the integrated code as a single constraint, but future analysis should separate these into distinct stories to account for the empirical tension: the code's coordination function is real, but asymmetrically captured by the elite's stratification mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hammurabi, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
