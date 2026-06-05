% ============================================================================
% CONSTRAINT STORY: originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_originalist_reading, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: originalist_reading
 *   human_readable: Originalist Constitutional Interpretation: Meaning Fixed at Ratification
 *   domain: constitutional_law/legal_theory
 *
 * SUMMARY:
 *   Originalism — the claim that constitutional meaning is fixed at
 *   ratification and legitimate interpretation recovers that original public
 *   meaning — instantiates a constraint that appears as natural law (the text
 *   cannot change) but functions as institutional extraction (the founding
 *   settlement was designed by and for property holders and excluded most of
 *   the population). This reading of the US Constitution kernel locks in the
 *   power distributions, property relations, and suffrage restrictions
 *   established in 1787. The constraint exhibits high suppression through
 *   interpretive foreclosure: rights not enumerable from founding-era
 *   understanding are unavailable regardless of contemporary circumstances.
 *   The tension between originalism as textual methodology and originalism as
 *   institutional practice reveals itself in the gap between stated principle
 *   (meaning is accessible from historical sources) and functional outcome
 *   (different originalist judges reach contradictory conclusions from the
 *   same historical materials). Over the past 50 years, originalism has
 *   accumulated theater through its increasing dominance in judicial rhetoric
 *   despite its declining predictive power — originalist justices apply
 *   originalist methodology but reach outcomes indistinguishable from living
 *   constitutionalists on many rights questions. Meanwhile, the substantive
 *   extractiveness has increased as originalism has been weaponized against
 *   unenumerated rights protections (privacy, bodily autonomy, marriage
 *   choice) while protecting founding-era property structures.
 *
 * KEY AGENTS:
 *   - Status Quo Property Holders and Founding-Era Power Distribution: Primary beneficiary (institutional/arbitrage) — originalism protects property rights, state sovereignty, and suffrage restrictions established in 1787
 *   - Originalist Judiciary (Originalist Scholars, Conservative Justices): Primary beneficiary (institutional/arbitrage) — occupies institutional authority to determine constitutional meaning; benefits from originalism's appearance of constraint-based (rather than discretionary) judging
 *   - Federalist Institutional Structure (Senate, Electoral College, State Governments): Secondary beneficiary (institutional/arbitrage) — originalism protects structural power imbalances designed to favor landed interests and state governments
 *   - Historically Excluded Groups (Women, Enslaved Persons, Non-Property Holders, Their Descendants): Primary victim (powerless/trapped) — face permanent exclusion from founding settlement that originalism locks in; cannot exit through interpretation, only through amendment
 *   - Contemporary Rights-Seekers (Privacy advocates, Marriage equality advocates, Bodily autonomy advocates): Secondary victim (moderate/constrained) — face interpretive barriers to recognition of unenumerated rights; can seek amendment but at high political cost
 *   - Living Constitutionalist Tradition: Institutional counter-agent (institutional/constrained) — nominally rejected but continually applied; represents degraded form of interpretive flexibility that originalism theoretically forecloses
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(originalist_reading, 0.58).
domain_priors:suppression_score(originalist_reading, 0.72).
domain_priors:theater_ratio(originalist_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(originalist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(originalist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(originalist_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(originalist_reading, tangled_rope).
narrative_ontology:human_readable(originalist_reading, "Originalist Constitutional Interpretation: Meaning Fixed at Ratification").
narrative_ontology:topic_domain(originalist_reading, "constitutional_law/legal_theory").

domain_priors:requires_active_enforcement(originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(originalist_reading, fixed_text).
narrative_ontology:cs_authority_grounding(originalist_reading, lineage).
narrative_ontology:cs_interpretation_layer_present(originalist_reading).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(originalist_reading, status_quo_property_holders).
narrative_ontology:constraint_beneficiary(originalist_reading, originalist_judiciary).
narrative_ontology:constraint_beneficiary(originalist_reading, federalist_institutional_structure).
narrative_ontology:constraint_victim(originalist_reading, rights_excluded_at_founding).
narrative_ontology:constraint_victim(originalist_reading, interpretive_flexibility).
narrative_ontology:constraint_victim(originalist_reading, groups_historically_excluded_from_suffrage).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HISTORICALLY EXCLUDED GROUPS (SNARE) — Women, enslaved persons, non-property holders, and their descendants face structural exclusion from the founding settlement that originalism locks in place. The constraint permits no exit: rights not enumerated or implicitly supported by founding public meaning are unavailable regardless of contemporary circumstances. High suppression through interpretive foreclosure.
constraint_indexing:constraint_classification(originalist_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CONTEMPORARY CITIZENS SEEKING UNENUMERATED RIGHTS (TANGLED ROPE) — Individuals and groups advocating for constitutional recognition of privacy, bodily autonomy, marriage equality, or other rights not explicitly enumerated or derivable from 1787 public meaning face significant barriers. Originalism provides genuine coordination for textual stability (beneficial) but extracts through the denial of interpretive expansion (costly). Moderate power with constrained exit — some litigation pathways exist but require amendment or political change.
constraint_indexing:constraint_classification(originalist_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FOUNDING-ERA PROPERTY AND INSTITUTIONAL POWER (ROPE) — Property holders, slaveholders, and federalist institutions (Senate, Electoral College, state governments retaining sovereignty over suffrage) benefit from originalist interpretation's lock-in of founding power distributions. The constraint functions as coordination: interpretive stability protects property rights and institutional prerogatives established in 1787. Net beneficiary — extraction flows toward this agent.
constraint_indexing:constraint_classification(originalist_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSTITUTIONAL AMENDMENT COALITION (SCAFFOLD) — Organized groups (civil rights movements, suffrage advocates, LGBTQ+ rights organizations) can exit originalist constraints through the Article V amendment process, albeit at high political cost and with generational timescales. The amendment pathway provides a sunset mechanism: when political coalitions reach sufficient scale, new rights can be constitutionalized, rendering originalist constraints obsolete. Theater is moderate because the amendment process itself has performative elements (failed amendment attempts), but the underlying mechanism is real. Sunset estimated at generational timescales (10-50 years per major rights wave).
constraint_indexing:constraint_classification(originalist_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: LIVING CONSTITUTIONALISM (PITON) — Despite originalism's theoretical dominance in recent decades, actual constitutional practice has continually evolved through reinterpretation, unenumerated rights discovery (privacy, substantive due process), and implicit constitutional doctrines that diverge from 1787 public meaning. Living constitutionalism operates as degraded but persistent institutional practice — nominally rejected but continuously applied. Theater ratio reflects the gap between stated judicial philosophy (originalism) and actual interpretive outcomes (functional living constitutionalism). The originalist framework's performative power has increased while its functional predictive power has declined.
constraint_indexing:constraint_classification(originalist_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / TEXTUAL IMMUTABILITY (MOUNTAIN) — From a civilizational perspective, the text of a constitutional document is temporally fixed: words written in 1787 cannot change their historical meaning regardless of contemporary interpretation. This is presented as an immutable logical/textual law: if meaning derives from public understanding at ratification, then meaning is locked at ratification. However, this perspective naturalizes what is actually a methodological choice about how to derive meaning from text. The engine's false summit detector will identify this as naturalization of a contestable epistemological claim.
constraint_indexing:constraint_classification(originalist_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(originalist_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(originalist_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(originalist_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(originalist_reading, TR),
    TR >= 0.70.

:- end_tests(originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Originalism extracts from groups seeking rights not enumerable from 1787 public meaning (contemporary LGBTQ+ individuals, women seeking full substantive equality beyond 1868/1920 enumerations, privacy-seekers). The extraction is substantial because the constraint prevents not just one right but whole categories of potential rights from entering the constitution. However, extraction is not maximal (snare-level) because the amendment pathway provides a nominal exit mechanism, even if costly. The rising extractiveness trajectory (0.48 → 0.58) reflects originalism's increasing institutional dominance and expanding application to roll back unenumerated rights doctrines that developed during the living constitutionalist period. Suppression (0.72): High. Multiple suppression mechanisms operate: (1) interpretive foreclosure — non-enumerated rights are unavailable regardless of contemporary need; (2) historical closure — founding-era public meaning is treated as determinative despite methodological disputes about its accessibility; (3) institutional path-blocking — courts reject unenumerated rights claims; (4) amendment barriers — supermajority requirement for constitutional change. Suppression is not absolute (1.0) because the amendment pathway exists, but it approaches maximum for groups without sufficient political power to command amendment majorities. Theater ratio (0.65): Moderate-high and rising. Originalism's stated methodology (recover original public meaning from historical sources) has increasing performance gaps: (a) different originalist scholars diverge on what founding meaning was; (b) originalist judges reach conclusions indistinguishable from living constitutionalists on many rights questions; (c) application-scope drift allows originalist reasoning to expand to contexts absent from founding (digital privacy, internet speech); (d) originalist rhetoric dominates judicial opinions despite its declining predictive power. The theater has increased because originalism now functions primarily as a legitimacy narrative — it *appears* to be a constraint on judicial discretion while actually enabling selective application of historical sources to reach predetermined ideological conclusions.
 *
 * PERSPECTIVAL GAP:
 *   The excluded groups and rights-seekers perceive a snare: permanent foreclosure of interpretive expansion with no functional exit. The beneficiary institutional actors perceive a rope: genuine coordination (interpretive stability protects property rights and state sovereignty). The amendment coalition perceives a scaffold: temporary constraint with a real exit mechanism requiring generational timescales. The living constitutionalist tradition perceives a piton: performative originalist rhetoric layered atop functional interpretive flexibility that persists despite theoretical rejection. The analytical observer risks perceiving a mountain: the text is fixed, meaning is fixed, therefore the constraint is immutable — but this naturalizes what is actually a methodological choice (original public meaning as interpretive anchor) that benefits identifiable institutional actors (status quo property holders, originalist judiciary). The false summit detector will identify that this apparent natural law has clear beneficiaries and victims, suggesting it is a constructed constraint defended through naturalization.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for each perspective is derived from the agent's structural position relative to the constraint. Status quo property holders and originalist judges occupy beneficiary positions (arbitrage exit) — they benefit from interpretive stability and face low cost to remain within the constraint. Their experienced extractiveness (χ) is negative or minimal because the constraint subsidizes them. Historically excluded groups occupy victim positions (trapped exit) — they bear costs of foreclosed rights with no practical exit. Their experienced extractiveness (χ) is maximum because they face suppression with no alternative. Rights-seeking citizens occupy intermediate victim positions (constrained exit) — they face barriers to new rights but can mobilize for amendment. Their experienced extractiveness (χ) is moderate because some escape pathway exists but at high cost. The amendment coalition occupies mobile exit position — organized actors who can mobilize sufficient political power to amend the constitution. Their experienced extractiveness (χ) is lower than individual rights-seekers because coordination capacity provides leverage. The piton perspective (living constitutionalism) occupies an interesting position: nominally rejected by the institutional actors who practice it, facing high suppression from originalist rhetoric while simultaneously functioning as the actual interpretive mechanism. This perspective has institutional power but constrained agency — judges continually apply living constitutionalist logic while claiming to be originalists.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that all classifications are legitimate perspectival readings of the same structural data. The question is not 'is originalism rope or snare?' but 'from what structural position in the constitutional order are you measuring?' From the beneficiary's position (institutional status quo actor), originalism is rope — genuine coordination mechanism protecting property rights and institutional prerogatives. From the excluded group's position (powerless, trapped), originalism is snare — permanent foreclosure of rights with suppression and no exit. From the organized amendment coalition's position, originalism is scaffold — temporary constraint with a real sunset mechanism (amendment). From the living constitutionalist tradition's position, originalism is piton — performatively rejected but functionally persistent, maintained by institutional inertia. The analytical observer risks seeing a mountain (text is immutable, therefore meaning is immutable) — but the false summit signature fires because identifiable institutional actors benefit from this constraint, and the empirical claim (founding meaning is accessible and determinative) is disputed within the originalist scholarly community itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_public_meaning_accessibility,
    'Can founding-era public meaning be reliably recovered from historical sources, and is there a determinate ''original public meaning'' or only contestable interpretations of fragmentary evidence?',
    'Comparative historical analysis: track divergence in originalist scholarly interpretations of the same constitutional clause; measure consensus/dissensus on specific founding-era understandings; correlate interpretive divergence with ideological position of interpreting scholar',
    'If founding meaning is accessible and determinate: originalism functions as a genuine constraint with predictive force. If founding meaning is fragmentary or indeterminate: originalism becomes theater — different scholars derive contradictory meanings from the same historical archive, revealing originalism as an interpretive framework that *appears* to constrain but actually enables discretionary reinterpretation dressed in historical language.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_public_meaning_accessibility, empirical, 'Accessibility and determinacy of founding public meaning').

omega_variable(
    application_scope_drift,
    'Does originalist method reliably prevent expansion of enumerated rights to novel contexts, or does application-scope drift allow founding principles to justify contemporary rights not imaginable in 1787?',
    'Longitudinal doctrinal analysis: identify founding principles applied in originalist opinions to circumstances absent from founding debates (digital privacy, internet commerce, modern surveillance, reproductive technology); measure ratio of cases where originalism blocked expansion vs. cases where originalist reasoning expanded rights to novel contexts',
    'If scope drift is negligible: originalism genuinely constrains rights recognition. If scope drift is substantial: originalism functions like living constitutionalism but with added historical theater, reducing extractiveness because the constraint''s stated purpose (freezing rights) is not achieved by its practice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(application_scope_drift, empirical, 'Whether originalist methodology prevents scope drift to novel applications').

omega_variable(
    kernel_reading_status,
    'This constraint instantiates the originalist reading of the US Constitution kernel. What are the structural differences between this reading (originalist), the living constitutionalist reading, and the textualist reading?',
    'Logical decomposition of the three readings'' structural claims: originalist = founding-era public meaning is binding; living constitutionalist = meaning evolves with contemporary constitutional values; textualist = ordinary current meaning of text governs. Identify which beneficiaries and victims differ across readings, which escape mechanisms each provides, and which institutional actors promote each reading.',
    'Each reading produces different ε, different beneficiary/victim sets, different suppression mechanisms, and different classification landscapes. Originalism locks in founding distributions (high suppression of unenumerated rights). Living constitutionalism enables rights expansion (low suppression of new rights). Textualism splits the difference (moderate suppression). The constitutional meaning kernel is under-determined by the text alone — the three readings are distinct constraints, not interpretations of a single constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_status, conceptual, 'Sibling readings of US Constitution kernel differ structurally').

omega_variable(
    institutional_commitment_vs_methodology,
    'Is originalism an institutional commitment enforced by judicial actors (structure), or a genuine methodological constraint (epistemology)? Can judges reliably be originalist, or does interpretive discretion always persist?',
    'Jurisprudential analysis: identify cases where originalist judges diverged in applying the same originalist methodology; measure success rate of originalist predictions (did originalist analysis correctly forecast outcomes?); compare consistency of originalist reasoning across ideologically similar vs. dissimilar judge pairs',
    'If originalism is a methodology: it constrains outcomes and exhibits low divergence. If originalism is an institutional commitment: it primarily signals loyalty to a coalition (status quo beneficiaries) while preserving interpretive flexibility for results-oriented decisions. If it is neither but rather theater: extractiveness may increase (constraint appears to bind but does not, masking discretionary power) or decrease (constraint becomes piton — performed but not functionally effective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_commitment_vs_methodology, empirical, 'Whether originalism is methodology or institutional commitment').

omega_variable(
    amendment_rate_as_exit_cost,
    'Given that constitutional amendment requires supermajoritarian consensus (two-thirds Congress, three-fourths states), what is the actual cost (in years, political capital, coalition-building burden) for excluded groups to exit originalism via amendment, and how does this cost vary by type of right?',
    'Historical measurement: time-from-initiation-to-ratification for rights-focused amendments (13th, 14th, 15th, 19th, 26th); correlation between amendment latency and demographic size of beneficiary group; comparison of amendment-enabled rights vs. judicially-created rights (privacy, substantive due process) in terms of temporal precedence and stability',
    'If amendment costs are affordable (< 10 years, < 10% political capital): scaffold perspective is valid and exit is real. If amendment costs are prohibitive (> 50 years, > 50% coalition-building): amendment pathway is nominal, not functional, and excluded groups remain trapped (snare classification appropriate). If amendment outcomes are unstable (amended rights later restricted): amendment exit is incomplete.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(amendment_rate_as_exit_cost, empirical, 'Cost and feasibility of constitutional amendment as exit mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(originalist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orig_tr_t0, originalist_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(orig_tr_t25, originalist_reading, theater_ratio, 25, 0.5).
narrative_ontology:measurement(orig_tr_t50, originalist_reading, theater_ratio, 50, 0.65).

% Extraction over time
narrative_ontology:measurement(orig_be_t0, originalist_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(orig_be_t25, originalist_reading, base_extractiveness, 25, 0.55).
narrative_ontology:measurement(orig_be_t50, originalist_reading, base_extractiveness, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(originalist_reading, attachment_coordination).
narrative_ontology:affects_constraint(originalist_reading, living_constitutionalist_reading).
narrative_ontology:affects_constraint(originalist_reading, textualist_reading).
narrative_ontology:affects_constraint(originalist_reading, unenumerated_rights_recognition).
narrative_ontology:affects_constraint(originalist_reading, amendment_process_efficiency).

% DUAL FORMULATION NOTE:
% The US Constitution text is a contested kernel with multiple readings. Originalism is one reading that locks meaning at ratification. Living constitutionalism and textualism are sibling readings with different structural constraints, different ε values, and different beneficiary/victim sets. Each reading should be modeled as a separate constraint in the corpus. Network edges show that each reading affects the others — they are in competition for institutional authority. The kernel decomposition principle (ε-invariance across observables) extends to kernel readings: if two interpretive methods produce different ε, they are analyzing different constraints, not the same constraint from different perspectives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(originalist_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
