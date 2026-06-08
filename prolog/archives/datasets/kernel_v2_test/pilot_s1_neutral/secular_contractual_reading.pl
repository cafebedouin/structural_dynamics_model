% ============================================================================
% CONSTRAINT STORY: secular_contractual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secular_contractual_reading, []).

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
 *   constraint_id: secular_contractual_reading
 *   human_readable: Marriage Authority as Secular Contractual Right
 *   domain: constitutional_law/family_law/legal_pluralism
 *
 * SUMMARY:
 *   The secular contractual reading of marriage authority grounds legitimacy
 *   in individual consent formalized as civil contract under state law,
 *   independent of religious identity or community recognition. This reading
 *   emerged during the 18th-century European legal codification and
 *   represents one of five structurally distinct formalizations of marriage
 *   authority found across contemporary legal systems. This constraint story
 *   instantiates only the secular contractual reading, treating marriage as a
 *   voluntary property and custody coordination mechanism governed by civil
 *   statute and enforced by state registry and courts. The reading's core
 *   claim is that marriage authority derives from the consenting parties'
 *   contract, not from religious doctrine, community judgment, or state
 *   paternalism regarding legitimate union. This stands in logical and
 *   structural tension with religious readings (Hindu, Muslim, Christian,
 *   Parsi) that ground marriage authority in religious law, community
 *   authority, or divinely-ordained status. The secular contractual frame
 *   permits gender-neutral rules, inter-faith marriage, and unilateral
 *   divorce; it naturalizes consent as the source of binding force. However,
 *   the constraint exhibits low but measurable extraction (0.25) due to state
 *   fees, formal procedures that exclude non-registered unions, and
 *   gender-asymmetric enforcement despite codified neutrality. Suppression
 *   has declined over the 50-year interval as social norms have normalized
 *   secular marriage and formalized divorce has reduced informal exit
 *   barriers.
 *
 * KEY AGENTS:
 *   - Secular contracting couples (moderate/mobile): Participants who understand marriage as voluntary contract under civil law. Experience the constraint as coordination with low extraction. Benefit from formal property and custody rules.
 *   - Socially-bound couples (moderate/constrained): Participants embedded in religious or community identity structures that treat marriage as binding beyond legal contract. Face informal suppression of exit via social stigma and kinship loss.
 *   - Civil registry authority (institutional/arbitrage): State apparatus that registers marriages, enforces property rules, collects fees. Benefits from administrative clarity; collects state revenue.
 *   - Women and marginalized genders (powerless/trapped, historical): Historically and contemporaneously, this reading's promise of gender-neutral rules conflicts with patriarchal enforcement. Face structural barriers to exit despite legal formalization of divorce.
 *   - Inter-faith couples (moderate/mobile): Communities navigating religious pluralism. Benefit from secular framework that permits cross-identity marriage where religious readings would be incoherent.
 *   - Religious authority holders (institutional/constrained): Clergy, community leaders whose institutional authority over marriage is diminished or eliminated by secular codification. Face structural pressure as their gatekeeping role is displaced by state registration.
 *   - Secular liberal establishment (institutional/constrained): State apparatus and intellectual class maintaining secular codification through law, education, and cultural authority. Maintains the constraint through institutional inertia and ideological commitment.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secular_contractual_reading, 0.25).
domain_priors:suppression_score(secular_contractual_reading, 0.2).
domain_priors:theater_ratio(secular_contractual_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secular_contractual_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(secular_contractual_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(secular_contractual_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secular_contractual_reading, rope).
narrative_ontology:human_readable(secular_contractual_reading, "Marriage Authority as Secular Contractual Right").
narrative_ontology:topic_domain(secular_contractual_reading, "constitutional_law/family_law/legal_pluralism").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secular_contractual_reading, 'bdebd75d-4714-4bd8-978a-7ee091eb5123').
narrative_ontology:cs_kernel_codification('bdebd75d-4714-4bd8-978a-7ee091eb5123', formalized).
narrative_ontology:cs_authority_grounding('bdebd75d-4714-4bd8-978a-7ee091eb5123', extraction).
narrative_ontology:cs_interpretation_layer_present('bdebd75d-4714-4bd8-978a-7ee091eb5123').
narrative_ontology:cs_reading_relation('bdebd75d-4714-4bd8-978a-7ee091eb5123', secular_contractual_reading__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('bdebd75d-4714-4bd8-978a-7ee091eb5123', secular_contractual_reading__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('bdebd75d-4714-4bd8-978a-7ee091eb5123', secular_contractual_reading__christian_colonial_reading, influences).
narrative_ontology:cs_reading_relation('bdebd75d-4714-4bd8-978a-7ee091eb5123', secular_contractual_reading__parsi_community_reading, coexists_with).
narrative_ontology:cs_axiom('bdebd75d-4714-4bd8-978a-7ee091eb5123', foundational, individual_consent_sufficient_for_binding).
narrative_ontology:cs_axiom_status(individual_consent_sufficient_for_binding, holdable).
narrative_ontology:cs_axiom_grounding('bdebd75d-4714-4bd8-978a-7ee091eb5123', individual_consent_sufficient_for_binding, deontological).
narrative_ontology:cs_axiom('bdebd75d-4714-4bd8-978a-7ee091eb5123', foundational, civil_law_authority_over_religious).
narrative_ontology:cs_axiom_status(civil_law_authority_over_religious, holdable).
narrative_ontology:cs_axiom_grounding('bdebd75d-4714-4bd8-978a-7ee091eb5123', civil_law_authority_over_religious, conventional).
narrative_ontology:cs_reference_frame('bdebd75d-4714-4bd8-978a-7ee091eb5123', individual_consent_as_binding_source).
narrative_ontology:cs_drift_state('bdebd75d-4714-4bd8-978a-7ee091eb5123', contemporary_2020, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('bdebd75d-4714-4bd8-978a-7ee091eb5123', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(secular_contractual_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secular_contractual_reading, secular_marriage_participants).
narrative_ontology:constraint_beneficiary(secular_contractual_reading, civil_registry_authority).
narrative_ontology:constraint_beneficiary(secular_contractual_reading, inter_faith_couples).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1 — Secular Contracting Couple (ROPE): Participants experience marriage as a voluntary contractual arrangement under civil law. Entry and exit are governed by formal registration and divorce procedures. Low extraction — the civil system provides genuine coordination (property, custody, tax, inheritance) without asymmetric rent-seeking. Mobile exit exists through formal divorce, though with biographical costs (emotional, social). The constraint appears as pure coordination: what problem does civil marriage solve? Registration of property rights, inheritance, child custody frameworks, tax coordination. Participants are net beneficiaries of the legal clarity.
constraint_indexing:constraint_classification(secular_contractual_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% Perspective 2 — Socially-Bound Couple (TANGLED ROPE): The civil contract coordinates property and custody, but participants also face informal social suppression (family disapproval, community stigma, especially in contexts where religious identity remains the normative frame for marriage legitimacy). Exit from the marriage carries not just legal costs but social costs: ostracism, loss of kinship status, pressure from religious-identity communities. The constraint has a genuine coordination function (civil property and custody coordination) but embedded extraction (social suppression of exit). Suppression is low relative to religious-codified readings but not negligible.
constraint_indexing:constraint_classification(secular_contractual_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective 3 — Civil Registry Authority (ROPE): The state apparatus that registers marriages experiences the constraint as pure coordination. Marriage registration solves the state's administrative problem: tracking lineage, enforcing property rules, allocating tax status. The state benefits from the formal framework but does not extract rent from marrying couples (in jurisdictions without mandatory registration fees that exceed administrative cost). The civil authority's role is infrastructure provision, not extraction. Arbitrage exit exists at the institutional level: the state can modify marriage law, though political constraints make this constrained rather than fully mobile.
constraint_indexing:constraint_classification(secular_contractual_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% Perspective 4 — Women and Marginalized Genders (SNARE, Historical): In the transition from religious to secular codification, this reading's promise is gender-neutral contract law, but the enforcement reality in many jurisdictions lags behind the codified form. Women face structural barriers to exit (economic dependency, child custody bias, informal social pressure) that the civil contract does not fully address. Suppression is embedded in the gap between the civil law's gender-neutral promise and patriarchal enforcement practices. Trapped: exit is theoretically available but practically catastrophic. The constraint's coordination function is real, but extraction (enforcement of gender-asymmetric dependency) persists within the civil frame.
constraint_indexing:constraint_classification(secular_contractual_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% Perspective 5 — Inter-Faith Coalition (ROPE): Communities navigating religious pluralism experience the secular contractual reading as pure coordination that solves a collective-action problem. In multi-faith societies, a civil marriage framework permits inter-faith marriages without requiring religious arbitration or conversion. The constraint coordinates across boundaries that religious codifications cannot. Beneficiary: families and couples whose religious identity mix would be unrecognizable under any single religious reading. Low extraction because the civil frame is genuinely enabling — it solves a real coordination problem (boundary-crossing marriage) without asymmetric rent-seeking.
constraint_indexing:constraint_classification(secular_contractual_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Perspective 6 — Secular Liberal Establishment (PITON): From the institutional vantage of secular liberal governance, this reading maintains the constraint's force through institutional inertia and ideological commitment. The civil marriage framework's original functional mission (securing property rights, regulating inheritance, establishing parental authority) has been substantially supplemented by additional social functions (equality signaling, secular legitimacy assertion, religious boundary-maintenance). The constraint's theater_ratio has risen: increasingly, the civil marriage ceremony is performative assertion of secular legitimacy rather than functional property coordination. The framework persists because institutions and ideology maintain it, not because the coordination problem it solved has grown more pressing.
constraint_indexing:constraint_classification(secular_contractual_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective 7 — Analytical Observer / Natural Law View (MOUNTAIN): From a universal/analytical position, the secular contractual reading claims that marriage as consensual property coordination is an emergent natural outcome of rational actors formalizing voluntary contracts. This perspective sees the framework as inevitable: wherever commerce and property exchange become complex enough, consensual contract formalization necessarily appears. However, this naturalizes a reading choice (secular/contractual) as if it were a structural necessity. The engine's false summit detector will identify this as a false natural law: the secular contractual frame is one among several viable formalizations of marriage authority, not an inevitable emergence.
constraint_indexing:constraint_classification(secular_contractual_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secular_contractual_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(secular_contractual_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(secular_contractual_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(secular_contractual_reading, TR),
    TR >= 0.70.

:- end_tests(secular_contractual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.25): Low and declining. The secular contractual frame genuinely solves a coordination problem — formalizing property rights, custody allocation, inheritance, and tax status. Participants benefit from this clarity. Extraction exists through state registration fees (negligible), procedural requirements that exclude informal unions (minor), and gender-asymmetric enforcement (measurable but declining). The reading does not inherently extract; enforcement asymmetry is a gap between codified form and patriarchal practice, not a feature of the civil frame itself. As gender-neutral enforcement improves (historical trend in the 50-year measurement span), extractiveness declines. Suppression (0.65→0.20): High initially, declining sharply. At codification (t=0, 1791), suppression was high because secular marriage required exit from religious authority — participants faced religious condemnation, kinship loss, and community ostracism. As secular norms normalized (t=25, 1970), informal suppression declined dramatically. Contemporary suppression (t=50, 2015) is low in liberal states because secular marriage is normative, though it remains moderately high in religiously-dominant communities. Theater ratio (0.30→0.35): Low and slightly rising. The civil marriage ceremony historically was primarily functional (property registration, legal recording). Contemporary secular marriage retains functional components but increasingly includes performative elements (status signaling, ideological assertion of secular legitimacy, boundary marker against religious identity). The rise is modest because even contemporary secular marriage is less theatrical than religious or status-based marriage ceremonies.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perspectival divergence primarily on the suppression dimension. The secular contracting couple (rope perspective) experiences low suppression because they operate in a framework where secular marriage is normative. The socially-bound couple (tangled rope perspective) experiences substantial suppression because informal social costs (family disapproval, religious community exclusion) persist beneath the formal civil equality. The historical women perspective (snare) reveals that gender-neutral codification has not produced gender-neutral enforcement — patriarchal suppression persists embedded in enforcement practices. The inter-faith coalition (rope) experiences suppression as minimal because the secular frame uniquely enables their marriage; religious readings would bar them entirely. The analytical mountain perspective risks naturalizing the secular contractual frame as inevitable, when it is actually a contingent institutional choice that emerged under specific conditions (state formation, commercial complexity, religious pluralism). The piton perspective reveals that the constraint's functional mission (property coordination) has been overlaid with performative assertion of secular legitimacy, raising theater_ratio as institutions maintain the frame for ideological reasons beyond coordination necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) measures each agent's structural position as beneficiary (d→0.0) or target (d→1.0) of the constraint. The secular contracting couple are beneficiaries (d~0.1): they gain from property formalization and experience low exit cost; mobile exit options damp directionality downward. The socially-bound couple are near-symmetric (d~0.5): the constraint coordinates property but suppresses exit through informal social barriers; constrained exit options place them in the middle. Women historically are targets (d~0.7): despite gender-neutral codification, enforcement bias against them increases directionality; trapped exit options amplify d. The civil registry is a beneficiary (d~0.1): the constraint's operation enriches state capacity and collects revenue; arbitrage exit options keep d low. The inter-faith coalition are beneficiaries (d~0.05): the constraint uniquely enables their marriages; mobile exit (they can choose civil marriage as preferred option) keeps d very low. Religious authorities are targets (d~0.8): secular codification displaces their gatekeeping role and reduces institutional power; constrained exit (they cannot exit from religious tradition while maintaining religious authority) keeps d high. The secular establishment is a beneficiary (d~0.1): the constraint maintains institutional structures and ideological authority; arbitrage exit options (they could modify the system) keep d low.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exhibits minimal mandatrophy. Its founding mandate — to formalize marriage property coordination through civil contract, enabling inter-faith marriage and gender-neutral rules — remains live and functional. The constraint's coordination function (property registration, custody allocation, inheritance) has not degraded. However, an incipient mandatrophy dynamic exists: the constraint's function has been overlaid with performative assertion of secular legitimacy. Contemporary institutions maintain the secular marriage frame partly for ideological reasons (boundary-marking against religious authority, signaling secular modernity) rather than purely for coordination necessity. This generates the slight rise in theater_ratio over the interval. The mandatrophy is not yet resolved — the constraint retains genuine function — but the wedge between functional and performative purposes is widening. If secular marriage becomes purely theatrical (ceremony without functional property coordination), mandatrophy would resolve and the constraint would reclassify toward piton. The current piton perspective from the secular establishment is predictive, not descriptive: it indicates that the constraint's functional foundation is eroding relative to its performative maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    contractual_vs_status_ontology,
    'Is marriage fundamentally a consensual contract, or a legal status assigned by community/state/religious authority?',
    'Historical analysis of which societies treat divorce as contract dissolution vs. status dissolution requiring external authority approval. Comparison of legal consequences of unilateral exit under each frame.',
    'If marriage is genuinely contractual: the secular reading permits unilateral exit (either party can terminate contract). If marriage is fundamentally a status: unilateral exit is theoretically impossible; divorce requires justification or third-party approval. This frames whether suppression is embedded in the constraint''s logical structure or in contingent enforcement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(contractual_vs_status_ontology, conceptual, 'Contractual vs. status ontology of marriage').

omega_variable(
    religious_authority_coexistence,
    'Can civil marriage authority coexist with religious marriage authority, or does secular codification logically foreclose recognition of religious marriages as state-valid?',
    'Empirical analysis of jurisdictions permitting both civil and religious marriage frameworks with equal state recognition. Examination of cases where civil and religious authorities issue conflicting marriage/divorce rulings.',
    'If coexistence is possible: this reading coexists_with religious readings; both operate in the same legal space without logical foreclosure. If civil framework requires exclusive authority: this reading forecloses religious marriage authority as competing legal source. The relationship between this reading and siblings depends on this resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(religious_authority_coexistence, empirical, 'Whether civil and religious marriage authority can coexist with equal legitimacy').

omega_variable(
    consent_formalization_gap,
    'Does formalizing consent in civil contract actually change the binding power of marriage, or does social reality treat marriage as binding regardless of legal exit procedures?',
    'Longitudinal data on divorce rates, remarriage patterns, and social re-entry after legal divorce in jurisdictions with high vs. low civil marriage enforcement. Comparison of informal vs. formal marriage exit barriers.',
    'If legal formalization changes binding: suppression should decrease when civil divorce is formalized. If binding persists socially regardless: extractiveness and suppression may not track legal codification; theater_ratio rises as the legal form diverges from social reality. This distinguishes whether the civil frame genuinely reduces suppression or merely formalizes what remains socially binding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_formalization_gap, empirical, 'Whether legal formalization of consent changes marriage''s binding force').

omega_variable(
    secular_codification_as_reading_choice,
    'Is the secular contractual reading a discovered natural structure of marriage authority, or a committer choice among viable formalizations?',
    'Comparative legal history: mapping all codifications of marriage authority across time and culture. Identifying whether secular contractual frames appear universally or emerge only under specific institutional conditions (state formation, commerce development, religious pluralism). If emergence correlates with institutional conditions rather than being universal, the reading is choice-dependent.',
    'If discovered/natural: mountain classification holds from analytical position. If choice-dependent: false summit detected; the constraint should reclassify as rope or tangled_rope reflecting the institutional choices that maintain it. This directly tests the mountain claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(secular_codification_as_reading_choice, conceptual, 'Whether secular contractual reading is natural or institutional choice').

omega_variable(
    gender_neutral_codification_vs_enforcement,
    'Does secular civil marriage codification of gender-neutral rules actually enforce gender-neutral outcomes, or does patriarchal social enforcement persist beneath the neutral legal text?',
    'Longitudinal analysis of divorce outcomes, property division, custody assignment, and enforcement compliance across gender-neutral vs. explicitly gendered marriage codes. Measurement of gap between codified rules and enforced outcomes.',
    'If enforcement is gender-neutral: suppression reflects only those costs inherent to formal contractual coordination. If enforcement is patriarchal despite gender-neutral codification: this reading''s key legitimation claim (gender neutrality) is theater; the constraint maintains patriarchal extraction under the cover of secular contractual form. This shapes whether to classify from enforcement-gap perspective as snare or piton rather than rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gender_neutral_codification_vs_enforcement, empirical, 'Whether gender-neutral codification produces gender-neutral enforcement').

omega_variable(
    kernel_reading_contest,
    'This constraint is one of five readings of the marriage authority kernel. Are all five readings equally viable within a single constitutional framework, or does secular codification logically require that other readings be suppressed?',
    'Mapping of which sibling readings are legally recognized alongside this reading in pluralist jurisdictions (India, Lebanon, Malaysia as empirical cases of multi-reading coexistence). Identification of whether non-coexistence is legal requirement or contingent political choice.',
    'If all five can coexist: this reading''s strength does not depend on foreclosing siblings; the constraint is a choice among valid options. If secular codification requires foreclosure of religious readings: this reading is more extractive than the coexistence reading suggests; it is a snare on religious communities rather than a rope for secular ones. This shapes the relationship structure in cs_structure.reading_relations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, empirical, 'Whether all five marriage authority readings can coexist in a single constitutional framework').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secular_contractual_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seccon_theater_t0_1791, secular_contractual_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(seccon_theater_t25_1970, secular_contractual_reading, theater_ratio, 25, 0.32).
narrative_ontology:measurement(seccon_theater_t50_2015, secular_contractual_reading, theater_ratio, 50, 0.35).

% Extraction over time
narrative_ontology:measurement(seccon_extractiveness_t0_1791, secular_contractual_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(seccon_extractiveness_t25_1970, secular_contractual_reading, base_extractiveness, 25, 0.28).
narrative_ontology:measurement(seccon_extractiveness_t50_2015, secular_contractual_reading, base_extractiveness, 50, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(seccon_suppression_t0_1791, secular_contractual_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(seccon_suppression_t25_1970, secular_contractual_reading, suppression_requirement, 25, 0.35).
narrative_ontology:measurement(seccon_suppression_t50_2015, secular_contractual_reading, suppression_requirement, 50, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secular_contractual_reading, resource_allocation).
narrative_ontology:affects_constraint(secular_contractual_reading, hindu_codified_reading).
narrative_ontology:affects_constraint(secular_contractual_reading, muslim_shariat_reading).
narrative_ontology:affects_constraint(secular_contractual_reading, christian_colonial_reading).
narrative_ontology:affects_constraint(secular_contractual_reading, parsi_community_reading).

% DUAL FORMULATION NOTE:
% The marriage authority kernel is instantiated through five structurally distinct readings (constraint stories), each with its own beneficiary/victim structure and extractiveness profile. The secular contractual reading (this story) has low extractiveness (0.25) because civil coordination is genuinely beneficial. Sibling readings have different extractiveness values reflecting their different operational mechanisms: hindu_codified_reading extracts through community authority gatekeeping; muslim_shariat_reading extracts through asymmetric consent requirements; christian_colonial_reading extracts through church institutional control; parsi_community_reading extracts through priesthood authorization requirements. All five affect each other through competitive institutional authority — where secular codification is dominant (liberal states), the sibling readings' extraction mechanisms are disabled or displaced. Where religious readings are dominant (religious-majority states), the secular reading's legitimacy is suppressed. The network captures this mutual affects relationship: the ε-invariance principle requires separate stories because the ε values differ by observable (measuring civil vs. religious extraction produces different extractiveness scores from the same marriage institution). The five readings form a constraint family linked by the shared kernel, not by temporal sequence. They are simultaneously-live alternative formalizations, not historical progression.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(secular_contractual_reading, institutional, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
