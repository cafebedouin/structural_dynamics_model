% ============================================================================
% CONSTRAINT STORY: governance__theocratic_rule
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_governance__theocratic_rule, []).

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
 *   constraint_id: governance__theocratic_rule
 *   human_readable: Theocratic Rule: Divine Authority and Clerical Interpretation
 *   domain: political/legal
 *
 * SUMMARY:
 *   Theocratic rule establishes political authority as a delegated power
 *   flowing from a divine order, with political obligation framed as a
 *   species of religious obligation. Those who claim interpretive access to
 *   the sacred law (the clerisy) administer this authority. The constraint
 *   exhibits a core ambiguity: to what extent does this system coordinate
 *   genuine collective goods (moral ordering, inheritance of stable
 *   authority, coherent law) versus extract material and spiritual benefits
 *   for the interpreting clerisy while presenting extraction as cosmic
 *   necessity? The suppression is particularly high (0.72) because
 *   disobedience is framed as sin rather than crime—it targets conscience and
 *   soul, not merely behavior. Extractiveness is moderate (0.48) because the
 *   system delivers real coordination benefits (stable succession, moral
 *   order, unified law) alongside extraction. The theater ratio is
 *   moderate-to-high (0.58) because theological elaboration increasingly
 *   becomes performative over time—commentaries multiply to explain away
 *   contradictions, liturgy becomes ornate, and the apparatus of cosmic
 *   justification grows even as institutional power has become the real
 *   mechanism. This reading of the governance kernel differs from autocratic
 *   rule (which grounds authority in the unmediated will of the ruler),
 *   constitutional government (which limits authority through higher law),
 *   customary rule (which grounds authority in immemorial practice), and
 *   direct democracy (which vests authority in the assembled citizens). The
 *   theocratic reading claims a unique source of authority legitimacy:
 *   alignment with transcendent divine order.
 *
 * KEY AGENTS:
 *   - Interpreting Clerisy: Primary beneficiary (institutional/arbitrage) — controls doctrine, legal interpretation, property allocation; benefits from monopoly on sacred knowledge
 *   - Religious Minorities: Primary victims (powerless/trapped) — cannot exit without leaving community; suppression targets conscience through framing of disobedience as sin
 *   - Heterodox Believers: Secondary victims (moderate/constrained) — share religious framework but different interpretation; experience mixed coordination and extraction
 *   - Secular Magistrate: Tertiary victim (powerful/constrained) — exercises temporal authority but legitimacy delegated from clerical establishment; constrained by clerical interpretation
 *   - Religious Subjects (Compliant): Beneficiary-victims (moderate/identity_locked) — benefit from moral order and stable succession; identity fused with religious obligation; cannot perceive exit from within frame
 *   - Analytical Observer: Observational stance (analytical/analytical) — risks naturalizing clerical authority as cosmic necessity rather than institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(governance__theocratic_rule, 0.48).
domain_priors:suppression_score(governance__theocratic_rule, 0.72).
domain_priors:theater_ratio(governance__theocratic_rule, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(governance__theocratic_rule, extractiveness, 0.48).
narrative_ontology:constraint_metric(governance__theocratic_rule, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(governance__theocratic_rule, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(governance__theocratic_rule, tangled_rope).
narrative_ontology:human_readable(governance__theocratic_rule, "Theocratic Rule: Divine Authority and Clerical Interpretation").
narrative_ontology:topic_domain(governance__theocratic_rule, "political/legal").

domain_priors:requires_active_enforcement(governance__theocratic_rule).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(governance__theocratic_rule, 'a0dc9377-ec75-4f05-9e71-3467a29ba6c1').
narrative_ontology:cs_kernel_codification('a0dc9377-ec75-4f05-9e71-3467a29ba6c1', formalized).
narrative_ontology:cs_authority_grounding('a0dc9377-ec75-4f05-9e71-3467a29ba6c1', lineage).
narrative_ontology:cs_interpretation_layer_present('a0dc9377-ec75-4f05-9e71-3467a29ba6c1').
narrative_ontology:cs_reading_relation('a0dc9377-ec75-4f05-9e71-3467a29ba6c1', governance__autocratic_rule, coexists_with).
narrative_ontology:cs_reading_relation('a0dc9377-ec75-4f05-9e71-3467a29ba6c1', governance__constitutional_government, forecloses).
narrative_ontology:cs_reading_relation('a0dc9377-ec75-4f05-9e71-3467a29ba6c1', governance__customary_rule, coexists_with).
narrative_ontology:cs_reading_relation('a0dc9377-ec75-4f05-9e71-3467a29ba6c1', governance__direct_democracy, coexists_with).
narrative_ontology:cs_axiom('a0dc9377-ec75-4f05-9e71-3467a29ba6c1', foundational, axiom_divine_delegation).
narrative_ontology:cs_axiom_status(axiom_divine_delegation, holdable).
narrative_ontology:cs_axiom_grounding('a0dc9377-ec75-4f05-9e71-3467a29ba6c1', axiom_divine_delegation, theological).
narrative_ontology:cs_axiom('a0dc9377-ec75-4f05-9e71-3467a29ba6c1', foundational, axiom_clerical_monopoly_interpretation).
narrative_ontology:cs_axiom_status(axiom_clerical_monopoly_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('a0dc9377-ec75-4f05-9e71-3467a29ba6c1', axiom_clerical_monopoly_interpretation, conventional).
narrative_ontology:cs_reference_frame('a0dc9377-ec75-4f05-9e71-3467a29ba6c1', divinely_ordered_cosmos).
narrative_ontology:cs_drift_state('a0dc9377-ec75-4f05-9e71-3467a29ba6c1', contemporary_secular_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a0dc9377-ec75-4f05-9e71-3467a29ba6c1', '').
narrative_ontology:cs_kernel_id(governance__theocratic_rule, governance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(governance__theocratic_rule, interpreting_clerisy).
narrative_ontology:constraint_victim(governance__theocratic_rule, religious_minorities).
narrative_ontology:constraint_victim(governance__theocratic_rule, heterodox_believers).
narrative_ontology:constraint_victim(governance__theocratic_rule, secular_subjects).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RELIGIOUS MINORITY (SNARE) — Powerless, trapped within the territorial jurisdiction of theocratic rule. Cannot exit without leaving family, property, and identity. Disobedience is framed as sin and heresy, not merely crime — suppression targets the conscience itself. Maximum extraction experienced: compliance is extracted through doctrinal enforcement, property confiscation, social exclusion, and the constant threat of damnation framed as cosmic justice rather than coercive punishment.
constraint_indexing:constraint_classification(governance__theocratic_rule, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: HETERODOX BELIEVER (TANGLED ROPE) — Moderate power, constrained exit. Shares the religious framework but interprets doctrine differently. Experiences both coordination function (the shared religious law provides order and meaning) and extraction (forced conformity to official interpretation, loss of teaching authority, exclusion from communal roles). Exit is costly but thinkable — some heterodox communities survive through retreat to remote regions or internal secrecy. Mixed classification reflects both coordination benefit and asymmetric extraction.
constraint_indexing:constraint_classification(governance__theocratic_rule, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INTERPRETING CLERISY (ROPE) — Primary beneficiary with institutional power and arbitrage options. Experiences the constraint as pure coordination: the divine order requires authorized interpreters to guide the faithful. Their authority derives from access to sacred knowledge, and maintaining that knowledge monopoly is the core coordination function. Extraction runs toward this agent — they control doctrine, legal interpretation, and property allocation. But they frame this as service to the divine order, not personal aggrandizement, and the beneficiary status is asymmetric.
constraint_indexing:constraint_classification(governance__theocratic_rule, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: SECULAR MAGISTRATE (TANGLED ROPE) — Powerful but structurally constrained. Exercises temporal authority but legitimacy is delegated from divine order via the clerical establishment. Benefits from coordination: the religious law provides stable ordering of subjects and inheritance of authority. But constrained by clerical interpretation — cannot unilaterally change law, and contradicting official doctrine risks delegitimation. Mixed experience: coordination function is real (stable hierarchical order) and so is extraction (dependence on clerical blessing, loss of temporal autonomy).
constraint_indexing:constraint_classification(governance__theocratic_rule, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: FORMAL THEOLOGICAL STRUCTURE (PITON) — Over generational timescales, the apparatus of theological justification becomes increasingly performative. Theological commentaries multiply to explain away contradictions in doctrine; elaborate ritual preserves the claim to divine connection even as practical authority has decoupled from doctrine. Theater ratio is moderate-to-high: the visual and linguistic apparatus (liturgy, judicial ceremony, clerical vestment) maintains the cosmic framing while the actual mechanism is institutional power delegation. This is not yet fully degraded (see analytical observer), but the trajectory is visible.
constraint_indexing:constraint_classification(governance__theocratic_rule, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational timescale, the constraint appears as an immutable law of human organization: authority must be grounded in some transcendent source (divine order, natural law, reason itself), and those who claim access to that source will inevitably exercise power. The specificity of theocratic framing is incidental — every authority system performs this naturalization. However, the structural data contradicts the mountain classification. The beneficiary (clerisy) is identifiable; the victims are identifiable; the extractiveness is real and moderate, not negligible. The engine will compute this as a false summit.
constraint_indexing:constraint_classification(governance__theocratic_rule, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(governance__theocratic_rule_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(governance__theocratic_rule, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(governance__theocratic_rule, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(governance__theocratic_rule, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(governance__theocratic_rule, TR),
    TR >= 0.70.

:- end_tests(governance__theocratic_rule_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-to-high. The clerisy extracts material benefits (tithes, property, labor service) and spiritual authority (monopoly on interpretation, control of conscience through sin/damnation framing). However, the system delivers real coordination benefits—stable moral order, coherent law, predictable succession of authority—that legitimate at least some of the extraction. The measurement reflects 48% of potential extractiveness, indicating that approximately half the institutional revenue could be understood as payment for coordination services and half as institutional rent. Measurement trajectory (0.38→0.44→0.48) shows gradual increase as clerical apparatus expands and extraction mechanisms become more elaborate. Suppression (0.72): High. The system suppresses alternatives through multiple mechanisms: literacy control (access to sacred texts restricted), institutional gatekeeping (only clerics can interpret law), social enforcement (heresy is communal shame and exile), and cosmic framing (disobedience is damnation, not merely crime). Importantly, suppression is internalized—subjects come to believe disobedience is objectively sinful, not just institutionally forbidden. This cognitive capture (identity_locked dynamic) makes measured suppression higher than coercive apparatus alone would suggest. Theater ratio (0.58): Moderate-to-high. The apparatus of theocratic legitimacy becomes increasingly performative over time. Theological commentaries multiply to explain contradictions; liturgical elaboration increases; doctrinal rigidity hardens even as practical authority decouples from doctrine. The theater is not as high as a fully degraded piton (0.70+), but the trajectory is toward piton-hood. The measurement trajectory (0.42→0.50→0.58) shows increasing theatricality as the system ages.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces stark perspectival disagreement. The interpreting clerisy sees Rope—pure coordination of divine order, their authority as service to transcendence. Religious minorities see Snare—no exit, maximum extraction, suppression targeting conscience. Heterodox believers see Tangled Rope—mixed coordination (shared moral order) and extraction (forced conformity to official interpretation). Secular magistrates see Tangled Rope—genuine coordination benefits (stable succession, coherent law) but constrained by clerical veto. The formal theological structure, viewed over generational timescales, appears as Piton—increasingly performative apparatus maintaining claim to cosmic authority. The analytical observer at civilizational scale risks seeing Mountain—treating theocratic rule as a natural law of authority (transcendence always requires priests). The false summit detector will flag this last perspective: the apparent immutability of theocratic rule is actually the institutional power of an identifiable beneficiary group (the clerisy) naturalizing their extraction as cosmic necessity. The perspectival gap reveals the constraint's core function: presenting institutional power as cosmic inevitability.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) measures each agent's structural position relative to the extraction flow. The interpreting clerisy (beneficiary + institutional + arbitrage) derives d≈0.05-0.15, producing f(d)≈-0.10-0.02 and negative effective extraction from their perspective—they benefit from the constraint. Religious minorities (victims + powerless + trapped) derive d≈0.95, producing f(d)≈1.40+, experiencing maximum effective extraction. Heterodox believers (victims + moderate + constrained) derive d≈0.70, producing f(d)≈1.10, experiencing moderate-to-high effective extraction. Secular magistrates (ambiguous + powerful + constrained) derive d≈0.50-0.60, producing f(d)≈0.65-0.90, experiencing moderate effective extraction and coordination benefit. The scope modifier σ(S) scales extractiveness by regional (0.9): a 0.48 base extractiveness becomes 0.43 effective extractiveness at regional scope, before directionality multiplication. The suppression metric (0.72) is not scaled—it is a raw structural property of how alternatives are suppressed.
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING RESOLUTION: This constraint instantiates the theocratic_rule reading of the governance kernel. The mandatrophy—the apparent incompatibility between the constraint's classification as Tangled Rope (has real coordination) and its classification as Snare (high extraction and suppression)—resolves by recognizing that theocratic rule genuinely coordinates some collective goods (moral order, stable succession, coherent law) while extracting material and spiritual benefits for the clerisy. The classification as Tangled Rope is not a compromise between two incompatible types; it is the correct classification for a constraint that has both functions. The false summit perspective (the analytical observer seeing Mountain/natural law) is exposed by the structural data: the constraint is not a natural law but an institutional arrangement that benefits identifiable agents (the clerisy) and harms identifiable others (minorities, heretics). The mandatrophy resolves when we accept that theocratic rule is a real coordination mechanism overlaid with real extraction, and stop trying to force it into one pure type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_access_monopoly_justification,
    'Is the clerisy''s claimed monopoly on divine interpretation a genuine epistemic barrier (no layperson can access sacred knowledge) or a social convention enforced by institutional power?',
    'Comparison across theocratic systems: do literacy barriers, initiation requirements, and language control constitute real epistemic barriers or performative gatekeeping? Test cases of layperson theological comprehension, forbidden scripture reading, and institutional response to lay interpretation.',
    'If genuine epistemic barrier: clerisy function is coordination (Mountain-shifted interpretation). If convention: monopoly is extraction mechanism (Snare-shifted interpretation). This omega resolves ambiguity in whether beneficiary status is legitimate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_access_monopoly_justification, empirical, 'Whether clerical monopoly on divine interpretation rests on real epistemic barriers or institutional gatekeeping').

omega_variable(
    suppression_as_cosmic_vs_coercive,
    'To what extent does the theocratic system''s suppression operate through internalized sin/damnation (cognitive frame) versus coercive apparatus (punishment, exile, property seizure)?',
    'Historical analysis of compliance patterns: In periods where clerical enforcement capacity is degraded, does suppression persist through internalized fear of damnation? Do populations continue compliance when physical punishment is absent? Comparative analysis across high-literacy and low-literacy populations within the same system.',
    'If primarily internalized: suppression is identity-locked, measuring closer to 0.50 (identity_coordination type, moderate floor). If primarily coercive: suppression measures 0.72–0.85 (enforcement_mechanism type, higher floor). This affects whether classification remains Tangled Rope or shifts toward Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_as_cosmic_vs_coercive, empirical, 'Mechanism of suppression: internalized cosmic obligation versus coercive apparatus').

omega_variable(
    clerical_extraction_as_offering_or_rent,
    'Are the material benefits the clerisy extracts (tithes, property, labor service) understood by the system itself as offerings freely given to the divine, or as institutional revenue that happens to be framed in religious language?',
    'Textual analysis of clerical justifications: do they center on voluntary devotion (offering frame) or on duty and obligation (rent frame)? Behavioral analysis: what happens when subjects refuse to provide the benefit? If framed as offering, refusal is sin; if framed as obligation, refusal is crime. The frame determines whether extractiveness is legitimated (offering) or exposed (rent).',
    'If offering frame is genuine (subjects internalize the legitimacy): extractiveness measures 0.30–0.40 and Rope classification is more accurate. If offering frame is cover story (institutional rent extraction): extractiveness measures 0.60–0.75 and Snare classification becomes more accurate. Current estimate (0.48) reflects ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(clerical_extraction_as_offering_or_rent, conceptual, 'Whether clerical material benefits are understood as voluntary offerings or as institutional rent extraction').

omega_variable(
    alternative_authority_readiness,
    'Are the sibling readings (constitutional government, customary rule, direct democracy) present as suppressed alternatives within the theocratic system, or are they epistemically unavailable to subjects?',
    'Textual evidence of internal critique: do theological or legal documents acknowledge and refute alternative authority principles? Do layperson petitions invoke customary law or consent frameworks? Is there a class (educated elites, merchants, rural nobility) that is aware of alternatives but chooses the theocratic frame for institutional stability?',
    'If alternatives are present but suppressed: suppression operates on visible options (higher actual suppression, Snare-shifted). If alternatives are genuinely epistemically unavailable: suppression operates through frame closure (identity_locked exit, different classification structure). If aware elites choose theocratic frame for stability: constraint is negotiated fiction (lower extractiveness, Rope-shifted from institutional perspective).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_authority_readiness, empirical, 'Whether alternative authority principles are suppressed options or epistemically unavailable').

omega_variable(
    kernel_reading_contest,
    'This constraint instantiates the theocratic_rule reading of the governance kernel. Sibling readings (autocratic_rule, constitutional_government, customary_rule, direct_democracy) represent alternative authority legitimacy claims. Which reading(s) coexist with this one, which foreclose it, and which are merely different?',
    'For each sibling: analyze whether the reading''s core premise contradicts theocratic_rule''s core premise such that no single framework could hold both (forecloses), or whether different political actors or traditions can hold both simultaneously without logical contradiction (coexists_with), or whether one reading creates structural pressure on the other without logical contradiction (influences). This is a structural analysis, not an empirical one.',
    'Mapping reading relations clarifies which alternatives are live political options, which are ruled out by theocratic commitment, and which are suppressed but viable. The network of relations shapes political possibility — a fully connected graph (all coexist) differs from a partially foreclosing graph (some combinations are impossible).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Structural relationships between theocratic_rule and sibling authority readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(governance__theocratic_rule, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theoc_tr_t0, governance__theocratic_rule, theater_ratio, 0, 0.42).
narrative_ontology:measurement(theoc_tr_t5, governance__theocratic_rule, theater_ratio, 5, 0.5).
narrative_ontology:measurement(theoc_tr_t10, governance__theocratic_rule, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(theoc_be_t0, governance__theocratic_rule, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(theoc_be_t5, governance__theocratic_rule, base_extractiveness, 5, 0.44).
narrative_ontology:measurement(theoc_be_t10, governance__theocratic_rule, base_extractiveness, 10, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(theoc_su_t0, governance__theocratic_rule, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(theoc_su_t5, governance__theocratic_rule, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(theoc_su_t10, governance__theocratic_rule, suppression_requirement, 10, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(governance__theocratic_rule, enforcement_mechanism).
narrative_ontology:affects_constraint(governance__theocratic_rule, governance__autocratic_rule).
narrative_ontology:affects_constraint(governance__theocratic_rule, governance__constitutional_government).
narrative_ontology:affects_constraint(governance__theocratic_rule, governance__customary_rule).
narrative_ontology:affects_constraint(governance__theocratic_rule, governance__direct_democracy).

% DUAL FORMULATION NOTE:
% The governance kernel contains five distinct constraint readings: theocratic_rule, autocratic_rule, constitutional_government, customary_rule, and direct_democracy. Each reading has its own ε, its own beneficiary/victim structure, and its own type distribution across perspectives. They are not variants of a single constraint; they are distinct structural arrangements that compete for legitimacy in different political contexts. All five stories link via affects_constraints, forming a kernel family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
