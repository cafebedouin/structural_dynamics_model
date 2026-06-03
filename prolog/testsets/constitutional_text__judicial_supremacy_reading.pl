% ============================================================================
% CONSTRAINT STORY: constitutional_text__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text__judicial_supremacy_reading, []).

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
 *   constraint_id: constitutional_text__judicial_supremacy_reading
 *   human_readable: Constitutional Text: Judicial Supremacy Reading
 *   domain: constitutional_theory/political_philosophy
 *
 * SUMMARY:
 *   This constraint story instantiates the judicial supremacy reading of the
 *   constitutional text kernel. The reading claims that the constitutional
 *   text grants courts final, conclusive, and non-overridable interpretive
 *   authority — that judicial invalidation of legislation IS the
 *   determination of constitutional meaning, not merely an authoritative
 *   opinion subject to legislative revision or popular amendment. Under this
 *   reading, courts are gatekeepers: legislatures cannot override judicial
 *   constitutional determinations through ordinary legislative processes; the
 *   people cannot revise constitutional meaning except through the formal
 *   amendment procedure (which is structurally difficult). The constraint
 *   generates six distinct classifications depending on the observer's
 *   structural position. To rights-claimants against majoritarian overreach,
 *   it appears as pure coordination (Rope) — courts enforce minority rights.
 *   To legislatures constrained by prior judicial determinations, it appears
 *   as pure extraction (Snare) — they are trapped. To the judiciary itself,
 *   it appears as mixed (Tangled Rope) — they coordinate review while
 *   extracting authority. To would-be amendment coalitions, it appears as
 *   mixed (Tangled Rope) — they can amend but at extreme cost. At
 *   civilizational timescale, it appears as degraded ritual (Piton) —
 *   judicial supremacy is formally claimed but practically distributed and
 *   routinely revised. The analytical observer risks naturalizing it as
 *   structural necessity (Mountain) — every constitution needs someone to
 *   interpret it — but this risks false-summitry: the other two readings
 *   (legislative sovereignty, popular sovereignty) are equally coherent
 *   logical solutions.
 *
 * KEY AGENTS:
 *   - Rights-Claimants Against Majoritarian Overreach: Primary beneficiary (institutional/arbitrage) — courts protect them from legislative expropriation; they experience the constraint as pure coordination
 *   - Legislative Majorities: Primary victim (powerless/trapped) — constrained by prior judicial determinations they cannot revise; no override mechanisms available
 *   - Judiciary: Secondary beneficiary (institutional/arbitrage) — monopolizes interpretive authority; extracts institutional prestige and power from gatekeeper role
 *   - Amendment-Seeking Coalitions: Secondary victim (moderate/constrained) — can revise through amendment but face supermajority procedural barriers
 *   - Executive Branch: Tertiary actor (institutional/constrained) — must implement judicial determinations but can sometimes non-comply; constrained but not trapped
 *   - Analytical Observer: Civilizational position (analytical/analytical) — risks naturalizing contingent reading as structural necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text__judicial_supremacy_reading, 0.52).
domain_priors:suppression_score(constitutional_text__judicial_supremacy_reading, 0.68).
domain_priors:theater_ratio(constitutional_text__judicial_supremacy_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text__judicial_supremacy_reading, "Constitutional Text: Judicial Supremacy Reading").
narrative_ontology:topic_domain(constitutional_text__judicial_supremacy_reading, "constitutional_theory/political_philosophy").

domain_priors:requires_active_enforcement(constitutional_text__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text__judicial_supremacy_reading, '317b2e7e-5138-4f5a-ac52-dde2f7635e6d').
narrative_ontology:cs_kernel_codification('317b2e7e-5138-4f5a-ac52-dde2f7635e6d', fixed_text).
narrative_ontology:cs_authority_grounding('317b2e7e-5138-4f5a-ac52-dde2f7635e6d', extraction).
narrative_ontology:cs_interpretation_layer_present('317b2e7e-5138-4f5a-ac52-dde2f7635e6d').
narrative_ontology:cs_reading_relation('317b2e7e-5138-4f5a-ac52-dde2f7635e6d', constitutional_text__legislative_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('317b2e7e-5138-4f5a-ac52-dde2f7635e6d', constitutional_text__popular_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('317b2e7e-5138-4f5a-ac52-dde2f7635e6d', foundational, courts_hold_final_interpretive_authority).
narrative_ontology:cs_axiom_status(courts_hold_final_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('317b2e7e-5138-4f5a-ac52-dde2f7635e6d', courts_hold_final_interpretive_authority, conventional).
narrative_ontology:cs_axiom('317b2e7e-5138-4f5a-ac52-dde2f7635e6d', foundational, legislative_override_is_illegitimate).
narrative_ontology:cs_axiom_status(legislative_override_is_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('317b2e7e-5138-4f5a-ac52-dde2f7635e6d', legislative_override_is_illegitimate, deontological).
narrative_ontology:cs_reference_frame('317b2e7e-5138-4f5a-ac52-dde2f7635e6d', judicial_supremacy_constitutional_order).
narrative_ontology:cs_drift_state('317b2e7e-5138-4f5a-ac52-dde2f7635e6d', contemporary_distributed_meaning_making, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('317b2e7e-5138-4f5a-ac52-dde2f7635e6d', '2026-02-27T14:32:00Z').
narrative_ontology:cs_kernel_id(constitutional_text__judicial_supremacy_reading, constitutional_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text__judicial_supremacy_reading, rights_claimants_against_majoritarian_overreach).
narrative_ontology:constraint_beneficiary(constitutional_text__judicial_supremacy_reading, judicial_institutional_authority).
narrative_ontology:constraint_victim(constitutional_text__judicial_supremacy_reading, democratic_responsiveness).
narrative_ontology:constraint_victim(constitutional_text__judicial_supremacy_reading, legislative_will_expression).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LEGISLATIVE MAJORITY (SNARE) — A legislative majority cannot revise the constitutional text's meaning once courts have settled it through judicial review. No legislative override mechanism; no ability to invoke notwithstanding clauses or constitutional amendment without supermajority requirements. The constraint extracts from legislative responsiveness — the majoritarian will is trapped by prior judicial determinations that cannot be reversed by ordinary legislative action.
constraint_indexing:constraint_classification(constitutional_text__judicial_supremacy_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RIGHTS-CLAIMANTS (ROPE) — Individuals and minorities claiming constitutional rights experience the judicial supremacy constraint as pure coordination: courts enforce the text against democratic majorities. Judicial review protects them from legislative expropriation. They benefit from the gatekeeper role; courts solve the coordination problem of preventing majority tyranny. This perspective perceives high coordination function, minimal extraction.
constraint_indexing:constraint_classification(constitutional_text__judicial_supremacy_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: JUDICIARY (TANGLED ROPE) — Courts benefit from exclusive interpretive authority and the institutional prestige that flows from gatekeeping constitutional meaning. But courts are also constrained by the text itself — they cannot simply legislate. The constraint coordinates the function of constitutional review (genuine coordination) while extracting authority from other branches. Enforcement requires active maintenance of the supremacy doctrine. Courts experience both coordination (reviewing legislation against constitutional text) and asymmetric extraction (excluding legislatures from final say).
constraint_indexing:constraint_classification(constitutional_text__judicial_supremacy_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: AMENDMENT-SEEKING COALITION (TANGLED ROPE) — Groups that wish to revise the constitutional meaning face supermajority requirements (supermajority in legislatures plus ratification). The judicial supremacy constraint coordinates amendment (there is a formal procedure) while extracting from amendment coalitions through high procedural barriers. Constrained exit: they can amend, but at extreme cost. Asymmetric: the constraint makes amendment harder relative to judicial revision of judicial doctrine (which requires no supermajority).
constraint_indexing:constraint_classification(constitutional_text__judicial_supremacy_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CIVILIZATIONAL OBSERVER (PITON) — At civilizational timescale, the judicial supremacy doctrine is increasingly performative. Courts claim final authority over constitutional meaning, but their determinations are routinely revised through constitutional amendment, executive non-compliance, legislative reinterpretation, or social movement pressure. The ritual persists (courts continue to claim supremacy; legislatures continue to defer formally) despite the practice showing distributed and contested authority. Theater ratio (0.58) reflects that the supremacy claim is maintained through doctrine while actual constitutional meaning-making is distributed across branches and social forces.
constraint_indexing:constraint_classification(constitutional_text__judicial_supremacy_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a logical/structural perspective, some agent or institution must have final interpretive authority over a written constitutional text — either courts, legislatures, or the demos itself. The necessity of interpretive authority appears immutable: a text without an authoritative reader is inert. This perspective naturalizes the constraint by framing judicial supremacy as the only coherent solution to the logical requirement that SOMEONE must settle constitutional meaning. However, the false summit detector will identify this as a reading-dependent naturalization: the legislative sovereignty and popular sovereignty readings are equally coherent logical solutions.
constraint_indexing:constraint_classification(constitutional_text__judicial_supremacy_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text__judicial_supremacy_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(constitutional_text__judicial_supremacy_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(constitutional_text__judicial_supremacy_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_text__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(constitutional_text__judicial_supremacy_reading, TR),
    TR >= 0.70.

:- end_tests(constitutional_text__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The judicial supremacy reading extracts from legislative responsiveness (legislatures cannot override through ordinary process) and from amendment coalitions (high barriers to formal revision). The extraction is not maximal because courts are themselves constrained by the text and because the demos retains amendment power (albeit at high cost). The measurement trajectory (0.38 → 0.52 over 100 time units) reflects accumulating extraction: as courts issue more determinations, the stock of legislative constraints grows, and the cost to amendment coalitions compounds. Suppression (0.68): High. The suppression mechanisms include (a) procedural barriers to amendment (supermajority in legislatures, state ratification), (b) the doctrine that courts have final say (preventing legislative override), (c) the norm that non-compliance with judicial determinations is illegitimate. These are substantial barriers; alternatives are suppressed (legislative meaning-making is treated as illegitimate; popular amendment is procedurally expensive). Theater ratio (0.58): Moderate. The constraint is partly functional (courts do review legislation against text) and partly performative (the supremacy claim is routinely revised through amendment, executive non-compliance, and doctrinal shift). The measurement trajectory (0.42 → 0.58) reflects increasing performativity: as the gap between the formal supremacy doctrine and the distributed practice of meaning-making widens, the theater component rises.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits the full perspectival range. Rights-claimants see coordination (Rope) because courts solve their problem. Legislatures see extraction (Snare) because they are trapped. Courts see mixed (Tangled Rope) because they coordinate and extract simultaneously. Amendment coalitions see mixed (Tangled Rope) because they can amend but at extreme cost. The civilizational observer sees degraded ritual (Piton) because the supremacy doctrine persists despite being routinely revised. The analytical observer risks seeing natural law (Mountain) by naturalizing the contingent claim that someone must be supreme. The perspectival gaps reveal that the judicial supremacy reading is not a logical necessity but a contingent institutional arrangement that benefits some actors (courts, rights-claimants) at the expense of others (legislatures, amendment coalitions). The competing readings (legislative sovereignty, popular sovereignty) are equally coherent framings that would produce different beneficiary/victim relationships and different perspectival gaps.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from whether the agent benefits from or bears the cost of the constraint. Rights-claimants benefit (low d, negative χ from their perspective because courts reduce their barriers). Legislatures bear the cost (high d, high χ from their perspective because courts trap them). Courts benefit (low d from their perspective). Amendment coalitions bear costs (high d, moderate χ because they have exit but at extreme cost). The judicial supremacy reading constructs the directionality relationships by deciding who has interpretive authority: courts get it (low d), legislatures lose it (high d), the people retain only the amendment option (constrained d). A different reading (legislative sovereignty) would reverse the directionality relationships: legislatures would have low d (beneficiaries), courts would have high d (constrained), the people would have moderate d (constrained by legislatures but not courts). The directionality relationships are not independent of the reading — they are constructed by which reading of the constitutional text is adopted.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that the tangled_rope classification captures the actual institutional dynamic: courts coordinate constitutional review (legitimate function) while extracting authority from legislatures and the people (asymmetric power allocation). The snare classification (from the legislative perspective) captures real structural entrapment: legislatures cannot exit. The rope classification (from rights-claimant perspective) captures real coordination benefit. The piton classification (from civilizational perspective) captures the degradation of the doctrine over time as its actual practice diverges from its formal supremacy claim. The false summit (mountain classification from analytical perspective) is a diagnostic alert: the claim that judicial supremacy is a logical necessity is itself a reading-dependent construction, not a structural feature independent of which reading is adopted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_override_mechanisms,
    'Can courts'' own prior determinations be effectively overridden through non-judicial mechanisms (constitutional amendment, executive non-compliance, legislative reinterpretation, social movement pressure), and if so, does this constitute a genuine supremacy or merely a formal claim?',
    'Historical analysis: cases where judicial determinations were reversed through amendment (14th Amendment overriding Dred Scott, 16th Amendment overriding Pollock, 26th Amendment overriding Oregon v. Mitchell). Cases where courts changed doctrine through internal revision (Lochner era overruling, substantive due process revival). Cases where executive non-compliance or legislative circumvention de facto reversed judicial meaning without formal override.',
    'If overrides are frequent and effective: supremacy is formal doctrine without structural reality (piton classification confirmed). If overrides are rare and difficult: supremacy is structurally real (snare/tangled_rope from legislative perspective). The ε value may decline from 0.52 to ~0.35 (rope-level governance with distributed review) if supremacy proves performative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(judicial_override_mechanisms, empirical, 'Whether judicial supremacy is substantively maintained or formally claimed while practically distributed').

omega_variable(
    reading_dependent_authority_structure,
    'Is the necessity of some interpretive authority independent of the reading, or does each reading construct its own legitimate authority structure from the same constitutional text?',
    'Comparative constitutional analysis: examine how identical constitutional texts (e.g., identical language in Westminster constitutions) support different authority structures in different jurisdictions. Analyze whether the ''necessity'' of interpretive authority derives from logic or from the reading''s own premises.',
    'If independent necessity: judicial supremacy is one valid answer to a shared structural problem; mountain perspective partly justified. If reading-dependent: no agent has logical necessity to be supreme; all three readings are equally coherent framings of the same text; the ''natural'' authority structure is constructed by each reading. Implication: false summit confirmed; ε remains tangled_rope throughout.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_dependent_authority_structure, conceptual, 'Whether interpretive authority necessity is independent of the reading or reading-constructed').

omega_variable(
    suppression_mechanism_structural_vs_doctrinal,
    'Is the measured suppression (0.68) a structural feature of judicial review itself, or a contingent feature of how this reading enforces supremacy doctrine through doctrine-maintenance rather than through institutional design?',
    'Comparative institutional analysis: contrast judicial review systems where override is procedurally possible (Canada''s notwithstanding clause, UK parliamentary sovereignty, Australia''s legislative amendment) with pure judicial supremacy systems. Measure legislative amendment frequency, executive non-compliance rates, and public opinion shifts toward constitutional revision.',
    'If structural: suppression is inherent to any system with centralized review authority; legislative supremacy and popular sovereignty readings would show comparable suppression. If doctrinal: the judicial supremacy reading uniquely enforces high suppression through denying override mechanisms; sibling readings show lower suppression (0.35-0.45) through allowing legislative or popular override. ε may reflect reading-specific enforcement rather than inherent feature.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_doctrinal, empirical, 'Whether suppression is structural or reading-specific enforcement').

omega_variable(
    contest_kernel_reading_asymmetry,
    'Is the judicial supremacy reading equally defensible as the legislative sovereignty and popular sovereignty readings, or does the constitutional text provide asymmetric support for judicial gatekeeping?',
    'Textual analysis of the specific constitutional text (varies by jurisdiction). Historical record of constitutional framers'' intent. Comparative analysis of how similar texts are interpreted in different constitutional traditions.',
    'If asymmetric support (text favors judicial reading): this reading is privileged; the others are contestations. ε value and theater_ratio may reflect doctrinal advantage. If symmetric (text supports multiple readings equally): all three readings are equally valid; the judicial reading''s apparent dominance is a contingent feature of institutional power, not textual authority; false summit confirmed and piton classification supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contest_kernel_reading_asymmetry, empirical, 'Whether constitutional text asymmetrically supports judicial supremacy reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text__judicial_supremacy_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(judsup_tr_t0, constitutional_text__judicial_supremacy_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(judsup_tr_t50, constitutional_text__judicial_supremacy_reading, theater_ratio, 50, 0.52).
narrative_ontology:measurement(judsup_tr_t100, constitutional_text__judicial_supremacy_reading, theater_ratio, 100, 0.58).

% Extraction over time
narrative_ontology:measurement(judsup_be_t0, constitutional_text__judicial_supremacy_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(judsup_be_t50, constitutional_text__judicial_supremacy_reading, base_extractiveness, 50, 0.48).
narrative_ontology:measurement(judsup_be_t100, constitutional_text__judicial_supremacy_reading, base_extractiveness, 100, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_text__judicial_supremacy_reading, constitutional_text__legislative_sovereignty_reading).
narrative_ontology:affects_constraint(constitutional_text__judicial_supremacy_reading, constitutional_text__popular_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is a reading of the contested kernel 'constitutional_text'. All three reading-specific constraints (judicial_supremacy_reading, legislative_sovereignty_reading, popular_sovereignty_reading) are members of a kernel family. They share the same underlying text but construct different authority structures from it. Network links indicate that these readings compete for legitimacy and that adoption of one reading affects the structural conditions for the others. The ε values differ across readings (judicial supremacy: 0.52; legislative sovereignty: 0.58; popular sovereignty: 0.38 [estimate]) because each reading instantiates different extraction mechanisms and different beneficiary/victim relationships from the same constitutional text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
