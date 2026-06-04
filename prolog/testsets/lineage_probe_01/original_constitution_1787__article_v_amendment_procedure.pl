% ============================================================================
% CONSTRAINT STORY: original_constitution_1787__article_v_amendment_procedure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_original_constitution_1787__article_v_amendment_procedure, []).

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
 *   constraint_id: original_constitution_1787__article_v_amendment_procedure
 *   human_readable: Article V Amendment Procedure: Constitutional Entrenchment via Double Supermajority
 *   domain: political/legal/constitutional_design
 *
 * SUMMARY:
 *   Article V of the 1787 Constitution establishes the formal procedure for
 *   constitutional amendment: a proposal requires either 2/3 of both chambers
 *   of Congress or a convention called by 2/3 of state legislatures;
 *   ratification requires approval by 3/4 of the states. This supermajority
 *   threshold creates a structural asymmetry: constitutional change is made
 *   vastly more difficult than ordinary legislation (simple majority), and
 *   the state-based voting structure means that just 13 states (representing
 *   a small fraction of the population) can block any amendment. The
 *   constraint exhibits a paradox embedded in its design: the 1787 text
 *   declares the Constitution 'alterable in principle' while entrenching it
 *   in practice behind barriers that compound over time. As the Constitution
 *   ages and amendment becomes rarer, the entrenchment's suppressive force
 *   increases — what began as a coordination mechanism ensuring stability
 *   becomes an extraction mechanism protecting the settled order from
 *   democratic revision. This reading focuses on Article V as the deepest
 *   commitment of the 1787 text, instantiating the amendment procedure
 *   (rather than federal supremacy, separation of powers, or slavery
 *   compromises) as the core constitutional kernel.
 *
 * KEY AGENTS:
 *   - Settled Constitutional Order: Institutional beneficiary (institutional/arbitrage) — federal structure, state sovereignty, property protections, and separation of powers as encoded benefit from supermajority protection
 *   - Reform Coalitions: Organized victim (organized/constrained) — civil rights movements, labor organizations, and progressive coalitions experience supermajority barrier as suppression of lawful constitutional change
 *   - Excluded Supermajority: Powerless victim (powerless/trapped) — coalitions commanding 60-65% of states/population cannot achieve formal amendment; trapped within constitutional bounds
 *   - Structural Minorities: Quasi-beneficiary (institutional/constrained) — states benefiting from constitutional provisions (Senate structure, electoral college) experience Article V as protective, though constrained by their own veto power
 *   - Constitutional Jurisprudence: Institutional degradation (institutional/arbitrage) — Supreme Court reinterpretation functions as de facto amendment while maintaining fidelity to fixed text; piton mechanism
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent historical choice (75% threshold, state-based voting) as inherent to written constitutionalism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(original_constitution_1787__article_v_amendment_procedure, 0.58).
domain_priors:suppression_score(original_constitution_1787__article_v_amendment_procedure, 0.72).
domain_priors:theater_ratio(original_constitution_1787__article_v_amendment_procedure, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(original_constitution_1787__article_v_amendment_procedure, extractiveness, 0.58).
narrative_ontology:constraint_metric(original_constitution_1787__article_v_amendment_procedure, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(original_constitution_1787__article_v_amendment_procedure, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(original_constitution_1787__article_v_amendment_procedure, tangled_rope).
narrative_ontology:human_readable(original_constitution_1787__article_v_amendment_procedure, "Article V Amendment Procedure: Constitutional Entrenchment via Double Supermajority").
narrative_ontology:topic_domain(original_constitution_1787__article_v_amendment_procedure, "political/legal/constitutional_design").

domain_priors:requires_active_enforcement(original_constitution_1787__article_v_amendment_procedure).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(original_constitution_1787__article_v_amendment_procedure, 'f06a7e61-adb2-4ef4-8e1b-29ee54d87dde').
narrative_ontology:cs_kernel_codification('f06a7e61-adb2-4ef4-8e1b-29ee54d87dde', fixed_text).
narrative_ontology:cs_authority_grounding('f06a7e61-adb2-4ef4-8e1b-29ee54d87dde', lineage).
narrative_ontology:cs_interpretation_layer_present('f06a7e61-adb2-4ef4-8e1b-29ee54d87dde').
narrative_ontology:cs_reading_relation('f06a7e61-adb2-4ef4-8e1b-29ee54d87dde', original_constitution_1787__federal_supremacy_design, influences).
narrative_ontology:cs_reading_relation('f06a7e61-adb2-4ef4-8e1b-29ee54d87dde', original_constitution_1787__separation_of_powers_design, influences).
narrative_ontology:cs_reading_relation('f06a7e61-adb2-4ef4-8e1b-29ee54d87dde', original_constitution_1787__slavery_compromises, influences).
narrative_ontology:cs_axiom('f06a7e61-adb2-4ef4-8e1b-29ee54d87dde', foundational, constitutional_alteration_requires_supermajority).
narrative_ontology:cs_axiom_status(constitutional_alteration_requires_supermajority, holdable).
narrative_ontology:cs_axiom_grounding('f06a7e61-adb2-4ef4-8e1b-29ee54d87dde', constitutional_alteration_requires_supermajority, deontological).
narrative_ontology:cs_axiom('f06a7e61-adb2-4ef4-8e1b-29ee54d87dde', foundational, state_based_veto_protects_federalism).
narrative_ontology:cs_axiom_status(state_based_veto_protects_federalism, holdable).
narrative_ontology:cs_axiom_grounding('f06a7e61-adb2-4ef4-8e1b-29ee54d87dde', state_based_veto_protects_federalism, instrumental).
narrative_ontology:cs_reference_frame('f06a7e61-adb2-4ef4-8e1b-29ee54d87dde', alterable_constitution_principle).
narrative_ontology:cs_drift_state('f06a7e61-adb2-4ef4-8e1b-29ee54d87dde', contemporary_ossification_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f06a7e61-adb2-4ef4-8e1b-29ee54d87dde', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(original_constitution_1787__article_v_amendment_procedure, original_constitution_1787).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(original_constitution_1787__article_v_amendment_procedure, settled_constitutional_order).
narrative_ontology:constraint_beneficiary(original_constitution_1787__article_v_amendment_procedure, status_quo_interests).
narrative_ontology:constraint_victim(original_constitution_1787__article_v_amendment_procedure, supermajority_coalitions_below_threshold).
narrative_ontology:constraint_victim(original_constitution_1787__article_v_amendment_procedure, emergent_reform_movements).
narrative_ontology:constraint_victim(original_constitution_1787__article_v_amendment_procedure, constitutional_evolution).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED SUPERMAJORITY (SNARE) — A coalition representing 60-65% of citizens and states cannot amend the Constitution despite commanding durable democratic support. Trapped: exit from the constraint would require either extraconstitutional change (revolution) or persuading additional states to cross the 75% threshold. Maximum experienced extraction — the constraint prevents lawful constitutional evolution even with supermajority democratic backing. No exit within constitutional bounds.
constraint_indexing:constraint_classification(original_constitution_1787__article_v_amendment_procedure, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REFORM COALITIONS (TANGLED ROPE) — Labor movements, civil rights organizations, and progressivist coalitions experience the constraint as a hybrid: genuine coordination function (supermajority requirement does ensure constitutional stability and prevents tyranny of simple majorities) combined with asymmetric extraction (their policy goals cannot reach constitutional status even with supermajority support). Constrained exit: they can pursue legislative victories within the existing constitutional frame, but transformative constitutional reform is blocked. They also benefit from the stabilizing coordination function when they are not seeking change.
constraint_indexing:constraint_classification(original_constitution_1787__article_v_amendment_procedure, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SETTLED CONSTITUTIONAL ORDER (ROPE) — Federal structure, separation of powers, property protections, and state sovereignty as encoded in the 1787 text experience Article V as pure coordination: it locks in a stable framework, prevents destabilizing revision, and enables long-term governance. The institutional actor (the constitutional system itself) sees the constraint as beneficial coordination. Arbitrage: the system can leverage the entrenchment to resist challenges without negotiating fundamental revision.
constraint_indexing:constraint_classification(original_constitution_1787__article_v_amendment_procedure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: STRUCTURAL MINORITIES / STATE GOVERNMENTS (TANGLED ROPE) — States that benefit from constitutional provisions protecting state sovereignty (the Senate structure, electoral college, treaty ratification) experience Article V as mixed: it protects their constitutional power (coordination function) while simultaneously locking out majorities that might curtail state power. Constrained exit: they could support amendment, but the supermajority requirement gives them veto power they benefit from exercising. They are both beneficiaries and quasi-victims depending on which constitutional provision is under pressure.
constraint_indexing:constraint_classification(original_constitution_1787__article_v_amendment_procedure, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSTITUTIONAL JURISPRUDENCE (PITON) — The judiciary's ongoing reinterpretation of the 1787 text via constitutional law effectively amends the Constitution without formal amendment. This process is largely performative — judges claim they are discovering original meaning while actually remapping it. The Supreme Court's jurisprudential layer is a degraded substitute for formal amendment: it achieves some constitutional evolution (adaptation to changed conditions) with less legitimacy than Article V change would have. Theater ratio reflects that judges publicly maintain fidelity to the text while substantially revising its meaning. The mechanism persists through institutional inertia — courts cannot formally amend, yet their reinterpretation functions as de facto amendment.
constraint_indexing:constraint_classification(original_constitution_1787__article_v_amendment_procedure, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, any enduring political order requires entrenched constitutional protection against casual revision. A supermajority threshold is not arbitrary — it reflects the structural reality that fundamental law must be harder to change than ordinary legislation, lest stability collapse into flux. This perspective treats Article V not as a contingent choice but as a natural consequence of having a written constitution at all. However, the structural data (identifiable beneficiaries, measurable suppression of alternative constitutional arrangements, compounding extraction with time) indicates this is a false summit — the 'natural law' framing obscures the contingent historical choices embedded in the specific supermajority percentage (75%, not 50% or 90%) and the state-based veto structure.
constraint_indexing:constraint_classification(original_constitution_1787__article_v_amendment_procedure, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(original_constitution_1787__article_v_amendment_procedure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(original_constitution_1787__article_v_amendment_procedure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(original_constitution_1787__article_v_amendment_procedure, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(original_constitution_1787__article_v_amendment_procedure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(original_constitution_1787__article_v_amendment_procedure, TR),
    TR >= 0.70.

:- end_tests(original_constitution_1787__article_v_amendment_procedure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, compounding over time. At adoption (t=0), the constraint functioned primarily as coordination — supermajority requirement ensured stability and protected minority interests. Over 239 years, as amendment became increasingly rare and the settled order accumulated constitutional authority, the extraction mechanism intensified. Measurements show extractiveness rising from 0.35 (1787) to 0.62 (1987), reflecting that the constraint now primarily suppresses constitutional evolution favored by supermajorities while protecting accumulated constitutional provisions. The beneficiaries are the existing constitutional order and the status quo interests embedded in the 1787 framework (federalism, state sovereignty, property protections, separation of powers). Suppression (0.72): High and stable. Thirteen states can block any amendment regardless of population. This suppression is structural (not easily overcome by mobilization) and absolute (no workaround within constitutional bounds except jurisprudential drift or extraconstitutional change). Theater ratio (0.35): Moderate and rising. Early constitutional culture treated amendment as legitimate and achievable (19 amendments in first 180 years); contemporary constitutionalism treats amendment as nearly impossible (only 17 amendments in past 150 years), creating a performative gap between the formal amendment procedure and actual constitutional change. The rise in theater_ratio reflects Supreme Court jurisprudence increasingly substituting for formal amendment — judges maintain fidelity to the 1787 text while remapping its meaning.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is maximal. The settled constitutional order experiences Article V as legitimate coordination (Rope) protecting durable rights and structural stability. Reform coalitions experience it as mixed coordination and extraction (Tangled Rope) — they benefit from stability when they are not seeking change, but are blocked from lawful constitutional evolution when they command supermajority support. The powerless excluded supermajority experiences pure extraction (Snare) — they are trapped below the amendment threshold despite democratic backing. Structural minorities (states benefiting from constitutional provisions) experience Article V as protective (Rope/Tangled Rope) because the supermajority requirement vetos amendments threatening their position. Constitutional jurisprudence experiences the constraint as a degraded ritual (Piton) — the Supreme Court's reinterpretation functions as de facto amendment while maintaining performative fidelity to the fixed text. The analytical observer risks seeing the supermajority requirement as a natural law inherent to written constitutionalism (Mountain), but the structural data (identifiable beneficiaries, measurable suppression, compounding extraction with time) indicates this is a false summit: the 75% state-based threshold is a contingent historical choice that naturalizes constructed entrenchment.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for each perspective derives from the agent's structural position and relationship to the amendment suppression. Beneficiaries of the settled order (institutional power with arbitrage exit) experience low d → negative or near-zero χ; the constraint subsidizes their position. Excluded supermajorities (powerless/trapped) experience high d → high χ; maximum extraction because they cannot exit constitutional bounds. Organized reform coalitions (organized/constrained) experience moderate d → moderate χ; they have some agency (legislative alternatives) but constrained exit from the constitutional frame. State governments (institutional/constrained) experience mixed d based on whether their state benefits from or threatens to break the entrenchment. The analytical observer (analytical/analytical) with canonical fallback d ≈ 0.73 experiences derived χ, but the false-summit mechanism recognizes that the mountain classification naturalizes a contingent institutional choice.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy through the reading structure itself. The mandatrophy is not 'which classification is correct?' but 'which reading of the 1787 kernel captures its deepest commitment?' The article_v_amendment_procedure reading instantiates the constraint as tangled_rope (moderate-high extraction, genuine coordination function, active enforcement of suppression). Alternative readings would produce different classifications: the federal_supremacy_design reading would emphasize centralized power over states and yield higher snare characteristics; the separation_of_powers_design reading would emphasize institutional gridlock and yield tangled_rope with different beneficiaries (branches checking each other); the slavery_compromises reading would focus on the 3/5 clause and fugitive slave provisions, yielding pure snare characteristics for enslaved people. No single type resolves the mandatrophy — the presheaf of readings over the contested kernel DOES. The engine's task is to recognize that 'the 1787 text' is not a single constraint but a contested kernel admitting multiple readings, each yielding a different classification from a different structural perspective.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    supermajority_threshold_contingency,
    'Is the 75% state supermajority threshold itself a natural constraint on constitutional amendment, or a contingent historical choice that could have been set differently (e.g., 60%, 80%, per-capita popular vote)?',
    'Comparative constitutional analysis: examine amendment procedures in other democracies (Canada, Australia, Germany, Japan) and their historical rationale; analyze 1787 debates over threshold selection; test whether 75% produces structurally different outcomes than alternative thresholds would.',
    'If threshold is natural: mountain classification holds — entrenchment is inherent to constitutionalism. If contingent: false summit confirmed — the entrenchment is a constructed constraint masquerading as natural law. Structural delta: a 60% threshold would yield ε ≈ 0.42, reshaping the tangled_rope/snare boundary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(supermajority_threshold_contingency, empirical, 'Whether 75% threshold is natural or historically contingent').

omega_variable(
    judicial_amendment_substitution_mechanism,
    'Does constitutional jurisprudence (Supreme Court reinterpretation of fixed text) function as a substitute for formal amendment, and if so, does it resolve or merely defer the entrenchment problem?',
    'Longitudinal analysis of constitutional jurisprudence drift: track how many major constitutional doctrines have been substantially revised through case law without formal amendment (e.g., interstate commerce, equal protection, free speech); assess whether jurisprudential change achieves comparable legitimacy or stability to formal amendment.',
    'If substitution is effective: piton classification confirmed, but the constraint''s actual suppression of formal amendment is partially mitigated by jurisprudential flexibility. If substitution is illegitimate: piton is degraded ritual without functional equivalent; formal entrenchment becomes more severe. Extraction compounds differently based on whether jurisprudential amendment is available.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_amendment_substitution_mechanism, conceptual, 'Whether constitutional jurisprudence substitutes for formal amendment').

omega_variable(
    state_sovereignty_veto_justification,
    'Is the state-based supermajority (requiring 3/4 of states, not 3/4 of population) justified by federalism principles, or does it entrench rural/structural minority veto power by design?',
    'Demographic and electoral analysis: map which states would have to defect to cross the 75% threshold for various historical amendment proposals; analyze whether state-based voting produces systematically different results than per-capita popular voting would.',
    'If justified by federalism: suppression of amendments is a feature of federalism, not extraction. If veto entrenchment: suppression is asymmetric extraction protecting rural/structural interests at cost of urban/majority preferences. Structural delta: per-capita threshold would shift classification toward rope; state-based veto deepens toward snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_sovereignty_veto_justification, empirical, 'State-based supermajority as federalism principle vs. minority veto mechanism').

omega_variable(
    time_dependent_extractiveness_ratchet,
    'Does the entrenchment mechanism itself accumulate extraction over time as the Constitution ages and constitutional amendments become rarer, compounding the lock-in effect?',
    'Historical analysis of amendment frequency: track frequency of constitutional amendments per decade from 1787 to present; measure correlation between passage of time and difficulty of amendment; analyze whether constitutional ossification increases the extractiveness of status quo bias.',
    'If extractiveness compounds: the constraint transitions from rope (near adoption, when amendment seems possible) to tangled_rope (mid-term, as barriers accumulate) to snare (long-term, as constitutional evolution becomes impossible). Measurements should show base_extractiveness rising over centuries. If flat: extractiveness is stable; the ossification narrative is overstated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(time_dependent_extractiveness_ratchet, empirical, 'Whether constitutional entrenchment extractiveness accumulates over time').

omega_variable(
    kernel_reading_contest,
    'Which of the four competing readings of the 1787 text captures its deepest commitment: Article V amendment procedure (this reading), federal supremacy design, separation of powers design, or slavery compromises?',
    'Textual analysis of relative entrenchment in the 1787 document; historical analysis of framers'' intent and statements of purpose; institutional analysis of which provision has proven most durable and resistant to change; analysis of which provision generates the most consequential structural constraints on constitutional evolution.',
    'If Article V is the deepest: the constraint''s extraction flows from the amendment mechanism itself. If federal supremacy is deepest: extraction flows from centralized power overriding state/local alternatives. If separation of powers: extraction flows from institutional gridlock. If slavery compromises: extraction flows from the 3/5 compromise, fugitive slave clause, and slave-trade protections (different constraint entirely). The choice reorganizes the causal structure of American constitutional constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which reading of the 1787 kernel captures its deepest structural commitment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(original_constitution_1787__article_v_amendment_procedure, 0, 239).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(articlev_theater_1787, original_constitution_1787__article_v_amendment_procedure, theater_ratio, 0, 0.15).
narrative_ontology:measurement(articlev_theater_1910, original_constitution_1787__article_v_amendment_procedure, theater_ratio, 120, 0.35).

% Extraction over time
narrative_ontology:measurement(articlev_extractiveness_1787, original_constitution_1787__article_v_amendment_procedure, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(articlev_extractiveness_1850, original_constitution_1787__article_v_amendment_procedure, base_extractiveness, 60, 0.48).
narrative_ontology:measurement(articlev_extractiveness_1910, original_constitution_1787__article_v_amendment_procedure, base_extractiveness, 120, 0.58).
narrative_ontology:measurement(articlev_extractiveness_1987, original_constitution_1787__article_v_amendment_procedure, base_extractiveness, 200, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(articlev_suppression_1787, original_constitution_1787__article_v_amendment_procedure, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(articlev_suppression_1910, original_constitution_1787__article_v_amendment_procedure, suppression_requirement, 120, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(original_constitution_1787__article_v_amendment_procedure, enforcement_mechanism).
narrative_ontology:affects_constraint(original_constitution_1787__article_v_amendment_procedure, original_constitution_1787__federal_supremacy_design).
narrative_ontology:affects_constraint(original_constitution_1787__article_v_amendment_procedure, original_constitution_1787__separation_of_powers_design).
narrative_ontology:affects_constraint(original_constitution_1787__article_v_amendment_procedure, original_constitution_1787__slavery_compromises).

% DUAL FORMULATION NOTE:
% The 1787 Constitution is a contested kernel admitting four structurally distinct readings. This constraint (article_v_amendment_procedure) models Article V as the foundational mechanism through which constitutional entrenchment operates. Alternative readings (federal_supremacy_design, separation_of_powers_design, slavery_compromises) are separate constraint stories with different beneficiary/victim structures and ε values. They are linked via network.affects_constraints because the amendment procedure constrains all of them — any attempt to revise the substantive constitutional provisions (federal power, separation of powers, or slave-state protections) runs into the Article V supermajority barrier. The readings are not alternative measurements of a single constraint; they are distinct constraints sharing a common kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
