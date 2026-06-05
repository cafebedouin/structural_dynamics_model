% ============================================================================
% CONSTRAINT STORY: original_constitution_1787__slavery_compromises
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_original_constitution_1787__slavery_compromises, []).

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
 *   constraint_id: original_constitution_1787__slavery_compromises
 *   human_readable: Constitutional Slavery Compromises of 1787
 *   domain: political/legal/constitutional_law
 *
 * SUMMARY:
 *   The 1787 Constitution is structured by three slavery compromises that
 *   constitute the document's core institutional logic. The Three-Fifths
 *   Compromise (Article I, Section 2) grants southern slaveholders
 *   representation for enslaved persons they do not allow to vote—converting
 *   enslaved persons into fractional political power for their enslavers. The
 *   Fugitive Slave Clause (Article IV, Section 2) empowers federal
 *   authorities to seize and return escaped enslaved persons to their
 *   enslavers, overriding northern state sovereignty and rendering the
 *   northern border no legal boundary. The slave-trade protection (Article I,
 *   Section 9) defers congressional power to abolish the international slave
 *   trade until 1808, guaranteeing 20 years of continued extraction. These
 *   three mechanisms—representation without representation, federal
 *   enforcement of state-level slavery status, and constitutional protection
 *   for slave-trade continuation—purchase southern agreement to a federal
 *   union by entrenching slaveholding economic and political power. The
 *   constraint is the constitutional text itself functioning as an
 *   enforcement mechanism for slavery's continuation. This reading is one of
 *   four competing readings of the 1787 constitutional kernel; the other
 *   readings (Article V amendment procedure, federal supremacy design,
 *   separation of powers) all treat those features as the constitutive logic,
 *   leaving slavery as an unfortunate content to be interpreted. This reading
 *   treats slavery compromises as structurally foundational—the amendments,
 *   federalism, and separation of powers all exist because slavery was
 *   entrenched.
 *
 * KEY AGENTS:
 *   - Enslaved Africans and Free Black Persons: Primary victims (powerless/trapped) — defined as 3/5 persons for representation purposes; subject to federal rendition without due process; trapped in a legal system that constitutionally denies their personhood
 *   - Slaveholding Southern States: Primary beneficiaries (institutional/arbitrage) — gain representation bonus (3/5 multiplier), federal enforcement of slavery status, 20-year protection for slave trade; achieved through credible exit threat at convention
 *   - Northern States and Federal Union: Secondary beneficiary with enforced complicity (institutional/constrained) — gain national market, unified tariff policy, continental expansion authority, but forced to participate in slavery enforcement against northern constituencies' moral preferences
 *   - Delegates to Constitutional Convention (1787): Agents with negotiating power (organized/constrained) — southern delegates present slavery entrenchment as necessary condition for union; northern delegates accept compromise to avoid convention collapse
 *   - Abolitionist Movement (1830s-1860s): Organized opposition (organized/mobile) — perceive slavery compromise as reversal-through-amendment, mobilize toward Thirteenth Amendment
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing specific constitutional choice as immutable law of political union
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(original_constitution_1787__slavery_compromises, 0.95).
domain_priors:suppression_score(original_constitution_1787__slavery_compromises, 0.98).
domain_priors:theater_ratio(original_constitution_1787__slavery_compromises, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(original_constitution_1787__slavery_compromises, extractiveness, 0.95).
narrative_ontology:constraint_metric(original_constitution_1787__slavery_compromises, suppression_requirement, 0.98).
narrative_ontology:constraint_metric(original_constitution_1787__slavery_compromises, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(original_constitution_1787__slavery_compromises, snare).
narrative_ontology:human_readable(original_constitution_1787__slavery_compromises, "Constitutional Slavery Compromises of 1787").
narrative_ontology:topic_domain(original_constitution_1787__slavery_compromises, "political/legal/constitutional_law").

domain_priors:requires_active_enforcement(original_constitution_1787__slavery_compromises).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(original_constitution_1787__slavery_compromises, '70c31d2a-1e5a-4c42-966f-0535c5ae1f72').
narrative_ontology:cs_kernel_codification('70c31d2a-1e5a-4c42-966f-0535c5ae1f72', formalized).
narrative_ontology:cs_authority_grounding('70c31d2a-1e5a-4c42-966f-0535c5ae1f72', extraction).
narrative_ontology:cs_interpretation_layer_present('70c31d2a-1e5a-4c42-966f-0535c5ae1f72').
narrative_ontology:cs_reading_relation('70c31d2a-1e5a-4c42-966f-0535c5ae1f72', original_constitution_1787__article_v_amendment_procedure, influences).
narrative_ontology:cs_reading_relation('70c31d2a-1e5a-4c42-966f-0535c5ae1f72', original_constitution_1787__federal_supremacy_design, influences).
narrative_ontology:cs_reading_relation('70c31d2a-1e5a-4c42-966f-0535c5ae1f72', original_constitution_1787__separation_of_powers_design, influences).
narrative_ontology:cs_axiom('70c31d2a-1e5a-4c42-966f-0535c5ae1f72', foundational, representation_requires_full_personhood).
narrative_ontology:cs_axiom_status(representation_requires_full_personhood, overridden).
narrative_ontology:cs_axiom_grounding('70c31d2a-1e5a-4c42-966f-0535c5ae1f72', representation_requires_full_personhood, deontological).
narrative_ontology:cs_axiom('70c31d2a-1e5a-4c42-966f-0535c5ae1f72', foundational, federal_power_protects_property_in_persons).
narrative_ontology:cs_axiom_status(federal_power_protects_property_in_persons, overridden).
narrative_ontology:cs_axiom_grounding('70c31d2a-1e5a-4c42-966f-0535c5ae1f72', federal_power_protects_property_in_persons, deontological).
narrative_ontology:cs_reference_frame('70c31d2a-1e5a-4c42-966f-0535c5ae1f72', slaveholding_federal_constitution).
narrative_ontology:cs_drift_state('70c31d2a-1e5a-4c42-966f-0535c5ae1f72', eighteen_sixty_civil_war, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('70c31d2a-1e5a-4c42-966f-0535c5ae1f72', '').
narrative_ontology:cs_kernel_id(original_constitution_1787__slavery_compromises, original_constitution_1787).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(original_constitution_1787__slavery_compromises, slaveholding_southern_states).
narrative_ontology:constraint_beneficiary(original_constitution_1787__slavery_compromises, northern_merchant_capital).
narrative_ontology:constraint_victim(original_constitution_1787__slavery_compromises, enslaved_africans).
narrative_ontology:constraint_victim(original_constitution_1787__slavery_compromises, free_black_persons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ENSLAVED AND FREE BLACK PERSONS (SNARE) — Maximal suppression codified at the constitutional level. The Three-Fifths Compromise denies full personhood; the Fugitive Slave Clause (Article IV, Section 2) renders northern borders meaningless for escape; the slave trade protection (Article I, Section 9, deferring abolition until 1808) guarantees continued extraction for 20 years. Exit is structurally impossible — the Constitution itself is the mechanism of entrapment. No legal recourse, no territorial refuge, no constitutional protection. This is snare in its purest form: extraction enforced by the highest law.
constraint_indexing:constraint_classification(original_constitution_1787__slavery_compromises, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SLAVEHOLDING SOUTHERN STATES (ROPE) — Experience the constraint as pure coordination, not extraction. The compromise solves a collective action problem: southern delegates threatened to exit the convention; slavery compromises granted them disproportionate power (Three-Fifths representation, Fugitive Slave Clause, slave-trade protection) in exchange for union. From their position, the constraint is coordination—pooling sovereignty to create a national market for enslaved persons, enforced by federal law. They have arbitrage options (exit to confederation, separate state, international arrangement) but chose union because the compromise more than compensates. Extractiveness toward them is negative (they are net beneficiaries).
constraint_indexing:constraint_classification(original_constitution_1787__slavery_compromises, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: NORTHERN STATES AND FEDERAL UNION (TANGLED ROPE) — The northern merchant and manufacturing interests experience genuine coordination benefit (access to southern agricultural markets, slave-produced cotton, unified tariff and commercial policy) alongside complicity in extraction. Northern states gain constitutional power to enforce fugitive rendition, participate in the national slave market via commerce, and benefit from slave-labor-produced commodities. But they also bear structural cost: commitment to enforce slavery against their own citizens' moral and political preferences; continued conflict over expansion into western territories; erosion of northern legitimacy. The constraint is hybrid—real coordination (union, commerce) with embedded extraction (slavery enforcement). Exit is costly (civil war, economic disruption) but theoretically possible. Theater ratio is low because the coordination function is genuinely present: northern political economy depends on access to southern agricultural and slave-labor resources.
constraint_indexing:constraint_classification(original_constitution_1787__slavery_compromises, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSTITUTIONAL TEXT AS INSTITUTIONAL AUTHORITY (PITON) — By the 1820s-1850s, the slavery compromises have become increasingly performative relative to their original coordination function. The Fugitive Slave Clause is invoked through federal marshals and judges, generating theater (legal proceedings, constitutional drama) but providing diminishing actual enforcement as northern resistance grows. The Three-Fifths Compromise persists as a formal rule but its rationale (union stability) is increasingly questioned. The constraint is maintained through institutional inertia—judges cite the text, politicians invoke 'original intent,' constitutional law professors teach 'fidelity to the framers'—while the underlying coordination logic has atrophied. The text becomes a theatrical prop for defending slavery, not an active mechanism for purchasing union. Theater ratio rises as extractive force must be defended through elaborate constitutional argument rather than straightforward bargain.
constraint_indexing:constraint_classification(original_constitution_1787__slavery_compromises, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ABOLITIONIST MOVEMENT AND FREE-STATE COALITION (SCAFFOLD) — The organized abolitionist movement (1830s onward) perceives the slavery compromise as a temporary constitutional arrangement with a constitutional sunset: the Thirteenth Amendment (proposed 1864, ratified 1865). Abolitionist organizing is structured by the perception that the constitutional entrenchment of slavery is reversible through constitutional amendment—a coordinated supermajority can overturn the original compromise. This perspective mobilizes political power through the amendment process itself, treating the 1787 compromise as a structural problem with a defined exit route (constitutional amendment requiring 3/4 state approval). The constraint appears mobile rather than trapped because the amendment pathway creates exit optionality for organized actors. Theater includes abolitionist rhetoric, legislative debate, and constitutional argument—all framed as building toward a constitutional fix. The sunset logic is enforced by the amendment outcome (Thirteenth Amendment actually succeeds in 1865).
constraint_indexing:constraint_classification(original_constitution_1787__slavery_compromises, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW FRAMING (MOUNTAIN) — From a civilizational analytical perspective, the 1787 slavery compromises might be framed as reflecting an immutable tension: any federal union in a world of differential economic interests and moral frameworks must somehow accommodate incompatible positions to achieve unity. The framers 'had no choice'—southern delegates would not accept a union that threatened slavery, northern delegates would not accept southern hegemony, enslaved persons had no seat at the table. The compromise appears as a law of political nature: the cost of union when one party's foundational institution is at stake. This perspective frames slavery as structurally inevitable given the bargaining positions. However, this reading is a FALSE SUMMIT: the 1787 text is not a natural law but a deliberate constitutional choice by specific agents benefiting from slavery. The naturalization of slavery as 'the price of union' is precisely the framing the slaveholding interest imposed to prevent discussion of alternatives (gradual abolition, compensation schemes, territorial separation, majority-rule democracy without slavery protections).
constraint_indexing:constraint_classification(original_constitution_1787__slavery_compromises, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(original_constitution_1787__slavery_compromises_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(original_constitution_1787__slavery_compromises, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(original_constitution_1787__slavery_compromises, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(original_constitution_1787__slavery_compromises, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(original_constitution_1787__slavery_compromises, TR),
    TR >= 0.70.

:- end_tests(original_constitution_1787__slavery_compromises_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.95): Near-maximal. The constraint extracts foundational political and economic value for slaveholders—representation without democratic accountability, federal enforcement of property claims in enslaved persons, and guaranteed market access. The enslaved are the primary targets with no exit. The extraction is constitutionally serviced (not merely illegal or informal but formally written into the supreme law). The value is 0.95 rather than 1.0 because a small amount of political space exists for interpretation disputes and constitutional challenge (evidenced by rising theater over time, particularly from 1830 onward). Suppression (0.98): Maximal. The enslaved are completely denied legal recourse, territorial escape routes (Fugitive Slave Clause closes northern borders), political representation (3/5 clause denies full personhood), and constitutional protection (Constitution explicitly authorizes slavery). Federal power is deployed to prevent escape and enforce rendition. The suppression includes legal prohibition, active enforcement, and constitutional nullification of alternatives. Theater ratio (0.08 at t=0, rising to 0.72 by t=88): Initially low because the compromise is freshly negotiated with clear payoffs understood by all parties—little pretense is needed. Southern delegates explicitly defend slavery as necessary; northern delegates accept the bargain knowing its cost. By 1830-1850, as northern antislavery sentiment grows, the theater rises dramatically: constitutional scholars develop 'original intent' arguments, judges conduct elaborate legal proceedings under the Fugitive Slave Clause, politicians invoke 'constitutional fidelity' as a cover for defending slavery against moral challenge. By 1860, the constraint has become almost entirely performative—southern delegates claim constitutional protection for slavery in western territories (Dred Scott decision), while northern states resist through state nullification and constitutional reinterpretation. The rising theater reflects the atrophying consensus: the original bargain is no longer accepted by all parties, so maintaining it requires increasingly elaborate constitutional theater.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. From the enslaved and free Black perspective, it is a snare—complete structural entrapment with constitutional enforcement. From the southern slaveholding perspective, it is rope—a coordination solution to a bargaining problem, with net benefit and options to exit (had northern states refused, southern delegates could have remained confederal). From the northern institutional perspective, it is tangled rope—genuine coordination benefits (national market, tariff uniformity) with embedded extraction (forced participation in slavery enforcement, moral/political cost). From the constitutional text as an authority structure, it is piton by the mid-19th century—the original coordination function has atrophied while the enforcement mechanism persists through institutional inertia and elaborate legal theater. From the abolitionist organized movement, it is scaffold—a temporary constitutional arrangement with a defined sunset (constitutional amendment). From the civilizational analytical observer, it is mountain—a natural law of political union—but this is a false summit. The perspectival gaps reveal structural divergence: each actor experiences the same constitutional text differently based on their position relative to slavery. The natural law framing is the analytically most dangerous because it naturalizes what is actually a deliberate choice benefiting specific actors.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from the agent's beneficiary/victim status and exit options. For enslaved persons: d = 0.98 (full target, structurally trapped, no exit available, complete extraction flow toward the constraint's beneficiaries). For southern states: d = 0.02 (full beneficiary, arbitrage options available—they threatened to exit the convention and got favorable terms, would not have accepted union without slavery protection). For northern states: d = 0.65 (both benefits and costs, constrained exit, forced to participate in slavery enforcement despite moral preference against it, but benefit from national market and tariff policy). The derived directionality values confirm the snare classification at the powerless/trapped perspective: high d produces high f(d) which produces high χ (effective extraction toward the trapped agent). The rope classification at the beneficiary perspective: low d produces low/negative f(d) which produces low or negative χ (constraint subsidizes this agent). The tangled rope at the institutional/constrained perspective: moderate d with genuine coordination function produces moderate χ and mixed experience.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy through definitional clarity: it is unambiguously a snare (high extraction, maximal suppression, low initial theater, no coordination benefit for the target). The mandatrophy question—'is this coordination or extraction?'—is resolved by identifying victims (enslaved persons) who derive zero coordination benefit from the constraint. A coordinate mechanism requires all parties to have net benefit or at least mutual understanding of benefit. The enslaved have neither: they are defined as property, denied legal recourse, denied territorial escape, and denied political voice. The constraint is pure extraction. The snare classification is confirmed by mandatrophy resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    counterfactual_union_architectures,
    'Were slavery compromises necessary to achieve a federal union in 1787, or were they choices among available alternatives?',
    'Comparative constitutional history: examination of alternative proposals at the convention (e.g., proportional representation without slavery multiplier, anti-slave-trade provisions without 20-year deferral, federal power to regulate slavery); counterfactual analysis of whether northern states would have ratified a union with stronger slavery restrictions; analysis of private correspondence showing whether delegates perceived alternatives as genuinely foreclosed or chose the compromise as superior bargain.',
    'If union required slavery compromises (necessity frame): the constraint might be reconceived as a tragic coordination problem where moral and political equality were sacrificed for union—classification remains snare but rationale shifts to structural inevitability. If slavery compromises were chosen among available alternatives (agency frame): the constraint is revealed as deliberate entrenchment of slaveholding power—snare classification confirmed as choice, not necessity. The false summit framing collapses and the natural law perspective becomes clearly false.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(counterfactual_union_architectures, empirical, 'Necessity vs. agency in constitutional choice of slavery compromises').

omega_variable(
    northern_complicity_threshold,
    'At what point does northern participation in slavery enforcement (Fugitive Slave Clause administration, commerce in slave-produced goods, representation arrangements) constitute complicity that retroactively makes northern states co-beneficiaries rather than constrained parties?',
    'Historical documentation of northern enforcement of fugitive slave law (numbers of renditions, federal marshals'' role, northern state judges'' participation); economic analysis of northern profit flows from slavery (cotton commerce, slave-trade finance, slave-ship construction); political analysis of northern states'' defense of slavery compromise despite antislavery organizing.',
    'If northern compliance is high: tangled_rope perspective is reconceived as rope (northern states are also net beneficiaries despite stated moral opposition). If northern compliance is low: tangled_rope classification confirmed (constrained parties forced to participate). The directionality of northern states'' d value (benefit vs. cost) is substantially altered.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(northern_complicity_threshold, empirical, 'Northern states'' actual complicity in slavery enforcement').

omega_variable(
    kernel_reading_alternative_framing,
    'Is this constraint genuinely a reading of the 1787 constitutional kernel (the authoritative text itself structures slavery into representation and fugitive law), or is it a reading of the undertext—the unspoken agreement among delegates that slavery must be entrenched for southern agreement?',
    'Textual analysis: does the Constitution''s explicit language (Three-Fifths Clause, Fugitive Slave Clause, slave-trade deferral) directly instantiate slavery in the kernel, or does the kernel delegate slavery to state law with only indirect constitutional service? Framing analysis: does the constitutional text itself name slavery or use euphemisms (persons held to service, fugitives held to labor)?',
    'If slavery is explicit in the kernel text: this reading is a direct textual reading of the constitutional document. If slavery is implicit (delegated to state law, referred to euphemistically): this reading is inferential—the constraint is slavery''s constitutional entrenchment, but the kernel text does not name slavery explicitly. The distinction affects how foundational axioms are grounded (empirically in the text vs. conceptually in the delegates'' intent).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_alternative_framing, conceptual, 'Whether slavery compromises are explicit in the kernel text or implicit in framers'' intent').

omega_variable(
    false_summit_natural_law_interrogation,
    'The analytical observer''s mountain classification naturalizes slavery as an immutable price of union. Is this naturalization a genuine structural insight (unions always require such compromises) or a rhetorical move by the beneficiary to prevent questioning of the specific slavery entrenchment?',
    'Comparative political history: examination of other federal unions (Swiss cantons, Dutch republic, German confederation, post-colonial federations) and whether they required equivalent slavery entrenchment or found alternative coordination mechanisms; discourse analysis of how delegates and subsequent interpreters used ''necessity of union'' framing; analysis of whether the framing appeared in documents contemporaneous with the choice or emerged later as justification.',
    'If naturalization is genuine structural insight: mountain classification and false-summit detection may be incorrect—the constraint genuinely reflects political structural limits. If naturalization is rhetorical move: the false-summit signature fires correctly, engine reclassifies to tangled_rope or snare depending on beneficiary/victim structure, and the analytical observer perspective is revealed as captured by the beneficiary''s framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law_interrogation, conceptual, 'Whether mountain framing naturalizes contingent choice as structural necessity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(original_constitution_1787__slavery_compromises, 0, 88).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(const_1787_slavery_theater_t0, original_constitution_1787__slavery_compromises, theater_ratio, 0, 0.08).
narrative_ontology:measurement(const_1787_slavery_theater_t40, original_constitution_1787__slavery_compromises, theater_ratio, 40, 0.35).
narrative_ontology:measurement(const_1787_slavery_theater_t88, original_constitution_1787__slavery_compromises, theater_ratio, 88, 0.72).

% Extraction over time
narrative_ontology:measurement(const_1787_slavery_extractiveness_t0, original_constitution_1787__slavery_compromises, base_extractiveness, 0, 0.95).
narrative_ontology:measurement(const_1787_slavery_extractiveness_t40, original_constitution_1787__slavery_compromises, base_extractiveness, 40, 0.96).
narrative_ontology:measurement(const_1787_slavery_extractiveness_t88, original_constitution_1787__slavery_compromises, base_extractiveness, 88, 0.98).

% Suppression requirement over time
narrative_ontology:measurement(const_1787_slavery_suppression_t0, original_constitution_1787__slavery_compromises, suppression_requirement, 0, 0.92).
narrative_ontology:measurement(const_1787_slavery_suppression_t40, original_constitution_1787__slavery_compromises, suppression_requirement, 40, 0.95).
narrative_ontology:measurement(const_1787_slavery_suppression_t88, original_constitution_1787__slavery_compromises, suppression_requirement, 88, 0.98).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(original_constitution_1787__slavery_compromises, enforcement_mechanism).
narrative_ontology:affects_constraint(original_constitution_1787__slavery_compromises, thirteenth_amendment_enforcement).
narrative_ontology:affects_constraint(original_constitution_1787__slavery_compromises, three_fifths_compromise_representation_power).
narrative_ontology:affects_constraint(original_constitution_1787__slavery_compromises, fugitive_slave_clause_federal_rendition).
narrative_ontology:affects_constraint(original_constitution_1787__slavery_compromises, slave_trade_protection_1787_to_1808).

% DUAL FORMULATION NOTE:
% This reading treats the 1787 slavery compromises as a unified structural constraint. The constraint could be decomposed into three separate stories (Three-Fifths representation power, Fugitive Slave enforcement, slave-trade protection) with distinct ε values reflecting their distinct mechanisms. However, they are functionally entangled in the historical text—southern ratification was contingent on ALL THREE being protected—and therefore are modeled as a single constraint with ε=0.95 representing the integrated extractive effect. The decomposition is noted here for completeness but not enacted, as the synthetic constraint better captures the historical contingency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(original_constitution_1787__slavery_compromises, institutional, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
