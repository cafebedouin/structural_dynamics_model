% ============================================================================
% CONSTRAINT STORY: revolutionary_constitutionalism__us_constitution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_revolutionary_constitutionalism__us_constitution, []).

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
 *   constraint_id: revolutionary_constitutionalism__us_constitution
 *   human_readable: The United States Constitution as Durable Revolutionary Framework
 *   domain: political/legal/constitutional_governance
 *
 * SUMMARY:
 *   The United States Constitution of 1787 is a paradigmatic durable
 *   revolutionary constitution—a short, formally rigid framework (Article V
 *   supermajorities for amendment) that has absorbed two centuries of
 *   transformation through constitutional amendment (27 amendments), judicial
 *   reinterpretation (doctrinal shifts from Lochner to New Deal to
 *   contemporary Commerce Clause jurisprudence), and administrative expansion
 *   (executive orders, agency regulation) without formal replacement. Unlike
 *   the French Constitution of 1791 (which collapsed within a year, replaced
 *   repeatedly through the 19th century) or the Soviet Constitution of 1936
 *   (which enumerated elaborate rights while actual authority ran entirely
 *   through the unmentioned Party apparatus), the US Constitution persists
 *   through a specific architectural strategy: entrenchment through
 *   supermajority requirements, combined with interpretive flexibility that
 *   allows the framework to accommodate change without formal amendment. This
 *   constraint instantiates one reading of revolutionary
 *   constitutionalism—the reading that emphasizes durability through
 *   entrenchment. It is one of three coexisting readings of the same kernel.
 *
 * KEY AGENTS:
 *   - Constitutional Supermajority Coalitions: Primary beneficiary (institutional/arbitrage) — whichever faction controls 2/3 Congress or 3/4 state legislatures holds veto power over constitutional change; benefits from Article V barrier that protects their interests from majoritarian override
 *   - Majoritarian Reform Movements: Primary victim (powerless/trapped) — popular movements holding majority support (51-65%) cannot constitutional­ize their program without achieving supermajority consensus; blocked by counter-majoritarian structure
 *   - Supreme Court / Interpretation Authority: Institutional actor (powerful/mobile) — holds monopoly on authoritative constitutional interpretation; benefits from this grant of power but constrained by text and precedent; experiences both coordination and extraction
 *   - Federal Legislatures (Congress and State Legislatures): Institutional actor (institutional/arbitrage) — de jure amendment authority; de facto constrained by supermajority requirement but de facto powerful through ordinary legislation and adjudication of constitutional scope
 *   - Future Constitutional Constituencies: Distributed victim (powerless/trapped) — constituencies born after constitutional freeze cannot renegotiate the constitutional settlement; inherit both its benefits and its constraints without consent
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing entrenchment as structural inevitability rather than design choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(revolutionary_constitutionalism__us_constitution, 0.38).
domain_priors:suppression_score(revolutionary_constitutionalism__us_constitution, 0.62).
domain_priors:theater_ratio(revolutionary_constitutionalism__us_constitution, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(revolutionary_constitutionalism__us_constitution, extractiveness, 0.38).
narrative_ontology:constraint_metric(revolutionary_constitutionalism__us_constitution, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(revolutionary_constitutionalism__us_constitution, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(revolutionary_constitutionalism__us_constitution, tangled_rope).
narrative_ontology:human_readable(revolutionary_constitutionalism__us_constitution, "The United States Constitution as Durable Revolutionary Framework").
narrative_ontology:topic_domain(revolutionary_constitutionalism__us_constitution, "political/legal/constitutional_governance").

domain_priors:requires_active_enforcement(revolutionary_constitutionalism__us_constitution).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(revolutionary_constitutionalism__us_constitution, 'c7a5cdfb-31f6-48f6-869a-0c42a5424608').
narrative_ontology:cs_kernel_codification('c7a5cdfb-31f6-48f6-869a-0c42a5424608', fixed_text).
narrative_ontology:cs_authority_grounding('c7a5cdfb-31f6-48f6-869a-0c42a5424608', lineage).
narrative_ontology:cs_interpretation_layer_present('c7a5cdfb-31f6-48f6-869a-0c42a5424608').
narrative_ontology:cs_reading_relation('c7a5cdfb-31f6-48f6-869a-0c42a5424608', revolutionary_constitutionalism__french_constitution_1791, coexists_with).
narrative_ontology:cs_reading_relation('c7a5cdfb-31f6-48f6-869a-0c42a5424608', revolutionary_constitutionalism__soviet_constitution_1936, coexists_with).
narrative_ontology:cs_axiom('c7a5cdfb-31f6-48f6-869a-0c42a5424608', foundational, entrenchment_as_stability_mechanism).
narrative_ontology:cs_axiom_status(entrenchment_as_stability_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('c7a5cdfb-31f6-48f6-869a-0c42a5424608', entrenchment_as_stability_mechanism, instrumental).
narrative_ontology:cs_axiom('c7a5cdfb-31f6-48f6-869a-0c42a5424608', secondary, interpretive_flexibility_within_fixed_text).
narrative_ontology:cs_axiom_status(interpretive_flexibility_within_fixed_text, holdable).
narrative_ontology:cs_axiom_grounding('c7a5cdfb-31f6-48f6-869a-0c42a5424608', interpretive_flexibility_within_fixed_text, conventional).
narrative_ontology:cs_reference_frame('c7a5cdfb-31f6-48f6-869a-0c42a5424608', constitutional_entrenchment_through_supermajority).
narrative_ontology:cs_drift_state('c7a5cdfb-31f6-48f6-869a-0c42a5424608', contemporary_administrative_governance, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c7a5cdfb-31f6-48f6-869a-0c42a5424608', '').
narrative_ontology:cs_kernel_id(revolutionary_constitutionalism__us_constitution, revolutionary_constitutionalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(revolutionary_constitutionalism__us_constitution, constitutional_supermajority_coalitions).
narrative_ontology:constraint_beneficiary(revolutionary_constitutionalism__us_constitution, interpretation_authority_holders).
narrative_ontology:constraint_victim(revolutionary_constitutionalism__us_constitution, majoritarian_reform_movements).
narrative_ontology:constraint_victim(revolutionary_constitutionalism__us_constitution, future_constitutional_constituencies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BLOCKED MAJORITY MOVEMENT (SNARE) — A coalition holding 51% of popular support for a constitutional change (e.g., campaign finance limits, expanded voting rights, redistribution mechanism) confronts Article V's supermajority requirement. Unable to exit the Constitution's jurisdiction; unable to amend it without 2/3 of both chambers or 3/4 of states. Maximum suppression, minimal coordination benefit to this agent. Extraction runs entirely toward entrenchment beneficiaries.
constraint_indexing:constraint_classification(revolutionary_constitutionalism__us_constitution, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INTERPRETATION-SEEKING REFORM COALITION (TANGLED ROPE) — A movement pursuing constitutional change through judicial interpretation rather than formal amendment (e.g., Living Constitutionalism) experiences genuine coordination function: the Constitution creates a common legal framework and binding court, enabling nationwide coordination otherwise impossible. BUT also experiences asymmetric extraction: interpretive authority is concentrated, interpretive outcomes are unpredictable and reversible, and coalitions lack direct control over the mechanism. Constrained by the need to persuade courts and limited by judicial doctrine; benefiting from the unified legal order.
constraint_indexing:constraint_classification(revolutionary_constitutionalism__us_constitution, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SUPERMAJORITY COALITION BENEFICIARY (ROPE) — Whichever coalition currently controls 2/3 majority in Congress (or equivalent state-legislature coordination) experiences the Constitution as pure coordination: it enables nationwide legal uniformity, enables enforcement of their preferred rules across all states, and most importantly, gives them veto power over constitutional change. The suppression mechanism (Article V) is experienced as legitimate coordination requirement, not as extractive barrier. Net beneficiary through arbitrage — can exit by simply maintaining coalition status or by negotiating alternative arrangements if needed.
constraint_indexing:constraint_classification(revolutionary_constitutionalism__us_constitution, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: FEDERAL JUDICIARY (TANGLED ROPE) — The courts benefit from the Constitution's grant of authority and the exclusivity of their interpretive power (coordination function), but also bear suppression: they are trapped within the document, constrained by precedent and text, unable to amend their own constraints without constitutional amendment. Yet they have mobile options: they can expand or narrow doctrine, reinterpret precedent, or strategically defer to legislatures. Experience moderate extraction as the constraint limits their power while the Constitution grants it.
constraint_indexing:constraint_classification(revolutionary_constitutionalism__us_constitution, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSTITUTIONAL RITUAL PERFORMANCE (PITON) — At civilizational scale, the Constitution itself (as performed in political discourse, judiciary, and governance) has become substantially theatrical. The text is invoked as binding authority, but 70% of significant governance occurs through legislative negotiation, administrative law, and executive order—all outside the formal constitutional amendment process. The constitutional form persists through institutional inertia: politicians invoke the Constitution as legitimacy cover for actions the Constitution's text does not authorize; courts perform deference to legislature while actually adjudicating constitutional questions. Theater ratio high; actual functional constraint lower. The performance maintains legitimacy of the system itself.
constraint_indexing:constraint_classification(revolutionary_constitutionalism__us_constitution, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / STRUCTURAL INEVITABILITY (MOUNTAIN) — From a civilizational and universal perspective, any durable written constitution necessarily embeds supermajority requirements to prevent constitutional collapse through factionalism. Entrenchment is inherent to the concept of a constitution—a unchangeable supreme law. The suppression mechanism (supermajority barrier) appears as a structural necessity of written constitutionalism itself, not as a contingent institutional design choice. However, this perspective risks naturalizing what is actually a design choice: alternative constitutional architectures (continuous amendment, delegated authority to interpret, constitutional sunset clauses) are logically coherent. The mountain classification is a false summit, revealing naturalization.
constraint_indexing:constraint_classification(revolutionary_constitutionalism__us_constitution, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(revolutionary_constitutionalism__us_constitution_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(revolutionary_constitutionalism__us_constitution, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(revolutionary_constitutionalism__us_constitution, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(revolutionary_constitutionalism__us_constitution, TR),
    TR >= 0.70.

:- end_tests(revolutionary_constitutionalism__us_constitution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint exhibits real asymmetry—a coalition holding 51% of popular support cannot amend the Constitution without achieving 67% Congress + 75% state ratification or equivalent consensual standard. This is genuine extraction: the counter-majoritarian structure suppresses alternative constitutional orders. However, extractiveness is not severe (not 0.65+) because: (1) supermajority requirement aligns with a defensible principle of constitutional stability (durable law should require broad consensus); (2) alternative pathways exist (judicial interpretation, ordinary legislation, constitutional convention); (3) the constraint is transparent—the supermajority barrier is explicit in Article V, not hidden. Suppression (0.62): Moderately high. The barrier to formal amendment is substantial and intentional. However, suppression is not extreme (not 0.85+) because: (1) judicial interpretation provides a functional amendment pathway; (2) ordinary legislation and executive action function as informal constitutional change; (3) the constitutional convention mechanism (Article V, Section 2) remains unused but available. Theater ratio (0.58): Moderate-high, rising over time. At the founding (0.35), the Constitution was substantially functional—the document directly governed the processes of government. By 1975 (0.55), significant governance occurred through legislative negotiation, administrative agencies, executive order, and judicial doctrine, with the Constitution invoked as legitimacy cover. By 2026 (0.58), the Constitution functions partly as performative legitimacy (politicians and judges invoke constitutional authority while acting under structures the document does not explicitly authorize) and partly as binding framework. Measurements show rising theater ratio: the constraint's functional governance role has diminished while its performance role has increased, but not sufficiently to classify as piton (which requires theater ≥ 0.70).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The blocked majority movement sees a snare: their supermajority requirement-induced trap with no exit. The constitutional beneficiary sees rope: their coordination mechanism that enables nationwide uniform rules and protects their veto. The interpretation-seeking coalition sees tangled rope: real coordination but asymmetric extraction through judicial authority. The judiciary sees tangled rope: beneficiary from interpretive grant but constrained by text. The ritual performance view sees piton: constitutional invocation has become substantially performative. The analytical observer sees a false-summit mountain: risks naturalizing entrenchment as inevitable. All six perspectives are analytically defensible. The perspectival gap reveals that the classification of this constraint depends entirely on observational position—which side of the supermajority barrier you occupy.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies sharply across perspectives. A powerless majority movement blocked by Article V experiences d ≈ 0.95 (full target, maximum extraction). The supermajority beneficiary experiences d ≈ 0.05 (full beneficiary, minimal extraction to them—the barrier protects their interests). The interpretation-seeking coalition experiences d ≈ 0.55 (symmetric: benefits from unified legal order, harmed by unpredictable interpretive authority). The federal judiciary experiences d ≈ 0.45 (benefits from interpretive authority grant, constrained by textual fidelity requirement). The analytical observer experiences d ≈ 0.72 (observing the system rather than embedded in it). These divergent d values produce the perspectival gap: the same constraint appears as snare to the blocked majority, rope to the beneficiary, tangled_rope to coalitions with mixed stakes. The chi formula scales these differently: a powerless trapped agent at national scope has f(d) ≈ 1.42, yielding chi ≈ 0.54 (snare threshold); an institutional arbitrage beneficiary has f(d) ≈ -0.12, yielding chi ≈ -0.05 (rope/coordination); an analytical observer has f(d) ≈ 1.15, yielding chi ≈ 0.44 (tangled_rope threshold).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves the mandatrophy through the kernel contest itself. This reading (US Constitution as durable framework through entrenchment) coexists with the French reading (revolutionary constitution as rupture—unable to maintain stability) and the Soviet reading (revolutionary constitution as facade—actual power runs through unmentioned structure). The three readings do not reduce to a single type; instead, they reveal that 'revolutionary constitution' is an irreducibly contested kernel. The US reading resolves the mandatrophy by declaring which MECHANISM produces durability (entrenchment + interpretation), not by claiming there is only one true classification. The tension between the blocked majority (snare) and the beneficiary (rope) is built into the structure—both perspectives are correct about their experience. The mandatrophy dissolves when we recognize that different constituencies genuinely experience the Constitution differently depending on their structural position relative to the supermajority barrier.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    amendment_versus_interpretation_boundary,
    'What distinguishes constitutional amendment (formal Article V change) from constitutional interpretation (judicial doctrine change) as an analytical boundary, and which is ''really'' changing the constraint?',
    'Historical case analysis: identify moments where courts or legislatures explicitly chose interpretation over amendment, and track whether interpretive precedent later crystallized into formal amendment or remained settled practice. Examine cases where movements pursued both paths simultaneously (e.g., commerce clause expansion through interpretation vs. federalism amendment attempts).',
    'If interpretation is recognized as amendment-equivalent: extractiveness rises (supermajority requirement is routinely bypassed) and suppression falls (the Article V barrier is largely ceremonial). If interpretation is subordinate to amendment: extractiveness remains moderate (some movements succeed through interpretation but lack guaranteed pathway) and suppression remains high (Article V barrier intact).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(amendment_versus_interpretation_boundary, conceptual, 'Whether constitutional interpretation constitutes functional amendment despite formal requirement').

omega_variable(
    entrenchment_supermajority_sufficiency,
    'Is the Article V supermajority requirement (2/3 Congress or 3/4 states) sufficiently high to prevent the Constitution from being captured by stable minority coalitions?',
    'Empirical analysis of voting power distribution: identify which coalitions have held veto power over constitutional amendments for sustained periods (>20 years). Calculate the minimum population percentage that can block amendment under various apportionment scenarios. Compare to historical amendment success rates and identify failed amendment campaigns that would have passed under lower supermajority thresholds.',
    'If supermajority proves insufficient (consistent minority veto): victim set expands, suppression rises toward 0.75+. If supermajority proves protective (no stable minority veto observed): victim set contracts to narrower constituencies, suppression moderates toward 0.50. Currently estimated: supermajority sufficient for protection but not perfectly proportional—approximately 35-40% population can block amendment under worst-case apportionment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(entrenchment_supermajority_sufficiency, empirical, 'Whether Article V supermajority adequately protects against minority constitutional capture').

omega_variable(
    revolutionary_constitutionalism_reading_contest,
    'Is the US Constitution''s durability evidence that it represents a durable solution to revolutionary instability, or is it evidence that it successfully froze a particular revolutionary moment''s power distribution?',
    'The kernel contest itself. This reading (US Constitution as durable framework) is one of three readings of revolutionary_constitutionalism. The French Constitution of 1791 reading (rupture logic) emphasizes revolutionary constitutions'' inherent instability—the attempt to rebuild from declarations of rights. The Soviet Constitution of 1936 reading (facade logic) emphasizes revolutionary constitutions as covers for actual power distribution beneath. This reading emphasizes durability through entrenchment and interpretive flexibility. Resolution: track which characteristics actually distinguish the three readings (durability vs. rupture vs. facade) empirically in historical record, and assess whether the three readings coexist or whether one forecloses another.',
    'If durability is read as stability-through-entrenchment (this reading): extractiveness moderate, supermajority barrier justified as protective. If durability is reread as ''frozen revolutionary bargain'' (French reading influence): extractiveness rises (entrenchment serves minority interests frozen at founding). If durability is reread as performative maintenance of facade (Soviet reading influence): theater_ratio rises above 0.70, classification shifts toward piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revolutionary_constitutionalism_reading_contest, conceptual, 'Whether US Constitution''s durability reflects stability mechanism or frozen revolutionary power distribution').

omega_variable(
    interpretation_authority_legitimacy_grounding,
    'Does the Supreme Court''s interpretive authority derive from Article III (the constitutional text itself), from institutional practice and precedent, or from popular acceptance of judicial supremacy—and does the answer change the classification of interpretation as coordinating versus extractive?',
    'Historical analysis of constitutional interpretation authority: trace the Supreme Court''s claim to interpretive supremacy from Marbury v. Madison (1803) forward. Identify moments of successful challenge to judicial authority (e.g., FDR court-packing threat, Lincoln''s rejection of Dred Scott). Assess whether interpretive authority persists through legal doctrine or through institutional inertia/performance. Examine whether judicial deference to legislature in some domains (political questions, secondary effects) constitutes genuine coordination or performative subordination.',
    'If authority is textually grounded (Article III reading): interpretation is legitimate coordination mechanism, tangled_rope classification holds. If authority is practice-grounded (precedent reading): interpretation is institutional convention vulnerable to challenge, increases omega uncertainty. If authority is performance-grounded (legitimacy reading): interpretation is theatrical ritual that could be disrupted, increases piton classification weight.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretation_authority_legitimacy_grounding, conceptual, 'Grounding of Supreme Court''s constitutional interpretation authority').

omega_variable(
    frozen_compromise_beneficiary_identification,
    'Who are the ''frozen compromise'' beneficiaries of the original Constitution, and does the benefit persist or degrade under contemporary demographic and political conditions?',
    'Historical analysis of 1787 compromise: identify which factions and interests shaped the original constitutional settlement (federalist merchant interests, southern slaveholding interests, northern manufacturing interests, etc.). Trace which contemporary political coalitions inherit benefit from the original structure (federalism protecting rural minorities, electoral college protecting low-population states, Senate apportionment protecting low-population states, counter-majoritarian judiciary). Compare: which original-beneficiary interests remain beneficiaries, and which have become victims? Where do contemporary supermajority coalition interests align with original interests vs. diverge?',
    'If original beneficiaries remain stable: constitutional structure reflects persistent coalition (lower victim perception). If original beneficiaries have shifted: constitutional structure now advantages different coalitions, making it appear arbitrary to current victims. If no stable beneficiary can be identified: constraint resembles false summit more strongly (natural feature rather than frozen bargain).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(frozen_compromise_beneficiary_identification, empirical, 'Identity and persistence of original constitutional compromise beneficiaries').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(revolutionary_constitutionalism__us_constitution, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(revcon_theater_1787, revolutionary_constitutionalism__us_constitution, theater_ratio, 0, 0.35).
narrative_ontology:measurement(revcon_theater_1865, revolutionary_constitutionalism__us_constitution, theater_ratio, 1, 0.42).
narrative_ontology:measurement(revcon_theater_1920, revolutionary_constitutionalism__us_constitution, theater_ratio, 2, 0.5).
narrative_ontology:measurement(revcon_theater_1975, revolutionary_constitutionalism__us_constitution, theater_ratio, 3, 0.55).
narrative_ontology:measurement(revcon_theater_2026, revolutionary_constitutionalism__us_constitution, theater_ratio, 4, 0.58).

% Extraction over time
narrative_ontology:measurement(revcon_extractiveness_1787, revolutionary_constitutionalism__us_constitution, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(revcon_extractiveness_1865, revolutionary_constitutionalism__us_constitution, base_extractiveness, 1, 0.42).
narrative_ontology:measurement(revcon_extractiveness_1920, revolutionary_constitutionalism__us_constitution, base_extractiveness, 2, 0.4).
narrative_ontology:measurement(revcon_extractiveness_1975, revolutionary_constitutionalism__us_constitution, base_extractiveness, 3, 0.38).
narrative_ontology:measurement(revcon_extractiveness_2026, revolutionary_constitutionalism__us_constitution, base_extractiveness, 4, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(revolutionary_constitutionalism__us_constitution, enforcement_mechanism).
narrative_ontology:affects_constraint(revolutionary_constitutionalism__us_constitution, amendment_difficulty_as_veto_power).
narrative_ontology:affects_constraint(revolutionary_constitutionalism__us_constitution, judicial_supremacy_in_constitutional_interpretation).

% DUAL FORMULATION NOTE:
% The US Constitution reading is one node in a network of revolutionary constitutionalism readings. This constraint (the US Constitution as durable through entrenchment) is upstream of two derived constraints: (1) amendment_difficulty_as_veto_power — the specific mechanism by which Article V creates supermajority veto; (2) judicial_supremacy_in_constitutional_interpretation — the specific mechanism by which interpretation becomes functional amendment. Both downstream constraints have higher extractiveness (closer to snare) because they focus on the specific suppression mechanisms rather than the overall durability frame. The French and Soviet readings are sibling constraints, not downstream. All three readings share the kernel (revolutionary constitutionalism) but differ in their diagnosis of what produces durability, rupture, or facade.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
