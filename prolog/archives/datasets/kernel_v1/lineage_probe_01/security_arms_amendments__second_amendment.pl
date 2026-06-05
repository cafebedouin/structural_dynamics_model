% ============================================================================
% CONSTRAINT STORY: security_arms_amendments__second_amendment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_arms_bearing_right, []).

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
 *   constraint_id: security_arms_amendments__second_amendment
 *   human_readable: Second Amendment Arms-Bearing Right (Originalist Reading)
 *   domain: constitutional_law/rights_protection
 *
 * SUMMARY:
 *   The Second Amendment secures the right to keep and bear arms — a
 *   constraint whose prefatory militia clause and operative right have
 *   divided interpreters for two centuries. This is ONE READING of the
 *   contested kernel 'security_arms_amendments.' The reading instantiated
 *   here is the originalist reading: the Amendment protects an individual
 *   right to bear arms for lawful purposes including self-defense, grounded
 *   in the natural law tradition and the founding generation's distrust of
 *   disarmament. The sister reading (third_amendment) addresses the parallel
 *   founders' concern about military billeting; together they instantiate a
 *   theory of protected domains against military coercion. This story models
 *   the Second Amendment reading at its maximum juridical visibility
 *   (post-Heller doctrine, 2008 onwards) as a tangled rope: genuine
 *   coordination function (enabling lawful arms-bearing culture, deterring
 *   tyranny through armed citizenry) combined with asymmetric extraction
 *   (regulation advocates bear burden of justifying restriction; arms-bearers
 *   invoke constitutional immunity). The constraint's suppression has risen
 *   over two centuries as regulation has expanded and the constitutional
 *   interpretation has crystallized. Theater ratio is moderate (0.48) because
 *   the doctrinal dispute is substantively contested, not performatively
 *   ritualized — courts genuinely grapple with text, history, and precedent
 *   rather than mechanically affirming either pole.
 *
 * KEY AGENTS:
 *   - Arms-Bearers (Individual and Organized): Primary beneficiaries (institutional/arbitrage, also organized/constrained for advocacy groups) — secure the right to acquire, possess, and carry arms; benefit from constitutional shield against disarmament; experience minimal extraction from Amendment
 *   - Gun Regulation Advocates: Primary victims (moderate/constrained) — face constitutional barrier to most direct policy tools; must repeatedly overcome the Amendment; bear burden of justifying regulation as constitutional
 *   - Disarmed or Heavily Regulated Citizens: Secondary victims (powerless/trapped) — nominal beneficiaries of the protective right but experience it as inaccessible in high-regulation jurisdictions; trapped in regulatory regimes they cannot exit
 *   - Arms Manufacturers and Retailers: Institutional beneficiaries (institutional/arbitrage) — coordinate business across state lines; protected from existential threat of confiscation; experience minimal extraction
 *   - Law Enforcement: Moderate actors (moderate/constrained) — constrained by jurisdictional variation; coordinate state monopoly on force through militia framing; experience moderate extraction from interpretive uncertainty
 *   - Constitutional Amendment Coalition: Organized agents (organized/constrained) — treat the Amendment as amendable through Article V; see constraint as temporary scaffolding, not immutable; have structural exit path
 *   - Militia Framing Legacy System: Institutional carrier (institutional/arbitrage) — prefatory militia clause persists as theater while operative right stands independent; maintains constitutional framing despite functional atrophy
 *   - Natural Law Originalists: Analytical observers (analytical/analytical) — treat right as pre-political natural right, secured by text; risk naturalizing contested constitutional claim as discovered law; false-summit candidate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(security_arms_amendments__second_amendment, 0.58).
domain_priors:suppression_score(security_arms_amendments__second_amendment, 0.62).
domain_priors:theater_ratio(security_arms_amendments__second_amendment, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(security_arms_amendments__second_amendment, extractiveness, 0.58).
narrative_ontology:constraint_metric(security_arms_amendments__second_amendment, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(security_arms_amendments__second_amendment, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(security_arms_amendments__second_amendment, tangled_rope).
narrative_ontology:human_readable(security_arms_amendments__second_amendment, "Second Amendment Arms-Bearing Right (Originalist Reading)").
narrative_ontology:topic_domain(security_arms_amendments__second_amendment, "constitutional_law/rights_protection").

domain_priors:requires_active_enforcement(security_arms_amendments__second_amendment).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(security_arms_amendments__second_amendment, 'c06547dd-cddc-4c1e-a116-7541a3561a4a').
narrative_ontology:cs_kernel_codification('c06547dd-cddc-4c1e-a116-7541a3561a4a', fixed_text).
narrative_ontology:cs_authority_grounding('c06547dd-cddc-4c1e-a116-7541a3561a4a', lineage).
narrative_ontology:cs_interpretation_layer_present('c06547dd-cddc-4c1e-a116-7541a3561a4a').
narrative_ontology:cs_reading_relation('c06547dd-cddc-4c1e-a116-7541a3561a4a', security_arms_amendments__third_amendment_quartering_protection, coexists_with).
narrative_ontology:cs_axiom('c06547dd-cddc-4c1e-a116-7541a3561a4a', foundational, self_defense_natural_right).
narrative_ontology:cs_axiom_status(self_defense_natural_right, holdable).
narrative_ontology:cs_axiom_grounding('c06547dd-cddc-4c1e-a116-7541a3561a4a', self_defense_natural_right, deontological).
narrative_ontology:cs_axiom('c06547dd-cddc-4c1e-a116-7541a3561a4a', foundational, disarmament_suppression_constitutional).
narrative_ontology:cs_axiom_status(disarmament_suppression_constitutional, holdable).
narrative_ontology:cs_axiom_grounding('c06547dd-cddc-4c1e-a116-7541a3561a4a', disarmament_suppression_constitutional, empirically_contingent).
narrative_ontology:cs_reference_frame('c06547dd-cddc-4c1e-a116-7541a3561a4a', natural_law_self_defense_right).
narrative_ontology:cs_drift_state('c06547dd-cddc-4c1e-a116-7541a3561a4a', post_heller_doctrine_2008_onwards, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c06547dd-cddc-4c1e-a116-7541a3561a4a', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(security_arms_amendments__second_amendment, security_arms_amendments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(security_arms_amendments__second_amendment, arms_bearers).
narrative_ontology:constraint_beneficiary(security_arms_amendments__second_amendment, self_defense_practitioners).
narrative_ontology:constraint_beneficiary(security_arms_amendments__second_amendment, militia_eligible_citizens).
narrative_ontology:constraint_victim(security_arms_amendments__second_amendment, gun_regulation_implementers).
narrative_ontology:constraint_victim(security_arms_amendments__second_amendment, disarmament_policy_advocates).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISARMED/REGULATED CITIZENS (SNARE) — Citizens in jurisdictions with strict gun controls experience the Amendment's protective scope as inaccessible. The constraint on state disarmament measures is invoked to prevent their exit from a regulatory regime they wish to escape. Maximum extraction: constitutional text that nominally protects them actually protects arms-bearers' exit from regulation while trapping non-arms-bearers in regulatory regimes that criminalize acquisition. No escape route; high suppression of alternative regulatory pathways.
constraint_indexing:constraint_classification(security_arms_amendments__second_amendment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: GUN RIGHTS ADVOCACY ORGS (TANGLED ROPE) — Genuine coordination function: the Amendment secures a common right that enables organized advocacy, collective practice, and intergenerational continuity of arms-bearing culture. Asymmetric extraction: organizations benefit from the constitutional shield while regulation advocates lack equivalent textual anchorage. The enforcement cost is high (continuous litigation) but the coordination payoff (stable right claim) is real. Not pure extraction because the community genuinely coordinates around the right; not pure coordination because the right systematically favors one faction.
constraint_indexing:constraint_classification(security_arms_amendments__second_amendment, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ARMS MANUFACTURERS/RETAILERS (ROPE) — Institutional beneficiaries with arbitrage options. The Amendment creates a coordination mechanism that enables their business operations across state boundaries and prevents wholesale confiscation or prohibition. They experience the constraint as primarily coordinative: the right enables a legal market. The constraint protects them from existential regulatory threats (confiscation, total prohibition) while allowing moderate regulation (licensing, background checks) that they can arbitrage across jurisdictions. Low experienced extraction because their exit options are substantial (relocation, lobbying).
constraint_indexing:constraint_classification(security_arms_amendments__second_amendment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LAW ENFORCEMENT (TANGLED ROPE) — Constrained by jurisdictional variation: the Amendment creates a legal landscape where police must distinguish lawful carry from criminal possession, vary enforcement by state/local jurisdiction, and navigate conflicting mandates. Genuine coordination function: the Amendment constrains vigilantism and clarifies the state's monopoly on legitimate force through the militia framing. Extraction: enforcement costs vary wildly across jurisdictions; confiscation authority is suppressed in some states, enabled in others. Moderate extraction because law enforcement has some agency (prosecutorial discretion) but is structurally bound by jurisdictional variation.
constraint_indexing:constraint_classification(security_arms_amendments__second_amendment, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: GUN VIOLENCE PREVENTION ADVOCATES (SNARE) — Constrained but not trapped; they have policy leverage in some jurisdictions. However, the Amendment creates a permanent structural asymmetry: regulation advocates must repeatedly overcome the constitutional barrier, while arms-bearers invoke it once and shift burden of proof. The constraint suppresses the most direct policy tools (confiscation, universal prohibition) and forces advocates into incremental approaches (background checks, waiting periods, licensing) that face constant constitutional challenge. The extraction: advocates bear the burden of proving regulation is constitutional; arms-bearers bear no burden. Theater ratio low because the doctrinal dispute is substantive, not performative — courts actually deliberate precedent and text rather than ritually affirming either pole.
constraint_indexing:constraint_classification(security_arms_amendments__second_amendment, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: CONSTITUTIONAL AMENDMENT COALITION (SCAFFOLD) — Organized agents seeking to modify the constraint itself through Article V amendment. This perspective treats the Second Amendment as a temporary constitutional settlement (sunset logic: amendable through Article V), not as immutable. The coalition sees the right as scaffolding — coordination infrastructure that solved an 18th-century problem but can be modified via legitimate constitutional process. Low effective extraction because the coalition has a structural exit path (amendment) that preserves legitimacy. Theater ratio low because amendment is the authentic mechanism, not a performative workaround.
constraint_indexing:constraint_classification(security_arms_amendments__second_amendment, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: MILITIA FRAMING (PITON) — The prefatory militia clause is substantially inert in 21st-century jurisprudence. District of Columbia v. Heller (2008) severed the operative right from meaningful militia service requirement. The militia framing persists as constitutional theater — interpreted as a statement of purpose rather than a limiting condition. The constraint functions despite the degradation: the right stands on its own, independent of organized militia. High theater ratio because the militia clause is invoked but not operationalized. Piton classification: the original functional coordination mechanism (militia as check on tyranny) has atrophied, but the textual commitment to militia remains, maintained by legal inertia and interpretive tradition.
constraint_indexing:constraint_classification(security_arms_amendments__second_amendment, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 8: NATURAL LAW ORIGINALISM (MOUNTAIN) — From a civilizational/analytical position, the originalist reading treats the right as a pre-political natural right to self-defense, secured but not created by the Amendment. The constraint emerges as immutable: suppression of natural-law self-defense rights is impossible in principle — any attempt to disarm citizens contradicts a foundational principle of human autonomy that transcends constitutional text. Zero degrees of freedom: no legitimate interpretation can extinguish the core right. However, the structural data (contested victim set, suppression contested, extractiveness of policy effects) reveals this as a false summit: a reading that naturalizes what is actually a contested constitutional claim, not a discovered law of nature. Engine will flag this as FSM candidate.
constraint_indexing:constraint_classification(security_arms_amendments__second_amendment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(security_arms_amendments__second_amendment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(security_arms_amendments__second_amendment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(security_arms_amendments__second_amendment, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(security_arms_amendments__second_amendment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(security_arms_amendments__second_amendment, TR),
    TR >= 0.70.

:- end_tests(security_arms_amendments__second_amendment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The Amendment creates a constitutional asymmetry where arms-bearers invoke a textual protection and regulation advocates must overcome it through constitutional argument. This asymmetry constitutes extraction: the burden of justification falls on those seeking to restrict rights, not those exercising them. However, extractiveness is not maximal (≥0.66 snare territory) because regulation is not eliminated — waiting periods, background checks, licensing, and capacity restrictions have survived constitutional challenge; the extraction is real but not absolute. Suppression (0.62): High. The Amendment explicitly suppresses confiscation and blanket prohibition; judicial doctrine post-Heller extends suppression to many regulatory categories. But suppression is not total (≤0.05 mountain) because the regulatory landscape remains complex — different states have vastly different rules, and constitutional doctrine remains contested. Theater ratio (0.48): Moderate. The prefatory militia clause is substantially inert in post-Heller jurisprudence (driving toward higher theater), but the operative right itself is substantively contested through real doctrinal dispute, not empty ritual (driving toward lower theater). The balance reflects a hybrid: the militia framing is performative legacy, but the core dispute is substantive. Claimed type (tangled_rope): The constraint exhibits both genuine coordination function (enables lawful arms-bearing culture, deters tyranny through armed citizenry) AND asymmetric extraction (burden of proof falls on regulation advocates). Both elements are required for tangled_rope gate; this constraint satisfies them.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence. The arms-bearer sees rope (coordination enabling their culture). The regulation advocate sees snare (systematic suppression of policy alternatives). The disarmed citizen sees snare (nominally protective text that shields more powerful actors). Law enforcement sees tangled rope (genuine state monopoly coordination, but with high complexity costs). The amendment coalition sees scaffold (temporary settlement amenable to revision). The militia clause sees piton (performative legacy). The natural law originalist sees mountain (immutable natural right), but structural evidence flags this as false summit — the right is secured by contested constitutional interpretation, not discovered law of nature. This perspectival range is itself diagnostic: a constraint that appears natural law (mountain) from one position but clearly contingent institutional (tangled rope / snare) from others reveals that the mountain classification is a reading effect, not structural inevitability.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective encodes whether the agent benefits from or bears costs from THIS constraint. Arms-bearers have d ≈ 0.05 (beneficiaries with arbitrage options): the constraint protects them with minimal cost. Regulation advocates have d ≈ 0.85 (victims with constrained exit): the constraint suppresses their preferred policies, forcing them into incremental approaches or constitutional amendment. Law enforcement has d ≈ 0.60 (symmetric: both coordinates state power and faces implementation complexity). Disarmed citizens have d ≈ 0.92 (victims trapped by the constraint when in high-regulation jurisdictions). The amendment coalition has d ≈ 0.50 (symmetric: genuinely sees the constraint as temporary and amendable, but is currently constrained by 235-year-old constitutional text). The natural law originalist has d ≈ 0.72 (analytical observer), but the false-summit signature flags that this position's perceived immutability is perspectival, not structural.
 *
 * MANDATROPHY ANALYSIS:
 *   CONTESTED KERNEL READING: This is the originalist/individual-right reading of the contested kernel 'security_arms_amendments.' The sister reading (third_amendment) models the founders' parallel concern about military quartering. The mandatrophy here is NOT 'which reading is correct?' but 'how do these two readings relate?' The originalist reading of the Second Amendment suppresses disarmament measures (core extraction mechanism); the Third Amendment reading suppresses military billeting measures (distinct extraction mechanism). They coexist as different readings of the same framing principle (protection of private domains against military coercion). The originalist reading itself resolves into tangled rope (not snare) because the genuine coordination function (deterring tyranny through armed citizenry, enabling lawful arms-bearing culture) is real, not merely a cover story for extraction. Regulation advocates are victims, but the constraint does provide coordination benefits to arms-bearers. The mandate-tropism arises from the perspectival gap: the same constraint that coordinates one faction systematically suppresses the alternatives of another. The engine models this as tangled_rope rather than snare because the coordination is genuine, not performative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    militia_clause_functional_scope,
    'Does the prefatory militia clause limit the operative right to militia contexts, or is it merely a statement of purpose?',
    'Historical analysis of founding-era militia practice; comparative constitutional study of similar prefatory clauses; longitudinal case law tracking (pre-Heller vs post-Heller jurisprudence)',
    'If limiting: the right applies only to militia-eligible citizens in organized contexts; suppression of individual carry is constitutionally legitimate. If merely purposive: the right applies broadly to all bearing-arms contexts; only confiscation is suppressed. The debate''s resolution directly determines victim set and suppression scope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(militia_clause_functional_scope, conceptual, 'Whether the militia clause limits the operative right or is merely a statement of purpose').

omega_variable(
    natural_law_vs_constitutional_claim,
    'Does the Amendment secure a pre-political natural right to self-defense, or does it constitute and limit the right through constitutional text?',
    'Historical originalism (founding-era understandings of natural law); jurisprudential analysis of whether the Amendment creates or merely secures a right; comparative constitutional study of how different jurisdictions ground gun rights (textual vs natural law vs statutory)',
    'If natural law: the right is immutable and inalienable; any suppression attempt is inherently illegitimate; the constraint approaches mountain-type inevitability. If constitutional claim: the right is secured by text and subject to textual interpretation; suppression is suppressed only where the text prevents it; the constraint is contingent institutional arrangement (tangled_rope or snare depending on perspective). This omega directly determines whether the mountain classification is genuine or false-summit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constitutional_claim, conceptual, 'Whether the right is natural law or constitutional claim').

omega_variable(
    regulatory_scope_indeterminacy,
    'What categories of regulation (licensing, waiting periods, background checks, capacity limits, class-of-person restrictions, carry location restrictions) are prohibited by the Amendment versus permitted?',
    'Longitudinal case law tracking (Heller and post-Heller doctrine); empirical study of how different regulatory regimes correlate with constitutional vulnerability; comparative state practice (which regulations survive constitutional challenge across jurisdictions)',
    'If scope is narrow (only confiscation and blanket prohibition are suppressed): most regulation is constitutionally legitimate; victim set is arms-bearers with high suppression costs; extractiveness is moderate. If scope is broad (many regulations are suppressed): regulation advocates face severe constraint; victim set is gun violence prevention advocates; extractiveness is high. The scope indeterminacy is the core contested terrain in Second Amendment interpretation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_scope_indeterminacy, empirical, 'Which regulatory categories are prohibited versus permitted by the Amendment').

omega_variable(
    founding_era_weapon_scope,
    'Does the Amendment protect only weapons that existed at the founding (muskets, flintlock pistols) or all weapons in common use?',
    'Historical analysis of founding-era weapon technology and practice; Supreme Court doctrine on technological evolution of protected classes; longitudinal study of which weapon categories face successful prohibition (full-auto conversion kits vs semi-auto rifles vs handguns)',
    'If limited to founding-era weapons: modern firearms may not receive full protection; suppression of modern regulation is constitutionally weaker. If extended to common-use modern weapons: modern arms receive full protection; suppression of modern regulation is constitutionally stronger. The doctrine shifted toward common-use in post-Heller cases; this omega tracks whether that trend continues.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_era_weapon_scope, empirical, 'Whether the Amendment protects only founding-era weapons or all weapons in common use').

omega_variable(
    militia_duty_and_right_decoupling,
    'In a fully professional standing military and decentralized state National Guard system, do citizens retain a constitutional militia duty, and if so, does it limit the arms-bearing right?',
    'Constitutional law analysis of militia duty post-10th Amendment federalism doctrine; historical study of state militia transformation; comparative international study of how constitutional militia clauses function in modern context',
    'If militia duty is defunct: the militia clause is purely historical context, not limiting condition. If militia duty persists but is non-operational: the clause is decorative (piton). If militia duty is active and limiting: the right is constrained to militia-eligible persons. This omega directly drives the debate between militia-dependent and individual-right interpretations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militia_duty_and_right_decoupling, conceptual, 'Whether citizens retain constitutional militia duty in modern military context').

omega_variable(
    confiscation_suppression_vs_regulation_suppression,
    'Does the Amendment suppress only confiscation/prohibition, or does it suppress regulation broadly?',
    'Jurisprudential analysis of pre-Heller vs post-Heller doctrine on what counts as ''regulation'' vs ''infringement''; empirical study of how many regulatory schemes are struck down on Second Amendment grounds post-Heller; comparative constitutional study of rights-protecting clauses and their relationship to regulation',
    'If narrow (only confiscation suppressed): regulation is constitutionally legitimate unless total; extractiveness is moderate for gun-bearers (regulation permitted); victim set is arms-bearers facing per se valid regulations. If broad (regulation is suppressed unless compelling): extractiveness is high for gun-bearers (regulation vulnerable); victim set is regulation advocates; the constraint functions as snare-like suppression of policy alternatives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(confiscation_suppression_vs_regulation_suppression, empirical, 'Whether Amendment suppresses confiscation only or regulation broadly').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(security_arms_amendments__second_amendment, 0, 235).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sa_theater_1791, security_arms_amendments__second_amendment, theater_ratio, 0, 0.25).
narrative_ontology:measurement(sa_theater_1891, security_arms_amendments__second_amendment, theater_ratio, 100, 0.38).
narrative_ontology:measurement(sa_theater_2026, security_arms_amendments__second_amendment, theater_ratio, 200, 0.48).

% Extraction over time
narrative_ontology:measurement(sa_extract_1791, security_arms_amendments__second_amendment, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(sa_extract_1891, security_arms_amendments__second_amendment, base_extractiveness, 100, 0.55).
narrative_ontology:measurement(sa_extract_1991, security_arms_amendments__second_amendment, base_extractiveness, 200, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(sa_supp_1791, security_arms_amendments__second_amendment, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(sa_supp_1891, security_arms_amendments__second_amendment, suppression_requirement, 100, 0.55).
narrative_ontology:measurement(sa_supp_2026, security_arms_amendments__second_amendment, suppression_requirement, 200, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(security_arms_amendments__second_amendment, enforcement_mechanism).
narrative_ontology:affects_constraint(security_arms_amendments__second_amendment, third_amendment_quartering_protection).
narrative_ontology:affects_constraint(security_arms_amendments__second_amendment, standing_army_militia_balance).
narrative_ontology:affects_constraint(security_arms_amendments__second_amendment, state_gun_regulation_doctrine).
narrative_ontology:affects_constraint(security_arms_amendments__second_amendment, collective_versus_individual_rights_framing).

% DUAL FORMULATION NOTE:
% The Second Amendment reading decomposes into three structurally distinct constraints with different ε values: (1) militia_clause_limiting_doctrine (ε ≈ 0.12, Mountain-like — if the militia clause limits the right, few modern weapons qualify) (2) individual_right_self_defense (ε ≈ 0.58, Tangled Rope — individual right with coordination function but extraction asymmetry) (3) regulation_scope_doctrine (ε ≈ 0.68, Snare — once the individual right is secured, regulation advocates face systematic suppression). This story models the full reading (#2) post-Heller; the militia-limiting interpretation would produce different ε. The sister reading (third_amendment) models quartering protection with its own ε reflecting military quartering suppression rather than arms suppression.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(security_arms_amendments__second_amendment, organized, 0.35).
constraint_indexing:directionality_override(security_arms_amendments__second_amendment, moderate, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
