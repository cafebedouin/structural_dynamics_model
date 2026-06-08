% ============================================================================
% CONSTRAINT STORY: christian_colonial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_christian_colonial_reading, []).

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
 *   constraint_id: christian_colonial_reading
 *   human_readable: Christian Colonial Marriage Authority: Ecclesiastical Tradition Codified and Judicially Enforced
 *   domain: comparative_law/legal_pluralism/constitutional_theory
 *
 * SUMMARY:
 *   This constraint models ONE READING of the marriage_authority_kernel: the
 *   Christian colonial reading, instantiated through ecclesiastical tradition
 *   as codified in secular statute and enforced by judicial apparatus. This
 *   reading grounds marriage authority in two-layered legitimacy: (1)
 *   ecclesiastical tradition (Christian doctrine of indissoluble matrimony,
 *   sacramental character, doctrinal immutability), and (2) colonial
 *   statutory codification (specifically targeting Christian subjects,
 *   encoding doctrine in secular positive law, enabling enforcement through
 *   secular courts). The constraint operates in jurisdictions where this
 *   reading was institutionalized — primarily India's Indian Christian
 *   Marriage Act (ICMA) and similar Christian personal law regimes inherited
 *   from colonial legal pluralism. The reading generates a tangled
 *   coordination-extraction hybrid: it solves genuine marriage governance
 *   problems (property division, custody, inheritance, marital capacity)
 *   through the judicial apparatus (coordination function) while
 *   simultaneously enforcing gendered divorce grounds, coverture doctrine,
 *   and ecclesiastical gatekeeping (extraction function). The structural
 *   tension emerges from the reading's core claim: that ecclesiastical
 *   authority (immune to legislative amendment before 2001) and civil
 *   authority (courts) together define marriage binding — creating a domain
 *   where religious doctrine is simultaneously a matter of private conscience
 *   and public law. The 2001 divorce amendment decriminalized exit without
 *   dismantling the ecclesiastical framing, leaving a constraint that is
 *   functionally degrading (increasing theater_ratio) while empirically
 *   extractive (persistent suppression of women's marital mobility). The
 *   measurement trajectory shows extractiveness declining over 101 years
 *   (1900-2001+) as alternatives accumulate and constitutional pressure
 *   mounts, while theater ratio rises (the statute's enforcement becomes
 *   increasingly performative as its underlying authority erodes). The
 *   constraint is simultaneously resolving (mandatrophy acknowledged: the
 *   founding function of ecclesiastical authority no longer drives judicial
 *   practice) and persisting (institutional inertia maintains the statutory
 *   form).
 *
 * KEY AGENTS:
 *   - Ecclesiastical Establishment: Primary beneficiary (institutional/arbitrage) — captures authority to define matrimonial validity, moral legitimacy, doctrinal interpretation. Enjoys low exit costs (can reinterpret doctrine or align with state) and derives rents from institutional entrenchment.
 *   - Married Woman (Christian): Primary victim (powerless/trapped) — subjected to coverture doctrine, limited divorce grounds until 2001, suppressed agency within marriage governance. Faces high exit costs (judicial intervention required; social stigma; property consequences). Most severely constrained agent.
 *   - Religious Minorities (Hindu, Muslim, Parsi, Sikh): Secondary victim (moderate/constrained) — navigate jurisdictional asymmetry created by legal pluralism. Christian statute remains civilizational default in constitutional texts and secular case law; alternatives require formal legal regime switching. Moderate suppression and moderate exit costs.
 *   - Judicial Enforcement Apparatus: Mixed (organized/constrained) — coordinates legitimate governance function (property, custody, inheritance) while enforcing extraction. Benefits from institutional authority; constrained by appellate oversight and constitutional amendment pressure.
 *   - Colonial Statutory Regime (as institutional form): Degraded institutional form (institutional/arbitrage) — persists through inertia despite foundation (ecclesiastical authority) substantially atrophied. Theater ratio high (enforcement increasingly performative).
 *   - Analytical Observer: Civilizational position (analytical/analytical) — capable of identifying false summit: the reading's claim to natural law status naturalizes what is plainly contingent institutional arrangement dependent on state enforcement.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(christian_colonial_reading, 0.58).
domain_priors:suppression_score(christian_colonial_reading, 0.62).
domain_priors:theater_ratio(christian_colonial_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(christian_colonial_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(christian_colonial_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(christian_colonial_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(christian_colonial_reading, tangled_rope).
narrative_ontology:human_readable(christian_colonial_reading, "Christian Colonial Marriage Authority: Ecclesiastical Tradition Codified and Judicially Enforced").
narrative_ontology:topic_domain(christian_colonial_reading, "comparative_law/legal_pluralism/constitutional_theory").

domain_priors:requires_active_enforcement(christian_colonial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(christian_colonial_reading, '9282a107-c58c-42cb-b318-889e9d9ec2ab').
narrative_ontology:cs_kernel_codification('9282a107-c58c-42cb-b318-889e9d9ec2ab', fixed_text).
narrative_ontology:cs_authority_grounding('9282a107-c58c-42cb-b318-889e9d9ec2ab', extraction).
narrative_ontology:cs_interpretation_layer_present('9282a107-c58c-42cb-b318-889e9d9ec2ab').
narrative_ontology:cs_reading_relation('9282a107-c58c-42cb-b318-889e9d9ec2ab', christian_colonial_reading__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('9282a107-c58c-42cb-b318-889e9d9ec2ab', christian_colonial_reading__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('9282a107-c58c-42cb-b318-889e9d9ec2ab', christian_colonial_reading__parsi_community_reading, coexists_with).
narrative_ontology:cs_reading_relation('9282a107-c58c-42cb-b318-889e9d9ec2ab', christian_colonial_reading__secular_contractual_reading, influences).
narrative_ontology:cs_axiom('9282a107-c58c-42cb-b318-889e9d9ec2ab', foundational, marriage_sacramental_binding_immutable).
narrative_ontology:cs_axiom_status(marriage_sacramental_binding_immutable, holdable).
narrative_ontology:cs_axiom_grounding('9282a107-c58c-42cb-b318-889e9d9ec2ab', marriage_sacramental_binding_immutable, theological).
narrative_ontology:cs_axiom('9282a107-c58c-42cb-b318-889e9d9ec2ab', foundational, ecclesiastical_authority_conjugal_validity_definition).
narrative_ontology:cs_axiom_status(ecclesiastical_authority_conjugal_validity_definition, overridden).
narrative_ontology:cs_axiom_grounding('9282a107-c58c-42cb-b318-889e9d9ec2ab', ecclesiastical_authority_conjugal_validity_definition, deontological).
narrative_ontology:cs_axiom('9282a107-c58c-42cb-b318-889e9d9ec2ab', secondary, indissolubility_divorce_impermissible_except_death).
narrative_ontology:cs_axiom_status(indissolubility_divorce_impermissible_except_death, overridden).
narrative_ontology:cs_axiom_grounding('9282a107-c58c-42cb-b318-889e9d9ec2ab', indissolubility_divorce_impermissible_except_death, theological).
narrative_ontology:cs_reference_frame('9282a107-c58c-42cb-b318-889e9d9ec2ab', ecclesiastical_doctrine_supremacy).
narrative_ontology:cs_drift_state('9282a107-c58c-42cb-b318-889e9d9ec2ab', post_2001_amendment_and_constitutional_review, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('9282a107-c58c-42cb-b318-889e9d9ec2ab', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(christian_colonial_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(christian_colonial_reading, ecclesiastical_establishment).
narrative_ontology:constraint_beneficiary(christian_colonial_reading, patriarchal_property_transmission).
narrative_ontology:constraint_victim(christian_colonial_reading, women_marital_mobility).
narrative_ontology:constraint_victim(christian_colonial_reading, religious_minority_agents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARRIED WOMAN (SNARE) — Trapped by common law doctrine of coverture (civil death upon marriage) codified through Christian marriage statutes. Exit from marriage requires proving cruelty or desertion under narrowly defined statutory grounds; divorce was unavailable until 2001 amendment. No alternatives to the ecclesiastical-judicial framework. Maximum experienced extraction — faces full suppression of agency with no exit option short of judicial intervention.
constraint_indexing:constraint_classification(christian_colonial_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RELIGIOUS MINORITY AGENTS (SNARE) — Constrained by colonial-era legal pluralism that preserves Christian marriage law for Christians but creates jurisdictional asymmetry. Non-Christian agents must navigate parallel personal law regimes while Christian statute remains the civilizational default in constitutional texts and secular case law. Exit requires either religious conversion or formal recognition of alternative personal law — both carry social and legal friction. High experienced extraction despite nominally available alternative regimes.
constraint_indexing:constraint_classification(christian_colonial_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: JUDICIAL ENFORCEMENT APPARATUS (TANGLED ROPE) — Organized agents (courts, bar associations, judges) both coordinate legitimate marriage governance (property settlement, child custody, inheritance) and extract through enforcement of gendered divorce grounds and jurisdictional gatekeeping. Benefits from institutional authority to adjudicate marriage disputes; constrained by appellate oversight and constitutional amendment pressure. Mixed function and extraction — genuine coordination problem (who decides property division?) layered with asymmetric enforcement.
constraint_indexing:constraint_classification(christian_colonial_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ECCLESIASTICAL ESTABLISHMENT (ROPE) — Institutional beneficiary with high exit flexibility (can reinterpret doctrine, align with state authority, or reinvent legitimacy claims). Experiences the constraint as pure coordination: marriage law enforcement maintains ecclesiastical authority to define matrimonial validity and moral legitimacy. Captures rents from this authority structure (marriage sanction, moral weight, institutional entrenchment) but benefits are coordination-adjacent. Rope classification reflects the establishment's experience as coordination with side benefits, not extraction as the primary function.
constraint_indexing:constraint_classification(christian_colonial_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: COLONIAL STATUTORY REGIME AS INSTITUTIONAL FORM (PITON) — The ecclesiastical-colonial statute itself has become largely performative. The 2001 divorce amendment decriminalized exit without entirely dismantling the ecclesiastical framing. Courts apply the statute mechanically; bishops no longer actively defend it; constitutional courts treat it as an embarrassing remnant. The constraint persists through institutional inertia (inherited from colonial codification) despite the founding function (ecclesiastical moral authority over marriage) having substantially atrophied. Theater ratio reflects that much judicial activity around Christian marriage law is now ritual compliance rather than active doctrine enforcement.
constraint_indexing:constraint_classification(christian_colonial_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective grounded in ecclesiastical natural law tradition, Christian marriage authority derives from immutable moral law (divine authority, doctrinal continuity, metaphysical binding). This reading sees the constraint as crystallizing unchangeable truths about matrimonial obligation. However, the structural data contradicts the mountain classification: the constraint depends on colonial statutory codification, secular judicial enforcement, and modern constitutional amendment. The 'immutable law of nature' framing naturalizes what is plainly a contingent institutional arrangement, making this a false summit — a mountain called by ecclesiastical authority but constructed through state power.
constraint_indexing:constraint_classification(christian_colonial_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(christian_colonial_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(christian_colonial_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(christian_colonial_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(christian_colonial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(christian_colonial_reading, TR),
    TR >= 0.70.

:- end_tests(christian_colonial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, declining trajectory. The constraint extracts from married women through limited divorce grounds (prior to 2001 amendment), coverture doctrine, and gendered property rights; from religious minorities through jurisdictional asymmetry and legal-regime-switching costs; from courts through enforcement labor. However, extraction is not maximal (as it would be in a pure snare) because the judicial apparatus genuinely coordinates legitimate marriage governance problems (custody, property division, inheritance). The measurement trajectory shows decline from 0.65 (1900s — maximum ecclesiastical control) to 0.42 (post-2001 amendment — divorce available on gender-neutral grounds), reflecting that decriminalization of exit substantially reduces extraction despite the statute's form remaining intact. Suppression (0.62): Moderate-high, declining trajectory. Married women faced insurmountable barriers to divorce until 2001 (cruelty and desertion as only grounds; judicial gatekeeping; social stigma). Religious minorities face persistent barriers to legal-regime switching (formal conversion costs, documentation friction, community pressure). Courts face pressures from constitutional review that limit enforcement discretion. The measurement trajectory shows suppression declining from 0.80 (1900s) to 0.45 (post-2001), reflecting both amendment-driven decriminalization and constitutional limitation of judicial discretion. Theater ratio (0.48): Moderate, increasing trajectory. Early period (1900s) shows low theater (0.22) — ecclesiastical authority was genuinely ascendant and judges enforced doctrine as binding. Modern period (post-2001) shows high theater (0.65) — courts apply the statute mechanically; ecclesiastical doctrine is no longer the locus of authority; compliance is ritualized rather than genuine. The rise in theater reflects the constraint's transition toward piton status: the institutional form persists while the underlying function has substantially atrophied.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates acute perspectival divergence. The ecclesiastical establishment experiences it as pure rope (coordination of marriage governance with authority side benefit); the married woman experiences it as snare (trapped with suppressed agency); religious minorities experience it as snare (forced into asymmetric legal pluralism); courts experience it as tangled_rope (mixed coordination and extraction labor); the statute itself, as institutional form, is piton (theater-heavy, inertial); the natural law observer risks seeing mountain (immutable ecclesiastical truth) but the structural data reveals false summit (state-enforced codification, not doctrine-driven inevitability). The gap between ecclesiastical establishment's experience (pure coordination, rope) and married woman's experience (pure extraction, snare) reveals that the 'coordination' function is one-directional: it coordinates marriage governance to the benefit of ecclesiastical authority and patriarchal property transmission, while coordinating constraint on married women's agency. This is why tangled_rope is the claimed type — the constraint genuinely solves a coordination problem (who decides property division?) but its solution asymmetrically distributes extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from structural position: power level, exit options, and beneficiary/victim status. Ecclesiastical establishment: institutional power + arbitrage exit + beneficiary status ⟹ d ≈ 0.15 (low directionality, low effective extraction — benefits from constraint). Married woman: powerless + trapped + victim status ⟹ d ≈ 0.95 (high directionality, high effective extraction — maximum extraction experienced). Religious minority agents: moderate power + constrained exit + victim status ⟹ d ≈ 0.70 (high-moderate directionality, high effective extraction). Judicial apparatus: organized power + constrained exit + mixed beneficiary/victim ⟹ d ≈ 0.50 (symmetric extraction and benefit). The engine's χ computation scales effective extraction by these d values and by scope (national scope = modest scaling, not maximum). The perspectival gap in experienced extraction (married woman's chi ≈ 0.85 vs ecclesiastical establishment's χ ≈ -0.10) is the core diagnostic signal that the constraint is tangled_rope, not rope.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED (base_properties.mandatrophy_resolved: true). The constraint's founding mandate was ecclesiastical authority definition of marriage binding — the reading's core function was to enforce doctrinal immutability against both secular legislative amendment and non-Christian alternative framings. This mandate has substantially outlived its function: (1) The 2001 divorce amendment decriminalized exit on gender-neutral grounds, stripping ecclesiastical gatekeeping authority over dissolution. (2) Constitutional equal protection jurisprudence (post-Navtej Singh Johar, 2018 — though this constraint predates that decision) undermines the gendered authority structure. (3) Secular contractual readings of marriage (in constitutional jurisprudence and legislative reform momentum) have displaced ecclesiastical authority as the civilizational default. The statute persists not because ecclesiastical doctrine demands enforcement but because institutional inheritance preserved it — making the constraint now a piton masquerading as tangled_rope. The measurement trajectory (theater_ratio rising from 0.22 to 0.65) and (suppression declining from 0.80 to 0.45) reflect this transition: the constraint's extraction mechanism is collapsing even as its institutional form persists. A genuine resolution would require either (A) ecclesiastical re-establishment of active doctrinal authority (reverse the piton trajectory — unlikely), or (B) complete disestablishment of the statute (convert mandate resolution into legal reform). The reading's persistence despite mandate-function gap is exactly what mandatrophy designates: a constraint whose founding authority has eroded while institutional form endures through inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ecclesiastical_authority_grounding_status,
    'Does this reading ground marriage authority in genuine ecclesiastical doctrine or in colonial statutory reification of authority that lacks intrinsic ecclesiastical foundation?',
    'Historical analysis of ecclesiastical doctrine pre-colonial and post-colonial; comparison of reading''s claimed doctrine against pre-colonial Christian marriage jurisprudence; examination of whether codification preserved or invented the doctrine',
    'If doctrine pre-colonial: reading''s legitimacy is traditional-lineage grounded. If doctrine invented via codification: reading is extraction masked as tradition, and classification shifts from tangled_rope toward snare across institutional perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ecclesiastical_authority_grounding_status, empirical, 'Whether ecclesiastical authority grounding is genuine pre-colonial doctrine or colonial statutory invention').

omega_variable(
    kernel_reading_alternative_competence,
    'Did Hindu/Muslim/Parsi personal law regimes provide functionally equivalent marriage governance, making the Christian statute redundant, or does the Christian statute impose unique constraints unavailable under alternatives?',
    'Comparative analysis of personal law regimes on: divorce grounds, property division, custody law, marital capacity. Measurement of actual exit costs for agents who switched legal regimes (conversion, informal adoption of alternative law).',
    'If alternatives equivalent: Christian statute is purely extractive (coordinate through Hindu law or Muslim law as readily; choosing Christian law is forced coercion). If alternatives structurally different: Christian statute coordinates a distinct function, supporting tangled_rope classification. If Christian statute uniquely advantageous to some agents: beneficiary set expands beyond ecclesiastical establishment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_alternative_competence, empirical, 'Functional equivalence of Hindu, Muslim, Parsi personal law alternatives to Christian marriage regime').

omega_variable(
    reading_forecast_trajectory,
    'Will this reading persist as a live constraint or accelerate toward complete disestablishment following the 2001 amendment and constitutional marriage equality pressure?',
    'Longitudinal tracking of: judicial citations to Christian statutory grounds vs. constitutional privacy/equality provisions; legislative momentum on uniform civil code; institutional church defense or abandonment of statutory authority; marital outcomes (divorce rates, property division asymmetry) pre- and post-2001.',
    'If persistent: reading remains tangled_rope with stable extraction. If accelerating disestablishment: reading is transitional scaffold with shortened sunset (not formally declared but empirically visible). If complete abandonment by 2035: reading becomes historical piton (institutional theater maintaining form without function).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_forecast_trajectory, empirical, 'Persistence or disestablishment trajectory of Christian colonial marriage authority post-2001 amendment').

omega_variable(
    kernel_identity_natural_law_or_constructed,
    'Is the ecclesiastical-colonial marriage authority genuinely grounded in irreducible natural law (theological/metaphysical), or is it a constructed institutional arrangement that has succeeded in naturalizing itself through state enforcement?',
    'Theological comparative analysis: whether Christian doctrine as held by this reading''s proponents makes the constraint logically inevitable or whether alternative Christian readings would produce different constraints. Constitutional analysis: whether secular legal systems can sustain equivalent marriage governance without ecclesiastical framing. Historical analysis: whether the constraint''s apparent immutability derives from doctrine or from enforcement capacity.',
    'If truly natural law: mountain classification is accurate, and the constraint is invariant across all institutional arrangements. If constructed: false summit is confirmed, classification shifts toward snare/tangled_rope, and the constraint''s persistence depends on active enforcement rather than inherent logical necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_identity_natural_law_or_constructed, conceptual, 'Whether ecclesiastical-colonial marriage authority is natural law or constructed institutional arrangement').

omega_variable(
    reading_versus_sibling_kernel_foreclosure,
    'Do the christian_colonial_reading''s foundational axioms logically foreclose the secular_contractual_reading, or do both remain live positions within plural legal frameworks?',
    'Formal analysis of the reading''s core claims (ecclesiastical authority supremacy, moral law immutability) against secular reading''s claims (marriage as private contract, state neutrality on doctrine). Test: can a single constitutional framework hold both readings simultaneously, or does adoption of one reading logically eliminate the other?',
    'If foreclosure: the two readings compete zero-sum; dominance of one reading requires elimination of the other. If coexistence: both readings persist as faction-specific framings within plural legal systems. Classification of reading_relations (forecloses vs coexists_with) depends on this resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_versus_sibling_kernel_foreclosure, conceptual, 'Logical foreclosure or coexistence between christian_colonial and secular_contractual marriage authority readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(christian_colonial_reading, 0, 101).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ccr_theater_1900, christian_colonial_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(ccr_theater_1950, christian_colonial_reading, theater_ratio, 50, 0.35).
narrative_ontology:measurement(ccr_theater_2000, christian_colonial_reading, theater_ratio, 100, 0.48).
narrative_ontology:measurement(ccr_theater_2001_post_amendment, christian_colonial_reading, theater_ratio, 101, 0.65).

% Extraction over time
narrative_ontology:measurement(ccr_extractiveness_1900, christian_colonial_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(ccr_extractiveness_1925, christian_colonial_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement(ccr_extractiveness_1950, christian_colonial_reading, base_extractiveness, 50, 0.58).
narrative_ontology:measurement(ccr_extractiveness_2000, christian_colonial_reading, base_extractiveness, 100, 0.48).
narrative_ontology:measurement(ccr_extractiveness_2001_post_amendment, christian_colonial_reading, base_extractiveness, 101, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(ccr_suppression_1900, christian_colonial_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(ccr_suppression_1950, christian_colonial_reading, suppression_requirement, 50, 0.68).
narrative_ontology:measurement(ccr_suppression_2000, christian_colonial_reading, suppression_requirement, 100, 0.62).
narrative_ontology:measurement(ccr_suppression_2001_post_amendment, christian_colonial_reading, suppression_requirement, 101, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(christian_colonial_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(christian_colonial_reading, hindu_codified_reading).
narrative_ontology:affects_constraint(christian_colonial_reading, muslim_shariat_reading).
narrative_ontology:affects_constraint(christian_colonial_reading, parsi_community_reading).
narrative_ontology:affects_constraint(christian_colonial_reading, secular_contractual_reading).

% DUAL FORMULATION NOTE:
% The marriage_authority_kernel decomposes into five structurally distinct constraints, one per major reading in Indian legal pluralism. Each reading has its own ε (extraction profile), its own beneficiary/victim structure, its own suppression mechanism, and its own exit-cost architecture. The christian_colonial_reading instantiates ecclesiastical tradition encoded through state statute; sibling readings instantiate alternative authority framings (Hindu dharma, Islamic jurisprudence, Parsi community, secular contract). The ε-invariance principle applies: these are not five perspectives on one constraint, but five distinct constraints sharing a kernel. The engine's network contamination protocol enables cross-reading coupling analysis — how does disestablishment of the christian_colonial constraint affect the stability of sibling readings? Conversely, how does the existence of functional alternatives (hindu_codified_reading) reduce the suppression and extraction experienced under this reading?

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
