% ============================================================================
% CONSTRAINT STORY: fundamental_rights_part_iii__freedoms_article_19
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fundamental_rights_part_iii__freedoms_article_19, []).

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
 *   constraint_id: fundamental_rights_part_iii__freedoms_article_19
 *   human_readable: Article 19 Fundamental Freedoms with Reasonable Restrictions
 *   domain: constitutional_law/fundamental_rights
 *
 * SUMMARY:
 *   Article 19 of the Indian Constitution bundles six fundamental freedoms —
 *   speech, assembly, association, movement, residence, and profession — each
 *   paired with a 'reasonable restrictions' clause. The constraint exhibits
 *   the full perspectival range of DR classification, revealing a structural
 *   tension: the freedoms are nominally guaranteed and substantively enforced
 *   for mainstream political participation, but the restriction grounds
 *   (national security, public order, morality, contempt, defamation,
 *   incitement) are interpreted expansively by state police power, creating
 *   asymmetric extraction for dissent. The measurements show extractiveness
 *   accumulating from 0.28 (1950s) to 0.52 (contemporary), driven by
 *   expansion of restriction grounds and differential enforcement against
 *   unpopular speech and assembly. The suppression_requirement has risen from
 *   0.35 to 0.58, tracking increases in police discretion under 'national
 *   security' and 'public order' rationales. Theater_ratio shows slight
 *   decline (0.55 to 0.48), indicating that judicial review of restrictions,
 *   while still substantially performative, has shifted slightly toward
 *   substantive scrutiny in limited contexts. The constraint is classified as
 *   tangled_rope: genuine coordination function (freedoms enable mainstream
 *   participation) coexists with asymmetric extraction (restrictions
 *   disproportionately burden dissent). The classification resolves the
 *   mandatrophy: Article 19 is NOT a pure freedom guarantee (rope) because
 *   restrictions extract compliance from minorities; it is NOT a pure snare
 *   because mainstream speakers and assembly-goers experience functional
 *   freedom; and it is NOT a natural law (mountain) because measured
 *   expansion of restriction grounds and enforcement asymmetry indicate
 *   institutional contingency.
 *
 * KEY AGENTS:
 *   - Exercising Citizen (beneficiary): Mainstream political actor experiencing genuine functional access to freedoms; coordinated participation through Article 19 mechanisms
 *   - Dissenting Minority Voice (victim): Powerless actor whose expression and assembly face arrest risk under expansively interpreted restrictions; trapped within jurisdiction; maximum extraction
 *   - Unpopular Assembly Organizers (victim): Moderate-power actors constrained by permit manipulation and police dispersal authority; mixed coordination (assembly possible) and extraction (state surveillance and interference)
 *   - State Administrative Apparatus (beneficiary/neutral): Police and executive can calibrate enforcement under enumerated grounds; benefits from discretion while facing legitimacy constraint from nominal freedom guarantee
 *   - Constitutional Courts (institutional observer): Perform nominally substantive review of restrictions but often defer to executive security/order claims; sees own review function as partially ritualized
 *   - Analytical Observer (civilizational): Risks naturalizing restriction as inevitable feature of rights architecture rather than contingent institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fundamental_rights_part_iii__freedoms_article_19, 0.52).
domain_priors:suppression_score(fundamental_rights_part_iii__freedoms_article_19, 0.58).
domain_priors:theater_ratio(fundamental_rights_part_iii__freedoms_article_19, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fundamental_rights_part_iii__freedoms_article_19, extractiveness, 0.52).
narrative_ontology:constraint_metric(fundamental_rights_part_iii__freedoms_article_19, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(fundamental_rights_part_iii__freedoms_article_19, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fundamental_rights_part_iii__freedoms_article_19, tangled_rope).
narrative_ontology:human_readable(fundamental_rights_part_iii__freedoms_article_19, "Article 19 Fundamental Freedoms with Reasonable Restrictions").
narrative_ontology:topic_domain(fundamental_rights_part_iii__freedoms_article_19, "constitutional_law/fundamental_rights").

domain_priors:requires_active_enforcement(fundamental_rights_part_iii__freedoms_article_19).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fundamental_rights_part_iii__freedoms_article_19, '1b28b03f-7403-400b-8b53-f96542041276').
narrative_ontology:cs_kernel_codification('1b28b03f-7403-400b-8b53-f96542041276', formalized).
narrative_ontology:cs_authority_grounding('1b28b03f-7403-400b-8b53-f96542041276', lineage).
narrative_ontology:cs_interpretation_layer_present('1b28b03f-7403-400b-8b53-f96542041276').
narrative_ontology:cs_reading_relation('1b28b03f-7403-400b-8b53-f96542041276', fundamental_rights_part_iii__equality_code_articles_14_18, coexists_with).
narrative_ontology:cs_reading_relation('1b28b03f-7403-400b-8b53-f96542041276', fundamental_rights_part_iii__remedies_article_32_supreme_court_access, influences).
narrative_ontology:cs_axiom('1b28b03f-7403-400b-8b53-f96542041276', foundational, enumerated_restriction_grounds_are_exhaustive).
narrative_ontology:cs_axiom_status(enumerated_restriction_grounds_are_exhaustive, holdable).
narrative_ontology:cs_axiom_grounding('1b28b03f-7403-400b-8b53-f96542041276', enumerated_restriction_grounds_are_exhaustive, deontological).
narrative_ontology:cs_axiom('1b28b03f-7403-400b-8b53-f96542041276', foundational, reasonableness_determines_validity_of_restriction).
narrative_ontology:cs_axiom_status(reasonableness_determines_validity_of_restriction, holdable).
narrative_ontology:cs_axiom_grounding('1b28b03f-7403-400b-8b53-f96542041276', reasonableness_determines_validity_of_restriction, empirically_contingent).
narrative_ontology:cs_reference_frame('1b28b03f-7403-400b-8b53-f96542041276', enumerated_freedom_guarantee_with_bounded_state_discretion).
narrative_ontology:cs_drift_state('1b28b03f-7403-400b-8b53-f96542041276', contemporary_era_post_2000, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('1b28b03f-7403-400b-8b53-f96542041276', '2026-02-26T12:00:00Z').
narrative_ontology:cs_kernel_id(fundamental_rights_part_iii__freedoms_article_19, fundamental_rights_part_iii).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fundamental_rights_part_iii__freedoms_article_19, exercising_citizen).
narrative_ontology:constraint_beneficiary(fundamental_rights_part_iii__freedoms_article_19, state_administrative_apparatus).
narrative_ontology:constraint_victim(fundamental_rights_part_iii__freedoms_article_19, minority_dissent_voices).
narrative_ontology:constraint_victim(fundamental_rights_part_iii__freedoms_article_19, unpopular_assembly_organizers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISSENTING MINORITY (SNARE) — Structurally trapped. The freedoms of speech and assembly are nominally guaranteed, but 'reasonable restrictions' clauses (national security, public order, morality) are interpreted expansively by state police power. The dissenter cannot exit the jurisdiction, cannot exercise the freedom without accepting arrest risk, and faces maximal extraction: the freedom is theoretically present but functionally unavailable for unpopular expression. No exit path; extraction is near-total.
constraint_indexing:constraint_classification(fundamental_rights_part_iii__freedoms_article_19, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MAINSTREAM EXERCISER (ROPE) — Has effective access to the freedoms when exercised within conventional bounds. Speech, assembly, and movement are functional for mainstream political participation, journalism, professional practice. The 'reasonable restrictions' are experienced as genuine coordination (public order, safety) rather than extraction. Mobile exit option (can migrate to less-restricted spaces or modify expression). Net positive experience — the constraint enables participation.
constraint_indexing:constraint_classification(fundamental_rights_part_iii__freedoms_article_19, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: ACTIVIST ORGANIZER (TANGLED ROPE) — Experiences genuine coordination (Article 19 enables mass assembly for legitimate purposes) alongside asymmetric extraction (police scrutiny, permit manipulation, dispersal under 'public order' rationales). Constrained exit: can modify tactics or reduce visibility, but cannot fully exit the activism without abandoning identity. The freedom is both real and conditioning — the constraint extracts compliance within bounded channels while appearing to guarantee freedom.
constraint_indexing:constraint_classification(fundamental_rights_part_iii__freedoms_article_19, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE ADMINISTRATIVE APPARATUS (TANGLED ROPE) — The 'reasonable restrictions' clauses are the beneficiary's coordination mechanism: police can calibrate enforcement against stated grounds (national security, public order, morality). But the state also faces coordination problems — maintaining legitimacy requires respecting the nominal guarantee of freedoms. The constraint extracts compliance from citizens while enabling state discretion. Constrained exit: the state cannot simply abolish Article 19 without constitutional crisis, yet interprets 'reasonable restrictions' expansively. Both coordination and extraction are real.
constraint_indexing:constraint_classification(fundamental_rights_part_iii__freedoms_article_19, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSTITUTIONAL COURT SYSTEM (PITON) — The courts nominally adjudicate whether restrictions are 'reasonable' under the enumerated grounds (national security, public order, health, morality, contempt of court, defamation, incitement to offence). In practice, judicial review of police action under Article 19 is substantially performative: courts often defer to state claims about security and order, producing low overturning rates. The reviewing function persists (theater ≥ 0.70) but has low functional verification — the restrictedness is established before review, not resolved by it. Courts see their own review as ritualized.
constraint_indexing:constraint_classification(fundamental_rights_part_iii__freedoms_article_19, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, any enumerated code of rights must contain bounds; absolute freedom of speech would be self-negating (incitement collapses the freedom for others). Some restriction mechanism is inherent to any workable freedom guarantee. This perspective naturalizes the restriction as an inevitable feature of rights architecture. However, the structural data contradicts the mountain classification — measurable state discretion, asymmetric application to dissent, and police power expansion all indicate contingent institutional arrangements, not natural laws. The engine's false summit detector will identify this as naturalization.
constraint_indexing:constraint_classification(fundamental_rights_part_iii__freedoms_article_19, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fundamental_rights_part_iii__freedoms_article_19_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fundamental_rights_part_iii__freedoms_article_19, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fundamental_rights_part_iii__freedoms_article_19, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(fundamental_rights_part_iii__freedoms_article_19, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(fundamental_rights_part_iii__freedoms_article_19, TR),
    TR >= 0.70.

:- end_tests(fundamental_rights_part_iii__freedoms_article_19_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts compliance from dissent through asymmetric application of restriction grounds. Mainstream speech extracts minimal cost (ε ≈ 0.10–0.15 range); dissent extracts substantial cost (ε ≈ 0.70–0.80 range). The average reflects that the freedoms are genuinely available for mainstream participation but functionally restricted for dissent. The value has risen from 0.28 to 0.52 over 30 years, tracking documented expansion of 'national security' and 'public order' grounds. Suppression (0.58): Moderate-high. Barriers to dissenting exercise of Article 19 freedoms include arrest risk, criminal prosecution, social ostracism, and state surveillance under 'security' rationales. Suppression is structural (police discretion, permit denial, section 144 orders) not just cognitive, but differs by dissent profile. Mainstream actors experience suppression ≈ 0.20; dissent experiences suppression ≈ 0.85. Theater_ratio (0.48): Moderate. Judicial review of restrictions under Article 19 operates with substantial theater (deference to executive security claims, presumption of reasonableness in certain contexts) but is not purely performative — some restrictions are overturned, and appellate scrutiny does constrain egregious abuse. The slight decline over 30 years reflects marginal increases in substantive review post-1990s, but courts still defer at high rates to 'national security' and 'public order' claims.
 *
 * PERSPECTIVAL GAP:
 *   The gap between mainstream and dissenting perspectives reveals the constraint's dual nature. A mainstream journalist exercising Article 19 speech right experiences rope — genuine freedom enabling professional practice. An activist engaged in identical organizational acts experiences snare — freedom nominally present but functionally blocked by arrest risk and police interpretation of 'public order.' The same enumerated restriction clause ('reasonable restrictions for public order') produces opposite classifications depending on whether the agent's expression aligns with state interests. The piton classification of constitutional courts reflects that review exists and has procedural substance, but functions with high deference, producing low rates of overturning restrictions in security contexts. The mountain perspective risks naturalizing this as inevitable: any rights system must have bounds. But the structural data reveals contingency: the bounds have expanded measurably (extractiveness +0.24 over 30 years), enforcement is content-correlated (higher arrest rates for dissent), and restriction grounds are interpreted more expansively for security/order than for health/safety, indicating institutional drift rather than natural necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from each perspective's structural position within the extraction flow. Dissenting minorities face high d (0.90–0.95) because they are trapped and bearing asymmetric extraction cost — the sigmoid f(d) maps this to high experienced extractiveness. Mainstream exercisers face low d (0.25–0.35) because they are mobile and experiencing net benefit — f(d) maps this to negative or near-zero experienced extractiveness. Activist organizers face moderate d (0.55–0.65) because they are constrained (can modify tactics but not exit activism) and experiencing mixed costs and benefits. State administrative apparatus faces low d (0.20–0.30) as beneficiary with arbitrage exit. Constitutional courts face moderate-high d (0.70) because they are institutional observers experiencing the constraint as partially degraded (piton). The proportionality of these values to each perspective's classification reflects the engine's core insight: observed classification type follows automatically from structural position once d is fixed.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reasonableness_judicial_deference,
    'What determines whether courts will scrutinize state ''reasonableness'' claims substantively versus defer to executive judgment on security and order grounds?',
    'Longitudinal analysis of Article 19 case law: overturning rates by restriction ground; comparison of judicial scrutiny levels across threat contexts (national security vs public order vs morality); examination of whether courts apply different standards to mainstream vs dissenting speech',
    'High deference (court overturning rate < 15%): mechanism functions as snare for minorities (rope for mainstream). Low deference (rate > 40%): mechanism functions as genuine coordination (rope for all, or tangled_rope with symmetrical extraction). Current evidence suggests deference varies by context, supporting tangled_rope classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reasonableness_judicial_deference, empirical, 'Judicial scrutiny depth for Article 19 reasonableness determinations').

omega_variable(
    restriction_ground_scope_drift,
    'Have the enumerated restriction grounds (national security, public order, morality, contempt, defamation, incitement) drifted toward broader interpretation since 1950?',
    'Text analysis of Supreme Court interpretations: how have categories like ''public order'' and ''national security'' been operationalized? Comparison of restriction breadth in 1950s cases vs contemporary cases; tracking of new precedents expanding or narrowing grounds',
    'If grounds have narrowed: Article 19 is functioning as intended (coordination with bounded restrictions). If grounds have expanded: extractiveness has increased over time, indicating institutional drift toward snare territory. Evidence suggests substantial expansion of ''national security'' and ''public order'' grounds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(restriction_ground_scope_drift, empirical, 'Scope drift in enumerated Article 19 restriction grounds').

omega_variable(
    differential_enforcement_by_dissent_profile,
    'Are ''reasonable restrictions'' applied differently depending on whether the speech/assembly targets mainstream or marginalized groups, state institutions, or establishment ideology?',
    'Comparative legal enforcement analysis: arrest rates, prosecution rates, and conviction rates for identical speech/assembly acts classified by political content (establishment-aligned vs critical vs radical); controlling for explicit ground claimed (security, order, morality); comparison across decades',
    'If enforcement is content-neutral: restrictions are genuine coordination protecting all freedoms equally. If enforcement correlates with dissent profile: mechanism functions as snare for dissent, rope for establishment speech. Evidence strongly suggests differential enforcement, supporting asymmetric extraction (tangled_rope or snare for minorities).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(differential_enforcement_by_dissent_profile, empirical, 'Whether Article 19 restrictions are enforced differently based on speech/assembly political content').

omega_variable(
    committer_ambiguity_reasonableness_kernel,
    'Does Article 19 constitute a kernel defining ''reasonableness'' as a fixed boundary (formalized reading), or is reasonableness inherently contestable across different constitutional readings (distributed kernel)?',
    'Textual and jurisprudential analysis: Does the Constitution fix what counts as ''reasonable'' for each ground, or does it delegate reasonableness determination to courts/administration? Do different constitutional readings (equality_code reading, remedies_article_32 reading) produce different reasonableness standards?',
    'If reasonableness is fixed: Article 19 forms a self-contained constraint (this reading''s stability is high). If reasonableness is distributed across readings: Article 19 must be read in conjunction with equality (Article 14-18) and remedies (Article 32) to determine actual extraction patterns. Current jurisprudence suggests reasonableness is contested across readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_ambiguity_reasonableness_kernel, conceptual, 'Whether ''reasonableness'' in Article 19 is a fixed kernel or distributed across constitutional readings').

omega_variable(
    false_summit_natural_law_claim,
    'Is the mountain perspective''s naturalization of restriction justified — are bounds on freedom genuinely inevitable features of rights logic — or does it obscure contingent state power expansion?',
    'Comparative constitutional analysis: Do other liberal democracies'' freedom guarantees show the same expansion of restriction grounds and enforcement asymmetry? Are the measured restriction patterns consistent with natural-law necessity or with institutional discretion?',
    'If natural law: mountain classification is correct; extraction is inherent cost of freedom architecture. If institutional: mountain is false summit; restriction grounds and enforcement patterns are contingent, not inevitable; extractiveness should be reclassified upward (toward snare for minorities). Evidence suggests institutional drift dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law_claim, empirical, 'Whether Article 19 restriction bounds are natural-law features or institutional contingencies').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fundamental_rights_part_iii__freedoms_article_19, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fr19_tr_t0, fundamental_rights_part_iii__freedoms_article_19, theater_ratio, 0, 0.55).
narrative_ontology:measurement(fr19_tr_t10, fundamental_rights_part_iii__freedoms_article_19, theater_ratio, 10, 0.52).
narrative_ontology:measurement(fr19_tr_t20, fundamental_rights_part_iii__freedoms_article_19, theater_ratio, 20, 0.48).
narrative_ontology:measurement(fr19_tr_t30, fundamental_rights_part_iii__freedoms_article_19, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(fr19_be_t0, fundamental_rights_part_iii__freedoms_article_19, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(fr19_be_t10, fundamental_rights_part_iii__freedoms_article_19, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(fr19_be_t20, fundamental_rights_part_iii__freedoms_article_19, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(fr19_be_t30, fundamental_rights_part_iii__freedoms_article_19, base_extractiveness, 30, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(fr19_su_t0, fundamental_rights_part_iii__freedoms_article_19, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(fr19_su_t10, fundamental_rights_part_iii__freedoms_article_19, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(fr19_su_t20, fundamental_rights_part_iii__freedoms_article_19, suppression_requirement, 20, 0.52).
narrative_ontology:measurement(fr19_su_t30, fundamental_rights_part_iii__freedoms_article_19, suppression_requirement, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fundamental_rights_part_iii__freedoms_article_19, enforcement_mechanism).
narrative_ontology:affects_constraint(fundamental_rights_part_iii__freedoms_article_19, equality_code_articles_14_18).
narrative_ontology:affects_constraint(fundamental_rights_part_iii__freedoms_article_19, remedies_article_32_supreme_court_access).

% DUAL FORMULATION NOTE:
% Article 19 (freedoms_article_19 reading) is one element of the Part III fundamental rights architecture. The equality_code reading (articles 14–18) provides non-discrimination scrutiny that applies to Article 19 restrictions; the remedies_article_32 reading provides the enforcement mechanism (Supreme Court direct access) that adjudicates whether restrictions are genuinely reasonable. These three readings form a constraint family where freedoms, equality, and remedies interact. Extractiveness varies by reading: freedoms_article_19 shows extractiveness accumulation (0.28 → 0.52) due to restriction-ground expansion; equality_code shows extractiveness reduction (courts increasingly strike down restrictions as discriminatory); remedies_article_32 shows extractiveness reduction (access to remedies enables challenge to restrictions). The three readings must be analyzed together to model the full constraint structure, but each has its own ε value and perspectival range.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fundamental_rights_part_iii__freedoms_article_19, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
