% ============================================================================
% CONSTRAINT STORY: later_amendment_eras__civil_rights_era_amendments
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_later_amendment_eras__civil_rights_era_amendments, []).

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
 *   constraint_id: later_amendment_eras__civil_rights_era_amendments
 *   human_readable: Civil Rights Era Franchise Amendments (23rd, 24th, 25th, 26th Amendments)
 *   domain: political/legal/constitutional
 *
 * SUMMARY:
 *   The Civil Rights era amendments (23rd, 24th, 25th, and 26th Amendments,
 *   ratified 1961–1971) collectively dismantled key franchise gatekeeping
 *   devices that had enabled the extraction of political power from poor,
 *   young, and minority voters. The poll tax (abolished by the 24th Amendment
 *   in 1964) was a direct extraction mechanism: poor voters faced a price
 *   barrier to voting, and Southern states collected revenue from the tax
 *   while using it to suppress Black voter participation post-Reconstruction.
 *   The 23rd Amendment (1961) granted presidential voting rights to D.C.
 *   residents, removing their exclusion from the highest office. The 26th
 *   Amendment (1971) extended the franchise to eighteen-year-olds, removing
 *   age-based gatekeeping. The 25th Amendment (1967), while primarily
 *   addressing presidential succession, also implicitly legitimized broader
 *   democratic participation norms that supported the other expansions. This
 *   constraint is ONE READING of the contested 'later_amendment_eras' kernel
 *   — a reading that emphasizes suppression of franchise gatekeeping and
 *   beneficiaries as the previously disenfranchised. Sibling readings
 *   (Reconstruction amendments, Progressive era amendments, structural
 *   housekeeping) emphasize different aspects of constitutional amendment
 *   dynamics: founding moments vs. procedural maintenance vs.
 *   economic/political restructuring. This reading's distinctiveness lies in
 *   its focus on the specific extraction mechanism of the poll tax and the
 *   specific beneficiary populations (poor voters, D.C. residents, young
 *   voters) whom it targeted. The constraint's extractiveness declines
 *   sharply over the measurement interval (0.65 → 0.18) as the amendments
 *   move from proposal through ratification to enforcement. Suppression
 *   declines as legal gatekeeping mechanisms are removed, but remains
 *   elevated (0.62 post-amendment) because secondary gatekeeping mechanisms
 *   (voter purges, precinct closures, voter ID requirements) partially
 *   persist. Theater is low throughout because the suppression mechanism is
 *   primarily structural (legal barriers, resource allocation) rather than
 *   performative.
 *
 * KEY AGENTS:
 *   - Disenfranchised poor voters: Primary victims (powerless/trapped) — bear full cost of poll taxes and gatekeeping barriers; experience the constraint as unmovable suppression with no coordination function
 *   - D.C. residents: Beneficiaries with compound status (moderate/constrained) — excluded from presidential voting despite full federal taxation; gain voting power through 23rd Amendment but remain subordinate to Congress
 *   - Young voters (age 18-20): Beneficiaries (moderate/mobile) — excluded by age-based gatekeeping; gain franchise through 26th Amendment with lower cost than poll-tax abolition required
 *   - Southern restrictionist state regimes: Primary extractors/victims of amendment (institutional/arbitrage pre-amendment → constrained post-amendment) — benefit from poll tax revenue and suppression mechanisms; lose gatekeeping power as amendments are enforced
 *   - Civil rights and voting rights advocacy movements: Organized beneficiaries (organized/constrained) — coordinate disenfranchised populations and federal allies to push ratification; bear costs of organizing under hostile conditions
 *   - Federal government (Congress and courts): Enforcer/beneficiary (institutional/arbitrage) — enforces amendments through Voting Rights Act, federal examiners, and judicial oversight; gains legitimacy from expanded democratic participation; benefits from compliance framing
 *   - Analytical observer: Civilizational view (analytical/analytical) — risks naturalizing contingent political victories as inevitable democratic logic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(later_amendment_eras__civil_rights_era_amendments, 0.18).
domain_priors:suppression_score(later_amendment_eras__civil_rights_era_amendments, 0.62).
domain_priors:theater_ratio(later_amendment_eras__civil_rights_era_amendments, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(later_amendment_eras__civil_rights_era_amendments, extractiveness, 0.18).
narrative_ontology:constraint_metric(later_amendment_eras__civil_rights_era_amendments, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(later_amendment_eras__civil_rights_era_amendments, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(later_amendment_eras__civil_rights_era_amendments, tangled_rope).
narrative_ontology:human_readable(later_amendment_eras__civil_rights_era_amendments, "Civil Rights Era Franchise Amendments (23rd, 24th, 25th, 26th Amendments)").
narrative_ontology:topic_domain(later_amendment_eras__civil_rights_era_amendments, "political/legal/constitutional").

domain_priors:requires_active_enforcement(later_amendment_eras__civil_rights_era_amendments).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(later_amendment_eras__civil_rights_era_amendments, 'a86aa6b3-85e9-4358-a385-c982a5ead663').
narrative_ontology:cs_kernel_codification('a86aa6b3-85e9-4358-a385-c982a5ead663', formalized).
narrative_ontology:cs_authority_grounding('a86aa6b3-85e9-4358-a385-c982a5ead663', lineage).
narrative_ontology:cs_interpretation_layer_present('a86aa6b3-85e9-4358-a385-c982a5ead663').
narrative_ontology:cs_reading_relation('a86aa6b3-85e9-4358-a385-c982a5ead663', later_amendment_eras__reconstruction_amendments, influences).
narrative_ontology:cs_reading_relation('a86aa6b3-85e9-4358-a385-c982a5ead663', later_amendment_eras__progressive_era_amendments, coexists_with).
narrative_ontology:cs_reading_relation('a86aa6b3-85e9-4358-a385-c982a5ead663', later_amendment_eras__structural_housekeeping_amendments, coexists_with).
narrative_ontology:cs_axiom('a86aa6b3-85e9-4358-a385-c982a5ead663', foundational, poll_tax_and_gatekeeping_suppression_impermissible).
narrative_ontology:cs_axiom_status(poll_tax_and_gatekeeping_suppression_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('a86aa6b3-85e9-4358-a385-c982a5ead663', poll_tax_and_gatekeeping_suppression_impermissible, deontological).
narrative_ontology:cs_axiom('a86aa6b3-85e9-4358-a385-c982a5ead663', secondary, federal_power_prerequisite_for_franchise_protection).
narrative_ontology:cs_axiom_status(federal_power_prerequisite_for_franchise_protection, holdable).
narrative_ontology:cs_axiom_grounding('a86aa6b3-85e9-4358-a385-c982a5ead663', federal_power_prerequisite_for_franchise_protection, instrumental).
narrative_ontology:cs_reference_frame('a86aa6b3-85e9-4358-a385-c982a5ead663', federal_republic_universal_suffrage_aspiration).
narrative_ontology:cs_drift_state('a86aa6b3-85e9-4358-a385-c982a5ead663', contemporary_post_amendment_enforcement_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a86aa6b3-85e9-4358-a385-c982a5ead663', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(later_amendment_eras__civil_rights_era_amendments, later_amendment_eras).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(later_amendment_eras__civil_rights_era_amendments, disenfranchised_poor_voters).
narrative_ontology:constraint_beneficiary(later_amendment_eras__civil_rights_era_amendments, district_of_columbia_residents).
narrative_ontology:constraint_beneficiary(later_amendment_eras__civil_rights_era_amendments, young_voters_under_21).
narrative_ontology:constraint_beneficiary(later_amendment_eras__civil_rights_era_amendments, african_american_voters_targeted_by_poll_tax).
narrative_ontology:constraint_victim(later_amendment_eras__civil_rights_era_amendments, southern_restrictionist_regimes).
narrative_ontology:constraint_victim(later_amendment_eras__civil_rights_era_amendments, state_gatekeeping_authorities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRE-AMENDMENT DISENFRANCHISED VOTER (SNARE) — Poll taxes, literacy tests, and residency barriers trap poor voters in a pure extraction regime. No alternatives exist; gatekeeping is enforced through legal machinery. The constraint prior to amendment is experienced as immovable suppression with no coordination function. Maximum extraction from a powerless, trapped agent at national scope.
constraint_indexing:constraint_classification(later_amendment_eras__civil_rights_era_amendments, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REFORM COALITION IN RESTRICTIONIST STATES (TANGLED ROPE) — Civil rights and voting rights advocates face high costs (arrest, violence, economic retaliation) but also generate coordination benefits by uniting disenfranchised populations. The amendments represent both genuine movement power (coordination function) and externally enforced change (requiring federal intervention to suppress state gatekeeping). Constrained exit at regional scope reflects that exit from anti-voting organizing in the South is possible but costly.
constraint_indexing:constraint_classification(later_amendment_eras__civil_rights_era_amendments, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: FEDERAL AUTHORITY POST-AMENDMENT (ROPE) — Federal enforcement of the amendments (via Voting Rights Act mechanisms, federal examiners, federal oversight) sees the constraint as pure coordination: extending the franchise solves the collective action problem of assuring all citizens can vote, without imposing asymmetric extraction on the federal level. The federal authority is the beneficiary of its own enforcement — it gains legitimacy through expansion of democratic participation. Low experienced extraction from a position of institutional power with arbitrage access to alternative governance models.
constraint_indexing:constraint_classification(later_amendment_eras__civil_rights_era_amendments, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: AMENDMENT RATIFICATION AS TEMPORARY COORDINATION (SCAFFOLD) — The constitutional amendment process itself is a temporary coordination mechanism with a sunset: once ratified, the amendment becomes permanent constitutional law. The ratification campaign (roughly 1960–1971 for the four amendments) exhibits scaffold properties: coordinating disparate state legislatures and public opinion, low effective extraction during the campaign window, clear endpoint at ratification. Post-ratification, the amendment transitions to mountain or rope status as law, not as a coordination problem. Theater is low during ratification because the mechanism is transparent (public legislative votes).
constraint_indexing:constraint_classification(later_amendment_eras__civil_rights_era_amendments, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: RESIDUAL GATEKEEPING APPARATUS POST-AMENDMENT (PITON) — State election officials who previously wielded poll taxes and literacy tests face the new constitutional requirement but maintain vestigial gatekeeping practices and voter suppression theater (purge lists, closing polling places in minority neighborhoods, voter ID requirements not directly addressed by the amendments). The apparatus persists through institutional inertia long after its primary legal instrument (the poll tax) is abolished. Theater is high because enforcement of suppression increasingly relies on informal, performative mechanisms rather than explicit law. Extractiveness is low because the primary extraction mechanism (the poll tax revenue) is gone, but suppression persists.
constraint_indexing:constraint_classification(later_amendment_eras__civil_rights_era_amendments, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, democratic maturation inexorably expands the franchise as a natural historical law: all democracies eventually enfranchise excluded groups, driven by logic internal to democratic legitimacy itself. Suppression and gatekeeping are temporary deviations from the underlying democratic trajectory. This perspective sees the amendments as inevitable products of democratic logic rather than contested political achievements. The engine flags this as a false summit: the amendments were not inevitable; they were hard-won political victories against determined resistance. The 'naturalness' of franchise expansion is a cover story that obscures the power required to overcome restrictionist regimes.
constraint_indexing:constraint_classification(later_amendment_eras__civil_rights_era_amendments, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(later_amendment_eras__civil_rights_era_amendments_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(later_amendment_eras__civil_rights_era_amendments, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(later_amendment_eras__civil_rights_era_amendments, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(later_amendment_eras__civil_rights_era_amendments, TR),
    TR >= 0.70.

:- end_tests(later_amendment_eras__civil_rights_era_amendments_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18 final): Low. The poll tax was a direct extraction mechanism — it extracted money from poor voters while suppressing their participation. Abolishing the poll tax removes this revenue stream and suppression tool. Post-amendment extractiveness is low because the primary extraction mechanism is gone, though secondary suppression mechanisms (voter purges, precinct closures, voter ID laws not addressed by these amendments) persist. The 23rd Amendment imposes minimal extraction on the federal system; presidential voting for D.C. is a coordination benefit. The 26th Amendment is similarly low-extraction — eighteen-year-old suffrage expands the electorate without imposing asymmetric costs on the federal or state systems (except loss of voting power concentration through age-restricted franchise). The temporal decline in extractiveness (0.65 → 0.18) reflects the progressive removal of gatekeeping mechanisms through the measurement interval. Suppression (0.62 final): Moderate-high. Legal suppression mechanisms (poll taxes, literacy tests, residency requirements) are explicitly abolished by constitutional amendment, dramatically reducing formal suppression. However, secondary suppression mechanisms persist: voter purges (many targeting minority voters under the guise of list maintenance), precinct closures in minority neighborhoods, voter ID requirements, provisional ballot procedures, and felony disenfranchisement. The decline in suppression (0.88 → 0.62) reflects removal of explicit legal barriers but not elimination of informal gatekeeping. Theater (0.35 final): Moderate-low. The actual suppression mechanisms (poll taxes, literacy tests, age restrictions) are structural and transparent — they are codified in law or practice, not performative. The amendment ratification process itself has low theater (transparent legislative voting). However, secondary suppression mechanisms that emerge post-amendment (voter purges framed as 'list maintenance,' precinct closures framed as 'efficiency improvements,' voter ID requirements framed as 'election security') exhibit higher theater — the suppression is performatively reframed as administrative necessity rather than gatekeeping. The rising theater over time reflects increasing reliance on informal, reframed mechanisms as explicit legal barriers become constitutionally impermissible.
 *
 * PERSPECTIVAL GAP:
 *   The disenfranchised voter's perspective (snare pre-amendment) and the federal authority's perspective (rope post-amendment) represent the core perspectival gap. Before amendment, the constraint is experienced as pure extraction and suppression with no coordination function (snare). After amendment, the constraint is experienced as coordination (enabling all citizens to vote) with minimal asymmetric extraction on the federal level (rope). The civil rights coalition sees the constraint as tangled rope — genuine movement power (coordination) mixed with asymmetric costs borne by organizing under hostile conditions. The analytical observer risks seeing the amendments as an inevitable natural law of democratic maturation (mountain), but the structural data reveals this as a false summit: the amendments were contingent political victories that required sustained organization, federal intervention, and overcoming determined resistance. The perspectival gap reveals that the 'inevitability' framing naturalizes contested institutional achievements.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective derives from structural position: beneficiary status, power level, and exit options determine how much extraction the agent experiences. Disenfranchised poor voters (powerless/trapped) experience maximum extraction prior to amendment — they have no exit option and bear the full cost of gatekeeping. The civil rights coalition (organized/constrained) experiences moderate extraction — they are organized enough to coordinate and create federal pressure, but face high costs (violence, arrest, economic retaliation) from restrictionist regimes. Federal authority (institutional/arbitrage) experiences negative extraction — it is the beneficiary of expanded franchise, which enhances its legitimacy. Southern restrictionist regimes experience the amendments as extraction removal (their privilege is reduced), but this is not extraction directed at them in the classical sense — rather, their monopoly on franchise control is removed. The measurement interval shows declining d values for the victim agents (as their exit options improve post-amendment) and increasing d values for the restrictionist regimes (as their enforcement capacity erodes). No override is needed; the structural derivation produces the correct directionality across all perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy through temporal dynamics: the same structural phenomenon appears as pure extraction (snare) before amendment, as tangled coordination-and-extraction during the ratification period (tangled rope), and as near-pure coordination (rope) post-amendment enforcement. The mandatrophy is resolved by recognizing that extractiveness itself changed as the legal mechanism changed. The initial measurement (ε=0.65, 1960) reflects the poll tax as active extraction mechanism. The final measurement (ε=0.18, 1970) reflects the poll tax as abolished legal mechanism with secondary suppression remaining. The constraint does not waver between coordination and extraction — rather, the enforcement landscape shifted the balance from extraction-dominant to coordination-dominant. The piton perspective (residual gatekeeping theater post-amendment) represents the tension between formal amendment and persistent informal suppression, capturing how institutional inertia keeps gatekeeping mechanisms alive long after their primary legal instrument is removed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    poll_tax_extractiveness_measurement,
    'Is the poll tax primarily a revenue mechanism (extracting money from poor voters) or a gatekeeping mechanism (pricing poor voters out of participation)?',
    'Historical accounting of poll tax revenues vs. enforcement costs; comparison of revenue impact across states with differential poll tax amounts; analysis of state justifications for maintaining vs. repealing the tax',
    'If primarily revenue: abolition represents significant economic extraction loss for state governments (ε increase post-abolition). If primarily gatekeeping: abolition removes pure suppression mechanism with minimal revenue function (ε decrease post-abolition). The constraint''s extractiveness depends on this distinction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(poll_tax_extractiveness_measurement, empirical, 'Whether the poll tax functioned as revenue or gatekeeping').

omega_variable(
    amendment_inevitability_vs_contingency,
    'Were the Civil Rights era amendments inevitable products of democratic logic, or contingent political victories that could have failed to achieve ratification?',
    'Historical counterfactual analysis: tracking opposition to each amendment, how close ratification votes were, what political conditions would have changed outcomes. Comparison with democracies that delayed franchise expansion without constitutional crisis.',
    'If inevitable: mountain perspective is defensible (natural law of democratic maturation). If contingent: mountain is false summit (naturalizes contested institutional arrangement). This determines whether the analytical observer''s perspective should classify as mountain or as tangled_rope concealed as mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_inevitability_vs_contingency, conceptual, 'Whether amendments followed inevitable democratic logic or contingent political victories').

omega_variable(
    federal_enforcement_extraction,
    'Does federal enforcement of the Civil Rights era amendments represent coordination (expanding the franchise for all) or extraction (federal power imposing its will on resistant states)?',
    'Examination of voting outcomes pre- and post-enforcement; analysis of whether voting rights actually expanded for target populations or merely created formal legal rights; study of secondary suppression mechanisms (voter intimidation, purging, precinct closure) that emerged post-amendment in response to federal enforcement',
    'If coordination: federal perspective is genuinely rope (low extraction). If extraction: federal enforcement is itself a snare for state governments, making the overall structure more extractive than amendments alone suggest. This affects whether the federal authority''s classification as rope is defensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_enforcement_extraction, empirical, 'Whether federal enforcement serves coordination or becomes secondary extraction').

omega_variable(
    kernel_reading_contest_amendment_family,
    'Is this constraint one reading of the ''later amendment eras'' kernel, or does it belong to a separate ''voting rights amendments'' kernel with its own siblings?',
    'Structural analysis of whether the Civil Rights era amendments cohere as a family distinct from Progressive era, Reconstruction, and housekeeping amendments. Analysis of whether they share a common beneficiary structure, common suppression mechanism, and common temporal logic.',
    'If separate kernel: this constraint''s ε, beneficiaries, and mandatrophy analysis change (it is a more cohesive family with higher coherence). If part of later amendments: this reading''s relationship to siblings becomes more clear and the constraint serves as a lens on the larger kernel''s structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_amendment_family, conceptual, 'Whether this reading belongs to later_amendment_eras kernel or constitutes a separate kernel').

omega_variable(
    young_voters_heterogeneity,
    'Do eighteen-year-olds constitute a unified beneficiary group with the poll-tax abolition beneficiaries (poor and African American voters), or are they a separate constituency with different extraction mechanisms and exit options?',
    'Demographic analysis of voting patterns 1971-present; comparison of disenfranchisement barriers facing young voters vs. poll-tax targeted voters; examination of whether eighteen-year-old suffrage motivated by similar voting-rights logic or by different political reasoning',
    'If unified: the amendment family is coherent (ε ~0.18). If heterogeneous: the young-voter amendment may warrant separate constraint status with different ε reflecting different suppression mechanisms. This affects whether all four amendments (23rd, 24th, 25th, 26th) belong in one constraint or should decompose.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(young_voters_heterogeneity, empirical, 'Whether young voters and poll-tax disenfranchised share unified beneficiary status').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(later_amendment_eras__civil_rights_era_amendments, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(crea_theater_t0_1960, later_amendment_eras__civil_rights_era_amendments, theater_ratio, 0, 0.18).
narrative_ontology:measurement(crea_theater_t5_1965, later_amendment_eras__civil_rights_era_amendments, theater_ratio, 5, 0.25).
narrative_ontology:measurement(crea_theater_t10_1970, later_amendment_eras__civil_rights_era_amendments, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(crea_extractiveness_t0_1960, later_amendment_eras__civil_rights_era_amendments, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(crea_extractiveness_t5_1965, later_amendment_eras__civil_rights_era_amendments, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(crea_extractiveness_t10_1970, later_amendment_eras__civil_rights_era_amendments, base_extractiveness, 10, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(crea_suppression_t0_1960, later_amendment_eras__civil_rights_era_amendments, suppression_requirement, 0, 0.88).
narrative_ontology:measurement(crea_suppression_t5_1965, later_amendment_eras__civil_rights_era_amendments, suppression_requirement, 5, 0.72).
narrative_ontology:measurement(crea_suppression_t10_1970, later_amendment_eras__civil_rights_era_amendments, suppression_requirement, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(later_amendment_eras__civil_rights_era_amendments, enforcement_mechanism).
narrative_ontology:affects_constraint(later_amendment_eras__civil_rights_era_amendments, reconstruction_amendments).
narrative_ontology:affects_constraint(later_amendment_eras__civil_rights_era_amendments, progressive_era_amendments).
narrative_ontology:affects_constraint(later_amendment_eras__civil_rights_era_amendments, structural_housekeeping_amendments).
narrative_ontology:affects_constraint(later_amendment_eras__civil_rights_era_amendments, secondary_voter_suppression_mechanisms).

% DUAL FORMULATION NOTE:
% The Civil Rights era amendments belong to a constraint family spanning multiple amendment eras. This story focuses on the specific extractiveness and suppression mechanisms of the 23rd, 24th, 25th, and 26th Amendments. The Reconstruction amendments have different extractiveness (higher, focused on slavery abolition and citizenship), different beneficiaries (freedmen), and different temporal dynamics. The Progressive era amendments focus on federal-state restructuring rather than franchise gatekeeping. The housekeeping amendments maintain existing machinery. All members of the later_amendment_eras family affect each other through institutional precedent and constitutional interpretation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
