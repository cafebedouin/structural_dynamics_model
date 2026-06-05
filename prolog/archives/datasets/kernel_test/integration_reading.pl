% ============================================================================
% CONSTRAINT STORY: integration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_integration_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: integration_reading
 *   human_readable: Single Market Labor Mobility Integration (Transfer of Labor Sovereignty)
 *   domain: political_economy/federalism/migration
 *
 * SUMMARY:
 *   This constraint instantiates one reading of a contested federation
 *   membership kernel: the INTEGRATION READING holds that single market
 *   completion requires irreversible supranational control of labor mobility.
 *   Under this reading, federation membership entails permanent transfer of
 *   labor sovereignty — member-states cannot restrict intra-federation labor
 *   mobility or protect domestic labor markets without violating membership
 *   terms. The beneficiaries are mobile skilled workers, capital owners, and
 *   receiving-state employers who gain access to larger labor pools at
 *   suppressed wages. The victims are unskilled workers in receiving states
 *   (wage competition), sending-state labor pools (brain drain), and
 *   sending-state governments (lost fiscal and demographic capacity). This
 *   reading presents the labor mobility regime as a technical requirement of
 *   market completion, not as a contingent political choice. The constraint's
 *   theater ratio (0.52) reflects moderate performative content: the
 *   federation frames labor mobility as 'freedom' and 'efficiency,'
 *   concealing the asymmetric extraction mechanism that concentrates costs on
 *   powerless agents and benefits on capital and mobile skill. The
 *   extractiveness rises from 0.35 to 0.58 over 10 years as wage pressure
 *   accumulates and demographic losses compound — the extraction mechanism
 *   strengthens as the irreversibility lock deepens.
 *
 * KEY AGENTS:
 *   - Unskilled workers in receiving states: Primary victims (powerless/trapped) — bear maximum wage competition and employment displacement; trapped by geographic immobility and skill structure
 *   - Sending-state labor pool and governments: Secondary victims (moderate/constrained) — experience demographic loss, brain drain, and fiscal capacity reduction; constrained by federation prohibition on labor mobility restrictions
 *   - Mobile skilled workers: Primary beneficiaries (powerful/mobile) — unfettered access to larger labor opportunity set; experience genuine coordination benefit alongside extraction of lower-skill labor value
 *   - Receiving-state employers and capital owners: Primary beneficiaries (institutional/arbitrage) — maximize workforce flexibility and wage suppression without state-level wage floors; can arbitrage labor costs across borders
 *   - Federation authority (supranational institutions): Institutional actor (organized/constrained) — enforces labor mobility sovereignty transfer; constrained by legitimacy requirement to maintain social cohesion alongside market integration
 *   - Analytical observer: External position (analytical/analytical) — risks naturalizing political choice (labor mobility regime) as economic necessity (market completion requirement)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(integration_reading, 0.58).
domain_priors:suppression_score(integration_reading, 0.68).
domain_priors:theater_ratio(integration_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(integration_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(integration_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(integration_reading, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(integration_reading, tangled_rope).
narrative_ontology:human_readable(integration_reading, "Single Market Labor Mobility Integration (Transfer of Labor Sovereignty)").
narrative_ontology:topic_domain(integration_reading, "political_economy/federalism/migration").

domain_priors:requires_active_enforcement(integration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(integration_reading, '40c2b41b-9a03-41ba-b0f1-d5fdff23863d').
narrative_ontology:cs_created_at('40c2b41b-9a03-41ba-b0f1-d5fdff23863d', '').
narrative_ontology:cs_kernel_codification('40c2b41b-9a03-41ba-b0f1-d5fdff23863d', formalized).
narrative_ontology:cs_authority_grounding('40c2b41b-9a03-41ba-b0f1-d5fdff23863d', lineage).
narrative_ontology:cs_interpretation_layer_present('40c2b41b-9a03-41ba-b0f1-d5fdff23863d').
narrative_ontology:cs_kernel_id(integration_reading, federation_membership_kernel).
narrative_ontology:cs_reading_relation('40c2b41b-9a03-41ba-b0f1-d5fdff23863d', sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('40c2b41b-9a03-41ba-b0f1-d5fdff23863d', hybrid_reading, influences).
narrative_ontology:cs_axiom('40c2b41b-9a03-41ba-b0f1-d5fdff23863d', foundational, complete_labor_market_integration_technically_necessary).
narrative_ontology:cs_axiom_status(complete_labor_market_integration_technically_necessary, holdable).
narrative_ontology:cs_axiom_grounding('40c2b41b-9a03-41ba-b0f1-d5fdff23863d', complete_labor_market_integration_technically_necessary, empirically_contingent).
narrative_ontology:cs_axiom('40c2b41b-9a03-41ba-b0f1-d5fdff23863d', foundational, sovereignty_transfer_irreversible_within_federation).
narrative_ontology:cs_axiom_status(sovereignty_transfer_irreversible_within_federation, holdable).
narrative_ontology:cs_axiom_grounding('40c2b41b-9a03-41ba-b0f1-d5fdff23863d', sovereignty_transfer_irreversible_within_federation, conventional).
narrative_ontology:cs_reference_frame('40c2b41b-9a03-41ba-b0f1-d5fdff23863d', single_market_labor_equilibrium).
narrative_ontology:cs_drift_state('40c2b41b-9a03-41ba-b0f1-d5fdff23863d', contemporary_wage_stagnation_era, gap(practice_drift, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(integration_reading, mobile_skilled_workers).
narrative_ontology:constraint_beneficiary(integration_reading, capital_owners).
narrative_ontology:constraint_beneficiary(integration_reading, complementary_labor).
narrative_ontology:constraint_beneficiary(integration_reading, receiving_state_employers).
narrative_ontology:constraint_victim(integration_reading, unskilled_workers_receiving_states).
narrative_ontology:constraint_victim(integration_reading, sending_state_labor_pool).
narrative_ontology:constraint_victim(integration_reading, receiving_state_wage_competition).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNSKILLED WORKERS IN RECEIVING STATES (SNARE) — Face maximum wage competition and employment displacement from intra-federation migration. Trapped by geographic immobility (housing costs, family ties, language) and skill structure. Supranational labor mobility transfer removes the nation-state's capacity to protect domestic labor through skill-selective immigration or sectoral quotas. No exit from this constraint — workers bear extraction with no coordination benefit.
constraint_indexing:constraint_classification(integration_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SENDING STATE LABOR POOL (SNARE) — Experiences demographic loss as working-age population emigrates; regional economies lose human capital stock. Constrained by inability to restrict outward migration or retain talent (federation membership forbids this). Over generational timescale, structural brain drain and aging population compound. Extraction appears as loss of demographic dividends and aging burden; minimal coordination benefit — mobility serves outmigrants, not their origin communities.
constraint_indexing:constraint_classification(integration_reading, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: MOBILE SKILLED WORKERS (TANGLED ROPE) — Primary beneficiaries of unfettered intra-federation mobility. Genuine coordination benefit: supranational labor market enables efficient skill-matching across borders. But asymmetric extraction embedded: the mobility regime extracts rents from low-skill workers (wage suppression via competition) to subsidize high-skill worker arbitrage. This agent sees coordination (can move freely, access larger opportunity set) layered with extraction of others' labor value.
constraint_indexing:constraint_classification(integration_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: RECEIVING STATE EMPLOYERS AND CAPITAL OWNERS (ROPE) — Net beneficiaries of the sovereignty transfer. Supranational labor mobility access enables wage suppression and workforce flexibility without state-level wage floors or sectoral protection. Experience this as coordination (larger talent pool, labor market efficiency) with extraction running toward them. Can arbitrage labor costs across borders within federation. Beneficiary position with maximum discretion.
constraint_indexing:constraint_classification(integration_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: SENDING STATE GOVERNMENTS (TANGLED ROPE) — Experience genuine coordination benefit (labor circulation can reduce unemployment, skills diaspora creates remittance flows and knowledge transfers) mixed with asymmetric extraction (lost fiscal capacity to control labor outflows, brain drain undermines receiving-state welfare contributions). Constrained by federation membership rules that forbid labor mobility restrictions. Active enforcement of supranational labor rights limits domestic policy autonomy.
constraint_indexing:constraint_classification(integration_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: FEDERATION AUTHORITY (TANGLED ROPE) — Coordinates single market efficiency (labor matching across regions) with asymmetric extraction embedded in labor mobility rules that privilege capital and mobile skill over territorial wage protection. Constrained by legitimacy requirement to maintain both market integration AND social cohesion. Theater ratio moderate: enforces mobility rights but conceals extraction mechanism through 'efficiency' and 'competition' framing. Active institutional enforcement required to maintain labor mobility sovereignty against member-state welfare pressures.
constraint_indexing:constraint_classification(integration_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / INEVITABILITY VIEW (MOUNTAIN) — From civilizational horizon, complete labor market integration is presented as natural law: capital mobility requires labor mobility for equilibration; single markets cannot function with balkanized labor. This perspective risks naturalizing the sovereignty transfer as inevitable economic consequence rather than a political choice about WHERE extraction accrues. Engine false summit detection will flag beneficiary declarations as evidence that this 'inevitability' is constructed.
constraint_indexing:constraint_classification(integration_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(integration_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(integration_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(integration_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(integration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(integration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The labor mobility regime extracts from low-skill workers through wage suppression (empirically documented 3-8% wage effects for competing demographics) and from sending states through demographic/fiscal losses. The extraction is not maximal (snare level ≥ 0.66) because sending-state governments retain some policy tools (education investment, sectoral development) and beneficiaries face supranational regulatory constraints (non-discrimination, freedom of movement reciprocity). Suppression (0.68): Moderately high. Unskilled workers face severe barriers to labor supply protection (federation rules forbid sectoral restrictions), housing cost barriers to intra-federation mobility, and language/credential barriers to competing for jobs. Sending-state governments are suppressed from restricting outward migration or imposing exit taxes. Theater ratio (0.52): Moderate. The federation frames labor mobility as 'freedom' and 'efficiency gains,' performative language that conceals extraction mechanism. But the mechanism has genuine coordination content — skills do match more efficiently across borders, some workers do benefit from larger opportunity sets. The theater is not as high as pure snare (which would be ≥ 0.60 theater) because real resource allocation gains exist; it is higher than pure rope (which would be ≤ 0.35) because the efficiency framing obscures who bears costs.
 *
 * PERSPECTIVAL GAP:
 *   Integration reading vs sibling readings: This reading (integration) presents labor mobility as technically necessary for single market completion — the sovereignty transfer is presented as inevitable given market integration objectives. The sovereignty_reading (sibling) would present member-state labor protection as necessary for political legitimacy — the sovereignty transfer is a violation, not a requirement. The hybrid_reading (sibling) would present labor mobility as achievable alongside member-state labor protections through sectoral agreements and compensatory transfers — the reading you are instantiating forecloses this by asserting irreversibility. See omega variables for how alternative readings would emerge from rejecting the 'inevitability' framing.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's experienced extractiveness (χ) derives from the agent's power level, exit options, and structural relationship to the labor mobility flow. Unskilled workers in receiving states: powerless + trapped exit → very high d (≈0.90) → very high f(d) → χ amplified to near-snare range despite moderate base extraction. Sending-state governments: institutional + constrained exit → moderate d (≈0.65) → moderate f(d) → moderate χ, reduced from snare range by institutional power but increased by irreversible sovereignty loss. Mobile skilled workers: powerful + mobile exit (can go anywhere federation extends, plus global options) → low d (≈0.35) → low f(d) → low or negative χ, beneficiary position. Receiving-state employers: institutional + arbitrage exit (can shift labor sourcing geographically) → very low d (≈0.10) → negative f(d) → negative χ, pure beneficiary position. The directionality derivation reveals why the same structural mechanism (labor mobility) appears as coordination to beneficiaries and extraction to victims: they have opposite d values derived from opposite structural positions in the labor flow.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraction_boundary_identification,
    'Is the measured extraction (0.58) concentrated on powerless agents (wage workers) or distributed across multiple agent types?',
    'Wage panel data comparing unskilled workers in high vs low migration receiving states; sectoral unemployment rates; skill premium analysis across federation member-states post-integration vs counterfactual protected labor markets',
    'If concentrated on powerless agents: snare classification strengthened for worker perspectives, federation authority reclassified to pure snare if enforcement mechanism disproportionately targets low-skill labor suppression. If distributed: tangled rope holds, extraction is ''true'' coordination overhead.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_boundary_identification, empirical, 'Concentration of extraction on powerless agents vs distribution across types').

omega_variable(
    sovereignty_transfer_reversibility,
    'Can labor mobility sovereignty be reclaimed by member-states without dissolving federation membership?',
    'Legal analysis of federation charter amendment procedures; historical precedent for labor mobility policy rollback; negotiation dynamics when member-states attempt sectoral labor restrictions',
    'If reversible: constrains the extraction mechanism — member-states maintain latent exit option (federation departure) even if costly. Reclassifies worker exit from trapped to constrained. If irreversible: extraction mechanism strengthened, worker exit locked, suppression gate rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_transfer_reversibility, empirical, 'Whether labor mobility sovereignty transfer is constitutionally reversible').

omega_variable(
    wage_competition_mechanism_isolation,
    'Does measured wage depression in receiving states derive primarily from intra-federation migration or from global trade/automation? How much extraction is attributable to the mobility regime vs other factors?',
    'Econometric decomposition of wage pressure sources (migration vs trade vs technological displacement); instrumental variables isolating federation membership exogeneity; comparison with similar non-federation labor markets',
    'If migration is dominant driver: extraction mechanism confirmed at measured 0.58. If migration contributes < 30%: base extraction should be downgraded and theater_ratio questions whether federation mechanism captures the true constraint (may be decomposable into separate trade/tech stories).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wage_competition_mechanism_isolation, empirical, 'Attribution of wage pressure to migration vs trade and automation').

omega_variable(
    sending_state_demographic_loss_irreversibility,
    'At what emigration threshold does sending-state demographic structure cross the point of self-reinforcing decline (aging spiral, declining birth rates, negative feedback on labor demand)?',
    'Demographic modeling of fertility/mortality/migration interactions; identification of threshold emigration rates beyond which population reconstitution fails; historical cases (post-Soviet states, Southern Europe) showing demographic recovery dynamics',
    'If threshold is crossed in early federation years: sending state becomes trapped in permanent demographic deficit, victim status intensifies from biological/structural irreversibility. If reversible: extraction is real but not permanent, shifts from mountain-like ''irreversible loss'' to tangled_rope ''costly but retrievable'' classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sending_state_demographic_loss_irreversibility, empirical, 'Irreversibility threshold for sending-state demographic loss').

omega_variable(
    reading_kernel_contestation,
    'Is federation membership''s labor mobility requirement a technical specification of economic integration or a political reading of what ''membership'' means?',
    'Historical tracing of federation charter origins and labor mobility provisions: were they inscribed as immutable economic law or as amendable political choice? Comparison with alternative federation designs (eg. EFTA, bilateral arrangements) that achieve market integration with labor mobility restrictions.',
    'If technical specification: integration_reading classification as tangled_rope holds; extraction is the ''cost'' of market completion. If political choice: the constraint decomposes — market completion (rope) and labor mobility regime (separate snare/tangled_rope story) become distinct constraints; the ''inevitability'' framing is revealed as committer-frame artifact rather than structural necessity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_contestation, conceptual, 'Whether labor mobility requirement is technical specification or political choice (instantiates kernel contestation between integration_reading, sovereignty_reading, hybrid_reading)').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(integration_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(integ_theater_t0, integration_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(integ_theater_t5, integration_reading, theater_ratio, 5, 0.45).
narrative_ontology:measurement(integ_theater_t10, integration_reading, theater_ratio, 10, 0.52).

% Extraction over time
narrative_ontology:measurement(integ_extractiveness_t0, integration_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(integ_extractiveness_t5, integration_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(integ_extractiveness_t10, integration_reading, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(integration_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(integration_reading, 0.18).
narrative_ontology:affects_constraint(integration_reading, wage_competition_receiving_states).
narrative_ontology:affects_constraint(integration_reading, demographic_decline_sending_states).
narrative_ontology:affects_constraint(integration_reading, federation_legitimacy_crisis).

% DUAL FORMULATION NOTE:
% Labor mobility sovereignty transfer decomposes into three linked constraints: (1) single market institutional coordination (ε~0.15, Rope) — genuine efficiency gains in skill matching; (2) wage suppression extraction mechanism (ε~0.62, Snare) — the 'supply shock' to receiving-state low-skill labor; (3) demographic loss mechanism (ε~0.70, Snare) — sending-state population decline and fiscal capacity loss. This story models the integrated regime that combines all three; the sibling stories isolate mechanism-specific extraction. The integration_reading presents all three as inseparable; alternative readings decompose them to enable policy variation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(integration_reading, institutional, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
