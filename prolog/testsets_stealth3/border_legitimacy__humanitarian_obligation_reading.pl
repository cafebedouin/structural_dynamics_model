% ============================================================================
% CONSTRAINT STORY: border_legitimacy__humanitarian_obligation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_legitimacy__humanitarian_obligation_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: border_legitimacy__humanitarian_obligation_reading
 *   human_readable: Humanitarian Obligation Reading of Border Legitimacy (Persecution-Floor Admission Duty)
 *   domain: political philosophy/international law/migration
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested border_legitimacy
 *   kernel: the humanitarian_obligation_reading, under which states owe a
 *   binding duty to admit people fleeing persecution or disaster while
 *   retaining legitimate discretion to exclude general economic migrants. The
 *   constraint's signature structure is bifurcation: the same admission
 *   apparatus that delivers real protection to recognized refugees
 *   simultaneously operates as a categorical wall against economic migrants
 *   and an evidentiary filter that fails many genuinely endangered claimants.
 *   The epsilon referent is the standing bifurcated arrangement itself,
 *   assessed by this reading's own lights — not the open-borders alternative
 *   the freedom_of_movement sibling would install, and not the
 *   unconstrained-exclusion world the sovereignty sibling defends. Claim and
 *   metrics are authored independently: I claim tangled_rope because I judge
 *   the structure to possess both a genuine coordination function (shared
 *   category, non-refoulement floor, adjudicable burden-sharing) and
 *   asymmetric extraction (categorical exclusion with identifiable payers),
 *   while the metric values describe what I take to be descriptively true of
 *   the arrangement's actual operation.
 *
 * KEY AGENTS:
 *   - destination_states: agenda-setter (institutional/arbitrage) — administers the threshold, captures the discretion the floor leaves intact
 *   - recognized_refugees: primary beneficiary (powerless/trapped) — receive the floor's protection
 *   - economic_migrants: primary target (powerless/constrained) — categorically denied any channel
 *   - rejected_asylum_seekers: secondary target (powerless/trapped) — harmed by the evidentiary filter inside the protected category
 *   - first_asylum_states: cost-bearing participant (organized/constrained) — hosts the displaced the regime nominally shares
 *   - unhcr: institutional beneficiary (institutional/identity_locked) — mandate, budget, and standing ride on the framework
 *   - migrant_sending_communities: excluded voice (powerless/constrained) — bear the arrangement's costs with no seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_legitimacy__humanitarian_obligation_reading, 0.58).
domain_priors:suppression_score(border_legitimacy__humanitarian_obligation_reading, 0.64).
domain_priors:theater_ratio(border_legitimacy__humanitarian_obligation_reading, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_legitimacy__humanitarian_obligation_reading, tangled_rope).
narrative_ontology:human_readable(border_legitimacy__humanitarian_obligation_reading, "Humanitarian Obligation Reading of Border Legitimacy (Persecution-Floor Admission Duty)").
narrative_ontology:topic_domain(border_legitimacy__humanitarian_obligation_reading, "political philosophy/international law/migration").

domain_priors:requires_active_enforcement(border_legitimacy__humanitarian_obligation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_legitimacy__humanitarian_obligation_reading, '1cfab057-a9dc-4f2f-9618-147121513d6b').
narrative_ontology:cs_kernel_codification('1cfab057-a9dc-4f2f-9618-147121513d6b', fixed_text).
narrative_ontology:cs_authority_grounding('1cfab057-a9dc-4f2f-9618-147121513d6b', lineage).
narrative_ontology:cs_interpretation_layer_present('1cfab057-a9dc-4f2f-9618-147121513d6b').
narrative_ontology:cs_reading_relation('1cfab057-a9dc-4f2f-9618-147121513d6b', border_legitimacy__sovereignty_reading, influences).
narrative_ontology:cs_reading_relation('1cfab057-a9dc-4f2f-9618-147121513d6b', border_legitimacy__freedom_of_movement_reading, coexists_with).
narrative_ontology:cs_axiom('1cfab057-a9dc-4f2f-9618-147121513d6b', foundational, admission_duty_tracks_harm_severity).
narrative_ontology:cs_axiom_status(admission_duty_tracks_harm_severity, holdable).
narrative_ontology:cs_axiom_grounding('1cfab057-a9dc-4f2f-9618-147121513d6b', admission_duty_tracks_harm_severity, deontological).
narrative_ontology:cs_axiom('1cfab057-a9dc-4f2f-9618-147121513d6b', secondary, categorical_economic_exclusion_is_legitimate).
narrative_ontology:cs_axiom_status(categorical_economic_exclusion_is_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('1cfab057-a9dc-4f2f-9618-147121513d6b', categorical_economic_exclusion_is_legitimate, conventional).
narrative_ontology:cs_reference_frame('1cfab057-a9dc-4f2f-9618-147121513d6b', persecution_floor_settlement).
narrative_ontology:cs_drift_state('1cfab057-a9dc-4f2f-9618-147121513d6b', contemporary_externalization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1cfab057-a9dc-4f2f-9618-147121513d6b', '').
narrative_ontology:cs_kernel_id(border_legitimacy__humanitarian_obligation_reading, border_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_legitimacy__humanitarian_obligation_reading, destination_states).
narrative_ontology:constraint_beneficiary(border_legitimacy__humanitarian_obligation_reading, recognized_refugees).
narrative_ontology:constraint_beneficiary(border_legitimacy__humanitarian_obligation_reading, unhcr).
narrative_ontology:constraint_victim(border_legitimacy__humanitarian_obligation_reading, economic_migrants).
narrative_ontology:constraint_victim(border_legitimacy__humanitarian_obligation_reading, rejected_asylum_seekers).
narrative_ontology:constraint_victim(border_legitimacy__humanitarian_obligation_reading, first_asylum_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(border_legitimacy__humanitarian_obligation_reading, first_asylum_states).
narrative_ontology:constraint_vindicates(border_legitimacy__humanitarian_obligation_reading, non_refoulement_doctrine).
narrative_ontology:constraint_vindicates(border_legitimacy__humanitarian_obligation_reading, persecution_threshold_category).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and administer asylum law: define admissibility criteria, run status determination, operate removals, and negotiate externalization agreements with neighboring and transit states. They concede a bounded admission duty to applicants who meet the persecution-or-disaster threshold while retaining full discretion over everyone else. What flows to them is preserved control over labor markets, demographics, and political coalitions; what flows out is processing infrastructure, hosting expenditure, and litigation exposure. In practice they can narrow definitions, sign cooperation deals, and outsource enforcement, so the regime's terms bind them loosely.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, destination_states, agenda_setter,
    institutional, generational, arbitrage, national).

% Fled persecution or disaster and secured formal status in another state. They receive residence, work authorization, and protection from forced return. They cannot safely go home, onward movement to preferred destinations is restricted by the same rules that admitted them, and family reunification proceeds slowly and at official discretion.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, recognized_refugees, beneficiary,
    powerless, biographical, trapped, global).

% Move primarily for work, wages, or household survival. They face no lawful admission channel under the persecution-or-disaster threshold, however severe their poverty or precarity, because the category of harm they flee does not qualify. Their realistic options are staying, migrating irregularly along dangerous routes, or competing for narrow labor programs most will never access.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, economic_migrants, payer,
    powerless, biographical, constrained, global).

% Filed protection claims that failed, often because persecution could not be evidenced to the required standard or because their harm was classified as economic or as generalized violence. They live in legal limbo pending removal: barred from formal work in many jurisdictions, unable to return safely, and ineligible for the protection that similarly situated people who cleared the evidentiary bar received.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, rejected_asylum_seekers, payer,
    powerless, biographical, trapped, global).

% Border the crisis zones and therefore host the large majority of the world's displaced, typically for decades. They receive earmarked humanitarian aid and remittance inflows, and can occasionally trade hosting cooperation for diplomatic or financial concessions, but they bear concentrated fiscal, environmental, and political costs while distant wealthy states fund small resettlement quotas.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, first_asylum_states, payer,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(border_legitimacy__humanitarian_obligation_reading, first_asylum_states, beneficiary).

% Holds the supervisory mandate over the regime: registers displaced people, advocates for protection, funds emergency response, and issues interpretive guidance on the persecution threshold. Its institutional purpose, budget, and diplomatic standing depend on the continued centrality of the framework it administers; fundamentally redefining the mandate would dissolve the organization's reason for existing.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, unhcr, beneficiary,
    institutional, generational, identity_locked, global).

% Households and villages whose members seek work abroad and depend on remittances. They regard migration as a survival strategy, yet they have no seat in the negotiations that set admission categories; their interests reach the table only filtered through origin-state diplomacy, which frequently trades migration cooperation for unrelated concessions.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, migrant_sending_communities, excluded,
    powerless, biographical, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_legitimacy__humanitarian_obligation_reading, destination_states).
narrative_ontology:fixing_cost_class(border_legitimacy__humanitarian_obligation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of protecting people fleeing catastrophic harm: it fixes a shared, adjudicable category (persecution or disaster), attaches a non-return duty to it, makes burden-sharing arguments legible between states, and rations a scarce good (admission) by severity of harm fled rather than by wealth or connections.
% TRANSFER_FUNCTION: Moves admission rights and legal protection from destination states to a defined subset of displaced people; moves the costs of exclusion and enforcement onto the excluded categories and onto smuggler-dependent irregular routes; and concentrates hosting costs on states geographically adjacent to crisis zones.
% ABSENT_VOICES: The migrants themselves. Both excluded categories and rejected claimants have no seat in treaty negotiation, doctrinal development, or the bilateral deals that externalize enforcement; sending-community households and transit-route populations absorb the arrangement's costs without representation, entering only through origin-state governments that trade their interests away.
% DISAPPEARANCE_RATIONALE: If the persecution-floor obligation vanished overnight, non-refoulement protection would evaporate for millions with recognized or recognizable claims, adjudicated asylum would collapse into purely discretionary charity, externalization deals would lose their legal anchor, and the entire architecture of status determination, resettlement quotas, and supervised returns would have to be rebuilt from scratch or abandoned.
% FOUNDING_PROBLEM: Post-war mass displacement and the Holocaust demonstrated that unbounded state discretion to exclude could be lethal at scale; the 1951 settlement was built to guarantee a floor of protection for people fleeing persecution, using a deliberately narrow and provable category so the duty would be enforceable rather than aspirational.
% FOUNDING_PROBLEM_CORROBORATION: Refugee-law scholars and human-rights monitoring bodies outside destination-state governments attest that the persecution floor remains live for a real subset of claimants. Economists and development researchers outside the benefiting parties attest that the persecution/economic boundary now excludes people facing comparable lethal deprivation, and that the line reflects Cold War-era drafting politics as much as moral urgency. UNHCR's own gap analyses, though an interested party, are corroborated by independent displacement-data projects documenting drivers (climate, state collapse, generalized violence) the 1951 category handles poorly.
narrative_ontology:disappearance_verdict(border_legitimacy__humanitarian_obligation_reading, world_rearranges).
narrative_ontology:founding_problem_status(border_legitimacy__humanitarian_obligation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_legitimacy__humanitarian_obligation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(border_legitimacy__humanitarian_obligation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(border_legitimacy__humanitarian_obligation_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_legitimacy__humanitarian_obligation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_legitimacy__humanitarian_obligation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_legitimacy__humanitarian_obligation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness sits at 0.58 rather than higher because the bifurcation cuts both ways: roughly half the structure's operation delivers genuine, life-saving protection, which offsets the categorical denial imposed on the other side of the line. It sits well above zero because the exclusion is total for economic migrants (no channel at all, regardless of deprivation severity), because the evidentiary burden falls on the most desperate claimants, and because hosting costs concentrate on states adjacent to crises while distant states enjoy the regime's legitimating benefits cheaply. Suppression (0.64) is structural: border enforcement, carrier sanctions, detention, and externalized processing actively prevent the excluded from exercising any alternative channel; it rose steeply after 1990 as safe-third-country doctrines, offshore processing, and pushback regimes matured — which is why suppression_requirement is tracked temporally here (the story specifically traces enforcement-capacity build-out, not mere extraction drift). Theater ratio (0.36): resettlement pledges, global compacts, and summitry perform burden-sharing while actual hosting stays regionally concentrated; the status-determination machinery itself remains functional, so theater is substantial but not dominant. Accessibility_collapse (0.45): alternatives do not fully collapse — irregular migration persists at scale, labor channels exist narrowly, and the sibling readings remain live political positions — so understanding the constraint forecloses less than a natural law would. Resistance (0.6): migrant-rights movements, transnational advocacy networks, Global South diplomatic blocs, and open-borders scholarship contest the line continuously; potential coalition power among the powerless victim seats exists (cross-border migrant associations, diaspora advocacy) but is blunted by the trapped/constrained exit profiles and by origin states trading migration cooperation for other concessions. All three metric series share one six-point time grid (1951, 1967, 1980, 1995, 2015, 2025) so the engine samples a complete row at every point.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the destination-state seat the arrangement is a principled compromise it built and can recalibrate at will — order plus mercy, with the mercy priced affordably. From the recognized-refugee seat it is a lifeline. From the economic-migrant seat the identical line is an arbitrary wall: the same desperation, differently labeled, yields death versus admission. From the first-asylum-state seat the regime is a cost-shifting device that lectures about universality while delivering concentration. The engine derives these divergent classifications from the declared directionalities and exit profiles; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Destination_states are declared beneficiaries and sit near the beneficiary end of d: the floor concedes little while legitimizing the broad exclusion they prefer, and their arbitrage-grade exit (redefining categories, externalizing enforcement) amplifies the subsidy. Recognized_refugees are beneficiaries with low d but no leverage — they collect protection without setting terms. Unhcr is a beneficiary whose identity lock fuses the organization with its mandate. Economic_migrants and rejected_asylum_seekers are declared victims near the full-target end: the former trapped outside the category entirely, the latter caught by the evidentiary filter inside it, both with trapped or constrained exits that amplify effective extraction. First_asylum_states are declared victims despite being regime participants — the nominal universality of the duty functions, for them, as a regional tax; their dual position (aid recipient and cost bearer) nets out on the paying side. Scope amplification applies mildly: the regime operates globally, where verification of compliance is hardest and externalization thrives.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a rope would erase the categorical victims — the economic migrants and failed claimants whose exclusion is not a coordination cost but the arrangement's operating product. Classifying it as a snare would erase the genuine protection delivered to recognized refugees and the real collective-action problem the persecution floor solves. The tangled_rope classification holds both facts visible, which matters because the arrangement's future depends on which component dominates: if the protective floor continues to atrophy under externalization while the legitimizing function grows, the structure drifts toward pure extraction wearing humanitarian form. The founding problem is contested rather than dead — the persecution floor still saves lives — so this is not yet a mandatrophy case; the mismatch consumer reads founding_problem_status=contested alongside disappearance_verdict=world_rearranges and finds no zombie flag, correctly. The floor_entrenchment_effect omega tracks the darker possibility: that the floor's principal contemporary function is making the larger exclusion look principled, which would convert coordination capital into extraction cover over time.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This story instantiates the humanitarian_obligation_reading of the border_legitimacy kernel; how would the victim set and effective extraction shift under the sibling readings?',
    'Compile and compare the three reading stories (border_legitimacy__sovereignty_reading, border_legitimacy__freedom_of_movement_reading) side by side, examining beneficiary/victim declarations and per-seat computed extraction under each.',
    'Under the sovereignty reading the persecution floor disappears and recognized_refugees join the victim set, driving epsilon sharply upward; under the freedom_of_movement reading the categorical exclusion disappears and economic_migrants leave the victim set, driving epsilon down toward pure-coordination levels. The disagreement between readings is located precisely in the status of the categorical line.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: one reading of a contested kernel; sibling readings instantiate structurally different constraints.').

omega_variable(
    persecution_poverty_boundary_coherence,
    'Is the persecution/economic boundary coherent for mixed-motive flight — state collapse, gang extortion, famine — or does the binary systematically relabel coerced, lethal deprivation as voluntary economic choice?',
    'Doctrinal analysis of mixed-claim adjudication outcomes and stated rejection grounds across major asylum systems, cross-referenced with country-condition evidence available to decision-makers at the time.',
    'If the boundary systematically misclassifies coerced flight as economic, the bifurcated victim set understates the true victim population and the measured epsilon of the categorical exclusion is biased downward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(persecution_poverty_boundary_coherence, conceptual, 'Whether the category line that defines this reading tracks a real moral distinction or an administrative convenience.').

omega_variable(
    regional_cost_concentration,
    'Does the nominally universal admission obligation operate as a de facto regional tax on states adjacent to crisis zones, while distant wealthy states pay little and are coordinated least?',
    'Compare per-displaced-person hosting expenditure, recognition rates, and resettlement intake against geographic distance from major crisis zones over the interval.',
    'If confirmed, the coordination function is thin for distant states — they contribute minimally to the burden the regime allocates — shifting the arrangement''s center of gravity from shared burden-bearing toward localized extraction from adjacent states and the displaced themselves.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_cost_concentration, empirical, 'Whether burden-sharing under the regime is real or geographically fictional.').

omega_variable(
    externalization_floor_integrity,
    'Do externalized enforcement practices — pushbacks, offshore processing, third-country transfer deals — breach the very non-refoulement floor this reading endorses?',
    'Litigation outcomes, monitoring-body findings, and systematic reporting on refoulement risk within externalization arrangements.',
    'If the floor is routinely breached by its own administrators, the reading''s operative content collapses toward the sovereignty reading in practice while retaining humanitarian form — a form/function split the classification should register as rising theater and decaying coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externalization_floor_integrity, empirical, 'Whether the reading''s protective floor survives its administrators'' enforcement methods.').

omega_variable(
    floor_entrenchment_effect,
    'Does conceding a narrow humanitarian floor stabilize the broader exclusion by making it appear principled — entrenching categorical exclusion of economic migrants more durably than an openly sovereigntist rule would?',
    'Comparative discourse and attitude analysis: whether humanitarian-framed regimes sustain higher public acceptance of economic-migration restrictions than explicitly sovereignty-framed ones, controlling for migration levels.',
    'If yes, part of the arrangement''s apparent coordination value is legitimization work performed for the exclusion — the hybrid hardens toward pure extraction as the protective component becomes cover, and the tangled_rope classification should be read as unstable in that direction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(floor_entrenchment_effect, conceptual, 'Whether the floor protects refugees, legitimizes exclusion, or both at once.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_legitimacy__humanitarian_obligation_reading, 1951, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t1951, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 1951, 0.15).
narrative_ontology:measurement_basis(bord_tr_t1951, observed).
narrative_ontology:measurement(bord_tr_t1967, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 1967, 0.17).
narrative_ontology:measurement_basis(bord_tr_t1967, observed).
narrative_ontology:measurement(bord_tr_t1980, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 1980, 0.21).
narrative_ontology:measurement_basis(bord_tr_t1980, observed).
narrative_ontology:measurement(bord_tr_t1995, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 1995, 0.27).
narrative_ontology:measurement_basis(bord_tr_t1995, observed).
narrative_ontology:measurement(bord_tr_t2015, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 2015, 0.33).
narrative_ontology:measurement_basis(bord_tr_t2015, observed).
narrative_ontology:measurement(bord_tr_t2025, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 2025, 0.36).
narrative_ontology:measurement_basis(bord_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(bord_be_t1951, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 1951, 0.35).
narrative_ontology:measurement_basis(bord_be_t1951, observed).
narrative_ontology:measurement(bord_be_t1967, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 1967, 0.38).
narrative_ontology:measurement_basis(bord_be_t1967, observed).
narrative_ontology:measurement(bord_be_t1980, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 1980, 0.42).
narrative_ontology:measurement_basis(bord_be_t1980, observed).
narrative_ontology:measurement(bord_be_t1995, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 1995, 0.48).
narrative_ontology:measurement_basis(bord_be_t1995, observed).
narrative_ontology:measurement(bord_be_t2015, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 2015, 0.54).
narrative_ontology:measurement_basis(bord_be_t2015, observed).
narrative_ontology:measurement(bord_be_t2025, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 2025, 0.58).
narrative_ontology:measurement_basis(bord_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t1951, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 1951, 0.3).
narrative_ontology:measurement_basis(bord_su_t1951, observed).
narrative_ontology:measurement(bord_su_t1967, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 1967, 0.33).
narrative_ontology:measurement_basis(bord_su_t1967, observed).
narrative_ontology:measurement(bord_su_t1980, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 1980, 0.4).
narrative_ontology:measurement_basis(bord_su_t1980, observed).
narrative_ontology:measurement(bord_su_t1995, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 1995, 0.49).
narrative_ontology:measurement_basis(bord_su_t1995, observed).
narrative_ontology:measurement(bord_su_t2015, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 2015, 0.61).
narrative_ontology:measurement_basis(bord_su_t2015, observed).
narrative_ontology:measurement(bord_su_t2025, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 2025, 0.64).
narrative_ontology:measurement_basis(bord_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_legitimacy__humanitarian_obligation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(border_legitimacy__humanitarian_obligation_reading, border_legitimacy__sovereignty_reading).
narrative_ontology:affects_constraint(border_legitimacy__humanitarian_obligation_reading, border_legitimacy__freedom_of_movement_reading).

% DUAL FORMULATION NOTE:
% 'Border legitimacy' is a single colloquial label covering three structurally distinct commitments, decomposed per the epsilon-invariance principle into separate constraint stories linked through network edges. This humanitarian_obligation_reading carries a bifurcated victim set and moderate epsilon; border_legitimacy__sovereignty_reading carries a maximal victim set (no floor) and high epsilon; border_legitimacy__freedom_of_movement_reading carries a minimal victim set (no categorical exclusion) and low epsilon. The upstream/downstream structure differs by sibling: this reading's concession that economic migration may be categorically excluded structurally bolsters the sovereignty reading's legitimacy conditions (influences), while it merely coexists with the freedom_of_movement reading as rival live positions held by different parties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
