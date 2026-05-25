% ============================================================================
% CONSTRAINT STORY: regional_human_rights_courts
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_regional_human_rights_courts, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: regional_human_rights_courts
 *   human_readable: Regional Human Rights Courts as Coordination and Extraction Hybrid
 *   domain: international_law/human_rights/institutional_governance
 *
 * SUMMARY:
 *   Regional human rights courts (such as the European Court of Human Rights,
 *   African Court on Human and Peoples' Rights, Inter-American Court of Human
 *   Rights) exist nominally to protect individual rights through binding
 *   supranational adjudication. Yet they represent a complex hybrid
 *   constraint: they genuinely coordinate a public good (creating focal
 *   points for human rights norms, giving voice to marginalized actors,
 *   deterring the worst abuses) while simultaneously extracting compliance
 *   costs asymmetrically from weaker states and vulnerable communities. The
 *   constraint exhibits all the hallmarks of a tangled rope: active
 *   enforcement (states must comply with judgments), genuine coordination
 *   function (the court deters violations and amplifies previously invisible
 *   grievances), but significant asymmetric extraction (litigation costs,
 *   state hostility, implementation gaps, power asymmetries in whose rights
 *   are heard). The increasing theater ratio (0.40 → 0.55 over 20 years)
 *   reflects institutional degradation: as states accumulate non-compliance
 *   and the court preserves its legitimacy through procedural elaboration
 *   rather than enforcement, the ritual elements grow while functional
 *   delivery stagnates. The extractiveness trajectory (0.35 → 0.52) reflects
 *   growing awareness that the system, while protecting some rights, has
 *   become a mechanism through which powerful states and international
 *   professionals shape weaker states' governance.
 *
 * KEY AGENTS:
 *   - Individual Rights Claimant: Primary victim (powerless/trapped) — bears full cost of multiyear litigation with uncertain outcome and unenforceable remedy
 *   - Marginalized Community: Secondary victim (moderate/constrained) — experiences coordination (amplified voice) alongside extraction (resource drain, state hostility)
 *   - Member State Government: Primary beneficiary (institutional/arbitrage) — experiences coordination as regional stability mechanism; exit costly but possible
 *   - International NGO Ecosystem: Beneficiary (organized/constrained) — mobilizes litigation and coordinates advocacy; benefits from resource flows while extracting agenda priority from communities
 *   - International Legal Professional Community: Beneficiary (institutional/arbitrage) — perceives pure coordination; experiences career advancement and resource flows
 *   - Court Institutional Apparatus: Actor with inertia (institutional/arbitrage) — maintains itself through procedural elaboration; sees its own degradation
 *   - Wealthy Member State Elite: Beneficiary (powerful/mobile) — experiences manageable extraction and coordination benefits; can reshape system through legal argument
 *   - Developing Nation Government: Victim (organized/constrained) — experiences disproportionate extraction; theoretically can exit but exit itself becomes violation
 *   - Analytical Observer: Sees full structure (analytical/analytical) — recognizes genuine coordination alongside structural extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(regional_human_rights_courts, 0.52).
domain_priors:suppression_score(regional_human_rights_courts, 0.48).
domain_priors:theater_ratio(regional_human_rights_courts, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(regional_human_rights_courts, extractiveness, 0.52).
narrative_ontology:constraint_metric(regional_human_rights_courts, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(regional_human_rights_courts, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(regional_human_rights_courts, tangled_rope).
narrative_ontology:human_readable(regional_human_rights_courts, "Regional Human Rights Courts as Coordination and Extraction Hybrid").
narrative_ontology:topic_domain(regional_human_rights_courts, "international_law/human_rights/institutional_governance").

domain_priors:requires_active_enforcement(regional_human_rights_courts).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(regional_human_rights_courts, member_state_elites).
narrative_ontology:constraint_beneficiary(regional_human_rights_courts, international_legal_professionals).
narrative_ontology:constraint_beneficiary(regional_human_rights_courts, ngos_advocacy_organizations).
narrative_ontology:constraint_victim(regional_human_rights_courts, individual_rights_claimants).
narrative_ontology:constraint_victim(regional_human_rights_courts, marginalized_communities).
narrative_ontology:constraint_victim(regional_human_rights_courts, developing_nations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL RIGHTS CLAIMANT (SNARE) — A person seeking justice for human rights violation is trapped within the constraint. The court offers theoretical access to remedy but confronts: extreme litigation costs, multiyear delays (8-15 years average), language barriers, lack of legal representation in marginalized regions, and enforcement failure when states ignore judgments. Exit is impossible — no alternative dispute resolution exists at comparable scope. Suppression is structural: economic dependency (claimants cannot afford counsel), geographic isolation (travel to regional capitals required), and institutional opacity (proceedings inaccessible to non-lawyers).
constraint_indexing:constraint_classification(regional_human_rights_courts, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: MARGINALIZED COMMUNITY (TANGLED ROPE) — Ethnic minorities, indigenous groups, and economically excluded populations experience the court as both coordinating their dispersed grievances and extracting compliance costs. The court provides a collective voice (coordination function) that amplifies invisible violations into recognized claims. But the extraction is real: the legal mobilization required to pursue cases drains limited organizational resources; successful judgments often trigger state hostility toward claimant communities; and the implementation gap means many victories remain symbolic. Constrained exit: communities cannot simply ignore rights abuses, but the court system is not the only pathway (domestic organizing, direct action, migration) — exit carries high cost but exists.
constraint_indexing:constraint_classification(regional_human_rights_courts, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: MEMBER STATE GOVERNMENT (ROPE) — A state perceives the court primarily as a coordination mechanism for resolving disputes without interstate conflict escalation. The constraint solves a genuine collective action problem: absent the court, human rights grievances accumulate, risking regional instability, refugee crises, or internal insurgency. For states with strong rule-of-law capacity, the court's rulings are manageable and sometimes align with domestic reform agendas. Arbitrage exit: states can exit the system (formally withdraw from the treaty protocol) but face sanctions, diplomatic isolation, and loss of legitimacy. The exit cost varies by state power — a wealthy state faces lower consequences than a developing nation. Most states perceive the arrangement as beneficial coordination despite occasional costly judgments.
constraint_indexing:constraint_classification(regional_human_rights_courts, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: HUMAN RIGHTS NGO ECOSYSTEM (TANGLED ROPE) — International and regional NGOs coordinate litigation strategy, provide legal support, and amplify verdicts through advocacy. The court enables their core function (mobilizing rights claims) and provides legitimacy and leverage. But extraction exists: litigation funding flows to major international NGOs rather than grassroots organizations; case selection is often driven by donor priorities rather than community needs; successful litigation can trigger state pressure on NGOs themselves. Constrained exit: NGOs depend on legal pathways for legitimacy and resource flows; exiting means losing a central tool, but alternatives (political pressure, direct action, capacity building) exist. The constraint is genuinely hybrid — coordination and extraction are structurally inseparable.
constraint_indexing:constraint_classification(regional_human_rights_courts, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: INTERNATIONAL LEGAL PROFESSIONAL COMMUNITY (ROPE) — Judges, advocates, legal scholars, and court staff perceive the constraint as pure coordination: the court mechanism allocates scarce international legal expertise to claims that would otherwise go unheard. Career advancement, publication opportunities, and professional networks are structured through the court system. Arbitrage exit: individual professionals can leave the field, but the institutional structure persists. The community is a pure beneficiary with arbitrage-level exit: they have professional options outside human rights law, but those options carry lower prestige and fewer resources. The constraint is experienced as enabling and legitimate.
constraint_indexing:constraint_classification(regional_human_rights_courts, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: COURT INSTITUTIONAL APPARATUS (PITON) — The regional court system, viewed as an institutional actor with its own organizational inertia, shows signs of degradation. Theater ratio (0.55) reflects that a significant portion of court activity is performative: issuing judgments that states ignore, conducting hearings that produce minimal behavioral change, maintaining elaborate procedural rituals while implementation rates stagnate. The court persists through path dependence (existing treaties, institutional budgets, professional careers) rather than because it reliably delivers justice. Many states comply with select judgments while ignoring others; enforcement is discretionary and political. The apparatus maintains legitimacy through the theater of due process while actual rights protection outcomes have plateaued.
constraint_indexing:constraint_classification(regional_human_rights_courts, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 7: WEALTHY MEMBER STATE ELITE (TANGLED ROPE) — Powerful states (typically with developed economies and strong institutions) experience the constraint as beneficial coordination with manageable extraction. They benefit from the court's role in stabilizing regional order and legitimizing their governance norms. They can afford to comply with costly judgments and use selective compliance to maintain legitimacy while resisting cases that threaten core interests. Mobile exit: powerful states face real but surmountable exit costs (diplomatic consequences, reputational damage) and can reshape the system through procedural influence and legal argument. The constraint is hybrid but skewed toward coordination for this actor — they experience less extraction than weaker states.
constraint_indexing:constraint_classification(regional_human_rights_courts, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 8: DEVELOPING NATION GOVERNMENT (SNARE) — Weaker states face disproportionate extraction through the court system. Litigation creates reputational costs that weaker states cannot absorb (they lack the soft power to contextualize judgments as isolated incidents). Compliance with expensive judgments strains limited budgets. The court becomes a tool through which other states, NGOs, and international pressure can impose values and policies. Constrained exit: theoretically possible to withdraw, but withdrawal itself becomes a violation subject to judgment; moreover, withdrawal triggers sanctions and isolation. Many developing nations feel trapped in a system designed by and for more powerful states, where the 'rule of law' framing masks structural power imbalance.
constraint_indexing:constraint_classification(regional_human_rights_courts, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, regional human rights courts are a genuine hybrid. They coordinate a public good (deterring the worst abuses, giving voice to the voiceless, creating focal points for norm-setting) AND extract from vulnerable actors (shifting power through litigation, creating resource asymmetries, imposing institutional frameworks designed by outsiders). The system is neither pure extraction (genuine coordination function) nor pure coordination (asymmetric beneficiary distribution). The classification is tangled_rope at global scope because the global beneficiaries (international legal professionals, NGOs, powerful states) are structurally different from the global victims (individual claimants, marginalized communities). Suppression (0.48) reflects that exit barriers exist but are not absolute: states can withdraw, communities can pursue alternative justice pathways, individuals can accept informal settlements. But alternatives carry costs.
constraint_indexing:constraint_classification(regional_human_rights_courts, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(regional_human_rights_courts_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(regional_human_rights_courts, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(regional_human_rights_courts, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(regional_human_rights_courts, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(regional_human_rights_courts, TR),
    TR >= 0.70.

:- end_tests(regional_human_rights_courts_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, rising over time. The constraint extracts from individual claimants (litigation costs, multiyear delays) and developing nations (compliance costs, reputational damage) while benefiting international professionals and some states. The rising trajectory reflects that states have learned to comply selectively while maintaining non-compliance on politically sensitive issues — the actual rights protection per unit of court activity has declined, meaning extraction relative to coordination has increased. Suppression (0.48): Moderate, stable. Exit barriers are real but not absolute. Individual claimants face near-total suppression (no alternative justice at comparable scope); states face partial suppression (formal withdrawal possible but costly); communities face high but surmountable barriers (alternative pathways exist but carry costs). Theater ratio (0.55): Moderate, rising. The court maintains legitimacy through due process ritual while implementation rates stagnate. Non-compliance has become normalized (states violate judgments with minimal sanctions); the court responds with more elaborate procedures and repeated re-litigations of the same violations. This is classic piton behavior: the apparatus persists through institutional inertia and because alternatives haven't fully replaced it, not because it reliably delivers justice.
 *
 * PERSPECTIVAL GAP:
 *   This constraint's perspectival gap reveals how the same institution can be genuinely beneficial (rope) and genuinely harmful (snare) simultaneously without either perception being false. The court coordinates a real public good (deterrence, voice amplification) that benefits all actors. But the distribution of costs and benefits is asymmetric: the costs concentrate on powerless individuals and weaker states, while benefits concentrate on professionals and powerful states. The gap is not between accurate and inaccurate observers — it is between observers embedded in different structural positions. The wealthy state is not wrong that the court is beneficial; the marginalized claimant is not wrong that it is extractive. Both are true.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values track the agent's structural position relative to extraction flow. Individual claimants: d ≈ 0.95 (maximum target) — they fund and risk the litigation but do not control outcomes. Developing nations: d ≈ 0.80 (heavy target) — they face compliance costs and reputational damage but have some exit option (withdrawal). Marginalized communities: d ≈ 0.65 (moderate target) — they benefit from amplified voice but lose agenda control to NGOs. NGOs: d ≈ 0.20 (moderate beneficiary) — they resource cases and control agenda. Wealthy states: d ≈ 0.30 (beneficiary) — they benefit from coordination and can absorb compliance costs. International professionals: d ≈ 0.10 (strong beneficiary) — they extract pure resources and prestige. Analytical observer: d ≈ 0.72 (sees full structure) — applies the sigmoid to show moderate effective extraction because the coordination and extraction functions are genuinely mixed at global scope.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that tangled rope is the only coherent classification. The beneficiary/victim structure is objectively asymmetric: specific actors benefit (international professionals, wealthy states, NGOs) while others bear costs (individual claimants, developing nations, marginalized communities). The coordination function is real: the court does deter abuses and amplify voices that would otherwise be silent. But the coordination is not symmetric: it amplifies some voices (those of established NGOs, organized states) more than others (grassroots communities, individuals without legal access). This combination of genuine coordination + asymmetric extraction is exactly what tangled rope names. The constraint cannot be classified as rope (that would ignore the asymmetry) or snare (that would ignore the genuine coordination and voice amplification). The mandatrophy is resolved by recognizing that institutions can be simultaneously beneficial and extractive depending on position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    state_compliance_measurement,
    'What constitutes genuine compliance vs performative deference to regional court judgments?',
    'Longitudinal tracking of behavioral change post-judgment: do structural reforms occur, or do states make symbolic gestures while maintaining the underlying practice? Measurement of actual rights improvements for claimant populations.',
    'If genuine compliance (>70%): constraint is more rope than snare; coordination function is real. If performative compliance (<40%): constraint is more snare; extraction mechanism dominates. The extractiveness value may need revision.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_compliance_measurement, empirical, 'Measurement of state compliance depth vs performance').

omega_variable(
    alternative_justice_pathway_viability,
    'Do alternative domestic or international justice mechanisms (domestic courts, truth commissions, customary law forums, direct negotiation) provide comparable or superior outcomes for marginalized communities?',
    'Comparative analysis: cost, timeline, enforcement rate, and satisfaction metrics for regional court cases vs domestic civil litigation vs grassroots dispute resolution. Survey of claimants on perceived justice delivery.',
    'If alternatives are viable: exit options are genuinely mobile for victims; constrained or mobile exit becomes appropriate classification, reducing snare interpretation. If alternatives are inferior: trap is real; snare classification confirmed. This shapes whether suppression reflects institutional monopoly vs structural barriers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_justice_pathway_viability, empirical, 'Viability of alternative justice pathways').

omega_variable(
    power_asymmetry_in_case_selection,
    'Does case selection and docket composition reflect whose rights are prioritized? Are powerful states'' alleged violations heard with equal frequency and prominence as vulnerable groups'' violations?',
    'Analysis of case database: categorize by respondent state power level, claimant group (individual vs NGO-backed), alleged violation type. Measure case selection bias toward violations amenable to legal remedy vs structural injustices requiring political solutions.',
    'If selection bias is severe: the constraint institutionalizes a particular vision of justice that benefits NGOs and legal professionals (who define what counts as a ''case'') more than marginalized communities (whose actual grievances may not fit the legal frame). This increases the extractive interpretation and piton features.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(power_asymmetry_in_case_selection, empirical, 'Bias in case selection toward powerful states and legal-friendly violations').

omega_variable(
    legitimacy_foundation_ambiguity,
    'Is the court''s authority legitimated by consent (states genuinely accept jurisdiction) or by external pressure (geopolitical/economic coercion)?',
    'Historical analysis of treaty ratification: voluntary vs conditional ratification, correlation with state capacity and external pressure. Interview data from state representatives on motivations for joining/maintaining membership.',
    'If consent is genuine: rope classification is stronger; membership is a coordination solution. If coercive: snare classification is stronger; the constraint extracts compliance under the guise of legitimacy. This ambiguity is especially acute for developing nations and weakens the institutional beneficiary''s arbitrage exit claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_foundation_ambiguity, conceptual, 'Legitimacy foundation: genuine consent vs external coercion').

omega_variable(
    implementation_gap_structural_source,
    'Is the gap between judgment and implementation due to state incapacity (states lack resources to comply) or state unwillingness (states choose not to comply)?',
    'Comparative analysis of compliance rates: wealthy states vs developing states, states with strong institutions vs weak institutions. Distinguish cases where states appeal judgments vs cases where they simply ignore them. Assess correlation between judgment cost and compliance.',
    'If incapacity: extraction is against developing nations specifically; the constraint is more snare for weaker states. If unwillingness: states retain choice and the constraint is more rope or tangled_rope. This determines whether suppression is structural or selective by state power.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(implementation_gap_structural_source, empirical, 'State incapacity vs unwillingness as source of implementation gap').

omega_variable(
    ngo_agenda_capture_scope,
    'To what extent do international NGO funding priorities and framing shape which violations are litigated, rather than community-identified priorities?',
    'Survey and interview: ask marginalized communities what their top justice priorities are; compare to actual cases brought. Analyze funding flows: where does litigation support come from and how does it correlate with case selection? Track whether NGO-led litigation aligns with or diverges from grassroots priorities.',
    'If NGO capture is substantial: the constraint is more extractive for communities; coordination function is real but directed by external actors. If community priorities align: coordination is genuinely responsive. This shapes whether victims experience snare, tangled_rope, or rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ngo_agenda_capture_scope, empirical, 'Scope of NGO agenda capture in case selection').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(regional_human_rights_courts, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rhc_tr_t0, regional_human_rights_courts, theater_ratio, 0, 0.4).
narrative_ontology:measurement(rhc_tr_t10, regional_human_rights_courts, theater_ratio, 10, 0.48).
narrative_ontology:measurement(rhc_tr_t20, regional_human_rights_courts, theater_ratio, 20, 0.55).
narrative_ontology:measurement(rhc_tr_t5, regional_human_rights_courts, theater_ratio, 5, 0.44).

% Extraction over time
narrative_ontology:measurement(rhc_be_t0, regional_human_rights_courts, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(rhc_be_t10, regional_human_rights_courts, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(rhc_be_t20, regional_human_rights_courts, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(rhc_be_t5, regional_human_rights_courts, base_extractiveness, 5, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(regional_human_rights_courts, enforcement_mechanism).
narrative_ontology:affects_constraint(regional_human_rights_courts, international_treaty_compliance).
narrative_ontology:affects_constraint(regional_human_rights_courts, state_sovereignty_constraints).
narrative_ontology:affects_constraint(regional_human_rights_courts, ngo_agenda_setting_power).

% DUAL FORMULATION NOTE:
% Regional human rights courts decompose into three structurally distinct constraints: the coordination mechanism (enforcement of human rights norms), the extraction mechanism (asymmetric compliance costs and NGO agenda capture), and the institutional apparatus (theater of due process maintaining legitimacy through procedural ritual). These are linked by network effects: improvements in enforcement increase extraction; institutional degradation increases theater; NGO capture reduces responsiveness to grassroots priorities. Each could be modeled as separate stories with different epsilon values; presented here as a unified tangled_rope because the coordination and extraction functions are inseparable in practice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(regional_human_rights_courts, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
