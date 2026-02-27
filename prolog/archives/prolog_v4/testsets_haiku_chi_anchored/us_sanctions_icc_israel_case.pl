% ============================================================================
% CONSTRAINT STORY: us_sanctions_icc_israel_case
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_sanctions_icc_israel_case, []).

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
 *   constraint_id: us_sanctions_icc_israel_case
 *   human_readable: US Sanctions Threat Against ICC Officials Investigating Israel
 *   domain: political/international_law
 *
 * SUMMARY:
 *   In 2024, the US Senate proposed legislation to impose sanctions (visa
 *   bans, asset freezes) against International Criminal Court officials
 *   investigating alleged Israeli war crimes in Gaza. This constraint
 *   exemplifies coercive pressure on international judicial institutions and
 *   exhibits sharp perspectival divergence. From the ICC's and international
 *   civil society's viewpoint, the sanctions threat constitutes pure
 *   extraction: a powerful state using unilateral coercive instruments to
 *   prevent an independent court from investigating claims within its
 *   mandate. From the beneficiary states (US legislative majority and Israeli
 *   government), the constraint operates as coordination: alignment of
 *   legislative will with executive foreign policy to deter investigations
 *   that would constrain allied security operations. The theater ratio
 *   reflects that sanctions threats are partly performative — political
 *   signaling to domestic constituencies and the Israeli government — and
 *   partly functional coercion. The extraction mechanism operates by raising
 *   the personal and institutional cost of ICC investigative work to the
 *   point where investigators face visa bans, asset seizures, and career
 *   termination, creating a chilling effect on prosecutorial activity. The
 *   suppression is high because ICC officials have no genuine exit option:
 *   they cannot resign without abandoning the institution's mandate, cannot
 *   negotiate away the threat without self-censorship, and cannot appeal to a
 *   higher authority.
 *
 * KEY AGENTS:
 *   - ICC Investigative Officials: Primary victims (powerless/trapped) — face personal sanctions (visa bans, asset freezes, potential arrest) for performing their mandated duties
 *   - ICC Institutional Independence: Structural victim (powerless/trapped) — prosecutorial autonomy is being coercively constrained; institution cannot exit without self-destruction
 *   - International Criminal Justice System: Systemic victim (powerless/trapped) — precedent-setting extraction mechanism that enables other states to weaponize sanctions against ICC prosecutors investigating their conduct
 *   - Potential Plaintiffs (Palestinians, Others): Indirect victims (powerless/trapped) — access to accountability mechanisms is being chilled; cannot prevent the sanctions threat
 *   - US Legislative Majority: Primary beneficiary (institutional/arbitrage) — uses sanctions threat as policy tool; experiences constraint as coordination of executive-legislative alignment
 *   - Israeli Government: Co-beneficiary (institutional/arbitrage) — benefits from deterrence of investigations; experiences constraint as allied support
 *   - US State/Judicial Branch: Institutional opponent (powerful/mobile) — executive and judicial branches may oppose sanctions as undermining international law credibility; exhibits internal state tension
 *   - Coalition of ICC Member States: Organized resistor (organized/constrained) — EU, Canada, sympathetic states can coordinate counter-sanctions but are constrained by dependence on US relationships
 *   - International Human Rights Community: Organized victim (organized/constrained) — can document and advocate against sanctions but lacks enforcement power; constrained by fear of secondary sanctions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_sanctions_icc_israel_case, 0.58).
domain_priors:suppression_score(us_sanctions_icc_israel_case, 0.72).
domain_priors:theater_ratio(us_sanctions_icc_israel_case, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_sanctions_icc_israel_case, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_sanctions_icc_israel_case, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(us_sanctions_icc_israel_case, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_sanctions_icc_israel_case, snare).
narrative_ontology:human_readable(us_sanctions_icc_israel_case, "US Sanctions Threat Against ICC Officials Investigating Israel").
narrative_ontology:topic_domain(us_sanctions_icc_israel_case, "political/international_law").

domain_priors:requires_active_enforcement(us_sanctions_icc_israel_case).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_sanctions_icc_israel_case, israeli_government).
narrative_ontology:constraint_beneficiary(us_sanctions_icc_israel_case, us_legislative_majority).
narrative_ontology:constraint_victim(us_sanctions_icc_israel_case, icc_institutional_independence).
narrative_ontology:constraint_victim(us_sanctions_icc_israel_case, international_criminal_justice_system).
narrative_ontology:constraint_victim(us_sanctions_icc_israel_case, potential_plaintiffs_palestinians).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ICC OFFICIALS (SNARE) — Cannot exit investigation without career termination and personal legal jeopardy. Sanctions threat (visa bans, asset freezes) targets them individually. No alternative career path in international law preserves their role. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.96. Pure extraction: coercion with no coordination benefit.
constraint_indexing:constraint_classification(us_sanctions_icc_israel_case, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ICC INSTITUTIONAL INDEPENDENCE (SNARE) — The Court's prosecutorial mandate and impartiality are the victims. Threatened sanctions constitute extrajudicial pressure on judicial process. The institution has no exit option: abandoning investigations abandons its legal mandate. Cannot negotiate away the threat without institutional self-destruction. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.98. Maximum extraction targeting abstract institutional commons.
constraint_indexing:constraint_classification(us_sanctions_icc_israel_case, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: INTERNATIONAL CRIMINAL JUSTICE SYSTEM (SNARE) — Precedent-setting extraction mechanism. Other states can weaponize sanctions against ICC prosecutors investigating their nationals. The system has no exit: accepting the precedent enables future abuse; rejecting it risks escalating sanctions. Trapped by structural interdependence. d≈0.93, f(d)≈1.40, σ=1.2 → χ≈0.97.
constraint_indexing:constraint_classification(us_sanctions_icc_israel_case, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 4: POTENTIAL PLAINTIFFS / PALESTINIANS (SNARE) — Access to accountability is being denied. Threats against investigators chilling the ICC process reduce credible avenues for justice claims. Plaintiffs have no leverage to prevent sanctions, no exit option, and no alternative court with jurisdiction. d≈0.94, f(d)≈1.41, σ=1.2 → χ≈0.97.
constraint_indexing:constraint_classification(us_sanctions_icc_israel_case, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 5: US LEGISLATIVE MAJORITY (ROPE) — Experiences constraint as coordination mechanism: signaling domestic support for Israeli government, demonstrating legislative-executive alignment on allied interests, and coordinating with other states on international pressure. Uses sanctions threat as policy tool. d≈0.08, f(d)≈-0.08, σ=1.0 → χ≈-0.005. Net beneficiary; sees it as legitimate coordination, not extraction.
constraint_indexing:constraint_classification(us_sanctions_icc_israel_case, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ISRAELI GOVERNMENT (ROPE) — Beneficiary using sanctions threat as policy tool to prevent investigations that would constrain its military/security operations. Experiences the constraint as coordination: alignment with a superpower ally. d≈0.10, f(d)≈-0.06, σ=1.0 → χ≈-0.004. Net beneficiary.
constraint_indexing:constraint_classification(us_sanctions_icc_israel_case, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: INTERNATIONAL HUMAN RIGHTS COMMUNITY (SNARE) — Organized but constrained. NGOs, legal scholars, and civil society can document the extraction mechanism and advocate for ICC protection, but lack enforcement power against US sanctions authority. Exit is constrained: accepting the precedent weakens international accountability; fighting it risks secondary sanctions and reputational attack. d≈0.82, f(d)≈1.18, σ=1.2 → χ≈0.87.
constraint_indexing:constraint_classification(us_sanctions_icc_israel_case, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: US STATE/JUDICIAL BRANCH (TANGLED ROPE) — Institutional tension. Executive and judicial branches may oppose sanctions as undermining international law and US credibility on human rights. Legislative branch benefits. Same nation-state, different branches, different exit options and structural interests. State has mobility (can refuse to enforce, can challenge constitutionality) but faces internal institutional conflict. d≈0.48, f(d)≈0.62, σ=1.0 → χ≈0.37.
constraint_indexing:constraint_classification(us_sanctions_icc_israel_case, tangled_rope,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 9: COALITION OF ICC MEMBER STATES (SCAFFOLD) — Organized actors (EU, Canada, and sympathetic states) can coordinate counter-sanctions and defend ICC investigative independence, but this coordination is temporary/situational. Exit is constrained by dependence on US relationships in other domains. The coalition sees the threat as a solvable problem through institutional negotiation and mutual support. Has agency but within bounds. d≈0.42, f(d)≈0.43, σ=1.1 → χ≈0.20.
constraint_indexing:constraint_classification(us_sanctions_icc_israel_case, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_sanctions_icc_israel_case_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_sanctions_icc_israel_case, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_sanctions_icc_israel_case, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_sanctions_icc_israel_case, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_sanctions_icc_israel_case_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The sanctions threat directly extracts from ICC officials through coercive instruments (visa bans, asset freezes) targeting them personally for performing their duties. The extraction is not absolute (some officials may have citizenship/assets outside US jurisdiction) but is severe enough to deter compliance. The trajectory (0.35 → 0.58 over 6 time units) reflects growing enforcement pressure and hardening of threats. Suppression (0.72): High. ICC officials have no practical exit options: (1) resign = institutional death, (2) investigate anyway = personal sanctions, (3) self-censor = institutional compromise. All three paths are closed. The suppression is structural to the threat model — the sanctions mechanism specifically targets the individuals performing the investigative work, creating a personal cost that cannot be absorbed by the institution. Theater ratio (0.65): Moderate-high. The sanctions threat is partly performative (signaling to domestic constituents that the US stands with Israel, demonstrating legislative-executive unity) and partly functional (creating actual legal jeopardy and institutional pressure). The theater reflects that the primary audience is often domestic US politics and the Israeli government rather than ICC procedural dynamics. As the threat hardens toward actual legislation, theater may decline slightly (more functional, less performative), but the signaling function remains core.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits one of the sharpest perspectival gaps in the corpus. From the ICC and international criminal justice perspectives, the constraint is an absolute Snare: high extraction (0.58), high suppression (0.72), targeting vulnerable institutions (the Court) and individuals (prosecutors) with no exit options. From the beneficiary perspectives (US legislative majority, Israeli government), the constraint is a Rope: they experience it as legitimate coordination of aligned interests, with d≈0.08 producing negative effective extraction (they are net beneficiaries). The US State/Judicial branch exhibits a Tangled Rope reading (internal institutional conflict within the US state itself). The international human rights community and allied state coalition see it as Snare but with some organized capacity for resistance (downgrade to Snare with constrained exit, not trapped exit). The perspective divergence is not observational (all perspectives agree on the base properties), but interpretive: the beneficiary perspective naturalizes coercion as coordination, while the victim perspective recognizes the coercive mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   ICC officials & ICC independence: Victims + trapped → d≈0.92-0.95, f(d)≈1.38-1.42. Maximum extraction. International criminal justice system: Victim + trapped (system-level) → d≈0.93, f(d)≈1.40. Near-maximum. Potential plaintiffs: Victim + trapped (no leverage) → d≈0.94, f(d)≈1.41. Near-maximum. US legislative majority: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Net beneficiary (negative extraction = coordination). Israeli government: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.06. Net beneficiary. US State/Judicial branch: Mixed (some benefit, some cost) + mobile → d≈0.48, f(d)≈0.62. Moderate effective extraction, reflecting internal institutional conflict. International human rights community: Victim + constrained (can resist but at cost) → d≈0.82, f(d)≈1.18. High extraction but not maximum, reflecting some organized capacity. Allied state coalition: Victim + constrained (can resist through coordination) → d≈0.42, f(d)≈0.43. Lower extraction than individual ICC officials because this agent has collective agency.
 *
 * MANDATROPHY ANALYSIS:
 *   CRITICAL MANDATROPHY RESOLUTION: The constraint's base properties (ε=0.58, suppression=0.72, theater=0.65) might be misread as Tangled Rope (hybrid coordination-extraction) if the beneficiary perspective were treated as primary. However, the mandatrophy is resolved by recognizing that the beneficiary's 'coordination' experience is actually coercive alignment: the US legislative majority experiences it as coordination because it benefits from coercion; the victims experience it as Snare because they bear the extraction. The constraint is fundamentally extractive (Snare), not hybrid. The beneficiary 'coordination' is simply the structural position of those who benefit from a coercive mechanism — it doesn't change the mechanism's character. The classification as Snare (not Tangled Rope) is justified by: (1) suppression=0.72 exceeds the Tangled Rope suppression minimum (0.40) and approaches Snare levels (0.60+), (2) the threat produces no genuine coordination benefit for the victims (ICC has no improved capacity to investigate, only reduced capacity to resist), (3) the constraint's existence depends entirely on coercive enforcement, not on solving a collective action problem. If the 'coordination' were real, removing the sanctions threat would result in continued cooperation. In reality, removing the threat would allow ICC investigations to proceed — proving the coordination narrative is false.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mandatory_vs_discretionary_sanctioning,
    'Does the bill''s language create a binding legal obligation to impose sanctions, or is implementation discretionary/subject to executive waiver?',
    'Legal analysis of bill text; precedent review of how similar US sanctions mandates have been enforced or waived; analysis of executive enforcement history',
    'If mandatory: extraction mechanism is automatic and inescapable (Snare confirmed for all perspectives). If discretionary: extraction becomes a threat of potential action rather than certain punishment (downgrade to Tangled Rope from beneficiary perspectives).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandatory_vs_discretionary_sanctioning, empirical, 'Whether sanctions are legally binding or subject to executive discretion').

omega_variable(
    icc_investigative_scope_and_selectivity,
    'Does the ICC investigation target Israeli conduct exclusively, or is it part of broader Palestine/Occupied Territories jurisdiction that would include Palestinian Authority conduct?',
    'Review of ICC prosecutor''s mandate and investigative scope documentation; analysis of cases actually initiated; interviews with ICC staff on investigation parameters',
    'If selective targeting of Israel: extraction mechanism is weaponized against a specific state (high-confidence Snare). If symmetric investigation of both Israeli and Palestinian conduct: sanctions constitute retaliation against investigation itself rather than bias (reframes threat from selective to coercive).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(icc_investigative_scope_and_selectivity, empirical, 'Whether ICC investigation is symmetric or selectively targets Israeli conduct').

omega_variable(
    us_credibility_cost_threshold,
    'At what point does the cost of US credibility erosion on international law and human rights enforcement exceed the political benefit of deterring ICC investigations?',
    'Longitudinal analysis of US soft power metrics, alliance confidence indices, and third-state willingness to cooperate on international legal instruments; cost-benefit analysis from US foreign policy establishment',
    'If credibility cost is already exceeded: sanctions bill generates Pyrrhic extraction (short-term coercion, long-term institutional cost). If threshold is far off: extraction mechanism is sustainable and Snare classification is durable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(us_credibility_cost_threshold, preference, 'Threshold at which credibility cost exceeds political benefit of deterrence').

omega_variable(
    allied_state_response_to_sanctions_precedent,
    'Will other US-aligned states (EU, Canada, UK, Japan) impose counter-sanctions or provide legal/diplomatic refuge to ICC officials, or will they defer to US pressure?',
    'Statements from allied governments; analysis of historical precedent for allied coordination against US sanctions; diplomatic pressure campaigns and their outcomes',
    'If strong allied coordination: ICC officials have de facto refuge, sanctions become performative (theater rises, effective extraction drops). If allied deference: sanctions are effective, extraction mechanism is fully enforced, Snare classification is robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(allied_state_response_to_sanctions_precedent, empirical, 'Whether allied states will support ICC officials against US sanctions').

omega_variable(
    investigation_suspension_vs_abandonment,
    'Can ICC suspend investigations under political pressure without formally abandoning them, creating a gray zone of indefinite delay?',
    'Review of ICC procedures for case suspension vs closure; precedent analysis of how political pressure has affected investigation timelines; institutional capacity assessment for managing suspended cases',
    'If suspension is possible: extraction mechanism can operate without triggering institutional death (investigation continues in name, extraction continues in practice). If suspension is not institutionally viable: ICC must choose between extraction (capitulating to sanctions) and institutional collapse (refusing and accepting sanctions).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(investigation_suspension_vs_abandonment, empirical, 'Whether ICC can suspend investigations indefinitely without formal closure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_sanctions_icc_israel_case, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ussicc_tr_t0, us_sanctions_icc_israel_case, theater_ratio, 0, 0.45).
narrative_ontology:measurement(ussicc_tr_t3, us_sanctions_icc_israel_case, theater_ratio, 3, 0.58).
narrative_ontology:measurement(ussicc_tr_t6, us_sanctions_icc_israel_case, theater_ratio, 6, 0.65).

% Extraction over time
narrative_ontology:measurement(ussicc_be_t0, us_sanctions_icc_israel_case, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ussicc_be_t3, us_sanctions_icc_israel_case, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(ussicc_be_t6, us_sanctions_icc_israel_case, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_sanctions_icc_israel_case, enforcement_mechanism).
narrative_ontology:affects_constraint(us_sanctions_icc_israel_case, international_criminal_court_independence).
narrative_ontology:affects_constraint(us_sanctions_icc_israel_case, us_executive_enforcement_capacity).
narrative_ontology:affects_constraint(us_sanctions_icc_israel_case, allied_state_coordination_capacity).

% DUAL FORMULATION NOTE:
% The sanctions threat against ICC officials is downstream of the broader constraint on international criminal justice independence. Two related but distinct constraints: (1) International Criminal Court Institutional Independence (higher ε, Mountain-trending claim about institutional autonomy) affects (2) US Sanctions Threat Against ICC Officials (lower ε but higher suppression, Snare mechanism). The sanctions threat is a specific instantiation of pressure on the broader system. They share the same beneficiaries (Israeli government, US security establishment) but the upstream constraint is broader (any investigation that disadvantages Israeli interests) while the downstream constraint is specific (this legislative mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_sanctions_icc_israel_case, organized, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
