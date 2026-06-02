% ============================================================================
% CONSTRAINT STORY: tenure_contract__institutional_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tenure_contract__institutional_extraction_reading, []).

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
 *   constraint_id: tenure_contract__institutional_extraction_reading
 *   human_readable: Tenure as Permanent Institutional Rent Extraction
 *   domain: higher_education/labor_economics/institutional_theory
 *
 * SUMMARY:
 *   This constraint models tenure as a permanent rent-extraction mechanism by
 *   early-career winners in the academic labor market, generating cascading
 *   costs borne by contingent faculty and students. The institutional
 *   extraction reading foregrounds how tenure, though claimed to protect
 *   academic freedom and institutional autonomy, functions as a cost-shifting
 *   device that locks resources into permanent salaries for protected faculty
 *   while loading flexibility costs onto contingent workers (adjuncts,
 *   visiting scholars, postdocs) and deferred resource investment in
 *   instruction and student support. Over the past 40 years (t=0 to t=40),
 *   extractiveness has risen from 0.35 to 0.62 as tenure-track hiring has
 *   declined from ~45% of faculty to ~25%, while contingent hiring has
 *   expanded and administrative overhead has grown. Suppression has increased
 *   from 0.42 to 0.68 as transparency about contingent labor conditions has
 *   worsened and the precarity of non-tenure-track positions has deepened.
 *   Theater ratio has risen from 0.32 to 0.58 as tenure's stated coordination
 *   functions (academic freedom, merit protection, shared governance) have
 *   remained rhetorically central while their actual operational role has
 *   diminished relative to the rent-extraction function. This reading
 *   decomposes the contested tenure kernel into its institutional extraction
 *   instantiation; sibling readings (academic_freedom_reading,
 *   demographic_reproduction_reading) emphasize coordination and
 *   knowledge-reproduction functions and have different ε values reflecting
 *   their different observable bases.
 *
 * KEY AGENTS:
 *   - Tenured Faculty (Early Winners): Institutional beneficiary (institutional/arbitrage) — secure permanent claim on institutional resources regardless of productivity; face low suppression; high mobility enables arbitrage between institutions.
 *   - Contingent Labor Force: Primary victim (powerless/trapped) — no path to security; bear full cost of institutional rigidity; cannot exit without abandoning academic career; face high suppression (limited bargaining power, low compensation, no benefits).
 *   - Student Body: Secondary victim (moderate/constrained) — trapped by enrollment requirements; pay tuition dollars that are locked into tenure lines rather than instruction; face reduced instructional investment and course availability.
 *   - University Administration: Institutional beneficiary (powerful/mobile) — extracts administrative overhead from the gap between tenure-locked instructional budgets and actual instructional expenditure; expands deans/provosts/compliance apparatus as instructional faculty are tenure-locked.
 *   - Faculty Senate / Governance Coalition: Mixed (organized/constrained) — experiences genuine coordination function (collective voice, curricular continuity) alongside internal extraction (tenured members benefit at expense of untenured members).
 *   - The Institutional Commitment to Tenure: Neutral performance (institutional/arbitrage) — the formal apparatus maintaining tenure rhetoric while enabling contingent labor expansion; theater function.
 *   - Analytical Observer: Neutral analysis (analytical/analytical) — risks naturalizing tenure as inherent necessity rather than contingent institutional design choice.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tenure_contract__institutional_extraction_reading, 0.62).
domain_priors:suppression_score(tenure_contract__institutional_extraction_reading, 0.68).
domain_priors:theater_ratio(tenure_contract__institutional_extraction_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tenure_contract__institutional_extraction_reading, tangled_rope).
narrative_ontology:human_readable(tenure_contract__institutional_extraction_reading, "Tenure as Permanent Institutional Rent Extraction").
narrative_ontology:topic_domain(tenure_contract__institutional_extraction_reading, "higher_education/labor_economics/institutional_theory").

domain_priors:requires_active_enforcement(tenure_contract__institutional_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tenure_contract__institutional_extraction_reading, 'a334f365-11c9-4e7d-b153-ac5aa5e86e4d').
narrative_ontology:cs_kernel_codification('a334f365-11c9-4e7d-b153-ac5aa5e86e4d', formalized).
narrative_ontology:cs_authority_grounding('a334f365-11c9-4e7d-b153-ac5aa5e86e4d', extraction).
narrative_ontology:cs_interpretation_layer_present('a334f365-11c9-4e7d-b153-ac5aa5e86e4d').
narrative_ontology:cs_reading_relation('a334f365-11c9-4e7d-b153-ac5aa5e86e4d', tenure_contract__academic_freedom_reading, coexists_with).
narrative_ontology:cs_reading_relation('a334f365-11c9-4e7d-b153-ac5aa5e86e4d', tenure_contract__demographic_reproduction_reading, coexists_with).
narrative_ontology:cs_axiom('a334f365-11c9-4e7d-b153-ac5aa5e86e4d', foundational, tenure_is_permanent_resource_claim).
narrative_ontology:cs_axiom_status(tenure_is_permanent_resource_claim, holdable).
narrative_ontology:cs_axiom_grounding('a334f365-11c9-4e7d-b153-ac5aa5e86e4d', tenure_is_permanent_resource_claim, empirically_contingent).
narrative_ontology:cs_axiom('a334f365-11c9-4e7d-b153-ac5aa5e86e4d', foundational, extraction_exceeds_coordination_value).
narrative_ontology:cs_axiom_status(extraction_exceeds_coordination_value, holdable).
narrative_ontology:cs_axiom_grounding('a334f365-11c9-4e7d-b153-ac5aa5e86e4d', extraction_exceeds_coordination_value, empirically_contingent).
narrative_ontology:cs_reference_frame('a334f365-11c9-4e7d-b153-ac5aa5e86e4d', meritocratic_appointment_security).
narrative_ontology:cs_drift_state('a334f365-11c9-4e7d-b153-ac5aa5e86e4d', contemporary_contingent_labor_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a334f365-11c9-4e7d-b153-ac5aa5e86e4d', '').
narrative_ontology:cs_kernel_id(tenure_contract__institutional_extraction_reading, tenure_contract).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tenure_contract__institutional_extraction_reading, tenured_faculty_early_winners).
narrative_ontology:constraint_beneficiary(tenure_contract__institutional_extraction_reading, senior_administrative_apparatus).
narrative_ontology:constraint_victim(tenure_contract__institutional_extraction_reading, contingent_labor_force).
narrative_ontology:constraint_victim(tenure_contract__institutional_extraction_reading, student_body).
narrative_ontology:constraint_victim(tenure_contract__institutional_extraction_reading, department_resource_flexibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONTINGENT LABOR (SNARE) — Trapped by structural necessity: universities require flexible labor to absorb enrollment volatility and cost pressures, but tenure-protected faculty prevent stable careers for contingent workers. No path to security; bear full cost of institutional rigidity. Contingent faculty have minimal exit options (career is built institution-by-institution; moving disciplines is costly) and zero benefit from the tenure regime protecting senior colleagues. Maximum extraction experienced.
constraint_indexing:constraint_classification(tenure_contract__institutional_extraction_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: STUDENT BODY (SNARE) — Trapped by enrollment requirements. When institutional resources are locked into tenure lines, investment in instructional quality, course availability, and student support services decline relative to administrative and tenured faculty compensation. Students pay tuition for increasingly efficient extraction by tenured faculty: their tuition dollars subsidize permanent claims rather than instruction. Cannot exit the institution without abandoning credential. High suppression: limited transparency about where tuition dollars actually flow.
constraint_indexing:constraint_classification(tenure_contract__institutional_extraction_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: TENURED FACULTY / EARLY WINNERS (ROPE) — Institutional beneficiary with high arbitrage capacity: can leverage tenure for mobility (move between institutions, secure funding, direct departmental priorities). Tenure provides coordination function from their perspective: it stabilizes research careers, protects speech, and enables long-term project investment. But this reading foregrounds the extraction: the coordination function exists *alongside* permanent resource-claiming that shifted costs to contingent labor and deferred resource flexibility. Net positive extraction experienced; moderate effective chi because institutional power and mobility reduce suppression.
constraint_indexing:constraint_classification(tenure_contract__institutional_extraction_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: FACULTY SENATE / GOVERNANCE COALITION (TANGLED ROPE) — Organized collective with constrained mobility (can lobby for policy change but cannot unilaterally dissolve tenure). Experiences genuine coordination function (tenure protects collective voice, prevents arbitrary administration, enables curricular continuity) *alongside* asymmetric extraction benefiting senior members. Coalition members have heterogeneous interests: early-career untenured faculty see the constraint as snare-like; tenured faculty see it as rope-like. The coalition's ability to organize around shared interests (academic freedom, shared governance) is real but increasingly strained by internal extraction dynamics.
constraint_indexing:constraint_classification(tenure_contract__institutional_extraction_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: UNIVERSITY ADMINISTRATION (TANGLED_ROPE) — Powerful actor with high mobility (can move to other institutions, influence policy, hire/not hire). Experiences tenure as hybrid coordination-extraction: coordination function enables budget predictability and institutional continuity; extraction function appears as administrative expansion (deans, provosts, compliance officers multiply as core faculty size shrinks — administration extracts from the gap between tenure-locked faculty resources and actual instructional expenditure). Administration has incentive to maintain tenure while expanding non-tenure-track hiring: tenure locks resources; contingency creates flexibility and cost reduction. Moderate effective extraction because administration's power and exit options reduce suppression experience.
constraint_indexing:constraint_classification(tenure_contract__institutional_extraction_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: INSTITUTIONAL COMMITMENT (PITON) — The tenure system itself, evaluated from the perspective of its own stated commitments (academic freedom, shared governance, merit protection). The institutional apparatus claims tenure is essential to these functions; empirically, tenure's coordination role persists but its primary function has shifted: it now functions primarily as rent-seeking machinery for already-protected faculty. The theater is high: institutions maintain tenure rhetoric (defending academic freedom, institutional autonomy) while dismantling tenure-track positions and loading flexibility costs onto contingent workers. The original coordination logic is atrophied; the constraint is maintained by institutional inertia, not because it accomplishes its stated aims. Theater ratio (0.58) reflects that roughly half the formal functions of tenure (academic freedom protection, merit determination) remain genuine, while half have become performative cover for cost-shifting.
constraint_indexing:constraint_classification(tenure_contract__institutional_extraction_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some form of stable employment protection is inherent to knowledge production: researchers need time horizons longer than market cycles to pursue high-risk, high-payoff projects. This perspective sees tenure as a natural structural response to the fundamental mismatch between research timescales and funding cycles. However, the structural data (identified beneficiaries, identified victims, high suppression) reveals this as a false summit: the 'inherent to knowledge production' framing naturalizes what is actually a specific institutional design choice. Other mechanisms (sabbaticals, long-term contracts, grant security, research institutes) could provide similar stability without permanent rents or contingent labor victimization.
constraint_indexing:constraint_classification(tenure_contract__institutional_extraction_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tenure_contract__institutional_extraction_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(tenure_contract__institutional_extraction_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(tenure_contract__institutional_extraction_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(tenure_contract__institutional_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(tenure_contract__institutional_extraction_reading, TR),
    TR >= 0.70.

:- end_tests(tenure_contract__institutional_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High-moderate, capturing the permanent resource claim by tenured faculty. This is not maximal (0.72+) because the coordination function is genuine — tenure does provide some research stability and intellectual protection. But the rising trajectory (0.35→0.62 over 40 years) reflects accumulating extraction as the coordination-to-rent ratio has shifted. The ε value reflects that in the current state, roughly 60% of tenure's structural effect is extractive (resource locking, cost externalization) and 40% is coordinative (stability, continuity). Suppression (0.68): High. Multiple layers of suppression maintain the constraint: (1) information suppression — institutional transparency about contingent labor conditions is limited; few faculty know what adjuncts are paid or what percentage of instruction is contingent. (2) structural suppression — contingent workers have limited bargaining power (competition for positions exceeds availability); cannot unionize effectively (enterprise bargaining is weak across institutions). (3) normative suppression — the academic freedom framing naturalizes tenure, making extraction less visible ('tenure is for everyone's benefit'). (4) exit suppression — leaving academia for non-academic work means abandoning decade-long credential investment. Theater ratio (0.58): Moderate-high. The constraint operates with significant performative content: (a) tenure committees perform merit evaluation processes that are often pro forma (decisions driven by seniority, field prestige, departmental politics rather than actual evaluation of teaching/research quality). (b) Shared governance (faculty senate, curriculum committees) performs institutional voice while administrative power has concentrated in provost/president structures. (c) Academic freedom rhetoric performs the constraint's legitimacy while contingent labor expands and academic speech restrictions (from market pressures, donor demands, admin oversight) increase. The theater is not maximal (≥0.70) because some real functions persist: tenure does constrain arbitrary dismissal (some legal weight), does provide some research continuity, does protect some intellectual speech. But the performative layer is substantial and rising.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a six-fold perspectival split. The tenured faculty see tenure as coordination (Rope) — their perspective is legitimate from their structural position; they experience genuine stability and intellectual freedom. The contingent labor force sees tenure as pure extraction (Snare) — their perspective is equally legitimate; they experience pure cost-shifting without benefit. The faculty senate sees a mixed coordination-extraction hybrid (Tangled Rope) — this reflects real heterogeneity within the collective (tenured members benefit; untenured members are harmed). The administration sees tangled rope as well, but with different beneficiary/victim allocation (administration extracts from the resource gap). The institution (piton perspective) sees a degraded ritual — tenure is maintained as theater while its functions atrophy. The analytical observer risks seeing an immutable natural law (mountain) — research requires stable timescales — but the structural data reveals this as a false summit: alternative mechanisms exist (research institutes, long-term contracts, legal protections) that could provide stability without permanent resource-locking. The perspectival gap reveals that tenure is not universally extractive or coordinative — it is asymmetrically beneficial depending on career stage and labor market position.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from the agent's structural position. The contingent labor force (d ≈ 0.95, trapped with zero arbitrage) experiences maximum effective extraction (χ amplified by f(d) ≈ 1.42 and scope modifier σ=1.0 for national scope). The tenured faculty (d ≈ 0.08, institutional with arbitrage) experience negative effective extraction (χ amplified by f(d) ≈ -0.12) — they are net beneficiaries. The student body (d ≈ 0.80, moderate/constrained, bearing tuition costs without benefit) experience high extraction (f(d) ≈ 1.15). The faculty senate (d ≈ 0.45, organized with constrained exit) experiences moderate extraction (f(d) ≈ 0.40). The administration (d ≈ 0.10, powerful with arbitrage) experiences negative extraction — they benefit from the gap. The institutional commitment (d ≈ 0.65, analytical observer perspective) experiences moderate extraction (f(d) ≈ 1.00). The analytical mountain perspective avoids computing directionality because it claims the constraint is natural law (no beneficiary/victim distinction). The false summit detector will flag this as a misapplication of the mountain classification given the identified beneficiaries and victims.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint's mandatrophy is resolved by distinguishing between the coordination and extraction functions at different career stages and institutional positions. For early-career faculty seeking research security and intellectual protection, tenure coordinates these functions effectively (Rope classification is legitimate). For contingent faculty and students, tenure extracts costs without coordinating their benefits (Snare classification is legitimate). For already-tenured faculty, tenure is a pure benefit-capture mechanism (near-zero-cost rent extraction). The institutional commitment to tenure performs both functions rhetorically while operationally shifting primarily toward extraction. No single type captures the constraint because the constraint's effect is position-dependent. The mandatrophy is resolved by recognizing that this is not a classification failure but a perspectival fact: the same institution-level policy generates different constraint types from different structural positions. The policy is simultaneously Rope (for those it coordinates), Snare (for those it exploits), and Piton (for the institution maintaining degraded rhetoric). The tension between these truths is not resolved by choosing one — it is resolved by modeling all three and recognizing them as the constraint's actual structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tenure_essential_to_academic_freedom,
    'Is tenure contractually essential to protecting academic freedom, or would alternative mechanisms (long-term contracts, legal protections, professional standards, research institute models) provide equivalent protection at lower extraction cost?',
    'Comparative empirical analysis: academic freedom protection levels in tenure vs non-tenure systems (research institutes, European universities with stronger contract law, international comparison); freedom-of-speech litigation outcomes in tenure vs tenure-less contexts; self-censorship metrics by protection mechanism.',
    'If tenure is essential: tenure constraint resolves to pure Rope (coordination function is genuine and irreplaceable). If alternatives suffice: constraint is Snare or Tangled Rope with dysfunctional extraction (rent-seeking without coordination value). This is the primary uncertainty governing whether the institutional extraction reading is a fair reading or a distortion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tenure_essential_to_academic_freedom, empirical, 'Whether tenure is contractually essential to academic freedom protection').

omega_variable(
    contingent_labor_cost_causation,
    'Is contingent labor expansion a direct causal consequence of tenure-locking resources, or would it exist anyway as a result of funding contraction, enrollment volatility, and neoliberal labor market dynamics independent of tenure?',
    'Historical analysis: correlation between tenure expansion (1960s-1970s) and contingent labor emergence (1980s-present); institutional-level comparison of non-tenure-track hiring rates controlling for total budget, enrollment volatility, and state funding levels; counterfactual modeling of labor market equilibrium without tenure locking.',
    'If causally central: the extraction reading is accurate — tenure directly forces contingency onto junior labor. If tenure is merely one factor among larger structural forces: the extraction attribution should weight other causes (funding contraction, market deregulation, degree inflation). Affects interpretation of suppression magnitude (0.68) and victim assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contingent_labor_cost_causation, empirical, 'Causal relationship between tenure expansion and contingent labor growth').

omega_variable(
    administrative_expansion_mechanism,
    'Is administrative expansion (deans, provosts, compliance offices) causally driven by the gap between tenure-locked instructional budgets and actual instructional expenditure, or is it an independent growth of bureaucratic apparatus regardless of tenure?',
    'Time-series analysis of administrative headcount vs contingent faculty headcount vs tenure-track faculty headcount, controlling for institution size and mission; inter-institutional comparison of administrative/instructional ratio in tenure-dominant vs tenure-minimal systems; budget allocation tracking (where do the dollars locked in tenure lines NOT go?).',
    'If causally linked: administrative growth is a direct mechanism of tenure-driven extraction (tenured resources are claimed but not delivered to students; the gap is filled by administrative roles). If independent: administrative expansion is a separate institutional pathology. Affects interpretation of who benefits and how the constraint extracts.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(administrative_expansion_mechanism, empirical, 'Causal mechanism linking tenure-locking to administrative expansion').

omega_variable(
    kernel_reading_alternative_framing,
    'Is this institutional extraction reading a fair characterization of tenure''s structural effects, or does it selectively foreground extraction while backgrounding genuine coordination functions that this reading''s siblings emphasize?',
    'Comparative analysis of the three sibling readings: academic_freedom_reading (foregrounds tenure''s role in protecting intellectual voice against political/economic pressure) and demographic_reproduction_reading (foregrounds tenure''s role in enabling demographic stability and intergenerational knowledge transmission). This omega acknowledges that all three readings are legitimate perspectival framings of the same institutional arrangement. The extraction reading is not uniquely true; it is one reading among coherent alternatives. The engine will compute foreclosure relationships via cs_structure.reading_relations.',
    'If the extraction reading is one legitimate reading among coequal alternatives: the constraint instantiates genuine epistemic pluralism (multiple framings, none alone complete). If the extraction reading is the correct one and its siblings are ideological cover: the constraint instantiates false consciousness. The uncertainty is conceptual (how to weigh competing legitimate frames) rather than empirical. This omega routes the kernel ambiguity through the formal apparatus.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_alternative_framing, conceptual, 'Whether this reading is one legitimate framing among alternatives or the correct reading disguised by ideological siblings').

omega_variable(
    international_model_viability,
    'Could the US higher education system adopt the European model (permanent security without tenure, via stronger employment law and union contracts) without losing the coordination functions tenure provides?',
    'Comparative institutional analysis: academic freedom metrics, research productivity, faculty mobility, and institutional autonomy in European tenure-lite systems vs US tenure systems; cost structures and resource allocation patterns; time-series data on research output and citation impact pre/post European labor law strengthening.',
    'If viable: tenure is one solution among many; the permanent-contract model demonstrates that coordination can be achieved without permanent resource-locking. Suggests the extraction reading is capturing real structural excess (rent-seeking beyond coordination necessity). If not viable: US tenure is a functional necessity given US institutional context (private funding, state-level governance fragmentation, weak employment law). Affects interpretation of whether extraction is contingent (fixable) or structural (inherent).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_model_viability, empirical, 'Viability of European employment law model as alternative to US tenure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tenure_contract__institutional_extraction_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tenure_theater_t0, tenure_contract__institutional_extraction_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(tenure_theater_t20, tenure_contract__institutional_extraction_reading, theater_ratio, 20, 0.48).
narrative_ontology:measurement(tenure_theater_t40, tenure_contract__institutional_extraction_reading, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(tenure_extraction_t0, tenure_contract__institutional_extraction_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(tenure_extraction_t20, tenure_contract__institutional_extraction_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(tenure_extraction_t40, tenure_contract__institutional_extraction_reading, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(tenure_suppression_t0, tenure_contract__institutional_extraction_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(tenure_suppression_t20, tenure_contract__institutional_extraction_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(tenure_suppression_t40, tenure_contract__institutional_extraction_reading, suppression_requirement, 40, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tenure_contract__institutional_extraction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(tenure_contract__institutional_extraction_reading, 0.12).
narrative_ontology:affects_constraint(tenure_contract__institutional_extraction_reading, tenure_contract__academic_freedom_reading).
narrative_ontology:affects_constraint(tenure_contract__institutional_extraction_reading, tenure_contract__demographic_reproduction_reading).
narrative_ontology:affects_constraint(tenure_contract__institutional_extraction_reading, contingent_labor_precarity).
narrative_ontology:affects_constraint(tenure_contract__institutional_extraction_reading, administrative_overhead_expansion).
narrative_ontology:affects_constraint(tenure_contract__institutional_extraction_reading, higher_education_cost_inflation).

% DUAL FORMULATION NOTE:
% The tenure_contract kernel decomposes into at least three structurally distinct constraints with different ε values: (1) institutional_extraction_reading (ε=0.62, this constraint) — foregrounds resource-locking and cost-shifting to contingent labor; (2) academic_freedom_reading (ε≈0.15, sibling) — foregrounds protection of intellectual voice against political/market pressure; (3) demographic_reproduction_reading (ε≈0.25, sibling) — foregrounds enabling intergenerational knowledge transmission. These are not the same constraint viewed from different angles. Their ε values differ by a factor of four. They have different beneficiary/victim structures, different failure modes, and different empirical signatures. Each reading is linked via network.affects_constraints because they compete to interpret the same institutional arrangement. The extraction reading claims that the freedom and reproduction readings are cover stories for extraction; the freedom reading claims that extraction is a side effect of genuine coordination; the reproduction reading claims that both extraction and freedom-protection are subordinate to demographic transmission. The sibling readings are separate constraint stories with their own perspectives, omegas, and measurements.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tenure_contract__institutional_extraction_reading, institutional, 0.08).
constraint_indexing:directionality_override(tenure_contract__institutional_extraction_reading, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
