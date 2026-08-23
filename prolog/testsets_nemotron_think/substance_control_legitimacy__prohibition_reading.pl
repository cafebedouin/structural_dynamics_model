% ============================================================================
% CONSTRAINT STORY: substance_control_legitimacy__prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_legitimacy__prohibition_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: substance_control_legitimacy__prohibition_reading
 *   human_readable: Prohibition Reading of Substance Control Legitimacy
 *   domain: public_health/criminal_justice/political_economy
 *
 * SUMMARY:
 *   The prohibition reading asserts that substance use is inherently harmful
 *   and that state authority derives from a moral duty to prevent this harm
 *   through criminalization. This reading instantiates the
 *   substance_control_legitimacy kernel by placing criminalization at the
 *   center of the state's protective function. The constraint operates
 *   through a carceral apparatus that extracts labor, liberty, and life
 *   chances from people who use drugs, while generating a black market
 *   violence externality that further justifies enforcement expansion.
 *   Beneficiaries include law enforcement agencies (budget/mandate), the
 *   prison-industrial complex (captive labor/population), political actors
 *   campaigning on 'tough on crime' platforms, and black market actors who
 *   capture the monopoly rents created by prohibition. Victims are people who
 *   use drugs (criminalized, incarcerated, denied healthcare), communities
 *   targeted by selective enforcement (racialized policing), families of
 *   incarcerated (economic/social devastation), and people denied healthcare
 *   access (fear of prosecution). The claimed coordination function —
 *   preventing drug harm — is the cover story; the operational reality is a
 *   snare where extraction is the function and harm prevention is the
 *   justification.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_legitimacy__prohibition_reading, 0.82).
domain_priors:suppression_score(substance_control_legitimacy__prohibition_reading, 0.88).
domain_priors:theater_ratio(substance_control_legitimacy__prohibition_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, accessibility_collapse, 0.73).
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_legitimacy__prohibition_reading, snare).
narrative_ontology:human_readable(substance_control_legitimacy__prohibition_reading, "Prohibition Reading of Substance Control Legitimacy").
narrative_ontology:topic_domain(substance_control_legitimacy__prohibition_reading, "public_health/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_legitimacy__prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_legitimacy__prohibition_reading, '6f7b319f-f53c-4f87-9990-8cd270f33424').
narrative_ontology:cs_kernel_codification('6f7b319f-f53c-4f87-9990-8cd270f33424', formalized).
narrative_ontology:cs_authority_grounding('6f7b319f-f53c-4f87-9990-8cd270f33424', extraction).
narrative_ontology:cs_interpretation_layer_present('6f7b319f-f53c-4f87-9990-8cd270f33424').
narrative_ontology:cs_reading_relation('6f7b319f-f53c-4f87-9990-8cd270f33424', substance_control_legitimacy__harm_reduction_reading, coexists_with).
narrative_ontology:cs_reading_relation('6f7b319f-f53c-4f87-9990-8cd270f33424', substance_control_legitimacy__legalization_reading, coexists_with).
narrative_ontology:cs_axiom('6f7b319f-f53c-4f87-9990-8cd270f33424', foundational, criminalization_is_moral_duty).
narrative_ontology:cs_axiom_status(criminalization_is_moral_duty, holdable).
narrative_ontology:cs_axiom_grounding('6f7b319f-f53c-4f87-9990-8cd270f33424', criminalization_is_moral_duty, deontological).
narrative_ontology:cs_axiom('6f7b319f-f53c-4f87-9990-8cd270f33424', foundational, use_is_inherently_harmful).
narrative_ontology:cs_axiom_status(use_is_inherently_harmful, holdable).
narrative_ontology:cs_axiom_grounding('6f7b319f-f53c-4f87-9990-8cd270f33424', use_is_inherently_harmful, empirically_contingent).
narrative_ontology:cs_axiom('6f7b319f-f53c-4f87-9990-8cd270f33424', secondary, state_owns_body_autonomy).
narrative_ontology:cs_axiom_status(state_owns_body_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('6f7b319f-f53c-4f87-9990-8cd270f33424', state_owns_body_autonomy, deontological).
narrative_ontology:cs_reference_frame('6f7b319f-f53c-4f87-9990-8cd270f33424', international_prohibition_regime).
narrative_ontology:cs_drift_state('6f7b319f-f53c-4f87-9990-8cd270f33424', contemporary_reform_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6f7b319f-f53c-4f87-9990-8cd270f33424', '').
narrative_ontology:cs_kernel_id(substance_control_legitimacy__prohibition_reading, substance_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, law_enforcement_agencies).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, prison_industrial_complex).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, political_actors_tough_on_crime).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, black_market_actors).
narrative_ontology:constraint_victim(substance_control_legitimacy__prohibition_reading, people_who_use_drugs).
narrative_ontology:constraint_victim(substance_control_legitimacy__prohibition_reading, communities_targeted_by_enforcement).
narrative_ontology:constraint_victim(substance_control_legitimacy__prohibition_reading, families_of_incarcerated).
narrative_ontology:constraint_victim(substance_control_legitimacy__prohibition_reading, people_denied_healthcare_access).
narrative_ontology:constraint_vindicates(substance_control_legitimacy__prohibition_reading, state_moral_duty_to_prevent_self_harm).
narrative_ontology:constraint_vindicates(substance_control_legitimacy__prohibition_reading, criminalization_as_protective_paternalism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Criminalized for possession/use; face incarceration, loss of housing/employment/children, denial of healthcare, stigma that fuses with identity ('addict' label). Exit requires stopping use (often physiologically/psychologically difficult), leaving jurisdiction (barred by record/poverty), or waiting for policy change (decades). The constraint makes their survival strategies (buying, using, sharing) into crimes.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, people_who_use_drugs, payer,
    powerless, biographical, identity_locked, national).

% Experience selective enforcement: higher arrest/incarceration rates despite similar use rates. Community fabric torn by mass incarceration — lost earners, caregivers, voters. Police violence, surveillance, asset forfeiture drain resources. Exit means geographic displacement (gentrification, economic pressure) or political organizing (met with repression).
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, communities_targeted_by_enforcement, payer,
    powerless, generational, constrained, regional).

% Bear diffuse costs: lost income, caregiving burden, trauma, stigmatization, barriers to visitation/communication, foster care system involvement for children. No direct leverage over the constraint; exit means severing family ties or absorbing costs indefinitely.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, families_of_incarcerated, payer,
    powerless, biographical, constrained, local).

% Fear of prosecution deters seeking overdose reversal, syringe exchange, medication-assisted treatment, prenatal care. Pregnant women avoid prenatal care to avoid child welfare reporting. People who inject drugs avoid wound care. The constraint turns healthcare into a surveillance trap.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, people_denied_healthcare_access, payer,
    powerless, immediate, trapped, national).

% Set enforcement priorities, control drug task forces, administer asset forfeiture programs, lobby for harsher penalties and funding. Drug enforcement provides ~30% of local police activity, justifies budgets, equipment, overtime, federal grants. Can pivot to other enforcement if drug war winds down (traffic, immigration, terrorism) — arbitrage-grade exit.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, law_enforcement_agencies, agenda_setter,
    institutional, generational, arbitrage, national).

% Private prisons, prison labor contractors, commissary/phone/service monopolies capture carceral revenue. Drug offenses provide ~20% of state/federal prison population — stable 'customer base.' Lobby for mandatory minimums, against sentencing reform. Can diversify into immigrant detention, electronic monitoring — mobile exit.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, prison_industrial_complex, beneficiary,
    organized, generational, mobile, national).

% Extract votes, donations, media attention from prohibition stance. 'Tough on crime' branding wins primaries, especially in suburban/rural districts. Constrained exit: reversing position risks primary challenge; but some have pivoted to reform rhetoric as public opinion shifts.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, political_actors_tough_on_crime, beneficiary,
    institutional, biographical, constrained, national).

% Capture monopoly rents created by prohibition — price inflation of 100x-1000x over production cost. Enforcement risk is cost of business; violence enforces contracts where courts cannot. Constrained exit: legalization would destroy business model; some diversify into legal cannabis, but core model depends on prohibition.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, black_market_actors, beneficiary,
    powerful, biographical, constrained, global).

% Advocate for harm reduction (syringe exchange, safe consumption, MAT, decriminalization) but structurally excluded from drug policy table — DEA, ONDCP, UN CND are law-enforcement-led. Funding restricted by 'anti-prostitution pledge' and similar ideological bars. Can move to research, international NGOs, or states with reform — mobile exit but voice excluded.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, public_health_practitioners, excluded,
    organized, biographical, mobile, national).

% Litigation, ballot initiatives, legislative lobbying, public education. Some victories (state cannabis legalization, Oregon decrim, federal sentencing reform) but federal prohibition and international treaties remain. Analytical seat: they see the full structure but lack power to change it directly.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, reform_advocates, observer,
    organized, generational, analytical, global).

% UN CND/INCB/UNODC maintain the 1961/1971/1988 treaty framework. WHO recommends decriminalization but CND blocks. Analytical seat: they administer the kernel's formalization but are captured by prohibition_reading's institutional inertia.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, international_bodies, observer,
    institutional, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Claims to solve: preventing substance use harm through supply reduction and deterrence. Actual coordination: synchronizes law enforcement, courts, prisons, and international treaties around a shared target population — a coordination of extraction, not care.
% TRANSFER_FUNCTION: Moves liberty, labor, civic rights, family integrity, and public health resources from people who use drugs and targeted communities to law enforcement budgets, prison contractors, political capital, and black market profits. The transfer is enforced by threat of cage; the 'service' (harm prevention) is not delivered.
% ABSENT_VOICES: People who use drugs are structurally excluded from policy formation — criminalization makes their participation illegal (cannot organize without conspiracy charges), and their credibility is dismissed by virtue of their status. Families of incarcerated lack political organization. Global South producer/transit countries bear violence/corruption costs but have no vote in CND. These voices would object to the constraint's harm multiplication if present.
% DISAPPEARANCE_RATIONALE: If prohibition vanished overnight: black markets would collapse (price compression, violence reduction); incarceration rates would plummet (immediate release for possession); healthcare access would expand (fear barrier removed); law enforcement would lose primary mandate (budget crisis); prison industry would lose 20% population; political 'tough on crime' platform would evaporate; international treaty regime would fracture. The world would rearrange dramatically — new regulatory frameworks would emerge (as with alcohol post-1933).
% FOUNDING_PROBLEM: Mid-20th century moral panic about 'narcotics' and 'addiction' framed as existential threat to social order, fueled by racialized media narratives (Anslinger's 'reefer madness'), international pressure (1961 Single Convention), and professional expansionism (law enforcement, psychiatry). The problem was framed as: 'How do we protect society from the scourge of drugs?'
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is dead per: (1) UNODC's own data: 60 years of prohibition, global drug use increased, markets diversified, potency increased; (2) Johns Hopkins-Lancet Commission (2016): prohibition fails health/human rights goals; (3) Former presidents (Cardoso, Zedillo, Gaviria) of Global Commission on Drug Policy: 'war on drugs failed'; (4) WHO/UNODC/UNAIDS joint statement (2017): decriminalize possession; (5) No credible empirical study shows criminalization reduces population-level use or harm. The prohibition_reading's own beneficiaries (DEA, ONDCP) no longer claim eradication — they claim 'disruption' and 'management,' implicitly conceding the founding problem is unsolvable by their means.
narrative_ontology:disappearance_verdict(substance_control_legitimacy__prohibition_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_legitimacy__prohibition_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_legitimacy__prohibition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(substance_control_legitimacy__prohibition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_legitimacy__prohibition_reading, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_legitimacy__prohibition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_legitimacy__prohibition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_legitimacy__prohibition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.82) because the carceral system extracts massive value (liberty, labor, civic participation, family integrity) from a targeted population while the constraint's stated purpose (harm prevention) is contradicted by outcomes: prohibition correlates with increased overdose mortality, disease transmission, and violence. Suppression is extreme (0.88) because alternatives (harm reduction, regulated supply, decriminalization) are actively suppressed through law, funding restrictions, and international treaty obligations. Theater ratio is moderate (0.42) — enforcement rituals (raids, seizures, public destruction of drugs) are performative; they do not reduce supply or use but sustain the constraint's legitimacy theater. Accessibility collapse is high (0.73) — once criminalized, users face collapsed exit options: legal supply is banned, healthcare access is surveilled, employment/housing are denied via record. Resistance is significant (0.61) from reform movements, affected communities, public health practitioners, and some jurisdictions — but resistance is fragmented and met with federal/international pushback.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (law enforcement, politicians), the constraint appears as necessary coordination — the only thing standing between society and drug-fueled collapse. From the payer seats (users, targeted communities, families), the same structure is experienced as a snare: extraction without consent, alternatives suppressed, harm multiplied. The engine computes this divergence from the structural data: the agenda_setter's low d yields low/negative χ (subsidy), while payers' high d yields χ ≈ ε. The black market actors' beneficiary status is structural irony — they profit from the suppression that the agenda_setter claims to oppose.
 *
 * DIRECTIONALITY LOGIC:
 *   People who use drugs are full targets (d ≈ 0.95): criminalization makes them the direct object of extraction, with trapped/identity_locked exit (criminal record, stigma, physiological dependence). Communities targeted by enforcement are high-target (d ≈ 0.85): selective policing concentrates extraction on racialized/poor communities with constrained exit (cannot easily leave jurisdiction). Families of incarcerated are moderate-target (d ≈ 0.65): bear diffuse costs (income loss, caregiving, trauma) with mobile exit but high switching costs. Law enforcement and prison industry are full beneficiaries (d ≈ 0.05): they collect budget, mandate, captive labor, political capital. Political actors are beneficiaries (d ≈ 0.15): extract votes/funding from prohibition stance. Black market actors are unintended beneficiaries (d ≈ 0.1): capture prohibition rents but face enforcement risk. Public health practitioners are excluded (not in directionality computation) — they would advocate harm reduction but are structurally barred from policy table. Reform advocates are observers (d ≈ 0.5 analytical).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (mid-20th century: perceived drug epidemic, moral panic, international pressure) is dead — drug use persists at similar or higher rates, harm has increased under prohibition, and the original moral framework has been contradicted by evidence. Yet the arrangement persists and has intensified (rising extractiveness trajectory). This is classic mandatrophy: the mandate (prevent harm through criminalization) has outlived its function (harm prevention) but the constraint remains because beneficiaries (enforcement, prisons, politicians, cartels) capture the extraction and block reform. The classification as snare (not tangled_rope) is correct because there is no genuine coordination function — the harm prevention claim is falsified by outcomes. The coordination story is pure cover.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_kernel_reading,
    'This constraint is the prohibition_reading of the substance_control_legitimacy kernel. What structural elements differ across the sibling readings (harm_reduction_reading, legalization_reading)?',
    'Compare the three readings'' victim sets, beneficiary structures, and extraction mechanisms. The prohibition reading uniquely places users in the victim set via criminalization; the harm reduction reading removes users from victim set; the legalization reading removes criminalization entirely.',
    'If the kernel''s structural delta across readings is mapped, the ε-invariance principle is satisfied — each reading gets its own constraint story with its own ε. Failure to decompose produces a single story with unstable ε depending on which reading''s lens is applied.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Kernel-reading decomposition for substance_control_legitimacy').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.88) primarily structural (legal barriers, carceral apparatus, denial of healthcare) or internalized (stigma, self-concept as criminal, identity fusion with ''addict'' label)?',
    'Post-decriminalization suppression trajectory: in jurisdictions that removed criminal penalties (Portugal 2001, Oregon 2020), measure whether suppression metrics decline toward structural baseline or persist at elevated levels indicating internalized component.',
    'If substantially internalized, the constraint''s effective suppression exceeds the structural measure — targets carry suppression with them after formal exit. This would increase effective extraction for identity_locked agents beyond what the structural d derivation captures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in drug prohibition').

omega_variable(
    black_market_violence_feature_or_bug,
    'Is the black market violence externality an unintended consequence of prohibition, or a structural feature that sustains the constraint''s enforcement apparatus?',
    'Trace resource flows: does enforcement funding correlate with violence metrics? Do enforcement agencies oppose violence-reduction measures that would shrink their mandate? Compare pre/post prohibition eras for violence-enforcement coupling.',
    'If structural feature, the prohibition reading''s claimed coordination function (preventing harm) is contradicted by its own operation generating greater harm — strengthening snare classification. If pure bug, the coordination story has more credibility.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(black_market_violence_feature_or_bug, conceptual, 'Whether black market violence sustains the prohibition apparatus').

omega_variable(
    harm_reduction_actual_harm_reduction,
    'Does the harm_reduction_reading''s coordination function actually reduce population-level harm, or does it create a parallel extraction layer (treatment mandates, surveillance, medicalized coercion)?',
    'Longitudinal comparison: jurisdictions adopting harm reduction without decriminalization vs. those decriminalizing. Measure overdose mortality, incarceration rates, treatment coercion incidents, and beneficiary capture of treatment funding.',
    'If harm reduction reading creates its own extraction layer, the kernel''s contest is between two extractive arrangements rather than extraction vs. coordination — changing the classification landscape for all three readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_reduction_actual_harm_reduction, empirical, 'Whether harm reduction reading solves or displaces extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_legitimacy__prohibition_reading, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scl_pr_tr_t0, substance_control_legitimacy__prohibition_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(scl_pr_tr_t15, substance_control_legitimacy__prohibition_reading, theater_ratio, 15, 0.32).
narrative_ontology:measurement(scl_pr_tr_t30, substance_control_legitimacy__prohibition_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(scl_pr_tr_t45, substance_control_legitimacy__prohibition_reading, theater_ratio, 45, 0.42).

% Extraction over time
narrative_ontology:measurement(scl_pr_be_t0, substance_control_legitimacy__prohibition_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(scl_pr_be_t15, substance_control_legitimacy__prohibition_reading, base_extractiveness, 15, 0.68).
narrative_ontology:measurement(scl_pr_be_t30, substance_control_legitimacy__prohibition_reading, base_extractiveness, 30, 0.75).
narrative_ontology:measurement(scl_pr_be_t45, substance_control_legitimacy__prohibition_reading, base_extractiveness, 45, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(scl_pr_su_t0, substance_control_legitimacy__prohibition_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(scl_pr_su_t15, substance_control_legitimacy__prohibition_reading, suppression_requirement, 15, 0.74).
narrative_ontology:measurement(scl_pr_su_t30, substance_control_legitimacy__prohibition_reading, suppression_requirement, 30, 0.81).
narrative_ontology:measurement(scl_pr_su_t45, substance_control_legitimacy__prohibition_reading, suppression_requirement, 45, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_legitimacy__prohibition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(substance_control_legitimacy__prohibition_reading, substance_control_legitimacy__harm_reduction_reading).
narrative_ontology:affects_constraint(substance_control_legitimacy__prohibition_reading, substance_control_legitimacy__legalization_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the colloquial label 'drug policy' into three structurally distinct claims with divergent ε values. The prohibition_reading (this story) is the upstream claim — the 1961 Single Convention and national criminal codes instantiate it. The harm_reduction_reading and legalization_reading are downstream contestations that the prohibition_reading's enforcement apparatus actively suppresses. The upstream claim's persistence creates the structural conditions (black markets, criminal records, stigma) that the downstream readings must contend with. If the prohibition_reading's enforcement collapsed, both sibling readings' operational landscapes would transform — the harm_reduction_reading would lose its 'alternative to prohibition' framing and become the default; the legalization_reading would face a regulated-market implementation problem rather than a prohibition-repeal problem.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_legitimacy__prohibition_reading, institutional, 0.15).
constraint_indexing:directionality_override(substance_control_legitimacy__prohibition_reading, organized, 0.1).
constraint_indexing:directionality_override(substance_control_legitimacy__prohibition_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
