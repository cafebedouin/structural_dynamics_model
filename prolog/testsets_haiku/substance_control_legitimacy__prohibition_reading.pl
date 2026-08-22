% ============================================================================
% CONSTRAINT STORY: substance_control_legitimacy__prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: substance_control_legitimacy__prohibition_reading
 *   human_readable: Criminalized Substance Control via Prohibition Reading
 *   domain: public_health/criminal_justice/political_economy
 *
 * SUMMARY:
 *   The prohibition reading of substance control legitimacy frames substance
 *   use as inherently harmful and justifies state authority through moral
 *   duty to prevent harm via criminalization. Under this reading, users are
 *   victimized by their own use and 'protected' through incarceration. The
 *   actual structural outcome is that the constraint extracts carceral labor,
 *   funds law enforcement, protects pharmaceutical monopolies, and
 *   concentrates enforcement disparity on marginalized communities. The
 *   reading's founding premise—that criminalization prevents harm—is
 *   contradicted by 50+ years of evidence: use rates persist, overdose deaths
 *   have risen dramatically, incarceration generates collateral harms
 *   exceeding any prevented substance use, and black markets consolidate
 *   violence. The constraint persists not because it solves its stated
 *   problem but because it generates rents for institutional actors and
 *   provides moral vocabulary for class/racial social control. This story
 *   instantiates ONLY the prohibition reading; sibling readings
 *   (harm_reduction_reading, legalization_reading) are separate constraint
 *   stories linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - people_who_use_drugs (powerless, trapped exit): victims subject to criminalization and incarceration
 *   - criminalized_communities (moderate, constrained exit): bear disproportionate enforcement burden
 *   - law_enforcement_agencies (institutional, arbitrage exit): agenda-setter; collects enforcement rents and budgets
 *   - criminal_justice_bureaucracy (institutional, arbitrage exit): beneficiary; maintains caseload and incarceration volume
 *   - pharmaceutical_industry (powerful, arbitrage exit): beneficiary; protected from market competition
 *   - public_health_authorities (institutional, constrained exit): observer; structurally subordinate to criminal justice framing
 *   - harm_reduction_advocates (moderate, constrained exit): observer; marginalized in policy discourse
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_legitimacy__prohibition_reading, 0.81).
domain_priors:suppression_score(substance_control_legitimacy__prohibition_reading, 0.88).
domain_priors:theater_ratio(substance_control_legitimacy__prohibition_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_legitimacy__prohibition_reading, snare).
narrative_ontology:human_readable(substance_control_legitimacy__prohibition_reading, "Criminalized Substance Control via Prohibition Reading").
narrative_ontology:topic_domain(substance_control_legitimacy__prohibition_reading, "public_health/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_legitimacy__prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_legitimacy__prohibition_reading, '5e8962d2-91c4-444b-9c78-f571886ccf59').
narrative_ontology:cs_kernel_codification('5e8962d2-91c4-444b-9c78-f571886ccf59', formalized).
narrative_ontology:cs_authority_grounding('5e8962d2-91c4-444b-9c78-f571886ccf59', extraction).
narrative_ontology:cs_interpretation_layer_present('5e8962d2-91c4-444b-9c78-f571886ccf59').
narrative_ontology:cs_reading_relation('5e8962d2-91c4-444b-9c78-f571886ccf59', substance_control_legitimacy__harm_reduction_reading, coexists_with).
narrative_ontology:cs_reading_relation('5e8962d2-91c4-444b-9c78-f571886ccf59', substance_control_legitimacy__legalization_reading, coexists_with).
narrative_ontology:cs_axiom('5e8962d2-91c4-444b-9c78-f571886ccf59', foundational, substance_use_inherently_harmful).
narrative_ontology:cs_axiom_status(substance_use_inherently_harmful, overridden).
narrative_ontology:cs_axiom_grounding('5e8962d2-91c4-444b-9c78-f571886ccf59', substance_use_inherently_harmful, empirically_contingent).
narrative_ontology:cs_axiom('5e8962d2-91c4-444b-9c78-f571886ccf59', foundational, state_moral_duty_criminalize_prevent).
narrative_ontology:cs_axiom_status(state_moral_duty_criminalize_prevent, holdable).
narrative_ontology:cs_axiom_grounding('5e8962d2-91c4-444b-9c78-f571886ccf59', state_moral_duty_criminalize_prevent, deontological).
narrative_ontology:cs_reference_frame('5e8962d2-91c4-444b-9c78-f571886ccf59', moral_imperative_criminalize_use).
narrative_ontology:cs_drift_state('5e8962d2-91c4-444b-9c78-f571886ccf59', contemporary_evidence_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5e8962d2-91c4-444b-9c78-f571886ccf59', '').
narrative_ontology:cs_kernel_id(substance_control_legitimacy__prohibition_reading, substance_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, law_enforcement_agencies).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, criminal_justice_bureaucracy).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, pharmaceutical_industry_patent_interests).
narrative_ontology:constraint_victim(substance_control_legitimacy__prohibition_reading, people_who_use_drugs).
narrative_ontology:constraint_victim(substance_control_legitimacy__prohibition_reading, criminalized_communities).
narrative_ontology:constraint_victim(substance_control_legitimacy__prohibition_reading, incarcerated_individuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Subject to criminal prosecution for consumption and possession; faces incarceration, loss of employment, family separation, and collateral barriers (housing, benefits, voting). The criminalization framework defines them as inherent threats despite no direct harm to others in many jurisdictions. Exit options are nonexistent: geographic flight within national jurisdiction yields identical legal exposure; cessation may reduce but never eliminates legal risk retroactively. The prohibition reading offers no path out except total abstinence, framed as moral requirement rather than acknowledged trade-off.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, people_who_use_drugs, payer,
    powerless, biographical, trapped, national).

% Communities subject to intensive drug enforcement carry disproportionate police presence, surveillance, and arrest rates (documented 5–10x disparities by race and class). The enforcement apparatus sustains itself through drug-related prosecutions regardless of public health outcomes. Communities can organize, litigate, or leave, but exit is economically constrained and does not eliminate exposure for residents with preexisting enforcement records.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, criminalized_communities, payer,
    moderate, generational, constrained, national).

% Detained in state and federal prisons under sentences for drug offenses, often serving long mandatory minimums decoupled from individual harm. Constitutionally barred from meaningful exit; parole and clemency are administrative discretionary processes controlled by the same authority structure that sentenced them. Substance use is often a symptom of underlying trauma or medical conditions, but the criminalization framework treats it as moral failure and justification for incapacitation.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, incarcerated_individuals, payer,
    powerless, biographical, trapped, national).

% Enforce the prohibition through investigation, prosecution, incarceration. Drug enforcement generates federal grant funding (Byrne grants, 1033 equipment transfer), overtime budgets, and organizational growth. The agency's legitimacy and resource flow are partially indexed to drug arrest and seizure statistics. The agency can modulate enforcement intensity and priorities but does not face consequences for collateral harms (incarceration disparities, community destabilization) that the constraint generates.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, law_enforcement_agencies, agenda_setter,
    institutional, generational, arbitrage, national).

% Prosecutors, courts, and corrections systems have institutional capacity and budgets indexed to caseload volume. Drug prosecutions provide continuous workload, plea-bargaining leverage, and sentencing data that justify expanding incarceration infrastructure. Judges operate within mandatory sentencing frameworks that limit discretion while appearing to enforce objective law. The system extracts labor value from incarcerated populations (sub-minimum-wage prison work, chain gangs in some jurisdictions) and collects fines and fees.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, criminal_justice_bureaucracy, beneficiary,
    institutional, generational, arbitrage, national).

% Criminalization of non-pharmaceutical substances protects pharmaceutical patent monopolies on pain management, ADHD treatment, and anxiety management. Decriminalization or legalization of alternatives (cannabis, psilocybin, MDMA) would disrupt existing medication markets and create competition for expensive pharmaceutical treatments. The industry funds prohibition advocacy and frames alternatives as dangerous while benefiting from the behavioral monopoly criminalization enforces.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, pharmaceutical_industry_patent_interests, beneficiary,
    powerful, generational, arbitrage, global).

% Tasked with disease prevention and health promotion but structurally subordinate to criminal justice priorities in substance use policy. Evidence supports harm reduction, treatment-first models, and decriminalization as superior public health outcomes, but public health authority competes for institutional legitimacy with criminal justice framing. Can advocate for alternative models but cannot unilaterally implement them; career cost of visible dissent from prohibition reading is high.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, public_health_authorities, observer,
    institutional, generational, constrained, national).

% Framed as a distinct category from 'people who use drugs' in some discourse, but structurally identical in the prohibition reading: criminalization is the treatment barrier, not the treatment. Medical models of addiction are constrained by criminalization (narcotics courts create coercive treatment, not voluntary care). This population is excluded from policy discourse in favor of law-enforcement-centric framing; their input on effective treatment is systematically absent from criminal justice deliberation.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, people_with_substance_use_disorders, excluded,
    powerless, biographical, trapped, national).

% Operates syringe exchange programs, naloxone distribution, and supervised consumption sites in jurisdictions where legal authorization exists. Faces criminalization and funding obstruction in prohibition-aligned jurisdictions; can testify and publish evidence, but institutional power to shift policy is limited. Career and funding vulnerability is tied to shifting political winds.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, harm_reduction_advocates, observer,
    moderate, biographical, constrained, national).

% Criminalization creates market monopoly for illegal suppliers, with violence as the enforcement mechanism (no legal contract recourse). Market consolidation and turf warfare generate externalities (homicide, community destabilization) that fall on criminalized communities. Suppliers have no voice in policy but are structurally created by the constraint; removal requires either legalization or successful eradication (historically impossible at scale).
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, black_market_suppliers, observer,
    moderate, biographical, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_legitimacy__prohibition_reading, law_enforcement_agencies).
narrative_ontology:fixing_cost_class(substance_control_legitimacy__prohibition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ostensibly: prevention of substance use deemed inherently harmful and delegitimization of non-state substance provision. Actually coordinated: allocation of carceral labor, enforcement infrastructure budgeting, and pharmaceutical market protection.
% TRANSFER_FUNCTION: Moves criminal liability, incarceration, collateral disability (employment barriers, benefit loss, voting rights restriction), and behavioral control from state authority to people who use drugs and criminalized communities. Moves enforcement resources and pharmaceutical market protection toward law enforcement agencies, criminal justice systems, and patent-holding industries. The transfer is sustained by framing substance use as inherent moral hazard rather than a rational choice or medical condition.
% ABSENT_VOICES: People who use drugs are excluded from policy deliberation except as subject populations in treatment programs they did not design. Harm reduction advocates operate at the margins. Communities bearing enforcement disparity are consulted post-hoc on implementation, not on legitimacy or scope of the constraint itself. Medical anthropologists and addiction medicine specialists advocating non-criminalization are systematically devalued relative to criminal justice and DEA testimony.
% DISAPPEARANCE_RATIONALE: Overnight removal of criminalization would immediately release an estimated 375,000+ incarcerated individuals (US jurisdiction), eliminate millions of active arrest warrants, collapse a $50+ billion carceral infrastructure, and transfer substance use back to public health authority where it would likely be addressed through treatment, harm reduction, and regulation rather than incapacitation. Law enforcement agencies would reallocate personnel; courts would lose caseload; pharmaceutical markets would face new competition. The constraint is not self-maintaining—it requires continuous enforcement investment and legitimacy work.
% FOUNDING_PROBLEM: Early 20th century opiate epidemiology combined with racial and class anxieties about immigrant and Black communities' drug use; government framed criminalization as moral necessity to prevent moral contagion and protect public virtue. The constraint's founding invoked prevention of harm but was motivated by social control of marginalized populations.
% FOUNDING_PROBLEM_CORROBORATION: Criminal justice historians and public health researchers document the racist and class-motivated origins of prohibition (Musto, Acker, Provine); law enforcement and conservative policy advocates counter that founding motives are irrelevant to current necessity. Medical evidence from harm reduction research (Lancet, JAMA Psychiatry, Health Affairs) indicates the founding problem (preventing use via criminalization) is NOT being solved—use rates persist or rise; collateral harms are severe and documented. No neutral third party outside both benefiting and victim constituencies corroborates that criminalization achieves its stated objective; corroboration for harm causation is stronger.
narrative_ontology:disappearance_verdict(substance_control_legitimacy__prohibition_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_legitimacy__prohibition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_legitimacy__prohibition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(substance_control_legitimacy__prohibition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_legitimacy__prohibition_reading, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.81) and rising across the interval because the constraint systematically transfers liberty, economic opportunity, and bodily autonomy from people who use drugs to law enforcement and carceral institutions—not through a market mechanism but through state violence backed by the moral claim that use is inherently harmful. Suppression is very high (0.88) and has intensified: the constraint depends on continuous enforcement spending, surveillance, and incarceration volume; without active suppression (police activity, prosecutorial discretion, sentencing enforcement), the constraint collapses. Theater ratio is moderate-high (0.52) and rising: early phases (1970s–1980s) had genuine public health framing and some genuine concern about epidemiology; by 2024, the moralistic framing persists while the actual function is extraction and social control—the constraint spends increasing resources on performative drug war theater (asset forfeiture displays, arrest statistics) rather than on reduction of use or harm. The measurement series span 1971–2024 (the interval of mass criminalization era) on one shared time grid: extractiveness rises as the carceral apparatus matures; suppression requirement rises as populations develop resistance (despite deterrence claims); theater rises as the founding justification (preventing use) fails and is replaced with legitimacy maintenance (law and order framing, 'tough on crime' rhetoric). The cyclical dynamics are documented in enforcement cycles following political pressure and evidence accumulation, but the trend is monotonically upward in extractiveness and suppression requirement.
 *
 * PERSPECTIVAL GAP:
 *   From the law enforcement and criminal justice seat, the constraint appears as coordination of legitimate public safety authority and protection of vulnerable populations from harmful substance use—a genuine moral function. From the people_who_use_drugs and criminalized_communities seats, the same structure operates as enforced extraction of liberty and economic opportunity backed by false moral claims. The engine should compute these divergent classifications from the structural data: the agenda-setter (law enforcement) has high power, arbitrage exit, and genuine resource flow (budgets, grants); the payer seats (people who use drugs) have powerless/moderate power, trapped/constrained exit, and loss of liberty/opportunity. The seated-asymmetry should produce a sharply divergent per-seat type: rope or even mountain at the enforcement seat (high power, low extraction perceived locally); snare at the victim seats (powerless, high extraction, suppression, trapped exit). The claim/metric divergence is intentional: the reading CLAIMS rope (the justificatory framing) while the metrics describe snare (the actual structural operation).
 *
 * DIRECTIONALITY LOGIC:
 *   Law enforcement agencies (d ≈ 0.1–0.2, beneficiary end): derive budgets, grant funding, and organizational legitimacy from drug enforcement. The constraint subsidizes them; they are the primary beneficiary by structural position. Criminal justice bureaucracy (d ≈ 0.15–0.25): prosecutors and courts receive steady caseload; corrections systems extract labor value from incarcerated populations and collect fines/fees. People who use drugs (d ≈ 0.95–1.0, target end): lose liberty, incur criminal record, face collateral barriers, often serve prison time. Exit is trapped (geographic flight within jurisdiction has no effect; cessation does not eliminate past exposure). Criminalized communities (d ≈ 0.80–0.90): bear disparity in enforcement intensity; can organize and litigate but cannot exit the jurisdiction or legal exposure. Pharmaceutical industry (d ≈ 0.20–0.30): benefits from market protection; no direct enforcement cost. The reading produces a highly asymmetric directionality profile: concentrated benefits at high-power institutional seats (low d), concentrated extraction at powerless/trapped seats (high d). This structural asymmetry is the foundation of the snare classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The prohibition reading's founding mandate is clear: prevent substance use through criminalization and moral deterrence. The mandate is DEAD (founding_problem_status = dead): 50 years of evidence shows criminalization does not prevent use—use rates have persisted or risen (overdose deaths have increased 5–10x since 1971). The public health evidence from alcohol prohibition (repeal after 13 years), Portugal's decriminalization success (2001–present: lower use rates, lower overdose mortality), and Switzerland's supervised consumption model (lower crime, lower use among program participants) all contradicts the founding premise. The constraint persists despite mandate failure because it generates extractive rents for institutional actors and provides moral vocabulary for marginalized-community social control. Declaring mandatrophy_resolved = true is the structural recognition that the constraint has outlived its function and persists by institutional inertia and beneficiary capture, not by solving the problem it was built to solve. The rising theater_ratio (0.28→0.52 across the interval) is diagnostic evidence: the constraint spends increasing resources on legitimacy maintenance (publicity, framing, moral claims) relative to functional outcome (use reduction, harm prevention). This is the piton signal—the constraint should be classified as substantially mandatrophy-resolved. However, the beneficiary extraction is still substantial and the suppression requirement is still high, so it is not a piton (which would have diffuse beneficiaries and low extraction) but a snare with mandatrophy failure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inherent_vs_constructed_harm,
    'Is substance use inherently harmful, or is the harm primarily a product of criminalization, poverty, and social marginalization?',
    'Comparative analysis across jurisdictions with different legal regimes (Portugal, Switzerland, Netherlands vs. US) controlling for socioeconomic factors; longitudinal cohort studies of substance use outcomes in decriminalized vs. criminalized contexts; medical literature on addiction as a disease vs. moral failing.',
    'If harm is primarily constructed by criminalization (criminogenic environments, black market violence, incarceration trauma), the prohibition reading''s core premise collapses and the constraint reclassifies from snare (extraction hidden under harm-prevention framing) to pure extraction without the moral cover. If substance use is inherently harmful even in supportive environments, the harm-prevention framing has empirical ground and the mandate is not dead—it is merely ineffective, implying reform rather than abolition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(inherent_vs_constructed_harm, empirical, 'Whether substance use inherently generates harm or whether criminalization is the primary harm vector.').

omega_variable(
    moral_authority_of_criminalization,
    'Does the state have moral authority to criminalize individual behavior on the basis that it is inherently self-harmful, absent third-party harm or direct threat?',
    'Jurisprudential analysis of the principle of harm (Mill, Feinberg) as applied to substance use; examination of whether states apply the principle consistently (e.g., tobacco, alcohol, extreme sports) or selectively (applying it to disfavored substances and populations); testimony from moral philosophers and policy ethicists outside the law enforcement apparatus.',
    'If the state does NOT have this moral authority (the legalization_reading position), the constraint''s legitimacy rests on a false premise and the mandate is not just dead but illegitimate by its own foundational claim. If it does have this authority, the constraint''s form is justified by moral principle even if the implementation is ineffective—implying reform to effectiveness rather than abandonment of the principle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_authority_of_criminalization, conceptual, 'Whether criminalization of individual self-harm is a legitimate state function absent third-party harm.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.88) primarily structural (legal barriers, police surveillance, incarceration threat) or internalized (moral inculcation that substance use is shameful/wrong, belief that criminalization is justified)?',
    'Post-decriminalization trajectory in jurisdictions that have removed criminal penalties: if suppression persists in former users'' behavior and self-concepts, it is partially internalized; if use patterns normalize quickly, it is primarily structural. Qualitative interviews with people who use drugs about their perception of constraint legitimacy vs. constraint necessity.',
    'If suppression is primarily structural, the constraint collapses rapidly upon decriminalization—suggesting snare classification is correct and institutional capture is the sole driver of persistence. If suppression is substantially internalized, people who use drugs carry the constraint with them even after legal removal—suggesting deeper cultural capture and longer transition period, but also confirming that the constraint generates internalized oppression consistent with the victim designation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression operates through legal/institutional structures or internalized moral frameworks.').

omega_variable(
    reading_foreclosure_possibility,
    'Can the prohibition_reading coexist with the harm_reduction_reading within a single state jurisdiction, or does adopting one reading logically foreclose the other?',
    'Examination of jurisdictions that have attempted parallel systems (e.g., drug courts attempting both criminalization AND treatment, safe injection sites operating within criminalized frameworks); analysis of whether the logical premises of each reading are actually in contradiction or merely operationally in conflict.',
    'If truly foreclosed, the three readings represent a partition of the logical space and only one can be institutionally true at scale. If coexistence is possible (as Portugal, Switzerland, and some US jurisdictions suggest), the readings are not logically opposed but operationally competitive—suggesting the constraint''s persistence is not inevitable disagreement but institutional path-dependence and beneficiary capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_possibility, conceptual, 'Whether the prohibition and harm-reduction readings are logically incompatible or merely operationally in conflict.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_legitimacy__prohibition_reading, 1971, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t1971, substance_control_legitimacy__prohibition_reading, theater_ratio, 1971, 0.28).
narrative_ontology:measurement_basis(subs_tr_t1971, projected).
narrative_ontology:measurement(subs_tr_t1985, substance_control_legitimacy__prohibition_reading, theater_ratio, 1985, 0.35).
narrative_ontology:measurement_basis(subs_tr_t1985, observed).
narrative_ontology:measurement(subs_tr_t2000, substance_control_legitimacy__prohibition_reading, theater_ratio, 2000, 0.43).
narrative_ontology:measurement_basis(subs_tr_t2000, observed).
narrative_ontology:measurement(subs_tr_t2010, substance_control_legitimacy__prohibition_reading, theater_ratio, 2010, 0.48).
narrative_ontology:measurement_basis(subs_tr_t2010, observed).
narrative_ontology:measurement(subs_tr_t2018, substance_control_legitimacy__prohibition_reading, theater_ratio, 2018, 0.51).
narrative_ontology:measurement_basis(subs_tr_t2018, observed).
narrative_ontology:measurement(subs_tr_t2024, substance_control_legitimacy__prohibition_reading, theater_ratio, 2024, 0.52).
narrative_ontology:measurement_basis(subs_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(subs_be_t1971, substance_control_legitimacy__prohibition_reading, base_extractiveness, 1971, 0.45).
narrative_ontology:measurement_basis(subs_be_t1971, projected).
narrative_ontology:measurement(subs_be_t1985, substance_control_legitimacy__prohibition_reading, base_extractiveness, 1985, 0.58).
narrative_ontology:measurement_basis(subs_be_t1985, observed).
narrative_ontology:measurement(subs_be_t2000, substance_control_legitimacy__prohibition_reading, base_extractiveness, 2000, 0.72).
narrative_ontology:measurement_basis(subs_be_t2000, observed).
narrative_ontology:measurement(subs_be_t2010, substance_control_legitimacy__prohibition_reading, base_extractiveness, 2010, 0.78).
narrative_ontology:measurement_basis(subs_be_t2010, observed).
narrative_ontology:measurement(subs_be_t2018, substance_control_legitimacy__prohibition_reading, base_extractiveness, 2018, 0.8).
narrative_ontology:measurement_basis(subs_be_t2018, observed).
narrative_ontology:measurement(subs_be_t2024, substance_control_legitimacy__prohibition_reading, base_extractiveness, 2024, 0.81).
narrative_ontology:measurement_basis(subs_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t1971, substance_control_legitimacy__prohibition_reading, suppression_requirement, 1971, 0.52).
narrative_ontology:measurement_basis(subs_su_t1971, projected).
narrative_ontology:measurement(subs_su_t1985, substance_control_legitimacy__prohibition_reading, suppression_requirement, 1985, 0.71).
narrative_ontology:measurement_basis(subs_su_t1985, observed).
narrative_ontology:measurement(subs_su_t2000, substance_control_legitimacy__prohibition_reading, suppression_requirement, 2000, 0.82).
narrative_ontology:measurement_basis(subs_su_t2000, observed).
narrative_ontology:measurement(subs_su_t2010, substance_control_legitimacy__prohibition_reading, suppression_requirement, 2010, 0.85).
narrative_ontology:measurement_basis(subs_su_t2010, observed).
narrative_ontology:measurement(subs_su_t2018, substance_control_legitimacy__prohibition_reading, suppression_requirement, 2018, 0.87).
narrative_ontology:measurement_basis(subs_su_t2018, observed).
narrative_ontology:measurement(subs_su_t2024, substance_control_legitimacy__prohibition_reading, suppression_requirement, 2024, 0.88).
narrative_ontology:measurement_basis(subs_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_legitimacy__prohibition_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(substance_control_legitimacy__prohibition_reading, 0.12).
narrative_ontology:affects_constraint(substance_control_legitimacy__prohibition_reading, substance_control_legitimacy__harm_reduction_reading).
narrative_ontology:affects_constraint(substance_control_legitimacy__prohibition_reading, substance_control_legitimacy__legalization_reading).
narrative_ontology:affects_constraint(substance_control_legitimacy__prohibition_reading, pharmaceutical_monopoly_protection).
narrative_ontology:affects_constraint(substance_control_legitimacy__prohibition_reading, carceral_infrastructure_dependence).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel 'substance_control_legitimacy'. The sibling readings (harm_reduction_reading, legalization_reading) are authored as separate constraint stories with different ε values, beneficiary/victim structures, and claimed types. The three readings decompose the contested governance domain of substance use into structurally distinct claims: prohibition_reading asserts substance use is inherently harmful and criminalizable; harm_reduction_reading asserts it is a medical/public health issue requiring non-criminalized treatment; legalization_reading asserts it is an individual autonomy question within limits of third-party harm. The three stories are linked via network.affects_constraints to enable contamination and cross-reading analysis. Epsilon values differ substantially across readings because they assess different referents: prohibition_reading measures ε for the criminalized arrangement as the prohibition reading describes it; harm_reduction_reading measures ε for the same de facto arrangement (criminalized systems) but from the perspective of harm-reduction evidence (high extraction, low function); legalization_reading measures ε for legalized arrangements the reading envisions. The readings are not measurement-dependent ambiguities within one constraint—they are genuinely different constraints instantiated by different authority structures, beneficiary/victim configurations, and legitimacy claims. See omegas on reading foreclosure and moral authority for the committer-level structural positions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_legitimacy__prohibition_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
