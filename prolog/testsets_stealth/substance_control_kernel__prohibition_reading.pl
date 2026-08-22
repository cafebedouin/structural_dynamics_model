% ============================================================================
% CONSTRAINT STORY: substance_control_kernel__prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_kernel__prohibition_reading, []).

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
 *   constraint_id: substance_control_kernel__prohibition_reading
 *   human_readable: Criminal Prohibition of Substance Use as Moral Transgression (Punitive Reading)
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   In this reading, the manufacture, sale, and possession of proscribed
 *   substances are constituted as moral transgressions against the social
 *   order, and the state's proper response is punishment: arrest,
 *   prosecution, incarceration, and the permanent civil disabilities that
 *   follow conviction. The arrangement operates at national scale through
 *   criminal codes, sentencing law, and a dedicated enforcement apparatus,
 *   and extends outward through supply-side interdiction into producing
 *   regions. Its operation produces flows the moral framing does not
 *   advertise: budgets, forfeiture proceeds, and contracts accumulate on
 *   enforcement-side seats; users and the communities they live in accumulate
 *   liabilities; and because supply is driven underground, disputes over it
 *   are settled outside the law, generating violence that falls on bystanders
 *   and on producing nations. The claim and the metrics are authored
 *   independently: the claimed type records what this reading's structure is
 *   judged to be; the metric values record how the arrangement actually
 *   operated across the interval.
 *
 * KEY AGENTS:
 *   - - criminal_enforcement_apparatus: Primary collector and day-to-day administrator (institutional/identity_locked) — receives appropriations, forfeiture, and mission legitimacy
 *   - - legislative_prohibition_coalition: Agenda setter (institutional/constrained) — writes and defends the statutes
 *   - - private_prison_operators and forfeiture_revenue_departments: Secondary collectors (organized/arbitrage, organized/constrained)
 *   - - criminalized_users: Primary bearer of costs (powerless/trapped) — arrest, incarceration, lifetime collateral consequences
 *   - - overpoliced_low_income_communities and black_market_violence_exposed_communities: Secondary bearers (powerless/trapped) — concentrated enforcement and market violence
 *   - - producer_transit_nation_civilians: Externalized bearers (powerless/trapped, continental scope) — absorb supply-side violence
 *   - - court_mandated_treatment_sector: Dual-positioned conscript (moderate/constrained) — funded by referrals, stripped of clinical autonomy
 *   - - harm_reduction_practitioners: Excluded voice (organized/constrained) — barred from practice and from the drafting table
 *   - - public_health_authorities: Analytical observer (institutional/analytical) — measures outcomes without a decision seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_kernel__prohibition_reading, 0.85).
domain_priors:suppression_score(substance_control_kernel__prohibition_reading, 0.87).
domain_priors:theater_ratio(substance_control_kernel__prohibition_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, suppression_requirement, 0.87).
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_kernel__prohibition_reading, snare).
narrative_ontology:human_readable(substance_control_kernel__prohibition_reading, "Criminal Prohibition of Substance Use as Moral Transgression (Punitive Reading)").
narrative_ontology:topic_domain(substance_control_kernel__prohibition_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_kernel__prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_kernel__prohibition_reading, '237d544d-6a79-4ce3-ba9a-c5869c50b2ac').
narrative_ontology:cs_kernel_codification('237d544d-6a79-4ce3-ba9a-c5869c50b2ac', formalized).
narrative_ontology:cs_authority_grounding('237d544d-6a79-4ce3-ba9a-c5869c50b2ac', extraction).
narrative_ontology:cs_interpretation_layer_present('237d544d-6a79-4ce3-ba9a-c5869c50b2ac').
narrative_ontology:cs_reading_relation('237d544d-6a79-4ce3-ba9a-c5869c50b2ac', substance_control_kernel__harm_reduction_reading, coexists_with).
narrative_ontology:cs_reading_relation('237d544d-6a79-4ce3-ba9a-c5869c50b2ac', substance_control_kernel__legalization_reading, forecloses).
narrative_ontology:cs_axiom('237d544d-6a79-4ce3-ba9a-c5869c50b2ac', foundational, intoxicant_use_is_moral_transgression).
narrative_ontology:cs_axiom_status(intoxicant_use_is_moral_transgression, holdable).
narrative_ontology:cs_axiom_grounding('237d544d-6a79-4ce3-ba9a-c5869c50b2ac', intoxicant_use_is_moral_transgression, deontological).
narrative_ontology:cs_axiom('237d544d-6a79-4ce3-ba9a-c5869c50b2ac', foundational, state_punishment_protects_social_order).
narrative_ontology:cs_axiom_status(state_punishment_protects_social_order, holdable).
narrative_ontology:cs_axiom_grounding('237d544d-6a79-4ce3-ba9a-c5869c50b2ac', state_punishment_protects_social_order, instrumental).
narrative_ontology:cs_reference_frame('237d544d-6a79-4ce3-ba9a-c5869c50b2ac', moral_transgression_condemnation_framework).
narrative_ontology:cs_drift_state('237d544d-6a79-4ce3-ba9a-c5869c50b2ac', contemporary_harm_evidence_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('237d544d-6a79-4ce3-ba9a-c5869c50b2ac', '').
narrative_ontology:cs_kernel_id(substance_control_kernel__prohibition_reading, substance_control_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, criminal_enforcement_apparatus).
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, private_prison_operators).
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, forfeiture_revenue_departments).
narrative_ontology:constraint_victim(substance_control_kernel__prohibition_reading, criminalized_users).
narrative_ontology:constraint_victim(substance_control_kernel__prohibition_reading, overpoliced_low_income_communities).
narrative_ontology:constraint_victim(substance_control_kernel__prohibition_reading, black_market_violence_exposed_communities).
narrative_ontology:constraint_victim(substance_control_kernel__prohibition_reading, producer_transit_nation_civilians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, court_mandated_treatment_sector).
narrative_ontology:constraint_victim(substance_control_kernel__prohibition_reading, court_mandated_treatment_sector).
narrative_ontology:constraint_vindicates(substance_control_kernel__prohibition_reading, punitive_deterrence_doctrine).
narrative_ontology:constraint_vindicates(substance_control_kernel__prohibition_reading, moral_order_preservation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafts and maintains the criminal statutes that define possession, sale, and manufacture as offenses; sets mandatory minimum sentences and the scheduling of substances; sustains the arrangement through appropriations and by defeating reform bills. Electorally rewarded for toughness; reversing position invites primary challenges and conflicts with international drug-control treaty commitments.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, legislative_prohibition_coalition, agenda_setter,
    institutional, biographical, constrained, national).

% Police departments, prosecutor offices, and corrections agencies whose staffing, promotion ladders, union contracts, and budget lines are built around drug enforcement. Receives appropriations, federal grant streams, overtime, and seized cash and property through forfeiture. Decades of mission statements, training pipelines, and institutional self-conception are bound up with continuing the mission; wholesale reassignment to other work would dissolve career structures and the institution's sense of what it is for.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, criminal_enforcement_apparatus, beneficiary,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__prohibition_reading, criminal_enforcement_apparatus, agenda_setter).

% Sell bed space to governments under per-diem contracts, some with occupancy guarantees. Lobbying expenditures target sentencing and parole policy. When drug-sentence populations shrink in one jurisdiction they bid for contracts elsewhere, including immigration detention, so revenue survives policy shifts.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, private_prison_operators, beneficiary,
    organized, immediate, arbitrage, national).

% Specialized seizure units and participating agencies whose equipment and operations are funded by proceeds of confiscated cash and vehicles, often routed outside ordinary appropriation through equitable-sharing programs. Budget planning assumes continued seizure volume.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, forfeiture_revenue_departments, beneficiary,
    organized, immediate, constrained, national).

% Clinics and counselors receive their referral stream and much of their funding because courts sentence people to treatment as an alternative or adjunct to incarceration. Clinicians report progress to judges, operate under abstinence conditions, and cannot set care plans independently; the funding relationship depends on the continuation of court referrals.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, court_mandated_treatment_sector, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__prohibition_reading, court_mandated_treatment_sector, payer).

% Bear arrest, prosecution, incarceration, and a permanent record that blocks employment, housing, licenses, and voting in many places. Disclosing use to a doctor risks legal exposure, so help-seeking is deferred until crisis. Leaving the situation requires money, mobility, and a record-clearing pathway that few possess, and dependence itself narrows what leaving would even mean.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, criminalized_users, payer,
    powerless, biographical, trapped, national).

% Live under concentrated patrol and arrest activity; lose wage earners to incarceration, children to foster placement, and trust to repeated stops. Moving away costs money and the policing pattern follows poverty to the next address. Organizing capacity exists but is thinned by the very removals the enforcement produces.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, overpoliced_low_income_communities, payer,
    powerless, generational, trapped, national).

% Residents of transit corridors and open-air markets where supply disputes are settled with firearms because neither side can call the police. Endure shootouts, curfews, and displacement; bystanders absorb casualties. Exit means relocating out of the corridor economy entirely.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, black_market_violence_exposed_communities, payer,
    powerless, biographical, trapped, regional).

% Farmers and villagers in growing and shipping regions live between trafficking organizations and eradication forces; crops are destroyed by fumigation with underfunded replacement programs, and territory changes hands violently. Consumer-country policy is made without their participation.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, producer_transit_nation_civilians, payer,
    powerless, generational, trapped, continental).

% Physicians, nurses, and outreach workers who could deliver syringe services, supervised consumption, drug checking, and evidence-based treatment are barred, defunded, or criminally exposed for doing so in prohibiting jurisdictions. Licensure and grant eligibility are tied to jurisdictional rules; their professional judgment is legislatively overruled and their testimony rarely reaches the drafting table.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, harm_reduction_practitioners, excluded,
    organized, biographical, constrained, national).

% Surveillance and research agencies that count overdose deaths, infections, and treatment gaps and publish the record of outcomes. They hold no vote in scheduling or sentencing decisions; their findings enter the process only as material the enforcement coalition may cite or ignore.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, public_health_authorities, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_kernel__prohibition_reading, criminal_enforcement_apparatus).
narrative_ontology:fixing_cost_class(substance_control_kernel__prohibition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a shared normative boundary: marks intoxicant use as conduct outside the moral community, giving citizens, congregations, and local institutions a common expressive standard, and concentrates state capacity around a single enforcement mission.
% TRANSFER_FUNCTION: Moves liberty, time, money, and civic standing from users and the communities they live in toward the state's enforcement institutions: years of life transferred into prison terms, property transferred through forfeiture, and budgets, employment, and contracts transferred to police, prosecution, and corrections.
% ABSENT_VOICES: People who use drugs hold no seat in statute-setting; harm-reduction clinicians are legislatively barred from practice and from the drafting table in prohibiting jurisdictions; civilians in producing and transit nations absorb supply-side violence with no vote in consumer-country policy; formerly incarcerated people are in many places disenfranchised, so the strongest prospective objectors are removed from the electorate by the arrangement itself.
% DISAPPEARANCE_RATIONALE: Enforcement agencies would reorganize around remaining missions within budget cycles; large sentenced populations would face resentencing or release; possession markets would convert toward regulated commerce where licensing exists, collapsing the risk premium that funds armed supply chains; treatment and prevention would refinance from health appropriations instead of court referrals; forfeiture revenue lines would close. Every dependency is institutional — nothing in the physical or economic environment requires the arrangement to persist.
% FOUNDING_PROBLEM: Early twentieth-century temperance and anti-narcotic campaigns confronted visible addiction, labor-discipline anxieties, and racialized panics attached to specific substances — opium with Chinese immigrant communities, cocaine with Black workers, later cannabis with Mexican migrants and countercultures. The arrangement was built to purge the condemned substances and their users from the social body and to affirm the state's custodial authority over morality.
% FOUNDING_PROBLEM_CORROBORATION: Historical scholarship and legislative-record analysis document the racialized and order-maintenance origins; epidemiological series published by public-health agencies outside the enforcement beneficiary set show use prevalence and addiction rates did not decline in proportion to enforcement intensity across the interval. The enforcement coalition attests the founding problem remains live; that attestation comes from inside the benefiting parties, while the corroborating record for the shifted-function reading comes from outside it.
narrative_ontology:disappearance_verdict(substance_control_kernel__prohibition_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_kernel__prohibition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_kernel__prohibition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(substance_control_kernel__prohibition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_kernel__prohibition_reading, 0.85, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_kernel__prohibition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_kernel__prohibition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_kernel__prohibition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.85): the arrangement converts use into a lifelong liability — incarceration, a record that bars employment, housing, and voting, family separation — while the flows it generates (appropriations, forfeiture, contracts) land on identifiable institutional seats. Suppression (0.87) covers both the coercive apparatus aimed at users and the active barring of alternatives: syringe services, supervised consumption, and drug checking are prohibited precisely where need is largest, and reform is fought in legislatures long after opinion shifts. Theater (0.44) splits roughly evenly: the incarceration is real, but a large fraction of visible activity — press-conference seizures, classroom programs repeatedly shown inert, symbolic busts timed to budget season — performs resolve rather than producing the promised order. Accessibility collapse is low-moderate (0.38): health-led and regulated alternatives remain visible and operating in neighboring jurisdictions, so understanding the arrangement does not close the option space the way a natural limit would. Resistance (0.68) is sustained and partly effective — ballot measures, sentencing reform, decriminalization ordinances — which is why the late-interval series dips and recovers rather than climbing monotonically. All three tracked series are authored on one shared eight-point grid (1971–2026) so no metric's row is backfilled from another's; suppression_requirement is tracked because enforcement capacity is the dynamic this story traces (militarization and mandatory-minimum ratchet through 2002, partial attrition thereafter).
 *
 * PERSPECTIVAL GAP:
 *   From the enforcement seat the arrangement is a calling: careers, pensions, and institutional pride are constituted by the mission, and its administrators experience criticism as ingratitude rather than evidence. From the payer seats the same statutes are experienced as a lifelong sentence that begins at arrest and continues through every background check. The treatment sector occupies an ambivalent middle — fed by referrals it did not choose and cannot refuse. The observer seat sees the divergence between promised order and measured outcomes. On coalition: the bearer population is enormous but deliberately fragmented — felony disenfranchisement removes exactly the most affected from the electorate, geography disperses the rest, and stigma suppresses self-identification; where coalition channels do open (ballot initiatives), supporters respond by pre-empting or overriding them, which is itself evidence of how much the balance of numbers threatens the arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations place the enforcement apparatus, prison contractors, and seizure-funded units near the subsidized end (d approximately 0.05–0.15): they collect directly, and identity lock or arbitrage-grade exit means nothing dampens their collection. Victim declarations place users and the three exposed community seats near the full-target end (d approximately 0.9): trapped exit and, for users, fusion of personal identity with the criminal label amplify exposure. The mandated-treatment sector derives to a middle value from its dual declaration — it collects referral revenue while bearing coerced conditions — and no directionality override is needed because the dual role plus constrained exit captures the ambivalence. Observers sit near symmetric. Scopes run national to continental, which the engine's verification-difficulty scaling registers; the structural declarations alone drive the spread.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding mandate — protect social order by punishing transgression — is contested rather than dead: order-protection language remains rhetorically live while the measurable order-outcomes (overdose mortality, market violence, treatment gaps) worsened across the intensification decades. Reading the structure as coercive-with-cover keeps the coordination story from laundering enforcement-side accumulation as coordination cost: a genuine collective good would not require disenfranchising its chief prospective beneficiaries of relief in order to survive electorally. At the same time the analysis refuses the opposite overcorrection — a real residue of expressive coordination exists (constituents who value communal condemnation as such), which is why a coordination type is declared and the coordination_cover omega is carried rather than assumed away. Because founding_problem_status is contested rather than dead, the R5 mismatch flag does not fire; the honest state is a mandate whose justification has migrated from demonstrated effect toward institutional self-perpetuation, with the empirical branch of its axioms under active override pressure (see drift_state).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_index_variance,
    'This file authors epsilon for the prohibition_reading only; how would the harm_reduction_reading and legalization_reading instantiate the same standing arrangement differently?',
    'Generate the sibling stories against the same interval and compare per-seat classifications; the divergence locates the reading-indexed component of epsilon.',
    'Sibling readings would remove users from the criminal victim set, replace the enforcement apparatus''s administrative role with health or regulatory bodies, and re-author epsilon over the same referent by their own lights — shifting computed types toward service-provision shapes rather than coercive ones.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_index_variance, conceptual, 'Committer-frame routing: one kernel, three readings, three constraints; no averaging across readings.').

omega_variable(
    disagreement_location_moral_status,
    'Where exactly do the three readings disagree — on the moral status of use per se, on who counts as a victim, or on the competence of the state?',
    'No empirical resolution; the dispute is normative. Structurally, the load-bearing difference is victim-set membership: user-as-offender versus user-as-patient versus user-as-rights-holder.',
    'Whichever moral-status premise a polity adopts determines the victim set, hence epsilon and the computed type; the readings cannot be merged into one constraint without violating epsilon-invariance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disagreement_location_moral_status, preference, 'Location of the inter-reading disagreement within the kernel.').

omega_variable(
    punitive_deterrence_empirics,
    'Does criminal punishment reduce substance-use prevalence or improve social order relative to regulated or health-led baselines?',
    'Jurisdiction-level natural experiments: decriminalization episodes, state-level legalization, paired-city enforcement-intensification studies; measure prevalence, overdose mortality, market violence, and treatment uptake against matched controls.',
    'Sustained null or negative deterrence findings strip the instrumental axiom of empirical grounding, leaving the reading resting wholly on the condemnation premise and accelerating the axiom_overriding drift already recorded in drift_state.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(punitive_deterrence_empirics, empirical, 'Empirical status of the deterrence premise underlying the reading.').

omega_variable(
    coordination_cover_question,
    'Is the expressive-condemnation function a coordination good that constituents genuinely value and would sustain voluntarily, or post-hoc cover for enforcement-side accumulation?',
    'Deliberative polling and revealed preference in jurisdictions that relaxed enforcement while retaining condemnation norms; compare voluntary norm persistence and willingness to fund services absent compulsion.',
    'If the condemnation function is genuine, the arrangement carries real coordination content alongside its asymmetric burdens; if it is cover, the coercive reading of the structure is unqualified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_cover_question, conceptual, 'Genuineness of the moral-coordination function versus its use as cover.').

omega_variable(
    stigma_internalization_pathway,
    'How much of the observed treatment gap reflects structural legal risk versus internalized criminal identity that persists after legal barriers fall?',
    'Treatment-seeking trajectories before and after decriminalization in matched cohorts; internalized-stigma instruments against disclosure-risk measures.',
    'If internalized, suppression outlives repeal — removing statutes will not rapidly restore treatment uptake, and post-reform projections will overestimate recovery speed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stigma_internalization_pathway, empirical, 'Structural versus internalized component of suppressed help-seeking.').

omega_variable(
    market_violence_attribution,
    'Is black-market violence attributable to prohibition as such, or to enforcement intensity and style within prohibition?',
    'Compare violence rates across jurisdictions with equal prohibition stringency but different enforcement intensities; examine homicide patterns in transit markets before and after legalization episodes.',
    'Determines whether the violence enters this reading''s victim ledger as an inherent produced externality or a contingent byproduct; materially affects victim-set composition even if epsilon moves only marginally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_violence_attribution, empirical, 'Attribution of the secondary externality to the arrangement itself.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_kernel__prohibition_reading, 1971, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prohibition_reading_tr_t1971, substance_control_kernel__prohibition_reading, theater_ratio, 1971, 0.25).
narrative_ontology:measurement_basis(prohibition_reading_tr_t1971, observed).
narrative_ontology:measurement(prohibition_reading_tr_t1980, substance_control_kernel__prohibition_reading, theater_ratio, 1980, 0.3).
narrative_ontology:measurement_basis(prohibition_reading_tr_t1980, observed).
narrative_ontology:measurement(prohibition_reading_tr_t1986, substance_control_kernel__prohibition_reading, theater_ratio, 1986, 0.38).
narrative_ontology:measurement_basis(prohibition_reading_tr_t1986, observed).
narrative_ontology:measurement(prohibition_reading_tr_t1994, substance_control_kernel__prohibition_reading, theater_ratio, 1994, 0.45).
narrative_ontology:measurement_basis(prohibition_reading_tr_t1994, observed).
narrative_ontology:measurement(prohibition_reading_tr_t2002, substance_control_kernel__prohibition_reading, theater_ratio, 2002, 0.48).
narrative_ontology:measurement_basis(prohibition_reading_tr_t2002, observed).
narrative_ontology:measurement(prohibition_reading_tr_t2010, substance_control_kernel__prohibition_reading, theater_ratio, 2010, 0.46).
narrative_ontology:measurement_basis(prohibition_reading_tr_t2010, observed).
narrative_ontology:measurement(prohibition_reading_tr_t2018, substance_control_kernel__prohibition_reading, theater_ratio, 2018, 0.43).
narrative_ontology:measurement_basis(prohibition_reading_tr_t2018, observed).
narrative_ontology:measurement(prohibition_reading_tr_t2026, substance_control_kernel__prohibition_reading, theater_ratio, 2026, 0.44).
narrative_ontology:measurement_basis(prohibition_reading_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(prohibition_reading_be_t1971, substance_control_kernel__prohibition_reading, base_extractiveness, 1971, 0.55).
narrative_ontology:measurement_basis(prohibition_reading_be_t1971, observed).
narrative_ontology:measurement(prohibition_reading_be_t1980, substance_control_kernel__prohibition_reading, base_extractiveness, 1980, 0.62).
narrative_ontology:measurement_basis(prohibition_reading_be_t1980, observed).
narrative_ontology:measurement(prohibition_reading_be_t1986, substance_control_kernel__prohibition_reading, base_extractiveness, 1986, 0.72).
narrative_ontology:measurement_basis(prohibition_reading_be_t1986, observed).
narrative_ontology:measurement(prohibition_reading_be_t1994, substance_control_kernel__prohibition_reading, base_extractiveness, 1994, 0.82).
narrative_ontology:measurement_basis(prohibition_reading_be_t1994, observed).
narrative_ontology:measurement(prohibition_reading_be_t2002, substance_control_kernel__prohibition_reading, base_extractiveness, 2002, 0.86).
narrative_ontology:measurement_basis(prohibition_reading_be_t2002, observed).
narrative_ontology:measurement(prohibition_reading_be_t2010, substance_control_kernel__prohibition_reading, base_extractiveness, 2010, 0.86).
narrative_ontology:measurement_basis(prohibition_reading_be_t2010, observed).
narrative_ontology:measurement(prohibition_reading_be_t2018, substance_control_kernel__prohibition_reading, base_extractiveness, 2018, 0.81).
narrative_ontology:measurement_basis(prohibition_reading_be_t2018, observed).
narrative_ontology:measurement(prohibition_reading_be_t2026, substance_control_kernel__prohibition_reading, base_extractiveness, 2026, 0.85).
narrative_ontology:measurement_basis(prohibition_reading_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(prohibition_reading_su_t1971, substance_control_kernel__prohibition_reading, suppression_requirement, 1971, 0.5).
narrative_ontology:measurement_basis(prohibition_reading_su_t1971, observed).
narrative_ontology:measurement(prohibition_reading_su_t1980, substance_control_kernel__prohibition_reading, suppression_requirement, 1980, 0.58).
narrative_ontology:measurement_basis(prohibition_reading_su_t1980, observed).
narrative_ontology:measurement(prohibition_reading_su_t1986, substance_control_kernel__prohibition_reading, suppression_requirement, 1986, 0.72).
narrative_ontology:measurement_basis(prohibition_reading_su_t1986, observed).
narrative_ontology:measurement(prohibition_reading_su_t1994, substance_control_kernel__prohibition_reading, suppression_requirement, 1994, 0.84).
narrative_ontology:measurement_basis(prohibition_reading_su_t1994, observed).
narrative_ontology:measurement(prohibition_reading_su_t2002, substance_control_kernel__prohibition_reading, suppression_requirement, 2002, 0.88).
narrative_ontology:measurement_basis(prohibition_reading_su_t2002, observed).
narrative_ontology:measurement(prohibition_reading_su_t2010, substance_control_kernel__prohibition_reading, suppression_requirement, 2010, 0.87).
narrative_ontology:measurement_basis(prohibition_reading_su_t2010, observed).
narrative_ontology:measurement(prohibition_reading_su_t2018, substance_control_kernel__prohibition_reading, suppression_requirement, 2018, 0.85).
narrative_ontology:measurement_basis(prohibition_reading_su_t2018, observed).
narrative_ontology:measurement(prohibition_reading_su_t2026, substance_control_kernel__prohibition_reading, suppression_requirement, 2026, 0.87).
narrative_ontology:measurement_basis(prohibition_reading_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_kernel__prohibition_reading, identity_coordination).
narrative_ontology:affects_constraint(substance_control_kernel__prohibition_reading, substance_control_kernel__harm_reduction_reading).
narrative_ontology:affects_constraint(substance_control_kernel__prohibition_reading, substance_control_kernel__legalization_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'drug prohibition' decomposes into three readings of one kernel (substance_control_kernel): this file instantiates the prohibition_reading only. All three readings assess the SAME standing arrangement — the existing criminalization regime — each by its own lights, so epsilon is reading-indexed over a shared referent rather than averaged across readings. This reading places users in the criminal victim set and vests authority in coercive state apparatus; the harm_reduction_reading relocates users to a patient set and authority to health bodies; the legalization_reading relocates users to a rights-bearing set and authority to regulators. Each sibling is a separate constraint story with its own epsilon, victim structure, and classification; family linkage runs through network.affects_constraints in all three files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
