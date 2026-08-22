% ============================================================================
% CONSTRAINT STORY: udhr_article_3__positive_entitlement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_udhr_article_3__positive_entitlement_reading, []).

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
 *   constraint_id: udhr_article_3__positive_entitlement_reading
 *   human_readable: UDHR Article 3 — Positive Entitlement Reading (State Material-Security Obligation)
 *   domain: constitutional_law/human_rights/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the positive entitlement reading of UDHR Article
 *   3: the state is under a standing obligation to secure the material
 *   conditions of life and personal security — subsistence income,
 *   healthcare, housing — for everyone within its jurisdiction. As an
 *   operating arrangement this is the constitutional welfare state:
 *   general-taxation finance, universal or categorical transfer systems,
 *   public healthcare and housing provision, eligibility administration, and
 *   a penumbra of expression rules that restrict incitement and threatening
 *   speech in the name of protecting persons. The arrangement solves a real
 *   collective-action problem (markets underprovide life-goods to those
 *   without market income) while imposing large, asymmetric, actively
 *   enforced burdens: compulsory finance falls on property and earnings, and
 *   expressive restriction falls on speakers, as the price of the guarantee.
 *   The claim and the metrics are authored independently: the claimed type
 *   records the structure I believe true (genuine coordination carrying
 *   asymmetric extraction under active enforcement), and the metrics record
 *   the operation I believe descriptively accurate. This file belongs to a
 *   decomposed family — the colloquial label 'the Article 3 right' covers
 *   structurally distinct claims with different burden structures, and the
 *   family members are linked in network.affects_constraints; the
 *   reading-level contest is recorded in kernel_context and the kernel omega,
 *   not adjudicated here.
 *
 * KEY AGENTS:
 *   - welfare_state_governments: Agenda-setter (institutional/constrained) — legislates, finances, and administers the provision obligation
 *   - welfare_administrative_agencies: Administrative beneficiary with secondary agenda-setting role (institutional/constrained) — accrues budgets, staffing, and discretion from program operation
 *   - welfare_recipients: Primary beneficiary (powerless/trapped) — subsistence flows in; exit forfeits it
 *   - public_healthcare_patients: Beneficiary (moderate/constrained) — receives taxed-financed care, bears rationing
 *   - public_housing_tenants: Beneficiary (moderate/constrained) — holds below-market tenures, bears allocation limits
 *   - property_owners_taxpayers: Primary payer (organized/constrained) — bears the financing burden on an immobile base
 *   - high_wealth_individuals: Payer with arbitrage-grade exit (powerful/arbitrage) — bears top rates but can relocate the base
 *   - regulated_public_speakers: Payer in kind (moderate/identity_locked) — bears expression restriction
 *   - civil_liberties_advocates: Excluded critic (organized/constrained) — objects from outside the budget-setting conversation
 *   - human_rights_treaty_bodies: Analytical observer (institutional/analytical) — reviews compliance, shapes legitimacy, commands no budgets
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_article_3__positive_entitlement_reading, 0.68).
domain_priors:suppression_score(udhr_article_3__positive_entitlement_reading, 0.6).
domain_priors:theater_ratio(udhr_article_3__positive_entitlement_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_article_3__positive_entitlement_reading, tangled_rope).
narrative_ontology:human_readable(udhr_article_3__positive_entitlement_reading, "UDHR Article 3 — Positive Entitlement Reading (State Material-Security Obligation)").
narrative_ontology:topic_domain(udhr_article_3__positive_entitlement_reading, "constitutional_law/human_rights/political_philosophy").

domain_priors:requires_active_enforcement(udhr_article_3__positive_entitlement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_article_3__positive_entitlement_reading, '44e9c191-55ae-44b4-b798-b4b4c14ab706').
narrative_ontology:cs_kernel_codification('44e9c191-55ae-44b4-b798-b4b4c14ab706', fixed_text).
narrative_ontology:cs_authority_grounding('44e9c191-55ae-44b4-b798-b4b4c14ab706', lineage).
narrative_ontology:cs_interpretation_layer_present('44e9c191-55ae-44b4-b798-b4b4c14ab706').
narrative_ontology:cs_reading_relation('44e9c191-55ae-44b4-b798-b4b4c14ab706', udhr_article_3__negative_liberty_reading, coexists_with).
narrative_ontology:cs_reading_relation('44e9c191-55ae-44b4-b798-b4b4c14ab706', udhr_article_3__procedural_hybrid_reading, influences).
narrative_ontology:cs_axiom('44e9c191-55ae-44b4-b798-b4b4c14ab706', foundational, rights_include_positive_provision_duties).
narrative_ontology:cs_axiom_status(rights_include_positive_provision_duties, holdable).
narrative_ontology:cs_axiom_grounding('44e9c191-55ae-44b4-b798-b4b4c14ab706', rights_include_positive_provision_duties, deontological).
narrative_ontology:cs_axiom('44e9c191-55ae-44b4-b798-b4b4c14ab706', secondary, security_of_person_justifies_expressive_restriction).
narrative_ontology:cs_axiom_status(security_of_person_justifies_expressive_restriction, holdable).
narrative_ontology:cs_axiom_grounding('44e9c191-55ae-44b4-b798-b4b4c14ab706', security_of_person_justifies_expressive_restriction, instrumental).
narrative_ontology:cs_reference_frame('44e9c191-55ae-44b4-b798-b4b4c14ab706', material_security_guarantee_state).
narrative_ontology:cs_drift_state('44e9c191-55ae-44b4-b798-b4b4c14ab706', contemporary_austerity_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('44e9c191-55ae-44b4-b798-b4b4c14ab706', '').
narrative_ontology:cs_kernel_id(udhr_article_3__positive_entitlement_reading, udhr_article_3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_article_3__positive_entitlement_reading, welfare_recipients).
narrative_ontology:constraint_beneficiary(udhr_article_3__positive_entitlement_reading, public_healthcare_patients).
narrative_ontology:constraint_beneficiary(udhr_article_3__positive_entitlement_reading, public_housing_tenants).
narrative_ontology:constraint_victim(udhr_article_3__positive_entitlement_reading, property_owners_taxpayers).
narrative_ontology:constraint_victim(udhr_article_3__positive_entitlement_reading, high_wealth_individuals).
narrative_ontology:constraint_victim(udhr_article_3__positive_entitlement_reading, regulated_public_speakers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(udhr_article_3__positive_entitlement_reading, welfare_administrative_agencies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enact and amend the statutes creating welfare, public healthcare, and housing programs; set the tax rates that finance them; define eligibility and sanction rules; answer to treaty-body reviews and constitutional courts. Their policy room is bounded by entrenched social-rights clauses and by electorate expectations that provision continues. They cannot exit the system they administer; electoral replacement is the only exit.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, welfare_state_governments, agenda_setter,
    institutional, generational, constrained, national).

% Operate the benefit offices, public health systems, and housing authorities day to day. Budgets, staffing levels, and discretionary power flow to them for as long as the provision obligation stands; they police eligibility, run sanction processes, and report performance upward. Their institutional continuation is tied to the programs they run, and they shape the detailed rules their principals only sketch.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, welfare_administrative_agencies, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(udhr_article_3__positive_entitlement_reading, welfare_administrative_agencies, agenda_setter).

% Receive income support and cash transfers that constitute most or all of household subsistence. Continued receipt depends on meeting administrative conditions; losing eligibility means immediate material hardship. Moving abroad or leaving the rolls forfeits the support that keeps them housed and fed, so exit is not a live option.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, welfare_recipients, beneficiary,
    powerless, immediate, trapped, national).

% Obtain care through publicly funded systems financed by general taxation. Access is largely free at the point of use but rationed by queues and coverage rules; buying faster private care is available for some procedures and incomes, while the public system carries the load for serious and chronic conditions.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, public_healthcare_patients, beneficiary,
    moderate, biographical, constrained, national).

% Occupy subsidized or allocated housing at rents pegged below market. Allocation runs through waiting lists tied to locality; moving means re-entering the queue elsewhere. Tenure security is strong; choice of dwelling and location is narrow.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, public_housing_tenants, beneficiary,
    moderate, biographical, constrained, regional).

% Hold homes, land, and business assets and pay the progressive income, property, and consumption taxes that finance provision. Political voice runs through voting and industry associations. Land cannot be moved; income and incorporation can be arranged to soften liability, but the tax base follows residence.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, property_owners_taxpayers, payer,
    organized, generational, constrained, national).

% Bear top marginal rates and, in some jurisdictions, wealth and estate levies. They retain the strongest avoidance and relocation capacity in the system — second residencies, asset migration, citizenship shopping — which caps how far rates can rise before the base erodes.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, high_wealth_individuals, payer,
    powerful, generational, arbitrage, global).

% Publish, preach, campaign, and post under speech rules restricting incitement, hateful, and threatening expression in the name of protecting persons' security. Penalties run from takedown to prosecution. Their voice is tied to communities and causes they cannot take with them; relocating does not relocate the audience.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, regulated_public_speakers, payer,
    moderate, biographical, identity_locked, national).

% Organize against speech restrictions, compelled funding, and benefit conditionality, arguing from classical liberty texts. They litigate and publish but hold few seats in the budget-setting conversation, where the welfare consensus frames the terms of debate.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, civil_liberties_advocates, excluded,
    organized, generational, constrained, national).

% Review state reports, issue general comments, and hear petitions under the human-rights treaties descended from the postwar instruments. Their findings shape legitimacy and soft-law expectations; they command no budgets and enforce through publicity and peer pressure.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, human_rights_treaty_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(udhr_article_3__positive_entitlement_reading, diffuse).
narrative_ontology:fixing_cost_class(udhr_article_3__positive_entitlement_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of universal material security: pooling risk and resources through compulsory finance so that life-preserving goods — subsistence income, healthcare, shelter — are available independent of individual market position, in domains where voluntary markets chronically underprovide for people without market income.
% TRANSFER_FUNCTION: Moves wealth (tax revenue drawn from property, earnings, and consumption) to welfare recipients, patients, and housing tenants; moves a share of gross product to the administrative apparatus that runs provision; and moves expressive freedom from all speakers to a regulated public sphere, justified as protecting persons' security.
% ABSENT_VOICES: Civil-liberties advocates and proponents of market or charitable provision are structurally outside the budget-setting conversation: the welfare consensus sets the terms, and dissent enters only as litigation after the fact. Future generations who will service provision-related debt are present in no forum at all.
% DISAPPEARANCE_RATIONALE: If the obligation vanished overnight, tens of millions of households would lose subsistence income immediately, public healthcare systems would lose their financing base mid-operation, subsidized tenancies would dissolve into housing markets, and governments would face emergency poor relief at scales unseen since the 1930s. The political order built on the social-rights settlement — party systems, public-sector employment, treaty-review machinery — would reorganize around whatever replaced it.
% FOUNDING_PROBLEM: Mass material insecurity in industrial societies: the interwar depression and the war had shown that labor markets alone left large populations without the material conditions of life, and the postwar settlement undertook to guarantee subsistence, care, and shelter as matters of right rather than charity.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: national statistical offices' continuing poverty, untreated-illness, and homelessness series; WHO and OECD health-coverage and housing-affordability data; UN Special Rapporteur reporting on extreme poverty; and the academic social-policy and health-economics literature all attest that material insecurity persists at scale. No credible external source attests that the founding problem is solved; the attestations of continued need come disproportionately from the provision apparatus itself, which is why the statistical and epidemiological sources are load-bearing here.
narrative_ontology:disappearance_verdict(udhr_article_3__positive_entitlement_reading, world_rearranges).
narrative_ontology:founding_problem_status(udhr_article_3__positive_entitlement_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_article_3__positive_entitlement_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(udhr_article_3__positive_entitlement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(udhr_article_3__positive_entitlement_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(udhr_article_3__positive_entitlement_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(udhr_article_3__positive_entitlement_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(udhr_article_3__positive_entitlement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.68 because the financing burden is large, compulsory, and only loosely coupled to what any individual payer consumes: progressive taxation, property levies, and consumption taxes transfer roughly a third to half of national income through the provision apparatus in mature systems, and the expression burden adds a non-fiscal taking. Suppression is 0.60: persistence rests on compulsion (tax law, mandatory participation, speech penalties) rather than voluntary subscription, but the machinery is legal-routine rather than violent. Theater is low (0.22) because the function is overwhelmingly real — transfers arrive, hospitals operate, houses are allocated — with a slow-growing performative margin (symbolic rights instruments, compliance reporting to treaty bodies, ribbon-cutting provision). Accessibility_collapse is 0.40: alternatives persist substantially (private insurance, charity, private saving, emigration), so understanding the arrangement does not collapse the option space the way a natural law would. Resistance is 0.55: sustained taxpayer politics, anti-tax movements, litigation over speech restrictions, and jurisdictional tax competition meet the arrangement continuously. The temporal series run on one shared nine-point grid (1948–2025) with every tracked metric authored at every point. Extractiveness climbs through the great mid-century expansion, dips in the 1980s retrenchment (privatizations, benefit cuts, tax reforms), then resumes climbing under aging demographics, healthcare-cost inflation, housing scarcity, and pandemic-era spending. Suppression_requirement is tracked deliberately because this story's enforcement history is one of machinery build-out: thin postwar administrations matured into comprehensive tax-collection, eligibility-policing, fraud-detection, and speech-regulation apparatuses — a rising enforcement-intensity trajectory, not a static picture. Theater rises gently as ceremonial and reporting activity accumulates around a still-functional core.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute radically different arrangements from the same text. From the government seat this is duty-fulfillment: the state doing what the constitution now says it must. From the administrative seat it is mission and budget at once. From the recipient seats it is a lifeline whose loss is unthinkably worse than its conditions. From the property-owner seat it is a standing lien on earned holdings enforced by criminal law. From the high-wealth seat it is a rate ceiling negotiated against their own mobility. From the regulated-speaker seat it is censorship wearing a security badge. From the excluded civil-liberties seat the whole structure is overreach that never had to be this size. The engine computes per-seat classifications from the structural data; the divergence between the beneficiary seats' near-subsidy experience and the payer seats' near-full-target experience is the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. Welfare recipients sit nearest the full-beneficiary end (d near 0.05): the arrangement subsidizes them and their exit is trapped, so effective burden inverts into subsidy. Patients and tenants sit slightly higher (partial private alternatives damp the subsidy). The administrative agencies derive low d from their beneficiary position, with a mild capture shading from their agenda-setting secondary role. Payers cluster near the target end: property owners approach full-target (immobile base, organized but unable to exit the tax net), regulated speakers approach full-target with identity-lock amplifying their exposure (their voice cannot move without abandoning its audience), while high-wealth individuals are pulled back toward the middle by arbitrage-grade exit — the engine's exit modulation correctly prices their capacity to move the base before the rate moves them. Spatial scopes (national for most seats, global for mobile capital) feed the engine's verification scaling. No directionality overrides were needed: the declarations plus exit atoms produce the correct relationships, and the engine owns the arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — mass material insecurity in industrial societies — is still live, so this is not a mandate outliving its function; mandatrophy is not resolved and no sunset applies. The classification work here is keeping both components visible against two symmetrical misreadings. Reading the arrangement as pure coordination (a rope) erases the real, actively enforced burdens on property and expression holders that ride the same structure. Reading it as pure extraction (a snare) erases the enormous genuine provision that distinguishes it from protection rackets — the transfers and services are not cover; they are the output. The tangled_rope claim holds both truths in one structure. The theater series is the early-warning instrument: if provision were privatized away or the duty reduced to ceremonial affirmation while the taxing and restricting machinery persisted, theater_ratio would spike and the structure would drift toward inertial maintenance — the measurements exist to catch that transition if it comes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article3_kernel_reading_contest,
    'This constraint is one reading (positive_entitlement_reading) of kernel udhr_article_3; what structural differences would instantiate under the sibling readings negative_liberty_reading and procedural_hybrid_reading?',
    'Cross-jurisdictional comparison of constitutional orders that adopt each reading: instrument the same Article 3-lineage text under a negative-liberty court, a social-rights court, and a procedural court, and compare victim/beneficiary structure, enforcement machinery, and effective burden distribution.',
    'Under negative_liberty_reading the structure inverts: the state''s provision apparatus becomes the threat, taxpayers and recipients recede as seats, and the enforcement machinery itself is the object of restraint. Under procedural_hybrid_reading the substantive burden question is bracketed entirely and only process guarantees carry weight. The current classification holds only for this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article3_kernel_reading_contest, conceptual, 'Committer structure: one reading of a contested kernel; sibling readings instantiate different constraints.').

omega_variable(
    principle_vs_implementation_extraction,
    'Is the measured burden on property and expression holders intrinsic to the positive-obligation principle itself, or an artifact of particular fiscal and administrative implementations (tax mix, overhead, sanction design)?',
    'Compare burden profiles across jurisdictions holding the same positive reading but different implementations (Nordic broad-base models vs contribution-insurance models vs means-tested models); isolate the variance attributable to design choice rather than principle.',
    'If most measured burden is implementation-contingent, the reading''s constraint is closer to a repairable coordination mechanism; if burden persists across implementations, it is structural to the reading and the tangled_rope reading hardens toward the extractive end.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(principle_vs_implementation_extraction, empirical, 'Whether extraction is principled into the reading or contingent on implementation.').

omega_variable(
    conditionality_discipline_drift,
    'Does the rising conditionality of provision (work requirements, sanction regimes, behavioral eligibility tests) convert the recipients'' subsidy into a disciplinary mechanism aimed at the recipients themselves?',
    'Track sanction incidence, appeal outcomes, and recipient-side exit attempts in jurisdictions that tightened conditionality versus those that did not; measure whether recipient directionality shifts upward as conditions multiply.',
    'If conditionality functions disciplinarily, the nominal beneficiary seat acquires a meaningful target component, flattening the apparent asymmetry and shifting the computed type toward symmetric hybrid; if not, recipients remain near the full-beneficiary end.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conditionality_discipline_drift, empirical, 'Whether provision conditionality disciplines the beneficiaries it subsidizes.').

omega_variable(
    security_exception_scope_drift,
    'How far can security-of-person justifications extend expressive restriction before the suppression component outgrows the coordination function it nominally serves?',
    'Doctrinal tracing of the expanding set of expression categories brought under security justifications (incitement, hateful expression, public-order offenses, online harms regimes) against measured effects on persons'' material security.',
    'If the restricted category set expands without measurable security gains, the expression burden is revealed as enforcement-path dependence rather than provision-necessary cost, raising effective suppression and pushing the structure toward the extractive end.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_exception_scope_drift, conceptual, 'Scope boundary of security justifications for expressive restriction.').

omega_variable(
    intergenerational_cost_incidence,
    'How much of the financing burden is deferred to future generations through debt-financed provision, and does that deferred cohort count as a bearer the current stakeholder surface fails to seat?',
    'Generational accounting of cumulative deficits attributable to provision commitments, combined with demographic projection of the contributor-to-beneficiary ratio.',
    'If a large share of the burden is deferred, the current victim set understates total extraction and an unseated future cohort is the constraint''s largest silent payer, strengthening the absent-voices finding and raising lifetime effective extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergenerational_cost_incidence, empirical, 'Deferred-burden incidence across generations of contributors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_article_3__positive_entitlement_reading, 1948, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_tr_t1948, udhr_article_3__positive_entitlement_reading, theater_ratio, 1948, 0.08).
narrative_ontology:measurement(udhr_tr_t1958, udhr_article_3__positive_entitlement_reading, theater_ratio, 1958, 0.1).
narrative_ontology:measurement(udhr_tr_t1968, udhr_article_3__positive_entitlement_reading, theater_ratio, 1968, 0.12).
narrative_ontology:measurement(udhr_tr_t1978, udhr_article_3__positive_entitlement_reading, theater_ratio, 1978, 0.14).
narrative_ontology:measurement(udhr_tr_t1988, udhr_article_3__positive_entitlement_reading, theater_ratio, 1988, 0.16).
narrative_ontology:measurement(udhr_tr_t1998, udhr_article_3__positive_entitlement_reading, theater_ratio, 1998, 0.17).
narrative_ontology:measurement(udhr_tr_t2008, udhr_article_3__positive_entitlement_reading, theater_ratio, 2008, 0.19).
narrative_ontology:measurement(udhr_tr_t2018, udhr_article_3__positive_entitlement_reading, theater_ratio, 2018, 0.21).
narrative_ontology:measurement(udhr_tr_t2025, udhr_article_3__positive_entitlement_reading, theater_ratio, 2025, 0.22).

% Extraction over time
narrative_ontology:measurement(udhr_be_t1948, udhr_article_3__positive_entitlement_reading, base_extractiveness, 1948, 0.34).
narrative_ontology:measurement(udhr_be_t1958, udhr_article_3__positive_entitlement_reading, base_extractiveness, 1958, 0.41).
narrative_ontology:measurement(udhr_be_t1968, udhr_article_3__positive_entitlement_reading, base_extractiveness, 1968, 0.5).
narrative_ontology:measurement(udhr_be_t1978, udhr_article_3__positive_entitlement_reading, base_extractiveness, 1978, 0.57).
narrative_ontology:measurement(udhr_be_t1988, udhr_article_3__positive_entitlement_reading, base_extractiveness, 1988, 0.54).
narrative_ontology:measurement(udhr_be_t1998, udhr_article_3__positive_entitlement_reading, base_extractiveness, 1998, 0.56).
narrative_ontology:measurement(udhr_be_t2008, udhr_article_3__positive_entitlement_reading, base_extractiveness, 2008, 0.61).
narrative_ontology:measurement(udhr_be_t2018, udhr_article_3__positive_entitlement_reading, base_extractiveness, 2018, 0.65).
narrative_ontology:measurement(udhr_be_t2025, udhr_article_3__positive_entitlement_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(udhr_su_t1948, udhr_article_3__positive_entitlement_reading, suppression_requirement, 1948, 0.28).
narrative_ontology:measurement(udhr_su_t1958, udhr_article_3__positive_entitlement_reading, suppression_requirement, 1958, 0.33).
narrative_ontology:measurement(udhr_su_t1968, udhr_article_3__positive_entitlement_reading, suppression_requirement, 1968, 0.38).
narrative_ontology:measurement(udhr_su_t1978, udhr_article_3__positive_entitlement_reading, suppression_requirement, 1978, 0.44).
narrative_ontology:measurement(udhr_su_t1988, udhr_article_3__positive_entitlement_reading, suppression_requirement, 1988, 0.47).
narrative_ontology:measurement(udhr_su_t1998, udhr_article_3__positive_entitlement_reading, suppression_requirement, 1998, 0.5).
narrative_ontology:measurement(udhr_su_t2008, udhr_article_3__positive_entitlement_reading, suppression_requirement, 2008, 0.53).
narrative_ontology:measurement(udhr_su_t2018, udhr_article_3__positive_entitlement_reading, suppression_requirement, 2018, 0.57).
narrative_ontology:measurement(udhr_su_t2025, udhr_article_3__positive_entitlement_reading, suppression_requirement, 2025, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_article_3__positive_entitlement_reading, resource_allocation).
narrative_ontology:affects_constraint(udhr_article_3__positive_entitlement_reading, udhr_article_3__negative_liberty_reading).
narrative_ontology:affects_constraint(udhr_article_3__positive_entitlement_reading, udhr_article_3__procedural_hybrid_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'the Article 3 right'. The natural-language concept covers at least three structurally distinct claims with materially different epsilon values and burden structures: the negative-liberty claim (negligible fiscal extraction; the state restrained), the positive-entitlement claim (this file; large compulsory finance plus expression restriction riding a genuine provision function), and the procedural-hybrid claim (process guarantees only; the substantive contest bracketed). Historically the negative reading is upstream — the 1948 text was first received through classical-liberty lenses — and the positive reading builds downstream on the freedom-from-want lineage (Four Freedoms, ICESCR, constitutional social-rights clauses), citing the shared text as warrant. Family members are mutually linked via affects_constraints; each file carries a single stable epsilon per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
