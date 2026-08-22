% ============================================================================
% CONSTRAINT STORY: equal_protection_kernel__antisubordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_kernel__antisubordination_reading, []).

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
 *   constraint_id: equal_protection_kernel__antisubordination_reading
 *   human_readable: Equal Protection Clause — Antisubordination Reading (Caste-Targeting Standard)
 *   domain: constitutional_law/education_policy/civil_rights
 *
 * SUMMARY:
 *   This story instantiates ONE reading of a contested kernel. The kernel is
 *   the Equal Protection Clause of the Fourteenth Amendment; this file
 *   generates the antisubordination_reading, under which the clause targets
 *   caste-like subordination of historically oppressed groups rather than
 *   racial classification as such. State action that entrenches hierarchy is
 *   void; state action that dismantles it is valid; majority-group plaintiffs
 *   have no cognizable claim against remedial measures. Per the
 *   epsilon-invariance discipline, the sibling readings (remedial_reading,
 *   colorblind_reading) are separate constraints in separate files with their
 *   own epsilon values and party structures — the colloquial label 'Equal
 *   Protection Clause' covers three structurally distinct arrangements,
 *   decomposed and linked via network.affects_constraints. The claim/metric
 *   gap here is deliberate and load-bearing: the reading is CLAIMED as
 *   tangled_rope on structural grounds (genuine adjudicative coordination
 *   function plus an explicit, embraced asymmetry between winners and losers,
 *   actively enforced by courts), while the metrics are authored from the
 *   reading's own evaluative seat, which sees the arrangement as mostly
 *   protective with modest inherent cost — the engine computes per-seat
 *   classifications from the structural data and measures that divergence.
 *
 * KEY AGENTS:
 *   - historically_subordinated_racial_groups: Primary beneficiary (organized/constrained) — holds both a shield against hierarchy-entrenching laws and a sword permitting race-conscious remediation; cannot exit the identity the protection attaches to
 *   - majority_group_members: Primary target (powerful/constrained) — denied equal-protection recourse against remedial measures; bears race-conscious burdens without constitutional remedy
 *   - article_iii_judiciary: Agenda-setter (institutional/constrained) — defines 'entrench' vs. 'dismantle,' decides which groups qualify as historically oppressed, dismisses majority-group claims as non-cognizable
 *   - public_universities: Program administrator (institutional/mobile) — dual position: validated in running race-conscious programs, exposed in defending them
 *   - civil_rights_advocacy_organizations: Secondary beneficiary (organized/mobile) — dockets, funding, and relevance ride on the reading's vitality
 *   - individual_burdened_applicants: Excluded voice (powerless/trapped) — objects on the basis of personal treatment; the framework has no slot for the objection
 *   - constitutional_law_scholars: Analytical observer (analytical/analytical) — maps the three readings, traces Reconstruction history, supplies comparative evidence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_kernel__antisubordination_reading, 0.28).
domain_priors:suppression_score(equal_protection_kernel__antisubordination_reading, 0.35).
domain_priors:theater_ratio(equal_protection_kernel__antisubordination_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_kernel__antisubordination_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_kernel__antisubordination_reading, "Equal Protection Clause — Antisubordination Reading (Caste-Targeting Standard)").
narrative_ontology:topic_domain(equal_protection_kernel__antisubordination_reading, "constitutional_law/education_policy/civil_rights").

domain_priors:requires_active_enforcement(equal_protection_kernel__antisubordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_kernel__antisubordination_reading, '59154213-8750-4243-89a5-868488d52969').
narrative_ontology:cs_kernel_codification('59154213-8750-4243-89a5-868488d52969', fixed_text).
narrative_ontology:cs_authority_grounding('59154213-8750-4243-89a5-868488d52969', lineage).
narrative_ontology:cs_interpretation_layer_present('59154213-8750-4243-89a5-868488d52969').
narrative_ontology:cs_reading_relation('59154213-8750-4243-89a5-868488d52969', equal_protection_kernel__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('59154213-8750-4243-89a5-868488d52969', equal_protection_kernel__remedial_reading, influences).
narrative_ontology:cs_axiom('59154213-8750-4243-89a5-868488d52969', foundational, subordination_not_classification_is_the_wrong).
narrative_ontology:cs_axiom_status(subordination_not_classification_is_the_wrong, holdable).
narrative_ontology:cs_axiom_grounding('59154213-8750-4243-89a5-868488d52969', subordination_not_classification_is_the_wrong, deontological).
narrative_ontology:cs_axiom('59154213-8750-4243-89a5-868488d52969', secondary, dismantling_action_stands_outside_clause_prohibition).
narrative_ontology:cs_axiom_status(dismantling_action_stands_outside_clause_prohibition, holdable).
narrative_ontology:cs_axiom_grounding('59154213-8750-4243-89a5-868488d52969', dismantling_action_stands_outside_clause_prohibition, instrumental).
narrative_ontology:cs_reference_frame('59154213-8750-4243-89a5-868488d52969', reconstruction_anticaste_guarantee).
narrative_ontology:cs_drift_state('59154213-8750-4243-89a5-868488d52969', contemporary_post_sffa_doctrine, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('59154213-8750-4243-89a5-868488d52969', '').
narrative_ontology:cs_kernel_id(equal_protection_kernel__antisubordination_reading, equal_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_kernel__antisubordination_reading, historically_subordinated_racial_groups).
narrative_ontology:constraint_victim(equal_protection_kernel__antisubordination_reading, majority_group_members).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__antisubordination_reading, public_universities).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__antisubordination_reading, civil_rights_advocacy_organizations).
narrative_ontology:constraint_victim(equal_protection_kernel__antisubordination_reading, public_universities).
narrative_ontology:constraint_vindicates(equal_protection_kernel__antisubordination_reading, antisubordination_principle).
narrative_ontology:constraint_vindicates(equal_protection_kernel__antisubordination_reading, reconstruction_caste_abolition_purpose).
narrative_ontology:constraint_vindicates(equal_protection_kernel__antisubordination_reading, group_disadvantage_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Communities whose members have lived under state-enforced racial hierarchy — descendants of enslaved people, alumni of segregated school systems, residents of neighborhoods shaped by redlining and concentrated disadvantage. Under this reading they hold a constitutional shield (laws that entrench their subordination fall) and a constitutional sword (race-conscious programs aimed at taking the hierarchy apart stand). They cannot exit the racial identity the protection attaches to, and the protection is worth having only for as long as the hierarchy persists.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, historically_subordinated_racial_groups, beneficiary,
    organized, generational, constrained, national).

% Members of racial majorities who are denied an equal-protection claim against race-conscious measures under this reading. An applicant or contractor disadvantaged by a remedial program has no cognizable constitutional injury here — the reading holds the clause was never addressed to them. Some bear concrete burdens (lost admissions seats, lost public contracts); all bear the loss of a legal instrument they would hold under either rival reading. Their recourse runs to politics, legislation, and the composition of courts, not to an equal-protection suit.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, majority_group_members, payer,
    powerful, biographical, constrained, national).

% Federal courts decide which state actions entrench hierarchy and which dismantle it, determine which groups count as historically oppressed, and dismiss majority-group challenges as non-cognizable. They uphold remedial programs and strike subordinating laws, and their own precedents bind them in turn. They cannot opt out of adjudicating the line they police; every new program and every new challenge lands on their docket.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, article_iii_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% State institutions running race-conscious admissions, outreach, pipeline programs, and faculty hiring. The reading validates these programs where rival readings would forbid them, letting institutions pursue integration goals openly. The same programs generate decades of litigation, compliance machinery, and — after adverse rulings — shutdowns and redesigns; institutions have spent fortunes defending programs whose fate turns on which reading governs.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, public_universities, beneficiary,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_kernel__antisubordination_reading, public_universities, payer).

% Litigating organizations whose dockets, funding bases, and public relevance depend on the reading's vitality. They defend remedial programs, attack subordinating legislation, and supply the theoretical apparatus courts cite. When the reading loses ground, their litigation model loses its centerpiece and resources shift toward statutory and political strategies.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, civil_rights_advocacy_organizations, beneficiary,
    organized, generational, mobile, national).

% Individuals denied admission or contracts under race-conscious programs. They file suit claiming injury from their own treatment; under this reading their claims are heard and dismissed as complaints about mere classification rather than subordination. The framework has no slot for their objection — it treats the premise they invoke (that their individual treatment matters irrespective of group history) as a mistake about what the clause is for. They cannot exit the classification applied to them and cannot appeal to a framework that does not recognize their injury.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, individual_burdened_applicants, excluded,
    powerless, immediate, trapped, national).

% Academic commentators who map the clause's rival readings, reconstruct the Reconstruction-era legislative record, and compare jurisdictions — India's caste-conscious reservations, South Africa's remedial provisions — where anti-subordination designs formally govern. They hold no enforcement power; their influence runs through citation networks, clerkship pipelines, and the slow turnover of judicial appointments.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, constitutional_law_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_kernel__antisubordination_reading, historically_subordinated_racial_groups).
narrative_ontology:fixing_cost_class(equal_protection_kernel__antisubordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, administrable standard for adjudicating the constitutionality of racial state action: courts ask whether a measure entrenches caste-like hierarchy or dismantles it, replacing open-ended balancing of racial classifications with a purposive test anchored to group subordination and a fixed rule on who may invoke the clause.
% TRANSFER_FUNCTION: Moves constitutional entitlement toward historically subordinated groups (shield against hierarchy-entrenching laws, authorization for dismantling measures); moves the cost of race-conscious burdens and the loss of equal-protection recourse onto majority-group members; moves adjudicative discretion over the entrench/dismantle line and the qualifying-group list to the courts.
% ABSENT_VOICES: Individual applicants bearing race-conscious burdens who object on the basis of personal treatment rather than group history — they file suit and are dismissed, their premise ruled out at the threshold. Colorblind constitutional theorists are likewise present in the wider conversation but their categorical premise is treated inside this framework as foreclosed rather than engaged. Both speak from dissenting opinions, failed petitions, and academic critique outside the doctrinal mainstream.
% DISAPPEARANCE_RATIONALE: If the antisubordination standard ceased to govern overnight, every race-conscious admissions, contracting, and districting program would face successful equal-protection challenge under a cognizability rule that includes majority plaintiffs; remedial infrastructure built over five decades would unwind through litigation; subordinated groups would lose both the shield against subordinating state action and the sword of authorized remediation; and advocacy organizations would lose their central litigation model. The constitutional politics of race would reorganize around whichever sibling reading replaced it.
% FOUNDING_PROBLEM: The Fourteenth Amendment was ratified to abolish the caste system created by slavery — to give the freed population real citizenship against state-entrenched hierarchy. The antisubordination reading claims direct fidelity to that founding problem: a clause designed to destroy caste, not to sort permissible from impermissible sorting.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: the Reconstruction Congress's own legislative history (framers' statements by Bingham, Stevens, and Howard on the clause's anti-caste purpose), historians of the Amendment across ideological lines, Thirteenth Amendment 'badge of servitude' case law, persistent measured disparities in wealth, schooling, and criminal justice compiled by government statistical agencies, and the parallel design of India's and South Africa's constitutions, which treat caste-like hierarchy as an ongoing rather than solved problem. No source outside the benefiting parties attests that the founding problem is dead.
narrative_ontology:disappearance_verdict(equal_protection_kernel__antisubordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_kernel__antisubordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_kernel__antisubordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equal_protection_kernel__antisubordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_kernel__antisubordination_reading, 0.28, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_kernel__antisubordination_reading_tests).
:- end_tests(equal_protection_kernel__antisubordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are authored independently. The tangled_rope claim rests on structure the reading itself announces: a real coordination function (an administrable standard replacing case-by-case balancing of racial classifications), an explicit asymmetric incidence (subordinated castes gain shield and sword; majority members lose recourse — this is the reading's declared structural delta, not a hidden defect), and active enforcement (courts must continuously police the entrench/dismantle line and hold the colorblind argument foreclosed within the framework). The metrics are reading-indexed: from the antisubordination seat, epsilon is low-moderate (0.28) because what flows to subordinated groups is judged owed rather than rented, and what is denied to majority members is judged never legitimately theirs — the residual is inherent adjudication cost, overbreadth/patronage drift risk, and the real non-consensual burdens the reading nonetheless imposes. Suppression (0.35) is structural, not internalized: binding precedent and non-cognizability rulings, with political and scholarly channels left open. Resistance is high (0.68) because the reading faces a sustained, well-funded counter-movement and has been losing in court for three decades. Accessibility_collapse is moderate (0.45): the colorblind premise collapses logically inside this framework, but survives fully outside it. The temporal series run on one shared seven-point grid (1978–2026) with all three metrics authored at every point. The suppression_requirement series traces enforcement decay — from the Fullilove-era binding force of antisubordination reasoning through Croson, Adarand, and Grutter narrowing to post-SFFA marginality — a falling trajectory modeling a reading whose enforcement infrastructure has eroded, not one whose suppression has hardened. Theater_ratio rises in step: as governing force drains away, anti-caste invocations migrate from holdings to dicta and commemoration.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute radically different types from identical structural data. From the majority-group seat, the arrangement operates as a bar on claims they regard as meritorious — extraction amplified by constrained exit (one cannot litigate one's way out of a non-cognizability ruling). From the subordinated-group seat, the same doctrine operates as subsidy and protection — directionality near the beneficiary pole. From the judiciary's seat, it is an administrability burden: a line ('entrench' vs. 'dismantle') that must be drawn case by case without personal gain or loss. The excluded applicant seat is commentary-grade only: their objection is real but the framework assigns it no cognizable slot, which is precisely the consensus-provenance question — unanimity inside the framework arises partly because dissenting premises were ruled out at the door.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: historically_subordinated_racial_groups (organized, constrained exit — the protection attaches to an identity they cannot shed, an identity-lock dynamic: the benefit is constituted through the group identity, and if the identity frame ever dissolved the beneficiary set would dissolve with it), civil_rights_advocacy_organizations, and public_universities-in-their-permitted-capacity sit near the subsidized pole. Victim declaration drives high directionality: majority_group_members (powerful globally, yet constrained here — political power does not purchase a cognizable claim) sit near the full-target pole, and their constrained exit amplifies effective extraction. The judiciary is the agenda-setter: it administers and enforces without collecting, and its directionality comes from the power-atom fallback rather than beneficiary/victim data. No directionality overrides were authored: the beneficiary/victim declarations plus exit options already produce the correct relationships, and the override mechanism keys on power atoms too coarsely to differentiate the dual-positioned university seat without distorting the other institutional agents.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two opposite misreadings. Calling this a snare would misread a principled, publicly embraced asymmetry as predation: the beneficiaries are the historically subordinated, not a rent-collecting elite, and the coordination function is genuine and central. Calling it a rope would erase the reading's own declared losers and its dependence on continuous judicial enforcement against resistance. On the genealogy interview, the founding problem — state-entrenched racial caste — is live and corroborated from outside the beneficiary set, so no zombie flag is expected; the arrangement has not outlived its mandate. The forward risk is piton-shaped rather than snare-shaped: if the repudiation trend completes, the reading could survive as ceremonial performance (commemorated anti-caste rhetoric, academic citation) while governing nothing — the rising theater_ratio series is the early indicator to watch. Relatedly, the scaffold temptation is resisted: dismantling-permission looks transitional, but the anti-caste prohibition at the reading's core is permanent, so no sunset clause is authored; the uncertainty is carried by the permanence omega instead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the equal_protection_kernel (the Fourteenth Amendment''s Equal Protection Clause as a stabilized commitment). What would the sibling readings change structurally, and where exactly is the disagreement located?',
    'Corpus-level comparison across the three sibling stories. Adoption of the colorblind reading flips the victim set (every racially classified individual becomes potentially aggrieved) and voids remedial programs outright; adoption of the remedial reading restores majority-group claims as cognizable subject to strict scrutiny while keeping programs possible. The disagreement is located in two structural elements: the clause''s target (caste subordination vs. classification per se) and the cognizability of majority-group claims.',
    'Every classification computed for this story indexes only the antisubordination seat. The indexical spread across the three sibling files — same text, different epsilon, different beneficiary/victim sets — is the measurement the kernel decomposition exists to take.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: this story instantiates the antisubordination_reading of the equal_protection_kernel; siblings are remedial_reading and colorblind_reading.').

omega_variable(
    subordination_boundary_problem,
    'Which groups count as historically oppressed castes, and what counts as caste-like subordination? Does the oppressor/oppressed binary hold for intermediate-position groups (e.g., Asian-American applicants caught between categories) and for multiracial individuals?',
    'Case-by-case doctrinal elaboration, with comparative borrowing from India''s scheduled-caste designation lists and South Africa''s designated-group remedial provisions.',
    'Boundary choices move entire classes between the protected and unprotected seats; a narrower beneficiary set raises measured asymmetry, a broader one dilutes it and invites the colorblind rejoinder that the category has dissolved.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subordination_boundary_problem, conceptual, 'Irreducible indeterminacy in the reading''s own beneficiary/victim boundary.').

omega_variable(
    dismantling_efficacy,
    'Do race-conscious dismantling measures actually dismantle hierarchy, or can they redistribute positions while entrenching group boundaries?',
    'Longitudinal outcome studies of desegregation, set-aside, and admissions programs; comparative data from long-running reservation regimes.',
    'If the measures entrench rather than dismantle, the reading''s permission structure loses its warrant and drifts toward colorblind outcomes; if effective, the low reading-indexed extraction estimate holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dismantling_efficacy, empirical, 'Empirical contingency beneath the reading''s instrumental permission axiom.').

omega_variable(
    permanence_of_dismantling_permission,
    'Is the permission for hierarchy-dismantling action transitional — withdrawable once hierarchy falls — or a permanent feature of the clause?',
    'Textual-historical analysis of the Reconstruction framers'' statements, plus doctrinal behavior in jurisdictions where measured disparities have narrowed.',
    'A transitional reading gives the arrangement an implicit sunset and scaffold-like character; a permanent reading anchors the tangled-rope classification indefinitely. The authored story declares no sunset clause, betting on permanence of the anti-caste prohibition even if remedial permissions lapse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(permanence_of_dismantling_permission, conceptual, 'Whether the reading carries an undeclared transitional mandate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_kernel__antisubordination_reading, 1978, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1978, equal_protection_kernel__antisubordination_reading, theater_ratio, 1978, 0.12).
narrative_ontology:measurement_basis(equa_tr_t1978, observed).
narrative_ontology:measurement(equa_tr_t1986, equal_protection_kernel__antisubordination_reading, theater_ratio, 1986, 0.13).
narrative_ontology:measurement_basis(equa_tr_t1986, observed).
narrative_ontology:measurement(equa_tr_t1994, equal_protection_kernel__antisubordination_reading, theater_ratio, 1994, 0.15).
narrative_ontology:measurement_basis(equa_tr_t1994, observed).
narrative_ontology:measurement(equa_tr_t2003, equal_protection_kernel__antisubordination_reading, theater_ratio, 2003, 0.16).
narrative_ontology:measurement_basis(equa_tr_t2003, observed).
narrative_ontology:measurement(equa_tr_t2013, equal_protection_kernel__antisubordination_reading, theater_ratio, 2013, 0.17).
narrative_ontology:measurement_basis(equa_tr_t2013, observed).
narrative_ontology:measurement(equa_tr_t2020, equal_protection_kernel__antisubordination_reading, theater_ratio, 2020, 0.19).
narrative_ontology:measurement_basis(equa_tr_t2020, observed).
narrative_ontology:measurement(equa_tr_t2026, equal_protection_kernel__antisubordination_reading, theater_ratio, 2026, 0.2).
narrative_ontology:measurement_basis(equa_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(equa_be_t1978, equal_protection_kernel__antisubordination_reading, base_extractiveness, 1978, 0.22).
narrative_ontology:measurement_basis(equa_be_t1978, observed).
narrative_ontology:measurement(equa_be_t1986, equal_protection_kernel__antisubordination_reading, base_extractiveness, 1986, 0.23).
narrative_ontology:measurement_basis(equa_be_t1986, observed).
narrative_ontology:measurement(equa_be_t1994, equal_protection_kernel__antisubordination_reading, base_extractiveness, 1994, 0.25).
narrative_ontology:measurement_basis(equa_be_t1994, observed).
narrative_ontology:measurement(equa_be_t2003, equal_protection_kernel__antisubordination_reading, base_extractiveness, 2003, 0.26).
narrative_ontology:measurement_basis(equa_be_t2003, observed).
narrative_ontology:measurement(equa_be_t2013, equal_protection_kernel__antisubordination_reading, base_extractiveness, 2013, 0.27).
narrative_ontology:measurement_basis(equa_be_t2013, observed).
narrative_ontology:measurement(equa_be_t2020, equal_protection_kernel__antisubordination_reading, base_extractiveness, 2020, 0.28).
narrative_ontology:measurement_basis(equa_be_t2020, observed).
narrative_ontology:measurement(equa_be_t2026, equal_protection_kernel__antisubordination_reading, base_extractiveness, 2026, 0.28).
narrative_ontology:measurement_basis(equa_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1978, equal_protection_kernel__antisubordination_reading, suppression_requirement, 1978, 0.5).
narrative_ontology:measurement_basis(equa_su_t1978, observed).
narrative_ontology:measurement(equa_su_t1986, equal_protection_kernel__antisubordination_reading, suppression_requirement, 1986, 0.52).
narrative_ontology:measurement_basis(equa_su_t1986, observed).
narrative_ontology:measurement(equa_su_t1994, equal_protection_kernel__antisubordination_reading, suppression_requirement, 1994, 0.44).
narrative_ontology:measurement_basis(equa_su_t1994, observed).
narrative_ontology:measurement(equa_su_t2003, equal_protection_kernel__antisubordination_reading, suppression_requirement, 2003, 0.4).
narrative_ontology:measurement_basis(equa_su_t2003, observed).
narrative_ontology:measurement(equa_su_t2013, equal_protection_kernel__antisubordination_reading, suppression_requirement, 2013, 0.33).
narrative_ontology:measurement_basis(equa_su_t2013, observed).
narrative_ontology:measurement(equa_su_t2020, equal_protection_kernel__antisubordination_reading, suppression_requirement, 2020, 0.26).
narrative_ontology:measurement_basis(equa_su_t2020, observed).
narrative_ontology:measurement(equa_su_t2026, equal_protection_kernel__antisubordination_reading, suppression_requirement, 2026, 0.2).
narrative_ontology:measurement_basis(equa_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_kernel__antisubordination_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_protection_kernel__antisubordination_reading, equal_protection_kernel__remedial_reading).
narrative_ontology:affects_constraint(equal_protection_kernel__antisubordination_reading, equal_protection_kernel__colorblind_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Equal Protection Clause' decomposes into three structurally distinct constraints per the epsilon-invariance principle — antisubordination_reading (this file: target is caste subordination; majority claims non-cognizable; epsilon low from its own seat), remedial_reading (narrowly tailored remediation permissible; majority claims cognizable under strict scrutiny; intermediate epsilon), and colorblind_reading (all racial classification forbidden; every classified individual a potential claimant; epsilon indexed to its own formal-equality seat). Each story links the other two. Downstream pressure runs in both directions: the colorblind reading currently dominates doctrine and starves the other two of enforcement, while the antisubordination reading supplies the historical warrant that remedial readings borrow when justifying programs.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
