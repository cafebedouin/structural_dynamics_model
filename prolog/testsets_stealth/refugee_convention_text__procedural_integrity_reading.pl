% ============================================================================
% CONSTRAINT STORY: refugee_convention_text__procedural_integrity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-25
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_refugee_convention_text__procedural_integrity_reading, []).

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
 *   constraint_id: refugee_convention_text__procedural_integrity_reading
 *   human_readable: Refugee Convention — Procedural Integrity Reading (Fair Individualized Assessment Floor)
 *   domain: international_law/migration_governance/human_rights
 *
 * SUMMARY:
 *   This story instantiates ONE reading — the procedural_integrity_reading —
 *   of the refugee_convention_text kernel (the 1951 Convention / 1967
 *   Protocol as a persisting commitment). The kernel decomposes into three
 *   structurally distinct constraints per the epsilon-invariance principle:
 *   the expansive_humanitarian_reading (Convention as unbendable humanitarian
 *   mandate; victim set includes those fleeing generalized violence), this
 *   procedural_integrity_reading (Convention as a fair-process floor; victim
 *   set is defined by procedural access — those denied a meaningful hearing),
 *   and the restrictive_sovereignty_reading (Convention as a minimum floor
 *   under maximum sovereign discretion; victim set shrinks toward those
 *   already inside jurisdiction). The epsilon referent here is the standing
 *   arrangement under contest — the international refugee protection regime
 *   as it actually operates — assessed by this reading's own lights: the
 *   reading holds the protection threshold flexible (so definitional
 *   narrowing is not itself the failure) but holds process integrity
 *   non-negotiable (so access denial IS the failure). Epsilon is therefore
 *   authored for the arrangement as the procedural reader sees it: a real
 *   procedural floor in functioning systems, eroding at the edges through
 *   interdiction, offshore siting, and accelerated tracks. The interval 0-30
 *   maps to approximately 1990-2020: post-Cold War asylum systems (t0),
 *   safe-third-country and carrier-sanction spread (t5), the Pacific Solution
 *   and mature maritime interdiction (t10), offshore expansion and fast-track
 *   regimes (t15), detention and procedures-directive consolidation (t20),
 *   the EU-Turkey statement, hotspots, and documented Mediterranean pushbacks
 *   (t25), and systematic pushback records with renewed offshore arrangements
 *   (t30). The sibling readings are separate constraint stories, linked
 *   through network.affects_constraints.
 *
 * KEY AGENTS:
 *   - contracting_states: Agenda-setter (institutional/constrained) — administers and funds the procedure, captures the flexibility and cost-avoidance that the arrangement's erosion yields
 *   - unhcr_supervisory_body: Agenda-setter and beneficiary (institutional/identity_locked) — supervises compliance; its institutional existence is fused with the determination system it polices
 *   - asylum_claimants_with_access: Beneficiary (powerless/trapped) — receives the individualized hearing the floor promises; pays in delay, evidentiary burden, and detention risk
 *   - procedural_access_denied_claimants: Primary target (powerless/trapped) — channeled into accelerated or offshore tracks without meaningful process; bears the gap between promise and delivery
 *   - intercepted_pushback_migrants: Target excluded from the conversation (powerless/trapped) — never reach any procedure; their cost is the arrangement's ultimate output
 *   - national_asylum_courts: Agenda-setter (institutional/constrained) — enforce the fairness standard, strike down defective tracks, define offshore guarantee content
 *   - refugee_law_practitioners: Beneficiary (moderate/mobile) — profession constituted by individualized claims
 *   - offshore_detention_contractors: Beneficiary (organized/arbitrage) — paid per person-day to run sites outside territorial procedure
 *   - comparative_refugee_law_academics: Observer (analytical/analytical) — documents the promise-delivery gap across jurisdictions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(refugee_convention_text__procedural_integrity_reading, 0.62).
domain_priors:suppression_score(refugee_convention_text__procedural_integrity_reading, 0.65).
domain_priors:theater_ratio(refugee_convention_text__procedural_integrity_reading, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(refugee_convention_text__procedural_integrity_reading, tangled_rope).
narrative_ontology:human_readable(refugee_convention_text__procedural_integrity_reading, "Refugee Convention — Procedural Integrity Reading (Fair Individualized Assessment Floor)").
narrative_ontology:topic_domain(refugee_convention_text__procedural_integrity_reading, "international_law/migration_governance/human_rights").

domain_priors:requires_active_enforcement(refugee_convention_text__procedural_integrity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(refugee_convention_text__procedural_integrity_reading, '6fbf8cbc-ae76-4b5d-ac3e-076d3f6b4150').
narrative_ontology:cs_kernel_codification('6fbf8cbc-ae76-4b5d-ac3e-076d3f6b4150', fixed_text).
narrative_ontology:cs_authority_grounding('6fbf8cbc-ae76-4b5d-ac3e-076d3f6b4150', lineage).
narrative_ontology:cs_interpretation_layer_present('6fbf8cbc-ae76-4b5d-ac3e-076d3f6b4150').
narrative_ontology:cs_reading_relation('6fbf8cbc-ae76-4b5d-ac3e-076d3f6b4150', refugee_convention_text__expansive_humanitarian_reading, coexists_with).
narrative_ontology:cs_reading_relation('6fbf8cbc-ae76-4b5d-ac3e-076d3f6b4150', refugee_convention_text__restrictive_sovereignty_reading, influences).
narrative_ontology:cs_axiom('6fbf8cbc-ae76-4b5d-ac3e-076d3f6b4150', foundational, process_integrity_non_negotiable).
narrative_ontology:cs_axiom_status(process_integrity_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('6fbf8cbc-ae76-4b5d-ac3e-076d3f6b4150', process_integrity_non_negotiable, deontological).
narrative_ontology:cs_axiom('6fbf8cbc-ae76-4b5d-ac3e-076d3f6b4150', foundational, definitional_flexibility_permissible).
narrative_ontology:cs_axiom_status(definitional_flexibility_permissible, holdable).
narrative_ontology:cs_axiom_grounding('6fbf8cbc-ae76-4b5d-ac3e-076d3f6b4150', definitional_flexibility_permissible, conventional).
narrative_ontology:cs_reference_frame('6fbf8cbc-ae76-4b5d-ac3e-076d3f6b4150', individualized_assessment_guarantee).
narrative_ontology:cs_drift_state('6fbf8cbc-ae76-4b5d-ac3e-076d3f6b4150', contemporary_non_entree_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6fbf8cbc-ae76-4b5d-ac3e-076d3f6b4150', '').
narrative_ontology:cs_kernel_id(refugee_convention_text__procedural_integrity_reading, refugee_convention_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(refugee_convention_text__procedural_integrity_reading, asylum_claimants_with_access).
narrative_ontology:constraint_beneficiary(refugee_convention_text__procedural_integrity_reading, unhcr_supervisory_body).
narrative_ontology:constraint_beneficiary(refugee_convention_text__procedural_integrity_reading, refugee_law_practitioners).
narrative_ontology:constraint_victim(refugee_convention_text__procedural_integrity_reading, procedural_access_denied_claimants).
narrative_ontology:constraint_victim(refugee_convention_text__procedural_integrity_reading, intercepted_pushback_migrants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(refugee_convention_text__procedural_integrity_reading, offshore_detention_contractors).
narrative_ontology:constraint_victim(refugee_convention_text__procedural_integrity_reading, asylum_claimants_with_access).
narrative_ontology:constraint_vindicates(refugee_convention_text__procedural_integrity_reading, non_refoulement_doctrine).
narrative_ontology:constraint_vindicates(refugee_convention_text__procedural_integrity_reading, individualized_status_determination_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Signatory governments that administer asylum systems: they design the procedure (interview formats, evidentiary rules, time limits, appeal routes), decide where assessment happens (territory, border, or offshore site), and fund or starve the machinery. They gain flexibility to narrow who qualifies, but remain bound to give anyone who reaches the process an individualized hearing before return. Denunciation of the Convention is legally possible but diplomatically costly, so exit from the obligation set is narrow.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, contracting_states, agenda_setter,
    institutional, generational, constrained, national).

% The UN refugee agency supervises state compliance, issues interpretive guidance on procedure, intervenes in individual cases and litigation, and runs its own status determination where states cannot or will not. Its mandate, budget, and institutional self-conception are built around the individualized-determination system it polices; it has no institutional existence apart from that role.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, unhcr_supervisory_body, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__procedural_integrity_reading, unhcr_supervisory_body, beneficiary).

% People who reach a status-determination process: they receive an individual interview, legal aid in some jurisdictions, written reasons, and a route of appeal. They pay for this in years of waiting, evidentiary burdens they often cannot meet, detention in accelerated tracks, and the standing risk that a scrupulously fair process still ends in return.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, asylum_claimants_with_access, beneficiary,
    powerless, immediate, trapped, national).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__procedural_integrity_reading, asylum_claimants_with_access, payer).

% People channeled into procedures in name only: accelerated tracks with days rather than months to prepare, offshore sites without counsel or independent review, manifestly-unfounded designations decided on paper. They bear the full cost of the gap between the procedural promise and the procedure delivered — rejection or return without a meaningful hearing. Exit is not available: they have already fled the country they cannot be returned to, and onward movement is barred by the same visa and carrier-sanction rules that channel them into the defective tracks.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, procedural_access_denied_claimants, payer,
    powerless, immediate, trapped, global).

% People interdicted at sea or pushed back at land borders before any registration occurs. They never enter the procedure whose fairness is contested; their accounts surface only through NGO reporting, journalism, and strategic litigation brought by survivors. They bear the arrangement's ultimate cost — return to the danger the process exists to assess — without a seat in any forum.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, intercepted_pushback_migrants, payer,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__procedural_integrity_reading, intercepted_pushback_migrants, excluded).

% Domestic courts and regional human-rights bodies that review whether the procedure a state actually ran met the fairness standard: adequate preparation time, legal representation, interpreter access, genuine individualized consideration, and non-refoulement screening before any return. Their rulings strike down fast-track schemes, order access to territory, and define what full procedural guarantees means at offshore sites. They cannot choose their docket and cannot decline the question once a case arrives.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, national_asylum_courts, agenda_setter,
    institutional, generational, constrained, national).

% Lawyers, legal-aid organizations, and adjudication professionals whose working lives are constituted by individualized claims: intake, country-of-origin evidence, hearings, appeals, strategic litigation. The individualized-assessment requirement is the demand side of their profession; they can move between jurisdictions, employers, and adjacent fields if any single system contracts.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, refugee_law_practitioners, beneficiary,
    moderate, biographical, mobile, global).

% Private security, detention-management, and processing-services firms paid per person-day to run offshore sites and reception centers. Their revenue scales with the number of people held outside standard territorial procedure; contracts are rebid across states and sites, so the collapse of any single arrangement threatens the firm's portfolio but not its existence.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, offshore_detention_contractors, beneficiary,
    organized, biographical, arbitrage, regional).

% Scholars and monitoring organizations that document how procedures operate across jurisdictions — grant-rate disparities, time-to-decision, access-to-territory data, pushback records — and test each interpretive reading of the Convention against that record. They hold no enforcement power and collect no revenue from the system.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, comparative_refugee_law_academics, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(refugee_convention_text__procedural_integrity_reading, contracting_states).
narrative_ontology:fixing_cost_class(refugee_convention_text__procedural_integrity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, contestable procedural floor for status determination: common standards for individual interviews, evidence, reasons, and appeal, so that a person claiming fear of persecution can have the claim heard once, by a process whose fairness other jurisdictions and courts can recognize — instead of every encounter between a state and an arriving person being resolved by unreviewable discretion.
% TRANSFER_FUNCTION: Moves administrative work, decision time, and legal attention from states (which would prefer categorical or summary exclusion) into individualized case processing, and moves security of legal status from state discretion to claimant entitlement where the process is honored. Where access is denied, it moves the entire risk of return — and its consequences — from the state onto the person interdicted or channeled into a defective track.
% ABSENT_VOICES: Interdicted and pushed-back people are the structurally absent voice: the procedure whose fairness this reading defends is precisely what they never reach. Their objection — that guarantees of fair hearing are worthless without a guaranteed path into the hearing — is voiced only by proxy, through NGO documentation, journalists, and strategic litigation brought by survivors. Destination-state publics are also absent: the fiscal and political costs of full procedural access are set by administrative practice and litigation rather than by their deliberate consent.
% DISAPPEARANCE_RATIONALE: If the procedural floor vanished overnight, status determination would reorganize around unreviewable state discretion: no individual interviews, no appeal routes, no reasons, no non-refoulement screening before return. UNHCR's supervisory role and the refugee-law profession would lose their object; offshore and interdiction practices would need no procedural fig leaf. The parts of the system that currently deliver protection through fair process would collapse into whatever each border official decides.
% FOUNDING_PROBLEM: The mass denationalizations and returns of the 1930s-40s: states turning people back to persecution without any hearing, and no mechanism by which a frightened person could force a state to look at their case before rejecting it. The Convention's founders built individualized determination together with non-refoulement so that rejection would require a decision about a person, not a category.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: regional human-rights jurisprudence on interdiction and pushbacks, border-monitoring consortia, and NGO documentation (Amnesty International, Human Rights Watch) all attest that returns without individual assessment continue at scale. Even states that read the Convention most narrowly formally accept the non-refoulement core — no party to the dispute claims the founding problem is solved.
narrative_ontology:disappearance_verdict(refugee_convention_text__procedural_integrity_reading, world_rearranges).
narrative_ontology:founding_problem_status(refugee_convention_text__procedural_integrity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(refugee_convention_text__procedural_integrity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(refugee_convention_text__procedural_integrity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(refugee_convention_text__procedural_integrity_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(refugee_convention_text__procedural_integrity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(refugee_convention_text__procedural_integrity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(refugee_convention_text__procedural_integrity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.62: substantial but not pure — the standing arrangement contains a real procedural floor that delivers protection in functioning systems, while access denial at the edges (interdiction, offshore without guarantees, fast-track) converts the floor's promise into cost-bearing for a defined victim set. Suppression 0.65 is authored as a raw, unscaled structural property: the exclusion machinery (maritime interdiction, offshore detention, carrier sanctions, visa requirements) is coercive, and the claimant seats are trapped with no alternative channel — the engine scales only extractiveness by directionality and scope, never suppression. Theater_ratio 0.36: individual interviews, reasons, and appeals are real work in most territorial systems, but a growing share of procedural activity is performance — offshore 'full guarantees' that consist of detention and paper review, hotspot 'registration' that feeds removal pipelines. Accessibility_collapse 0.40: the reading deliberately leaves alternatives open — group (prima facie) determination, humanitarian visas, and definitional narrowing all remain workable, which is exactly the flexibility this reading concedes to states; the constraint collapses only the no-hearing alternative. Resistance 0.65: states actively resist — interdiction fleets, offshore contracting, safe-third-country designations, treaty reservations — making this a construct that must be continuously defended through litigation and monitoring. The three measurement series run on ONE shared seven-point grid (t=0,5,10,15,20,25,30) with every metric authored at every point; suppression_requirement is authored because the story specifically tracks enforcement-capacity change — the exclusion machinery's build-up from ad hoc interdiction to contracted, legislated, litigated infrastructure. The drift is monotonic, not cyclical: no intermittent-reinforcement mechanism is claimed.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seats compute differently from the same structure. From contracting_states, the arrangement is a manageable obligation they fund and shape — definitional flexibility is theirs, and the procedural floor is a legitimacy asset. From procedural_access_denied_claimants and intercepted_pushback_migrants, the same arrangement is a hearing that never happens: the floor's value is entirely conditional on access they are denied. UNHCR computes as bound to the system it supervises (identity-fused beneficiary): its critique of state practice is also a defense of its own mandate. Offshore_detention_contractors profit precisely from the promise-delivery gap and hold arbitrage exit, so they experience no pressure toward reform. The powerless claimant seats cannot form an effective coalition: they are dispersed across jurisdictions, frequently detained, linguistically fragmented, and stripped of the standing that coalition power requires — their only enforcement channel is other people's litigation.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (asylum_claimants_with_access, unhcr_supervisory_body, refugee_law_practitioners) derive low directionality — the arrangement subsidizes them, amplified toward the beneficiary end by trapped exit for claimants and identity-lock for UNHCR. Declared victims (procedural_access_denied_claimants, intercepted_pushback_migrants) derive high directionality near the full-target end, amplified by trapped exit: they bear the arrangement's costs with no alternative channel, and the pushback seat sits at the extreme — it receives no benefit whatsoever, not even the process. contracting_states carry no beneficiary or victim declaration, deliberately: they are the constrained party that both pays the coordination cost (funding fair process) and captures the erosion gains (flexibility, cost-avoidance), sitting near symmetric with a slight beneficiary tilt on the standing arrangement. Their seat therefore computes from the canonical institutional fallback rather than structural derivation. No directionality_overrides are authored: an override is keyed by power atom, and an institutional-wide override would corrupt the UNHCR and court seats, which derive correctly from their structural declarations — the story accepts the fallback's coarseness for the state seat rather than degrading three accurate derivations to fix one.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — returns without any hearing — is live and corroborated by parties outside the beneficiary set, so no mandatrophy is declared and none is due. The classification guards against both mislabeling errors. Reading the arrangement as pure coordination (a clean procedural standard) would erase the victim set: people whose only contact with the 'fair process' is its denial. Reading it as pure extraction would erase the functioning core: territorial systems where individualized assessment genuinely delivers protection and appeal reversals are routine. The tangled_rope structure holds both: genuine coordination function (shared, contestable procedural floor) AND asymmetric extraction (a victim set defined by procedural access, with gains accruing to the states that administer the gap). The forward risk is mandatrophy-by-drift rather than mandatrophy-now: if the founding problem were ever genuinely solved (universal safe third countries, or the restrictive sibling's world in which no one outside jurisdiction has a claim), the procedural apparatus would persist as performance — hearings held for outcomes already decided — and theater_ratio would carry the drift signal that the measurements here already show beginning.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading (procedural_integrity_reading) of the refugee_convention_text kernel; which reading''s structure does the standing arrangement actually instantiate, and how would each sibling reading change the victim set and enforcement structure?',
    'Comparative mapping of each reading''s declared victim set against observable access and refoulement data; the sibling stories (expansive_humanitarian_reading, restrictive_sovereignty_reading) carry their own structures, and the divergence among the three classifications is itself the measurement.',
    'Under the restrictive sibling, the victim set collapses toward those already inside jurisdiction and offshore exclusion becomes lawful; under the expansive sibling, the victim set expands to those fleeing generalized violence regardless of process; under this reading, the victim set is defined by procedural access alone — whoever never reaches a meaningful hearing bears the cost.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which reading of the Convention kernel governs, and what each sibling would change structurally.').

omega_variable(
    process_protection_deliverability,
    'Does procedural integrity actually deliver protection, or can a scrupulously fair process coexist with near-universal rejection — in which case does this reading legitimate restrictive outcomes rather than constrain them?',
    'Longitudinal cohort analysis of claimants who received full procedural guarantees (legal aid, adequate time, effective appeal): grant rates, appeal reversal rates, and refoulement outcomes compared against summary-track cohorts.',
    'If fair process reliably yields return anyway, the reading''s protective value collapses and its enforcement functions as legitimation for restrictive outcomes — pushing this constraint toward the extractive end; if appeal reversals are substantial, process integrity is doing real protective work and the coordination reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(process_protection_deliverability, empirical, 'Whether the reading''s process-centered bet delivers substantive protection.').

omega_variable(
    offshore_guarantee_possibility,
    'Can offshore processing ever carry the ''full procedural guarantees'' this reading requires, or is offshore siting structurally incompatible with them (no counsel, no independent review, no effective appeal, no genuine refoulement screening)?',
    'Audit of offshore sites against territorial-procedure benchmarks: counsel access rates, interpreter quality, independent oversight presence, appeal timelines, and refoulement-screening outcomes.',
    'If structurally incompatible, the reading''s offshore permissiveness collapses into prohibition and the constraint tightens sharply; if compatible in some configurations, the permissive clause holds and offshore-with-guarantees remains lawful under this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(offshore_guarantee_possibility, empirical, 'Structural compatibility of offshore processing with full procedural guarantees.').

omega_variable(
    enforcement_capacity_race,
    'The rising suppression series: is it the exclusion machinery ratcheting (states building interdiction, offshore, and carrier-sanction capacity faster than courts strike it down), or the norm''s enforcement intensifying (courts and monitors closing the gap)?',
    'Track the ratio of successful procedural challenges to new avoidance mechanisms per interval, and the lag from mechanism deployment to first judicial strike-down.',
    'A ratchet outcome supports reading the extractiveness trend as entrenching access denial; a strike-down-dominant outcome supports a self-correcting coordination system whose current erosion is transient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_race, empirical, 'Whether state avoidance capacity is outpacing judicial enforcement of procedural norms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(refugee_convention_text__procedural_integrity_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refu_tr_t0, refugee_convention_text__procedural_integrity_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(refu_tr_t0, observed).
narrative_ontology:measurement(refu_tr_t5, refugee_convention_text__procedural_integrity_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement_basis(refu_tr_t5, observed).
narrative_ontology:measurement(refu_tr_t10, refugee_convention_text__procedural_integrity_reading, theater_ratio, 10, 0.23).
narrative_ontology:measurement_basis(refu_tr_t10, observed).
narrative_ontology:measurement(refu_tr_t15, refugee_convention_text__procedural_integrity_reading, theater_ratio, 15, 0.27).
narrative_ontology:measurement_basis(refu_tr_t15, observed).
narrative_ontology:measurement(refu_tr_t20, refugee_convention_text__procedural_integrity_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement_basis(refu_tr_t20, observed).
narrative_ontology:measurement(refu_tr_t25, refugee_convention_text__procedural_integrity_reading, theater_ratio, 25, 0.33).
narrative_ontology:measurement_basis(refu_tr_t25, observed).
narrative_ontology:measurement(refu_tr_t30, refugee_convention_text__procedural_integrity_reading, theater_ratio, 30, 0.36).
narrative_ontology:measurement_basis(refu_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(refu_be_t0, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(refu_be_t0, observed).
narrative_ontology:measurement(refu_be_t5, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 5, 0.46).
narrative_ontology:measurement_basis(refu_be_t5, observed).
narrative_ontology:measurement(refu_be_t10, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement_basis(refu_be_t10, observed).
narrative_ontology:measurement(refu_be_t15, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 15, 0.54).
narrative_ontology:measurement_basis(refu_be_t15, observed).
narrative_ontology:measurement(refu_be_t20, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 20, 0.57).
narrative_ontology:measurement_basis(refu_be_t20, observed).
narrative_ontology:measurement(refu_be_t25, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 25, 0.6).
narrative_ontology:measurement_basis(refu_be_t25, observed).
narrative_ontology:measurement(refu_be_t30, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement_basis(refu_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(refu_su_t0, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(refu_su_t0, observed).
narrative_ontology:measurement(refu_su_t5, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 5, 0.44).
narrative_ontology:measurement_basis(refu_su_t5, observed).
narrative_ontology:measurement(refu_su_t10, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 10, 0.49).
narrative_ontology:measurement_basis(refu_su_t10, observed).
narrative_ontology:measurement(refu_su_t15, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 15, 0.53).
narrative_ontology:measurement_basis(refu_su_t15, observed).
narrative_ontology:measurement(refu_su_t20, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 20, 0.57).
narrative_ontology:measurement_basis(refu_su_t20, observed).
narrative_ontology:measurement(refu_su_t25, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 25, 0.61).
narrative_ontology:measurement_basis(refu_su_t25, observed).
narrative_ontology:measurement(refu_su_t30, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 30, 0.65).
narrative_ontology:measurement_basis(refu_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(refugee_convention_text__procedural_integrity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(refugee_convention_text__procedural_integrity_reading, expansive_humanitarian_reading).
narrative_ontology:affects_constraint(refugee_convention_text__procedural_integrity_reading, restrictive_sovereignty_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the Refugee Convention' covers three structurally distinct constraints (per the epsilon-invariance principle) — three readings of one fixed text, each with its own epsilon, victim set, and classification. This file is the procedural_integrity_reading (epsilon 0.62: genuine procedural floor, extraction concentrated where access is denied). The expansive_humanitarian_reading authors epsilon for the same standing arrangement assessed against an unbendable humanitarian mandate (higher extraction — every denied generalized-violence claim is a violation); the restrictive_sovereignty_reading authors epsilon against a minimum-floor standard (lower extraction — only core refoulement counts). The readings are linked because each cites the same text and each constrains the others' legitimacy conditions: this reading is the scrutiny layer through which restrictive definitions must pass (influences), and it coexists with the expansive mandate as positions held by different factions of the same dispute. Decomposing the label — rather than averaging across readings — is what keeps each story's epsilon stable and its victim set precise.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
