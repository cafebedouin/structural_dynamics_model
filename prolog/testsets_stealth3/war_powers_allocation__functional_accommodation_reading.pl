% ============================================================================
% CONSTRAINT STORY: war_powers_allocation__functional_accommodation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_powers_allocation__functional_accommodation_reading, []).

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
 *   constraint_id: war_powers_allocation__functional_accommodation_reading
 *   human_readable: Functional Accommodation Reading of War Powers Allocation
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   Under this reading, the lawfulness of unilateral military action is a
 *   function of operational context: a threat's imminence licenses the
 *   president to act without prior congressional authorization, while a
 *   campaign's prolongation shifts the allocation toward congressional
 *   consent. The rule's operative facts — imminence, duration, scope — are
 *   classified in the first instance by the executive, the very party whose
 *   authority expands with an imminent classification, and the enforcement
 *   machinery attached to the prolonged branch (termination clocks, funding
 *   conditions, litigation) has repeatedly failed against that classification
 *   control. This story is authored as one reading of the
 *   war_powers_allocation kernel; the categorical sibling readings are
 *   separate constraint stories with their own beneficiary structures and
 *   their own extraction profiles. The claim/metrics gap is deliberate: the
 *   reading presents itself as balanced accommodation between the branches,
 *   while the authored metrics describe a systematically executive-favoring
 *   operation — the engine measures that divergence; the claim is not
 *   reconciled to the metrics.
 *
 * KEY AGENTS:
 *   - presidential_office: agenda-setter seat (institutional/arbitrage) — classifies the operational context, collects the arrangement's gains, sets the precedents that widen its own room
 *   - congress_as_institution: primary payer seat (institutional/constrained) — bears the dilution of its declare-war assignment; holds a secondary beneficiary position whenever a campaign is conceded to be prolonged
 *   - citizen_electorate: payer seat (moderate/trapped) — bears war costs without a deliberative gate between elections
 *   - military_command_establishment: beneficiary seat (organized/constrained) — collects operational flexibility under the imminent-action branch
 *   - olc_interpretive_gatekeepers: beneficiary seat (institutional/identity_locked) — collects interpretive authority from certifying which branch's power covers each operation
 *   - federal_courts: observer seat (institutional/analytical) — maintains the framework's legitimacy while declining to police the gray zone
 *   - populations_subject_to_gray_zone_force: excluded seat (powerless/trapped) — bear the constraint's direct costs with no seat in the allocation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_powers_allocation__functional_accommodation_reading, 0.7).
domain_priors:suppression_score(war_powers_allocation__functional_accommodation_reading, 0.58).
domain_priors:theater_ratio(war_powers_allocation__functional_accommodation_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_powers_allocation__functional_accommodation_reading, tangled_rope).
narrative_ontology:human_readable(war_powers_allocation__functional_accommodation_reading, "Functional Accommodation Reading of War Powers Allocation").
narrative_ontology:topic_domain(war_powers_allocation__functional_accommodation_reading, "constitutional/political").

domain_priors:requires_active_enforcement(war_powers_allocation__functional_accommodation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_powers_allocation__functional_accommodation_reading, 'ec7685c9-7f20-4428-b5d2-28899ad051a4').
narrative_ontology:cs_kernel_codification('ec7685c9-7f20-4428-b5d2-28899ad051a4', fixed_text).
narrative_ontology:cs_authority_grounding('ec7685c9-7f20-4428-b5d2-28899ad051a4', practice).
narrative_ontology:cs_interpretation_layer_present('ec7685c9-7f20-4428-b5d2-28899ad051a4').
narrative_ontology:cs_reading_relation('ec7685c9-7f20-4428-b5d2-28899ad051a4', war_powers_allocation__congressional_primacy_reading, influences).
narrative_ontology:cs_reading_relation('ec7685c9-7f20-4428-b5d2-28899ad051a4', war_powers_allocation__inherent_executive_reading, influences).
narrative_ontology:cs_axiom('ec7685c9-7f20-4428-b5d2-28899ad051a4', foundational, context_sensitive_allocation_governs).
narrative_ontology:cs_axiom_status(context_sensitive_allocation_governs, holdable).
narrative_ontology:cs_axiom_grounding('ec7685c9-7f20-4428-b5d2-28899ad051a4', context_sensitive_allocation_governs, conventional).
narrative_ontology:cs_axiom('ec7685c9-7f20-4428-b5d2-28899ad051a4', secondary, imminence_permits_unilateral_force).
narrative_ontology:cs_axiom_status(imminence_permits_unilateral_force, holdable).
narrative_ontology:cs_axiom_grounding('ec7685c9-7f20-4428-b5d2-28899ad051a4', imminence_permits_unilateral_force, instrumental).
narrative_ontology:cs_reference_frame('ec7685c9-7f20-4428-b5d2-28899ad051a4', tripartite_contextual_allocation_framework).
narrative_ontology:cs_drift_state('ec7685c9-7f20-4428-b5d2-28899ad051a4', post_2001_aumf_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ec7685c9-7f20-4428-b5d2-28899ad051a4', '').
narrative_ontology:cs_kernel_id(war_powers_allocation__functional_accommodation_reading, war_powers_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_powers_allocation__functional_accommodation_reading, presidential_office).
narrative_ontology:constraint_beneficiary(war_powers_allocation__functional_accommodation_reading, military_command_establishment).
narrative_ontology:constraint_victim(war_powers_allocation__functional_accommodation_reading, congress_as_institution).
narrative_ontology:constraint_victim(war_powers_allocation__functional_accommodation_reading, citizen_electorate).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(war_powers_allocation__functional_accommodation_reading, congress_as_institution).
narrative_ontology:constraint_beneficiary(war_powers_allocation__functional_accommodation_reading, olc_interpretive_gatekeepers).
narrative_ontology:constraint_vindicates(war_powers_allocation__functional_accommodation_reading, youngstown_tripartite_framework).
narrative_ontology:constraint_vindicates(war_powers_allocation__functional_accommodation_reading, political_question_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Initiates and sustains military operations and, in practice, decides which operational label applies: it declares a threat imminent, defines an operation's scope, and determines when a campaign has become prolonged enough to need Congress. It files war-powers reports, obtains authorizations when convenient, and stretches old authorizations when not. It cannot exit the arrangement — it is the arrangement's principal — but it exploits every ambiguity the context rule leaves open, and each precedent it sets widens its own room.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, presidential_office, agenda_setter,
    institutional, generational, arbitrage, global).

% Holds the constitutional assignment to declare war and the power of the purse. Under the prolonged branch it can demand authorization and condition funding; in practice it receives operations already underway, classified by the executive as imminent or as covered by decades-old authorizations, and its enforcement tools — termination clocks, funding conditions, litigation — have repeatedly failed. It also collects real leverage whenever a campaign is conceded to be prolonged: the 2001 and 2002 authorizations were genuine congressional products.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, congress_as_institution, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(war_powers_allocation__functional_accommodation_reading, congress_as_institution, beneficiary).

% Bears the costs of wars — casualties, spending, long-term obligations — decided without a deliberative gate between elections. Its leverage is the vote and public opinion, both blunt and slow against operations measured in weeks; it cannot exit the polity, and its attention cycles rarely align with operational timelines.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, citizen_electorate, payer,
    moderate, biographical, trapped, national).

% Plans and executes operations under the classification the White House assigns. It gains operational flexibility and freedom from congressional micromanagement under the imminent-action branch, and bears the risk that a contested classification triggers a funding or authorization fight mid-campaign.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, military_command_establishment, beneficiary,
    organized, biographical, constrained, global).

% Career lawyers in the Office of Legal Counsel write the opinions that certify which branch's authority covers a given operation. The office collects institutional authority from its gatekeeping position — its memos bind the executive branch — and its alumni network runs through the solicitor general's office and the federal bench. Leaving would mean forgoing the career path the office anchors.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, olc_interpretive_gatekeepers, beneficiary,
    institutional, biographical, identity_locked, national).

% Adjudicate the framework's edges, applying the tripartite analysis to specific exercises of power, while declining — through justiciability doctrines — to police the gray zone itself. They maintain the framework's legitimacy and absorb drift into doctrine without resolving the underlying allocation.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, federal_courts, observer,
    institutional, generational, analytical, national).

% Live under operations conducted in the gray zone — strike campaigns, proxy engagements, interventions short of declared war. They bear the direct costs of the arrangement and hold no seat in it: they cannot vote in the deciding branches, appear in the authorization process, or contest the classification that subjected them.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, populations_subject_to_gray_zone_force, excluded,
    powerless, biographical, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_powers_allocation__functional_accommodation_reading, presidential_office).
narrative_ontology:fixing_cost_class(war_powers_allocation__functional_accommodation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the constitutional text's underdetermination about who may commit the nation to force: it provides a workable allocation that lets the system act at operational speed against imminent threats while preserving a congressional authorization channel for sustained campaigns, without requiring a categorical settlement the text cannot yield and the courts will not impose.
% TRANSFER_FUNCTION: Moves war-initiation authority — and with it the costs, risks, and blame of force — from Congress and the deliberative public to the sitting president in any context the executive can classify as imminent or as covered by an old authorization; retroactively, it moves political blame for unpopular wars between the branches depending on how the classification lands.
% ABSENT_VOICES: Populations subjected to gray-zone force hold no seat: they cannot vote in the deciding branches, appear in the authorization process, or contest the classification that targeted them. Members of Congress who would enforce the termination machinery lack a procedural vehicle once the courts abstain, and the governments that host deployments are outside the allocation entirely.
% DISAPPEARANCE_RATIONALE: If the context-dependent allocation vanished overnight, every force decision would reorganize around a categorical rule — one branch's or the other's — and the legality of every ongoing gray-zone operation (strike campaigns, advisory deployments, proxy engagements) would be immediately contestable. The executive would lose classification control, Congress would face immediate authorization votes on operations already underway, and the courts would face a justiciable allocation question they have spent seventy years routing around.
% FOUNDING_PROBLEM: The Constitution assigns the power to declare war to Congress and the command of military forces to the president without specifying how the two combine, and the courts declined to settle the combination. By the mid-twentieth century the gap between the speed modern threats demand and the deliberation the text's assignment implies had produced two centuries of unsettled practice; the functional accommodation was built to let the government act inside that gap — fast against imminent threats, through Congress for sustained wars — without waiting for the categorical constitutional settlement that never came.
% FOUNDING_PROBLEM_CORROBORATION: Justice Jackson's Youngstown concurrence — an analytical seat outside both branches' benefiting positions — attests the textual underdetermination and the practice-based resolution of it. The bipartisan National War Powers Commission (2008) and successive congressional war-powers hearings corroborate that the speed/legitimacy tension remains unresolved. The executive, which benefits from the arrangement, attests that the problem is solved; that self-attestation is discounted.
narrative_ontology:disappearance_verdict(war_powers_allocation__functional_accommodation_reading, world_rearranges).
narrative_ontology:founding_problem_status(war_powers_allocation__functional_accommodation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_powers_allocation__functional_accommodation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(war_powers_allocation__functional_accommodation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(war_powers_allocation__functional_accommodation_reading, 0.7, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_powers_allocation__functional_accommodation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_powers_allocation__functional_accommodation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_powers_allocation__functional_accommodation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.70 at interval end) because the context rule transfers authority through a self-judging classification: the executive labels the operational context, and the label determines whose consent is required. The series ratchets rather than cycles — each gray-zone episode (Korea, Vietnam, the post-2001 extension of a single authorization to new theaters, the 2011 hostilities determination) sets a precedent that widens the executive's room, with step increases at 21 (the resolution passed over veto after accumulated extraction), 49, and 60. Suppression (0.58) is structural rather than raw coercion: justiciability abstention forecloses judicial policing, collective-action and blame-avoidance costs paralyze congressional enforcement, and the arrangement suppresses both categorical rules by leaving no case clean enough to vindicate either. Its trajectory rises through 49 as the executive built compliance-management machinery (signing statements, office opinions, funding-only compliance) against the resolution's resistance, then eases slightly as congressional acquiescence normalized — enforcement decay, not settlement. Theater ratio (0.45) reflects the consultation-and-reporting apparatus, much of which is performed (reports filed 'consistent with,' the 60-day clock triggered and disregarded in 2011) without functional effect, while major-war authorizations remain real. Accessibility collapse (0.48): the categorical alternatives remain fully visible — scholarship, proposed legislation, recurring war-powers resolutions that passed both chambers in 2019-2020 — but are practically inert, so alternatives collapse halfway: visible, unreachable. Resistance (0.55) is real and recurring but lacks traction. Coalition potential among the payer seats exists — bipartisan coalitions have repeatedly formed and passed enforcement legislation — but the coalition dies at the veto and at justiciability, which is why high recurring resistance coexists with zero structural change. The identity-lock seat is the legal gatekeepers' office: professional career-path dependence plus institutional fusion of the office with its gatekeeping role; if that frame broke and the office opined against classification control, the executive would lose its internal legality apparatus and the gray zone would narrow sharply. All series share one time grid (0, 21, 28, 49, 60, 74; T0 = 1952 Youngstown, T-end = present).
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the presidential office the arrangement is a workable accommodation it administers — a rule that lets it act at operational speed while preserving a legitimacy channel it can invoke when convenient. From Congress the same structure is a systematic dilution of its textual assignment: it receives wars already underway, classified beyond its reach, and its consent is sought retroactively or not at all. The courts occupy a maintenance seat — they keep the framework's legitimacy while refusing, through justiciability doctrines, to police its center. The electorate experiences the arrangement as a deliberative gate that opens only after the decision has been made. The engine derives these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The presidential office sits at the beneficiary end: it collects the arrangement's gains (initiation authority, classification control, precedent) and is the receipt seat for those gains. The legal gatekeepers and the military command are subsidiary beneficiaries — one collects interpretive authority from its gatekeeping position, the other collects operational flexibility. Congress is the primary target but not at the full-target end: the prolonged branch genuinely subsidizes its authority whenever a campaign is conceded to be prolonged (the 2001 and 2002 authorizations were real exercises of its power), moderating its position below a pure target's. The electorate is a target with trapped exit — it bears costs with no procedural remedy between elections. Gray-zone populations are excluded rather than targeted through any seat: the constraint's costs land on them without registering in the beneficiary/victim derivation at all, which is itself a structural fact the absent-voices answer records. Suppression is a raw structural property, unscaled by power or scope — the 0.58 reflects the doctrinal and institutional barriers themselves, not an amplified figure; only extraction is scaled by directionality and scope in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — the text's underdetermination combined with the speed/legitimacy tension — is live, so this is not a mandate outliving its function; the arrangement still does real work, and founding_problem_status = live with disappearance_verdict = world_rearranges raises no zombie flag. What has atrophied is the enforcement layer attached to it: the resolution's consultation and termination machinery has decayed into substantial performance (theater ratio rising from 0.20 to 0.45 across the interval), while the allocation itself keeps operating. The tangled-rope classification prevents mislabeling in both directions: calling the whole arrangement pure extraction erases the genuine coordination function (a non-categorical rule is arguably the only kind the text can support, and both branches retain real claims), while calling it pure coordination erases the self-judging classification mechanism that converts the rule's formal symmetry into systematic executive advantage. The atrophy is in the enforcement apparatus, not the mandate — a decayed enforcement layer on a live mandate, not a dead mandate kept alive theatrically.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is the functional_accommodation_reading of the war_powers_allocation kernel; how would the beneficiary/victim structure and extraction change under the categorical sibling readings, and where exactly is the disagreement located?',
    'Author and compare the sibling stories (war_powers_allocation__congressional_primacy_reading, war_powers_allocation__inherent_executive_reading): the disagreement is located in whether the allocation rule is categorical or context-dependent — a conceptual difference resolvable only by framing choice, not by data.',
    'Under congressional primacy the presidential office loses beneficiary status and the gray zone, with its self-judging extraction mechanism, dissolves; under inherent executive the congressional payer position deepens and extraction rises further. This reading''s characteristic extraction depends entirely on the zone existing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: one reading of the war-powers kernel; either categorical sibling would restructure beneficiaries and dissolve the ambiguity zone.').

omega_variable(
    imminence_classification_self_judging,
    'Is the gray zone''s classification genuinely indeterminate, or determinate but strategically misclassified by the executive that controls it?',
    'Compare classification decisions (imminence determinations, hostilities determinations, authorization-scope readings) against ex post factual records — intelligence timelines, operational durations, target geography.',
    'If classifications are strategic, the ambiguity zone is a deliberate extraction mechanism and extraction should be revised upward; if genuinely indeterminate, part of the measured extraction is the irreducible cost of any non-categorical rule and the coordination reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imminence_classification_self_judging, empirical, 'Whether the executive''s context classifications track facts or strategy.').

omega_variable(
    congressional_acquiescence_mechanism,
    'Is Congress''s failure to enforce the prolonged-campaign branch structural (collective action, blame-avoidance, procedural barriers) or internalized (institutional identity fused with deference to executive expertise on national security)?',
    'Post-acquiescence trajectory: if enforcement revives when political conditions flip (unified opposition Congress, low-salience conflicts), the suppression was structural; if enforcement attempts fail even under favorable conditions, internalization has set in.',
    'If internalized, effective suppression exceeds the structural measure — Congress carries the deference across institutional configurations, and rule-level remedies alone will fail; the internalized share (roughly a third of the measured suppression) would persist after any procedural fix.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(congressional_acquiescence_mechanism, empirical, 'Structural versus internalized source of congressional non-enforcement.').

omega_variable(
    gray_zone_scope_expansion,
    'Does the ambiguity zone expand endogenously — each gray-zone episode setting precedent that widens the zone — or does its breadth track exogenous technological change (drones, cyber, proxy forces creating genuinely novel contexts)?',
    'Compare zone breadth across threat-technology eras while holding institutional variables (branch control, court posture) constant.',
    'If endogenous, the constraint''s extraction ratchets with each episode and the arrangement trends toward pure extraction; if exogenous, the zone is a stable accommodation cost and the coordination function is more robust than the current metrics suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gray_zone_scope_expansion, empirical, 'Endogenous ratchet versus exogenous technology as the driver of the gray zone''s growth.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_powers_allocation__functional_accommodation_reading, 0, 74).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t0, war_powers_allocation__functional_accommodation_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(war__tr_t0, observed).
narrative_ontology:measurement(war__tr_t21, war_powers_allocation__functional_accommodation_reading, theater_ratio, 21, 0.25).
narrative_ontology:measurement_basis(war__tr_t21, observed).
narrative_ontology:measurement(war__tr_t28, war_powers_allocation__functional_accommodation_reading, theater_ratio, 28, 0.28).
narrative_ontology:measurement_basis(war__tr_t28, observed).
narrative_ontology:measurement(war__tr_t49, war_powers_allocation__functional_accommodation_reading, theater_ratio, 49, 0.35).
narrative_ontology:measurement_basis(war__tr_t49, observed).
narrative_ontology:measurement(war__tr_t60, war_powers_allocation__functional_accommodation_reading, theater_ratio, 60, 0.42).
narrative_ontology:measurement_basis(war__tr_t60, observed).
narrative_ontology:measurement(war__tr_t74, war_powers_allocation__functional_accommodation_reading, theater_ratio, 74, 0.45).
narrative_ontology:measurement_basis(war__tr_t74, observed).

% Extraction over time
narrative_ontology:measurement(war__be_t0, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(war__be_t0, observed).
narrative_ontology:measurement(war__be_t21, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 21, 0.55).
narrative_ontology:measurement_basis(war__be_t21, observed).
narrative_ontology:measurement(war__be_t28, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 28, 0.58).
narrative_ontology:measurement_basis(war__be_t28, observed).
narrative_ontology:measurement(war__be_t49, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 49, 0.64).
narrative_ontology:measurement_basis(war__be_t49, observed).
narrative_ontology:measurement(war__be_t60, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 60, 0.67).
narrative_ontology:measurement_basis(war__be_t60, observed).
narrative_ontology:measurement(war__be_t74, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 74, 0.7).
narrative_ontology:measurement_basis(war__be_t74, observed).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t0, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(war__su_t0, observed).
narrative_ontology:measurement(war__su_t21, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 21, 0.52).
narrative_ontology:measurement_basis(war__su_t21, observed).
narrative_ontology:measurement(war__su_t28, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 28, 0.55).
narrative_ontology:measurement_basis(war__su_t28, observed).
narrative_ontology:measurement(war__su_t49, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 49, 0.62).
narrative_ontology:measurement_basis(war__su_t49, observed).
narrative_ontology:measurement(war__su_t60, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 60, 0.6).
narrative_ontology:measurement_basis(war__su_t60, observed).
narrative_ontology:measurement(war__su_t74, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 74, 0.58).
narrative_ontology:measurement_basis(war__su_t74, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_powers_allocation__functional_accommodation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(war_powers_allocation__functional_accommodation_reading, war_powers_allocation__congressional_primacy_reading).
narrative_ontology:affects_constraint(war_powers_allocation__functional_accommodation_reading, war_powers_allocation__inherent_executive_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'war powers allocation' covers three structurally distinct claims with different extraction profiles: the functional-accommodation reading (this story — context-dependent allocation with an executive-classified gray zone), the congressional-primacy reading (categorical authorization necessity; no unilateral zone, no classification-control extraction), and the inherent-executive reading (categorical presidential permission; congressional payer status at maximum). They are separate constraint stories linked by this network. This reading's extraction value is stable because its referent — the context-dependent allocation as it actually operates — does not vary with the observable used to measure it; the siblings' referents are different arrangements, not different measurements of this one.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
