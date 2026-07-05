% ============================================================================
% CONSTRAINT STORY: nsl_legal_text__democratic_enclosure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nsl_legal_text__democratic_enclosure_reading, []).

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
 *   constraint_id: nsl_legal_text__democratic_enclosure_reading
 *   human_readable: Hong Kong National Security Law — Democratic Enclosure Reading
 *   domain: constitutional_law/political_sociology/international_relations
 *
 * SUMMARY:
 *   This story instantiates one specific reading of the contested NSL kernel:
 *   the law enacted by Beijing for Hong Kong in June 2020 functions as a
 *   mechanism for permanent closure of democratic space, criminalizing
 *   dissent under expansively defined categories of secession, subversion,
 *   terrorism, and foreign collusion. Under this reading, civil society,
 *   independent press, and the pro-democracy opposition are the structural
 *   victims, and Beijing's central authorities together with the Hong Kong
 *   establishment camp and the national security apparatus are the structural
 *   beneficiaries. This is deliberately NOT a story about the narrow question
 *   of whether Hong Kong needed any national security statute (that narrower
 *   coordination claim is acknowledged in the six_questions
 *   coordination_function answer but is not what this story's ε measures) and
 *   NOT a story about jurisdictional/legal-system transplantation mechanics
 *   (see the sibling story jurisdictional_capture_reading) or about the
 *   sovereign-legitimacy claim that the law restored constitutional order
 *   after 2019 (see sovereignty_restoration_reading). Those are structurally
 *   distinct claims with their own ε and their own stakeholder sets; this
 *   file measures only the enclosure-of-democratic-space claim.
 *
 * KEY AGENTS:
 *   - beijing_central_authorities: agenda_setter (institutional/analytical) — drafts, enforces, retains override authority
 *   - hong_kong_establishment_camp: beneficiary (organized/arbitrage) — gains uncontested political dominance
 *   - national_security_apparatus_personnel: beneficiary (institutional/arbitrage) — gains expanded powers and career incentive
 *   - pro_democracy_opposition_politicians: payer (powerless/trapped) — mass prosecution, disqualification, exile
 *   - independent_press_and_journalists: payer (moderate/trapped) — forced closures, self-censorship
 *   - civil_society_organizations: payer (moderate/trapped) — mass dissolution under prosecutorial risk
 *   - general_hong_kong_electorate: payer (powerless/constrained) — electoral system redesigned to foreclose opposition victory
 *   - international_observers_and_foreign_governments: excluded (institutional/analytical) — no domestic standing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nsl_legal_text__democratic_enclosure_reading, 0.88).
domain_priors:suppression_score(nsl_legal_text__democratic_enclosure_reading, 0.91).
domain_priors:theater_ratio(nsl_legal_text__democratic_enclosure_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, accessibility_collapse, 0.87).
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nsl_legal_text__democratic_enclosure_reading, snare).
narrative_ontology:human_readable(nsl_legal_text__democratic_enclosure_reading, "Hong Kong National Security Law — Democratic Enclosure Reading").
narrative_ontology:topic_domain(nsl_legal_text__democratic_enclosure_reading, "constitutional_law/political_sociology/international_relations").

domain_priors:requires_active_enforcement(nsl_legal_text__democratic_enclosure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nsl_legal_text__democratic_enclosure_reading, '4cf46a73-1d6e-4c47-9f1e-c04a8bfaa8d9').
narrative_ontology:cs_kernel_codification('4cf46a73-1d6e-4c47-9f1e-c04a8bfaa8d9', formalized).
narrative_ontology:cs_authority_grounding('4cf46a73-1d6e-4c47-9f1e-c04a8bfaa8d9', extraction).
narrative_ontology:cs_interpretation_layer_present('4cf46a73-1d6e-4c47-9f1e-c04a8bfaa8d9').
narrative_ontology:cs_reading_relation('4cf46a73-1d6e-4c47-9f1e-c04a8bfaa8d9', nsl_legal_text__sovereignty_restoration_reading, coexists_with).
narrative_ontology:cs_reading_relation('4cf46a73-1d6e-4c47-9f1e-c04a8bfaa8d9', nsl_legal_text__jurisdictional_capture_reading, influences).
narrative_ontology:cs_axiom('4cf46a73-1d6e-4c47-9f1e-c04a8bfaa8d9', foundational, civic_political_space_is_constitutionally_protected_autonomy).
narrative_ontology:cs_axiom_status(civic_political_space_is_constitutionally_protected_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('4cf46a73-1d6e-4c47-9f1e-c04a8bfaa8d9', civic_political_space_is_constitutionally_protected_autonomy, deontological).
narrative_ontology:cs_axiom('4cf46a73-1d6e-4c47-9f1e-c04a8bfaa8d9', foundational, expansive_security_categories_used_to_foreclose_peaceful_competition_constitute_illegitimate_extraction).
narrative_ontology:cs_axiom_status(expansive_security_categories_used_to_foreclose_peaceful_competition_constitute_illegitimate_extraction, holdable).
narrative_ontology:cs_axiom_grounding('4cf46a73-1d6e-4c47-9f1e-c04a8bfaa8d9', expansive_security_categories_used_to_foreclose_peaceful_competition_constitute_illegitimate_extraction, empirically_contingent).
narrative_ontology:cs_reference_frame('4cf46a73-1d6e-4c47-9f1e-c04a8bfaa8d9', one_country_two_systems_civic_autonomy).
narrative_ontology:cs_drift_state('4cf46a73-1d6e-4c47-9f1e-c04a8bfaa8d9', post_2020_enactment_era, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('4cf46a73-1d6e-4c47-9f1e-c04a8bfaa8d9', '').
narrative_ontology:cs_kernel_id(nsl_legal_text__democratic_enclosure_reading, nsl_legal_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nsl_legal_text__democratic_enclosure_reading, beijing_central_authorities).
narrative_ontology:constraint_beneficiary(nsl_legal_text__democratic_enclosure_reading, hong_kong_establishment_camp).
narrative_ontology:constraint_beneficiary(nsl_legal_text__democratic_enclosure_reading, national_security_apparatus_personnel).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, pro_democracy_opposition_politicians).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, independent_press_and_journalists).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, civil_society_organizations).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, protest_participants).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, trade_unionists_and_labor_organizers).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, general_hong_kong_electorate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafted and imposed the law directly (bypassing the Hong Kong legislature), retains override authority through Article 55 case-transfer provisions and the Office for Safeguarding National Security, and appoints the judges who hear national security cases. Sets the scope of 'secession,' 'subversion,' 'terrorism,' and 'collusion with foreign forces' through implementing guidance and can expand enforcement at will.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, beijing_central_authorities, agenda_setter,
    institutional, civilizational, analytical, national).

% Pro-Beijing legislators, business elites, and pro-establishment media benefit from the removal of electoral and street-level competition: opposition candidates are disqualified, protest is criminalized, and the LegCo composition is reshaped to entrench establishment dominance. They retain full civic and economic mobility and face none of the law's restrictions in practice.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, hong_kong_establishment_camp, beneficiary,
    organized, generational, arbitrage, regional).

% Staff of the new National Security Department, prosecutors, and vetted judges gain expanded investigatory powers, asset-freezing authority, and career advancement tied to the law's enforcement. Their institutional position and personal incentives grow directly with the law's expansive application.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, national_security_apparatus_personnel, beneficiary,
    institutional, generational, arbitrage, regional).

% Elected legislators and primary-election organizers have been mass-arrested (the 47 defendants case), disqualified from running, or forced into exile. Remaining in Hong Kong means indefinite pretrial detention risk under the law's no-bail presumption; leaving means permanent exile and loss of political voice. There is no lawful path to organized opposition remaining open.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, pro_democracy_opposition_politicians, payer,
    powerless, biographical, trapped, regional).

% Apple Daily and Stand News were raided and forced to close under NSL-linked charges against their editors and executives; reporters self-censor or leave the profession. Foreign correspondents face visa non-renewal. The chilling effect extends beyond formally prosecuted cases to routine editorial decisions across all remaining outlets.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, independent_press_and_journalists, payer,
    moderate, biographical, trapped, regional).

% Unions, churches, student groups, and NGOs (including the Civil Human Rights Front and the Hong Kong Alliance) have dissolved rather than risk prosecution for 'collusion with foreign forces' or 'subversion' for routine advocacy activity. Continued operation requires abandoning core advocacy functions; dissolution forfeits decades of organizational capacity and membership networks.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, civil_society_organizations, payer,
    moderate, biographical, trapped, regional).

% Individuals who participated in 2019 protests or display banned slogans face retroactive-feeling prosecution years after the fact; public assembly itself now carries prosecutorial risk under an ill-defined secession/subversion standard. Many have left Hong Kong; those who remain face ongoing surveillance and the risk that past, once-lawful conduct will be reinterpreted as criminal.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, protest_participants, payer,
    powerless, biographical, trapped, local).

% Independent unions formed during the 2019-2020 mobilization (including in healthcare and aviation) have disbanded under NSL-linked pressure; organizing strikes or work stoppages now risks characterization as subversive activity. Collective bargaining leverage has collapsed as the organizational infrastructure that would support it disappeared.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, trade_unionists_and_labor_organizers, payer,
    powerless, biographical, trapped, local).

% Voters lost the ability to elect an opposition-majority LegCo after electoral 'reforms' following the NSL reduced directly elected seats and added a candidate-vetting committee. Casting a vote no longer carries the possibility of changing the governing coalition; emigration is the only remaining form of political exit, and it is available mainly to those with capital or foreign passports.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, general_hong_kong_electorate, payer,
    powerless, generational, constrained, regional).

% Foreign governments, UN human rights bodies, and international press freedom organizations issue statements and impose targeted sanctions but have no standing inside Hong Kong's legal process and no mechanism to compel amendment or repeal. Their objections are heard globally but carry no domestic legal force.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, international_observers_and_foreign_governments, excluded,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nsl_legal_text__democratic_enclosure_reading, beijing_central_authorities).
narrative_ontology:fixing_cost_class(nsl_legal_text__democratic_enclosure_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: There is a genuine, narrow coordination problem the law's proponents can point to: a jurisdiction lacking any codified national security statute (Hong Kong had none, unlike the mainland or most sovereign states) is vulnerable to externally organized secessionist or violent activity, and some baseline security framework was arguably a coordination gap. This story evaluates the enclosure function the enacted law actually performs, not that narrow gap.
% TRANSFER_FUNCTION: Moves political voice, organizational capacity, and freedom of expression away from opposition politicians, press, civil society, and the general electorate, and consolidates unchallenged governing authority and career advancement in Beijing-appointed administrators, vetted judges, national security personnel, and the pro-establishment political camp.
% ABSENT_VOICES: The disqualified legislators, exiled activists, and dissolved civil society organizations who would testify that the law's scope vastly exceeds any security rationale are not in the room: they are in exile, in prison, or have shut down the organizations that would have voiced the objection. International bodies raise the objection from outside but have no domestic standing.
% DISAPPEARANCE_RATIONALE: If the law and its enforcement apparatus disappeared overnight, opposition parties would re-form, disqualified legislators would seek re-election, independent unions and NGOs would reconstitute, and press outlets would resume investigative reporting without prior-restraint risk — the entire suppressed democratic and civic infrastructure of 2019 would substantially re-emerge, indicating the constraint is actively holding down arrangements that would otherwise exist, not reflecting an absence of underlying demand for them.
% FOUNDING_PROBLEM: Officially: the absence of any national security law in Hong Kong left the jurisdiction unable to prosecute secession, subversion, terrorism, or foreign collusion, a gap exposed by the scale and duration of the 2019 protests and by explicit calls for independence and foreign intervention voiced during them.
% FOUNDING_PROBLEM_CORROBORATION: Beijing and the Hong Kong government attest the founding problem (a genuine security gap) remains live and the law addresses it proportionately. Independent corroboration from outside the benefiting parties — UN Human Rights Committee reviews, the Hong Kong Bar Association's early legal commentary, and academic legal scholars specializing in Hong Kong Basic Law — assesses that the enacted scope (covering slogans, journalism, union organizing, and electoral candidacy) is structurally disproportionate to any narrow security gap and functions primarily to foreclose peaceful political competition rather than to prevent violence or foreign-directed subversion.
narrative_ontology:disappearance_verdict(nsl_legal_text__democratic_enclosure_reading, world_rearranges).
narrative_ontology:founding_problem_status(nsl_legal_text__democratic_enclosure_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nsl_legal_text__democratic_enclosure_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nsl_legal_text__democratic_enclosure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nsl_legal_text__democratic_enclosure_reading, 0.88, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nsl_legal_text__democratic_enclosure_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nsl_legal_text__democratic_enclosure_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nsl_legal_text__democratic_enclosure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.88) and rising sharply across the interval because the law's application has moved well beyond any narrow security-gap rationale into routine political, press, and labor activity — the 47-defendants prosecution, the closure of Apple Daily and Stand News, and the dissolution of dozens of unions and civil society groups within roughly two years of enactment are the empirical basis for this trajectory. Suppression is authored even higher (0.91) and reaches its plateau faster than extraction because the coercive infrastructure (the National Security Department, the vetted-judges list, the extraterritorial reach provisions, and the no-bail presumption) was established rapidly and then held constant, while the range of activity actually prosecuted continued to widen. Theater ratio is moderate (0.42): enforcement is substantially real, not merely performative, but a meaningful share of prosecutorial activity — reopening cases against long-departed activists, prosecuting symbolic slogans — functions as ongoing deterrent theater beyond what incapacitating any operational security threat would require. Accessibility collapse is high (0.87): once the law's scope is understood, no legal channel remains for organized opposition, independent unionism, or investigative press criticism of the government to operate safely. Resistance is moderate (0.58), reflecting continued underground and diaspora organizing, international legal challenges, and individual acts of noncompliance, tempered by the fact that most organized domestic resistance capacity has already been dismantled.
 *
 * PERSPECTIVAL GAP:
 *   From Beijing's and the establishment camp's seats, the law is functioning exactly as intended — restoring order, closing a genuine institutional gap, and is experienced as legitimate governance. From the payer seats, the identical structure is experienced as an unbounded, retroactively-applied criminal exposure with no safe harbor. The engine computes these as structurally different per-seat classifications from the same authored data; this story does not average or hedge between them — it authors the payer-weighted reading as its own separate constraint (this file), leaving the beneficiary-weighted framing to the sibling sovereignty_restoration_reading file.
 *
 * DIRECTIONALITY LOGIC:
 *   Beijing sits at the full-beneficiary/agenda-setter end: it authored the law outside the local legislature, retains override authority via Article 55 case transfer, and bears none of its costs. The Hong Kong establishment camp and national security personnel are beneficiaries whose institutional and electoral position strengthens directly as enforcement widens — their exit options are arbitrage-grade (they can align with the arrangement at zero cost) rather than exit in the ordinary sense. The payer seats — opposition politicians, journalists, civil society, unionists, protest participants, and the general electorate — are trapped: they cannot benefit from redefining their conduct as compliant, since the categories (subversion, collusion) are defined and applied by the same authority that benefits from expansive application. This directional asymmetry, not a symmetric cost story, is why the reading computes as extractive rather than as balanced coordination.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding-problem apparatus prevents this story from either dismissing the law as pure invention (there was a genuine, narrow security gap in 2019-2020) or accepting the security rationale at face value (the enacted scope and enforcement pattern vastly exceed what closing that gap would require). The mismatch the R5 fields are designed to surface is exactly present here: founding_problem_status is authored contested, and disappearance_verdict is world_rearranges — a status of 'dead or vastly overextended function persisting' paired with 'the world would substantially reorganize if it vanished' is the classic capture/zombie signature the corroboration cross-check exists to flag, distinguishing this from a case where the security function is genuinely still needed at the scope currently enforced.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    security_gap_versus_enclosure_scope,
    'How much of the law''s actual enforcement scope is attributable to closing the genuine 2019-era security gap, versus enforcement that exceeds any plausible security rationale and instead forecloses ordinary political competition?',
    'Comparative case-coding of prosecutions: classify each NSL prosecution by whether the underlying conduct involved organized violence/foreign-directed operational activity versus speech, electoral candidacy, journalism, or peaceful assembly. A high proportion of the latter would corroborate the enclosure reading; a high proportion of the former would support the sovereignty_restoration_reading instead.',
    'If enforcement is shown to concentrate overwhelmingly on political/civic activity rather than security threats, it strengthens the case that this reading (not the sovereignty_restoration sibling) captures the law''s dominant real-world function, and would support treating the sibling reading as increasingly counterfactual rather than merely differently weighted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_gap_versus_enclosure_scope, empirical, 'Whether NSL enforcement in practice tracks security threats or political/civic suppression.').

omega_variable(
    beneficiary_naturalness_ambiguity,
    'Is the concentration of political and institutional benefit in Beijing and the HK establishment camp best read as the intended design of the law, or as an unintended but foreseeable byproduct of a genuinely security-motivated instrument?',
    'Analysis of legislative drafting history, NPCSC deliberation records (where available), and comparison to the security-instrument design choices of other jurisdictions facing comparable secessionist movements, to assess whether the breadth of categories (e.g., ''collusion with foreign forces'' covering routine international NGO contact) was a foreseeable, avoidable design choice.',
    'If the breadth was avoidable and was chosen anyway, it strengthens the reading that beneficiary capture was intended rather than incidental, reinforcing the tangled_rope/snare-adjacent extractive characterization. If the breadth reflects genuine drafting difficulty in a novel jurisdiction, it would soften the enclosure reading toward something closer to negligent overreach rather than designed extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_naturalness_ambiguity, conceptual, 'Whether beneficiary concentration was designed or incidental to the law''s drafting.').

omega_variable(
    kernel_reading_boundary_location,
    'Where exactly does the enclosure reading''s claim diverge from the sovereignty_restoration reading''s claim, given that both readings can point to the identical enforcement actions (e.g., the 47-defendants prosecution) as evidence?',
    'The two readings do not disagree about which actions occurred; they disagree about the normative and causal characterization of those actions (extraction of political voice versus legitimate restoration of order). This is resolved, per the ε-invariance principle, by treating them as genuinely separate constraints rather than seeking a single adjudicated ε — the disagreement is located in the beneficiary/victim mapping and the founding_problem_status assessment, not in a disputed fact about what happened.',
    'Confirms that decomposition into separate constraint files (rather than a single hedged file) is the structurally correct authoring choice for this kernel, consistent with the ε-invariance principle.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_boundary_location, conceptual, 'Locates the structural disagreement between sibling readings in normative characterization, not disputed fact.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nsl_legal_text__democratic_enclosure_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nsl__tr_t0, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(nsl__tr_t6, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 6, 0.22).
narrative_ontology:measurement(nsl__tr_t12, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 12, 0.3).
narrative_ontology:measurement(nsl__tr_t24, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 24, 0.36).
narrative_ontology:measurement(nsl__tr_t36, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 36, 0.39).
narrative_ontology:measurement(nsl__tr_t48, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 48, 0.41).
narrative_ontology:measurement(nsl__tr_t60, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 60, 0.42).

% Extraction over time
narrative_ontology:measurement(nsl__be_t0, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(nsl__be_t6, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(nsl__be_t12, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 12, 0.72).
narrative_ontology:measurement(nsl__be_t24, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 24, 0.81).
narrative_ontology:measurement(nsl__be_t36, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 36, 0.85).
narrative_ontology:measurement(nsl__be_t48, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 48, 0.87).
narrative_ontology:measurement(nsl__be_t60, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 60, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(nsl__su_t0, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(nsl__su_t6, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 6, 0.74).
narrative_ontology:measurement(nsl__su_t12, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 12, 0.85).
narrative_ontology:measurement(nsl__su_t24, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 24, 0.9).
narrative_ontology:measurement(nsl__su_t36, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 36, 0.91).
narrative_ontology:measurement(nsl__su_t48, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 48, 0.91).
narrative_ontology:measurement(nsl__su_t60, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 60, 0.91).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nsl_legal_text__democratic_enclosure_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nsl_legal_text__democratic_enclosure_reading, nsl_legal_text__sovereignty_restoration_reading).
narrative_ontology:affects_constraint(nsl_legal_text__democratic_enclosure_reading, nsl_legal_text__jurisdictional_capture_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the nsl_legal_text kernel. democratic_enclosure_reading (this file) authors civil society/press/opposition as victims and Beijing/HK establishment as beneficiaries, with high extractiveness reflecting suppression of democratic infrastructure. sovereignty_restoration_reading authors the same enacted text as a legitimate security instrument restoring constitutional order, with correspondingly lower extractiveness and a different beneficiary framing (the HK/PRC constitutional order itself as vindicated, not merely factional beneficiaries). jurisdictional_capture_reading addresses a structurally distinct claim about common-law erosion and mainland legal transplantation, with its own victim set (the legal profession, judiciary) distinct from this file's civic/political victim set. All three share the identical legal text as their object but diverge in ε, beneficiary/victim structure, and claimed_type — per the ε-invariance principle, they are authored as separate files rather than as one hedged story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
