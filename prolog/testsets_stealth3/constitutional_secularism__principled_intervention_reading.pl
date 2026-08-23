% ============================================================================
% CONSTRAINT STORY: constitutional_secularism__principled_intervention_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_secularism__principled_intervention_reading, []).

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
 *   constraint_id: constitutional_secularism__principled_intervention_reading
 *   human_readable: Principled-Intervention Reading of Constitutional Secularism: Reform-Justified State Entry into Religious Affairs
 *   domain: constitutional/political/religious_governance
 *
 * SUMMARY:
 *   This story instantiates the principled-intervention reading of the
 *   constitutional-secularism kernel: the standing arrangement under which
 *   the state holds a justified PERMISSION to enter religious affairs —
 *   legislating access, transferring institutional administration, overriding
 *   exclusionary custom — when the entry advances social reform or protects
 *   weaker sections within a community. The arrangement solves a real
 *   deadlock (hierarchically controlled communities rarely reform exclusion
 *   from inside) while transferring definitional and administrative authority
 *   from custodial hands to state organs, and its justification standard is
 *   elastic enough to be steered by electoral majorities. Per the
 *   epsilon-invariance principle this is one of three linked stories, not a
 *   survey of the debate: the strict-neutrality and reformist readings
 *   instantiate different constraints with different epsilon, victim sets,
 *   and classifications; they are linked through network edges, not averaged
 *   here. The claim and the metrics are independent authored facts:
 *   claimed_type states what this reading's structure is (tangled_rope —
 *   genuine coordination plus asymmetric extraction under active
 *   enforcement), while the metrics describe the arrangement's actual
 *   operation as this reading's own lights assess it, including the capture
 *   risk the reading itself acknowledges.
 *
 * KEY AGENTS:
 *   - constitutional_apex_judiciary: Agenda-setter (institutional/analytical) — defines essential practice, authorizes or restrains intervention, collects doctrinal authority
 *   - legislative_reform_authorities: Agenda-setter and beneficiary (institutional/arbitrage) — enacts reform statutes, administers taken-over institutions, collects legitimacy and control; can redeploy the machinery
 *   - historically_excluded_castes: Primary beneficiary (organized/trapped) — holds access and protection gains that persist only under continued enforcement
 *   - women_denied_equal_worship: Beneficiary (moderate/identity_locked) — collects court-opened access; cannot exit the membership she is equalizing
 *   - hereditary_custodians_and_trustees: Primary payer (powerful/trapped) — loses administrative and definitional control; office bound to the institution
 *   - autonomous_minority_denominations: Payer (moderate/constrained) — bears the majoritarian-capture exposure; autonomy rests on restraint they cannot enforce
 *   - majority_community_traditionalists: Payer with beneficiary secondary role (powerful/constrained) — absorbs autonomy losses at home while steering deployment outward
 *   - syncretic_folk_practitioners: Excluded (powerless/trapped) — absent from both reform and custodial frames; their practice gets recoded without a seat
 *   - comparative_constitutional_scholars: Analytical observer — documents the gap between reform justification and deployment record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_secularism__principled_intervention_reading, 0.58).
domain_priors:suppression_score(constitutional_secularism__principled_intervention_reading, 0.6).
domain_priors:theater_ratio(constitutional_secularism__principled_intervention_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, resistance, 0.66).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_secularism__principled_intervention_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_secularism__principled_intervention_reading, "Principled-Intervention Reading of Constitutional Secularism: Reform-Justified State Entry into Religious Affairs").
narrative_ontology:topic_domain(constitutional_secularism__principled_intervention_reading, "constitutional/political/religious_governance").

domain_priors:requires_active_enforcement(constitutional_secularism__principled_intervention_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_secularism__principled_intervention_reading, 'dbcd75e7-1398-492f-b333-9d31be387fc6').
narrative_ontology:cs_kernel_codification('dbcd75e7-1398-492f-b333-9d31be387fc6', fixed_text).
narrative_ontology:cs_authority_grounding('dbcd75e7-1398-492f-b333-9d31be387fc6', lineage).
narrative_ontology:cs_interpretation_layer_present('dbcd75e7-1398-492f-b333-9d31be387fc6').
narrative_ontology:cs_reading_relation('dbcd75e7-1398-492f-b333-9d31be387fc6', constitutional_secularism__strict_neutrality_reading, forecloses).
narrative_ontology:cs_reading_relation('dbcd75e7-1398-492f-b333-9d31be387fc6', constitutional_secularism__reformist_reading, coexists_with).
narrative_ontology:cs_axiom('dbcd75e7-1398-492f-b333-9d31be387fc6', foundational, reform_objective_justifies_differential_treatment).
narrative_ontology:cs_axiom_status(reform_objective_justifies_differential_treatment, holdable).
narrative_ontology:cs_axiom_grounding('dbcd75e7-1398-492f-b333-9d31be387fc6', reform_objective_justifies_differential_treatment, conventional).
narrative_ontology:cs_axiom('dbcd75e7-1398-492f-b333-9d31be387fc6', foundational, weaker_section_protection_overrides_internal_autonomy).
narrative_ontology:cs_axiom_status(weaker_section_protection_overrides_internal_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('dbcd75e7-1398-492f-b333-9d31be387fc6', weaker_section_protection_overrides_internal_autonomy, deontological).
narrative_ontology:cs_axiom('dbcd75e7-1398-492f-b333-9d31be387fc6', secondary, intervention_requires_proportionate_principled_justification).
narrative_ontology:cs_axiom_status(intervention_requires_proportionate_principled_justification, holdable).
narrative_ontology:cs_axiom_grounding('dbcd75e7-1398-492f-b333-9d31be387fc6', intervention_requires_proportionate_principled_justification, instrumental).
narrative_ontology:cs_reference_frame('dbcd75e7-1398-492f-b333-9d31be387fc6', reform_objective_bounded_permission).
narrative_ontology:cs_drift_state('dbcd75e7-1398-492f-b333-9d31be387fc6', contemporary_majoritarian_politics, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('dbcd75e7-1398-492f-b333-9d31be387fc6', '').
narrative_ontology:cs_kernel_id(constitutional_secularism__principled_intervention_reading, constitutional_secularism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_secularism__principled_intervention_reading, historically_excluded_castes).
narrative_ontology:constraint_beneficiary(constitutional_secularism__principled_intervention_reading, women_denied_equal_worship).
narrative_ontology:constraint_beneficiary(constitutional_secularism__principled_intervention_reading, legislative_reform_authorities).
narrative_ontology:constraint_victim(constitutional_secularism__principled_intervention_reading, hereditary_custodians_and_trustees).
narrative_ontology:constraint_victim(constitutional_secularism__principled_intervention_reading, autonomous_minority_denominations).
narrative_ontology:constraint_victim(constitutional_secularism__principled_intervention_reading, majority_community_traditionalists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_secularism__principled_intervention_reading, majority_community_traditionalists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Determines which practices count as essential and therefore shielded, and which are open to intervention; upholds or strikes down reform statutes; draws the line between permissible reform entry and impermissible interference. Collects doctrinal authority from adjudicating that boundary and publishes the standards other seats must argue within.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, constitutional_apex_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Enacts access and reform statutes, runs boards that administer taken-over endowments, and staffs the departments that manage religious institutions under intervention. Gains legitimacy, mobilization narratives, and direct administrative control from sponsorship; because the justification standard is elastic, the same machinery can be redeployed toward whichever community or practice carries electoral value.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, legislative_reform_authorities, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(constitutional_secularism__principled_intervention_reading, legislative_reform_authorities, beneficiary).

% Received statutory guarantees of temple entry, protection from ritual exclusion, and access to institutions their own communities denied them. The gains are real but held only by continued enforcement; they cannot exit the social order that assigns their status, so their position depends on the intervention machinery staying willing and able to act on their behalf.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, historically_excluded_castes, beneficiary,
    organized, generational, trapped, national).

% Seek entry and equal ritual standing in denominations that bar them. Court-opened access is the benefit they collect. Leaving the faith to escape exclusion would forfeit the very belonging they are trying to equalize, so they pursue reform from inside a membership they cannot put down; their leverage rises and falls with judicial attention.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, women_denied_equal_worship, beneficiary,
    moderate, biographical, identity_locked, national).

% Administered temples, endowments, and ritual calendars under customary and hereditary control. Intervention statutes transfer administration to state boards and open access against their rules; office, livelihood, and status are bound to the institution, so there is nothing to exit to. They litigate, delay, and negotiate, and they bear the definitional loss of having their authority reclassified as dispensable custom.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, hereditary_custodians_and_trustees, payer,
    powerful, generational, trapped, regional).

% Bear the capture risk built into the arrangement: a permission justified by reform can be redeployed against minority practice wherever majority preference moves. Their institutional autonomy survives on a restraint they cannot themselves enforce; disestablishment or emigration is not a working exit, so they pay in foreclosed self-governance and chronic exposure.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, autonomous_minority_denominations, payer,
    moderate, generational, constrained, national).

% Lose customary gatekeeping where intervention lands on their own institutions, yet hold the electoral weight that steers which communities intervention visits. They absorb autonomy losses at home while helping aim the machinery outward at minority practice, collecting the protective and status dividends of a reform vocabulary they did not build.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, majority_community_traditionalists, payer,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_secularism__principled_intervention_reading, majority_community_traditionalists, beneficiary).

% Practice localized blended traditions that fit neither the scriptural-reform categories the state legislates nor the orthodox custodial claims the custodians defend. Absent from both the reform litigation and the custodial negotiations, they watch interventions recode their practice into categories they do not hold, with no seat in the process that redraws it.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, syncretic_folk_practitioners, excluded,
    powerless, generational, trapped, local).

% Track intervention deployments across jurisdictions and decades, comparing the reform justifications offered at enactment with the administrative and political uses that followed. Neither collect from nor pay into the arrangement; their output is the deployment record other seats argue over.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_secularism__principled_intervention_reading, legislative_reform_authorities).
narrative_ontology:fixing_cost_class(constitutional_secularism__principled_intervention_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Breaks the internal-reform deadlock of hierarchically governed communities: where those who benefit from exclusion hold agenda control, internal consensus cannot produce access or protection, and the state supplies external, enforceable leverage that weaker members cannot generate alone.
% TRANSFER_FUNCTION: Moves definitional and administrative authority over religious practice from hereditary custodians to state organs; moves physical and ritual access to previously excluded members; moves legitimacy and mobilization capital to the political actors who sponsor each intervention.
% ABSENT_VOICES: Syncretic folk practitioners whose traditions fit neither reform nor orthodox categories are absent from both sides of the contest; minority denominations hold no seat in a reform conversation framed mostly in majority-community categories; internal dissenters who want change but not state-mediated change have no channel. Each would object to the arrangement's category system if present.
% DISAPPEARANCE_RATIONALE: If the intervention permission vanished overnight, access guarantees would face immediate rollback pressure from restored custodial control, protection regimes for weaker sections would lapse back to communal bargaining where the weaker side lost for generations, and state boards administering taken-over endowments would unwind or fight to keep their assets. Communities would reorganize around internal power balances that historically reproduced exclusion.
% FOUNDING_PROBLEM: At the constitutional founding, religiously sanctioned hierarchy (ritual caste exclusion, gender-barred access) had survived generations of internal reform effort because the excluded had no agenda control inside their own communities; the founders needed a way to break that deadlock without full disestablishment of religion.
% FOUNDING_PROBLEM_CORROBORATION: Defenders of the arrangement attest liveness by citing each new exclusion that surfaces; critics outside the benefiting parties — judicial records documenting exclusion at the moment of each intervention, law-commission and human-rights-body reports, and sociological studies of post-intervention access outcomes — corroborate that the original legal-form targets (statutory untouchability, formal entry bars) are substantially resolved while newer exclusions persist. Corroboration is partial and framing-dependent: no seat fully outside the normative dispute exists, and the scholars' deployment record is the closest to independent.
narrative_ontology:disappearance_verdict(constitutional_secularism__principled_intervention_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_secularism__principled_intervention_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_secularism__principled_intervention_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_secularism__principled_intervention_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_secularism__principled_intervention_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_secularism__principled_intervention_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_secularism__principled_intervention_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_secularism__principled_intervention_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.58: the referent is the standing intervention arrangement assessed by this reading's own lights — the reading endorses the permission, but its own structural delta names the costs (expanded state authority, capture risk), and the deployment record shows reform justification progressively layered with asset management and selective targeting. Suppression 0.60 is a raw structural property, unscaled by power or scope: the arrangement's persistence requires active machinery — police protection of entrants, statutory override of custom, contempt and takeover proceedings — and it suppresses the alternative of communal self-settlement wherever it deploys. Theater 0.32: a growing share of activity is symbolic (announced reforms without implementation, commissions that do not report, access granted where attendance is already safe). Accessibility collapse 0.48: internal-reform channels are partly crowded out by the state channel, but community self-help and litigation-by-custodians remain workable, so alternatives degrade without vanishing. Resistance 0.66: sustained institutional litigation, protest at contested sites, and non-cooperation by custodial bodies. The three measurement series share one time grid (t=0,6,12,18,24,30) so every metric is authored at every examined point; trajectories show extraction accumulation and enforcement hardening rather than oscillation. Coalition note: the payer seats are heterogeneous (two powerful, one moderate); a custodian-minority coalition against expansion is structurally possible, but the traditionalists' capture leverage splits it — relevant to any coalition-power analysis of the less powerful victims.
 *
 * PERSPECTIVAL GAP:
 *   The engine computes per-seat types from the structural data, and the seats diverge sharply. From the agenda-setter seats the arrangement is coordination they personally operate: the judiciary sees doctrine it authors, the legislature sees a reform instrument it wields. From the custodian seat the same structure computes as expropriation of office and definition. From the minority-denomination seat it computes as a loaded instrument aimed elsewhere today and here tomorrow. From the beneficiary seats it computes as subsidy — access and protection flowing in. No single authored claim could reconcile these; the divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for historically_excluded_castes and women_denied_equal_worship; the women's identity_locked exit does not push them toward the target end because the lock binds them to the community whose access they are gaining, not to a cost they cannot flee. Payer declarations drive high directionality for custodians (trapped exit amplifies: nothing to exit to) and minority denominations (constrained exit keeps them near-full targets despite moderate power). Legislative authorities sit near the beneficiary end through their secondary_role collection, with arbitrage-grade exit damping even that. Majority traditionalists are genuinely dual-positioned: payer losses at home pull d upward, capture dividends pull it down; the derivation lands them mid-range, which matches their split position, so no directionality override is used anywhere in this story — the structural data produces the right relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim is what prevents mislabeling in both directions. Reading the arrangement as pure snare erases the real, documented access gains that weaker sections hold only through it; reading it as pure rope ignores the accumulating management rents, the elastic justification standard, and the capture exposure the reading's own tradition concedes. The temporal series shows the classic tangled-rope signature — coordination function intact while base extractiveness climbs — and the founding_problem_status=contested x disappearance_verdict=world_rearranges cell routes the obsolescence question to the mismatch consumer rather than letting either the flattering origin myth or the pure-extraction reading settle it. If the founding problem audits as dead while the power keeps expanding, the piton/zombie flag fires on the mismatch, not on a tuned metric.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Which reading of the constitutional_secularism kernel governs the intervention permission — this bounded-permission reading, strict neutrality, or the reformist duty reading?',
    'Sustained apex-court doctrinal realignment or formal constitutional amendment selecting among the readings; short of that, tracked shifts in the essential-practices doctrine''s scope.',
    'Adoption of strict neutrality would shrink the permission (lower epsilon on this referent, higher suppression of reform outcomes); adoption of the reformist reading would convert permission into duty, raising epsilon and expanding the victim set to every autonomy-bearing religious institution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'This constraint is one reading of the constitutional_secularism kernel; sibling readings instantiate different constraints with different victim sets and classifications.').

omega_variable(
    majoritarian_capture_deployment_audit,
    'Is the intervention power deployed symmetrically across communities, or steered by majority electoral preference toward minority practice?',
    'Comparative audit of intervention statutes, endowment takeovers, and access litigation coded by target community and sponsoring coalition, over the full interval.',
    'Confirmed asymmetric deployment converts the minority-facing face of the arrangement toward snare; symmetric deployment supports the tangled_rope reading and locates the extraction in custodial displacement rather than communal targeting.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majoritarian_capture_deployment_audit, empirical, 'Whether the capture risk named in this reading''s structural delta has materialized in the deployment record.').

omega_variable(
    reform_efficacy_vs_management_control,
    'Do interventions deliver durable access and protection outcomes, or primarily administrative control of religious assets and personnel?',
    'Longitudinal outcome studies of temple-entry, endowment-takeover, and personal-law interventions measured against pre-intervention baselines, separating access gains from administrative-transfer effects.',
    'If control dominates, the coordination story is cover and the arrangement drifts toward snare; if access outcomes hold independent of asset transfer, the coordination function is genuine and the measured extraction is contested surplus above the coordination floor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reform_efficacy_vs_management_control, empirical, 'Whether the genuine coordination component of the tangled_rope structure is delivering its function or has been displaced by management rent.').

omega_variable(
    founding_problem_liveness,
    'Is the founding problem — exclusion enforced by communal hierarchies unable to reform internally — still live, or has the intervention power outlived its mandate?',
    'Independent audit of remaining exclusionary practices and of demonstrated internal-reform capacity in communities facing new exclusions without state leverage.',
    'A dead founding problem under a persisting, expanding power flags the mandatrophy/piton trajectory and strengthens the status-x-verdict mismatch signal; a live problem supports the arrangement''s coordination leg regardless of extraction levels.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_liveness, empirical, 'Genealogy check on whether the R5 founding problem still exists or the permission now runs on inherited momentum.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_secularism__principled_intervention_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cs_principled_intervention_tr_t0, constitutional_secularism__principled_intervention_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(cs_principled_intervention_tr_t6, constitutional_secularism__principled_intervention_reading, theater_ratio, 6, 0.19).
narrative_ontology:measurement(cs_principled_intervention_tr_t12, constitutional_secularism__principled_intervention_reading, theater_ratio, 12, 0.23).
narrative_ontology:measurement(cs_principled_intervention_tr_t18, constitutional_secularism__principled_intervention_reading, theater_ratio, 18, 0.26).
narrative_ontology:measurement(cs_principled_intervention_tr_t24, constitutional_secularism__principled_intervention_reading, theater_ratio, 24, 0.29).
narrative_ontology:measurement(cs_principled_intervention_tr_t30, constitutional_secularism__principled_intervention_reading, theater_ratio, 30, 0.32).

% Extraction over time
narrative_ontology:measurement(cs_principled_intervention_be_t0, constitutional_secularism__principled_intervention_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(cs_principled_intervention_be_t6, constitutional_secularism__principled_intervention_reading, base_extractiveness, 6, 0.44).
narrative_ontology:measurement(cs_principled_intervention_be_t12, constitutional_secularism__principled_intervention_reading, base_extractiveness, 12, 0.48).
narrative_ontology:measurement(cs_principled_intervention_be_t18, constitutional_secularism__principled_intervention_reading, base_extractiveness, 18, 0.52).
narrative_ontology:measurement(cs_principled_intervention_be_t24, constitutional_secularism__principled_intervention_reading, base_extractiveness, 24, 0.55).
narrative_ontology:measurement(cs_principled_intervention_be_t30, constitutional_secularism__principled_intervention_reading, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(cs_principled_intervention_su_t0, constitutional_secularism__principled_intervention_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(cs_principled_intervention_su_t6, constitutional_secularism__principled_intervention_reading, suppression_requirement, 6, 0.53).
narrative_ontology:measurement(cs_principled_intervention_su_t12, constitutional_secularism__principled_intervention_reading, suppression_requirement, 12, 0.56).
narrative_ontology:measurement(cs_principled_intervention_su_t18, constitutional_secularism__principled_intervention_reading, suppression_requirement, 18, 0.58).
narrative_ontology:measurement(cs_principled_intervention_su_t24, constitutional_secularism__principled_intervention_reading, suppression_requirement, 24, 0.59).
narrative_ontology:measurement(cs_principled_intervention_su_t30, constitutional_secularism__principled_intervention_reading, suppression_requirement, 30, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_secularism__principled_intervention_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_secularism__principled_intervention_reading, constitutional_secularism__strict_neutrality_reading).
narrative_ontology:affects_constraint(constitutional_secularism__principled_intervention_reading, constitutional_secularism__reformist_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'constitutional secularism' per the epsilon-invariance principle. The label conflates three structurally distinct arrangements: absolute non-interference (strict_neutrality_reading), bounded reform-justified permission (this story), and overriding elimination duty (reformist_reading). Their epsilon values differ materially because their victim sets differ: the neutrality reading's costs fall on excluded members seeking state leverage; this reading's costs fall on custodial autonomy and minority-denomination exposure; the reformist reading's costs fall on all autonomy-bearing institutions. Family linkage runs through network.affects_constraints in all three files; the strict-neutrality story is the upstream baseline each intervention precedent is argued against, and the reformist story is the downstream escalation this reading's precedents normalize.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
