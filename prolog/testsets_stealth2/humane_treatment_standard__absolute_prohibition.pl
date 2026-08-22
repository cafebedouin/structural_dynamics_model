% ============================================================================
% CONSTRAINT STORY: humane_treatment_standard__absolute_prohibition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_humane_treatment_standard__absolute_prohibition, []).

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
 *   constraint_id: humane_treatment_standard__absolute_prohibition
 *   human_readable: Common Article 3 Absolute Humane-Treatment Floor (Non-Derogable Minimum Standards)
 *   domain: international_humanitarian_law/state_security/human_rights
 *
 * SUMMARY:
 *   Common Article 3 of the 1949 Geneva Conventions sets a non-derogable
 *   minimum floor of humane treatment binding every party to every armed
 *   conflict, and this story instantiates its absolute_prohibition reading:
 *   the floor admits no security exception, detainees hold the full protected
 *   set, and interrogation method families are closed categorically. The
 *   arrangement coordinates reciprocal restraint in the conflicts where
 *   reciprocity is weakest, while concentrating its compliance costs on state
 *   security apparatuses and its benefits on captive and civilian populations
 *   — a hybrid structure sustained by an enforcement machinery (international
 *   tribunals, universal jurisdiction, treaty-body pressure, donor
 *   conditionality) that has grown continuously since 1949. CONSTRAINT FAMILY
 *   NOTE: the colloquial label 'the humane treatment standard' decomposes
 *   into three structurally distinct readings of one kernel
 *   (humane_treatment_standard). This file authors the absolute_prohibition
 *   reading with epsilon 0.35 over the standing absolute-floor arrangement.
 *   The sibling files — humane_treatment_standard__contextual_necessity
 *   (security override admitted; detainees leave the full rights-holder set
 *   during emergencies; epsilon redistributes toward captive populations) and
 *   humane_treatment_standard__proportionality_balancing (threshold tradable
 *   case-by-case; intermediate epsilon) — are separate constraints with their
 *   own metrics, linked through network.affects_constraints. The
 *   upstream/downstream gradient runs from this file outward: the absolute
 *   reading is the entrenched reference against which the other two define
 *   themselves. KEY AGENTS (by structural relationship): -
 *   detainees_and_hors_de_combat_persons: Primary protected class
 *   (powerless/trapped) — receives the floor's guarantees unconditionally -
 *   state_security_services: Primary paying seat (institutional/constrained)
 *   — surrenders method families and bears prosecution exposure -
 *   ratifying_state_parties: Agenda-setting collective
 *   (institutional/arbitrage) — authors and selectively enforces -
 *   international_criminal_tribunals: Enforcement administrator
 *   (institutional/constrained) - humanitarian_organizations: Institutional
 *   beneficiary (organized/identity_locked) — mandate fused with
 *   custodianship - non_state_armed_groups: Bound non-author
 *   (organized/constrained) — pays restraint costs, receives mirrored
 *   protection - human_rights_treaty_bodies: Analytical observer
 *   (institutional/analytical) — documentation without enforcement power -
 *   national_armed_forces_of_ratifying_states: Reciprocity beneficiary
 *   (organized/constrained) - civilian_populations_in_conflict_zones: Diffuse
 *   protected population (powerless/trapped) -
 *   governments_facing_accountability: Litigation-exposed payer
 *   (institutional/constrained)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(humane_treatment_standard__absolute_prohibition, 0.35).
domain_priors:suppression_score(humane_treatment_standard__absolute_prohibition, 0.66).
domain_priors:theater_ratio(humane_treatment_standard__absolute_prohibition, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, extractiveness, 0.35).
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(humane_treatment_standard__absolute_prohibition, tangled_rope).
narrative_ontology:human_readable(humane_treatment_standard__absolute_prohibition, "Common Article 3 Absolute Humane-Treatment Floor (Non-Derogable Minimum Standards)").
narrative_ontology:topic_domain(humane_treatment_standard__absolute_prohibition, "international_humanitarian_law/state_security/human_rights").

domain_priors:requires_active_enforcement(humane_treatment_standard__absolute_prohibition).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(humane_treatment_standard__absolute_prohibition, '831d9592-0004-484a-bd4b-b33ca50d1218').
narrative_ontology:cs_kernel_codification('831d9592-0004-484a-bd4b-b33ca50d1218', fixed_text).
narrative_ontology:cs_authority_grounding('831d9592-0004-484a-bd4b-b33ca50d1218', lineage).
narrative_ontology:cs_interpretation_layer_present('831d9592-0004-484a-bd4b-b33ca50d1218').
narrative_ontology:cs_reading_relation('831d9592-0004-484a-bd4b-b33ca50d1218', humane_treatment_standard__contextual_necessity, forecloses).
narrative_ontology:cs_reading_relation('831d9592-0004-484a-bd4b-b33ca50d1218', humane_treatment_standard__proportionality_balancing, influences).
narrative_ontology:cs_axiom('831d9592-0004-484a-bd4b-b33ca50d1218', foundational, torture_prohibition_admits_no_exception).
narrative_ontology:cs_axiom_status(torture_prohibition_admits_no_exception, holdable).
narrative_ontology:cs_axiom_grounding('831d9592-0004-484a-bd4b-b33ca50d1218', torture_prohibition_admits_no_exception, deontological).
narrative_ontology:cs_axiom('831d9592-0004-484a-bd4b-b33ca50d1218', secondary, detainee_full_rights_holder_inclusion).
narrative_ontology:cs_axiom_status(detainee_full_rights_holder_inclusion, holdable).
narrative_ontology:cs_axiom_grounding('831d9592-0004-484a-bd4b-b33ca50d1218', detainee_full_rights_holder_inclusion, conventional).
narrative_ontology:cs_reference_frame('831d9592-0004-484a-bd4b-b33ca50d1218', non_derogable_humanitarian_floor).
narrative_ontology:cs_drift_state('831d9592-0004-484a-bd4b-b33ca50d1218', contemporary_counterterrorism_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('831d9592-0004-484a-bd4b-b33ca50d1218', '').
narrative_ontology:cs_kernel_id(humane_treatment_standard__absolute_prohibition, humane_treatment_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(humane_treatment_standard__absolute_prohibition, detainees_and_hors_de_combat_persons).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__absolute_prohibition, civilian_populations_in_conflict_zones).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__absolute_prohibition, humanitarian_organizations).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__absolute_prohibition, national_armed_forces_of_ratifying_states).
narrative_ontology:constraint_victim(humane_treatment_standard__absolute_prohibition, state_security_services).
narrative_ontology:constraint_victim(humane_treatment_standard__absolute_prohibition, governments_facing_accountability).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__absolute_prohibition, ratifying_state_parties).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__absolute_prohibition, non_state_armed_groups).
narrative_ontology:constraint_victim(humane_treatment_standard__absolute_prohibition, non_state_armed_groups).
narrative_ontology:constraint_vindicates(humane_treatment_standard__absolute_prohibition, human_dignity_inviolability).
narrative_ontology:constraint_vindicates(humane_treatment_standard__absolute_prohibition, jus_cogens_peremptory_status).
narrative_ontology:constraint_vindicates(humane_treatment_standard__absolute_prohibition, non_reciprocity_of_humane_treatment_obligations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Persons in the custody of a party to an armed conflict — captured fighters, suspected insurgents, civilian internees. The arrangement guarantees them a floor of treatment: no violence to life and person, no hostage-taking, no outrages on personal dignity, no sentences passed without due process. They chose nothing and can exit nothing; every protection they hold arrives through rules other people wrote and other people enforce.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, detainees_and_hors_de_combat_persons, beneficiary,
    powerless, immediate, trapped, global).

% Live inside the zones where the minimum standards operate. Benefit when parties to a conflict internalize the floor — fewer reprisals, less hostage-taking, protected medical care. Cannot leave; their protection depends entirely on the parties' compliance and on monitors gaining access.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, civilian_populations_in_conflict_zones, beneficiary,
    powerless, biographical, trapped, regional).

% Visit camps, register detainees, transmit family news, document abuses. Their access mandates, their funding, and their institutional purpose all rest on the minimum-standards framework being honored; generations of staff careers and organizational identity are fused with custodianship of these rules.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, humanitarian_organizations, beneficiary,
    organized, generational, identity_locked, global).

% Soldiers of states that accepted the standards. Gain reciprocal protection when adversaries honor the floor, and gain clear lawful-conduct doctrine for their own operations. Bear training burdens and rules-of-engagement friction; their protection is only as good as the worst adversary's compliance.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, national_armed_forces_of_ratifying_states, beneficiary,
    organized, biographical, constrained, continental).

% Intelligence agencies, military interrogation units, interior ministries. The arrangement removes entire method families from their toolkits with no emergency carve-out, exposes officers to prosecution under implementing statutes and universal jurisdiction, and requires them to open custody to monitors they would rather exclude. They cannot withdraw from customary-law coverage, and treaty denunciation carries costs few governments will pay.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, state_security_services, payer,
    institutional, biographical, constrained, national).

% Governments confronting suits, inquiries, tribunal referrals, or donor conditionality over custody practices. Pay in litigation, sanctions exposure, alliance friction, and forced archival disclosure. Their exit runs through non-cooperation with courts, which compounds the reputational cost it is meant to avoid.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, governments_facing_accountability, payer,
    institutional, generational, constrained, national).

% The collective of states that drafted, ratified, and periodically reinforce the standards through diplomatic conferences and Security Council action. They administer the regime, appoint its judges and treaty bodies, and decide case by case which violations meet consequence. They benefit from the order the floor maintains and from the legitimacy of authorship, while retaining practical discretion over enforcement selectivity.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, ratifying_state_parties, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(humane_treatment_standard__absolute_prohibition, ratifying_state_parties, beneficiary).

% The ICC and ad hoc tribunals prosecute torture and cruel treatment as war crimes and crimes against humanity. Their dockets, budgets, and institutional survival depend on state cooperation for arrests and evidence; they enforce a floor they cannot themselves amend.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, international_criminal_tribunals, agenda_setter,
    institutional, generational, constrained, global).

% UN treaty bodies, special rapporteurs, and commissions of inquiry. They receive reports, conduct investigations, publish findings, and name non-compliant parties. They hold no enforcement power of their own; their instrument is documentation and publicity.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, human_rights_treaty_bodies, observer,
    institutional, generational, analytical, global).

% Insurgent and rebel movements bound by the same minimum standards although none of them sat at the drafting table. Restraint costs them interrogation leverage and coercive control over captured enemies; the same rules protect their own wounded and detained when adversaries comply. They cannot exit the rules' coverage, which follows the facts of conflict rather than signature.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, non_state_armed_groups, payer,
    organized, immediate, constrained, regional).
narrative_ontology:stakeholder_secondary_role(humane_treatment_standard__absolute_prohibition, non_state_armed_groups, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(humane_treatment_standard__absolute_prohibition, detainees_and_hors_de_combat_persons).
narrative_ontology:fixing_cost_class(humane_treatment_standard__absolute_prohibition, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the reciprocal-cruelty spiral of non-international armed conflict: before 1949, persons captured or disarmed in civil wars fell outside every protection, and each side's atrocities licensed the other's. Common Article 3 sets one minimum floor binding every party to a conflict — government or insurgent, signatory or not — so that restraint no longer depends on matching status or matching mercy.
% TRANSFER_FUNCTION: Moves bodily security from state coercive apparatuses to persons in their custody: interrogation leverage, punitive power over captives, and emergency discretion are surrendered by security services and arrive as enforceable guarantees for detainees. It also moves adjudicatory reach upward — custody abuse becomes prosecutable before international tribunals — and moves reputational credit toward complying states.
% ABSENT_VOICES: Detainees are the regime's entire protected class and sit at none of its tables: drafted by states, adjudicated by courts, monitored by agencies — never consulted. Non-state armed groups are bound without having been represented at the drafting conference. Victims of ongoing abuse enter the record only through forensic documentation, NGO testimony, and leaked archives.
% DISAPPEARANCE_RATIONALE: Overnight removal of the floor would reorganize custody doctrine in every active conflict within months: interrogation practice would re-expand to the full coercive toolkit, reprisal spirals would resume where they were damped, the ICC's war-crimes docket would lose its object, and monitor-access mandates would collapse with their legal basis. The accountability architecture built on the floor — universal jurisdiction statutes, implementing legislation, tribunal jurisprudence — would be left governing a vacuum.
% FOUNDING_PROBLEM: The inter-communal atrocities of the 1930s-40s and the Spanish Civil War showed that in civil war no law reached the captive or the wounded civilian: POW protections required regular armies and reciprocal recognition, exactly what internal conflicts lack. The 1949 Diplomatic Conference built a minimum floor that would bind all parties to any armed conflict, however irregular.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: ICRC custody-visit and abuse documentation across dozens of current conflicts; UN commissions of inquiry finding torture in Syria and elsewhere; national military doctrines citing the operational costs of abuse; and prosecutorial records at the ICC and ad hoc tribunals. State parties also attest liveness, but the independent documentation apparatus does not depend on their assent.
narrative_ontology:disappearance_verdict(humane_treatment_standard__absolute_prohibition, world_rearranges).
narrative_ontology:founding_problem_status(humane_treatment_standard__absolute_prohibition, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(humane_treatment_standard__absolute_prohibition, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(humane_treatment_standard__absolute_prohibition, 'none', 1).
narrative_ontology:epsilon_provenance(humane_treatment_standard__absolute_prohibition, 0.35, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(humane_treatment_standard__absolute_prohibition_tests).
:- end_tests(humane_treatment_standard__absolute_prohibition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.35: the floor transfers real capability from security apparatuses to captive populations and layers prosecution exposure onto ratifying states, but the transfer is the arrangement's stated purpose rather than parasitic rent, and reciprocity returns part of the cost to payers over time. Suppression is high (0.66) because the floor's persistence depends on active enforcement — tribunals, universal jurisdiction, conditionality — not on voluntary preference; the suppression_requirement series is authored as a rising trajectory because the story's dynamic IS enforcement-capacity accumulation (ICTY 1993, Rome Statute 1998, the post-2001 ratchet), which is exactly the case the temporal guidance reserves for that series. Theater is 0.45 and climbing: condemnation output (resolutions, statements, inquiries) has grown faster than consequence delivery, and selective enforcement lets powerful violators pay in rhetoric what weaker ones pay in dockets — the series approaches but does not cross the proxy-substitution threshold. Accessibility collapse is 0.55: open endorsement of custodial torture is legally dead everywhere, yet covert practice keeps a shadow-alternative alive, so alternatives are suppressed but not annihilated. Resistance is 0.65: documented abuse recurs across conflicts and eras, states resist through secrecy, reinterpretation, and non-cooperation, and the post-2001 decade showed that even the regime's authors defect under security pressure. All three series share one nine-point grid (1949-2025) so every metric is authored at every examined time point; the extractiveness hump at 2006 marks the collision of the enforcement ratchet with mass defection, when compliant actors bore both their own compliance costs and the system's hypocrisy discount.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute sharply differently. From the detainee seat the arrangement is near-pure protection: unconditional guarantees arriving from rules the seat never chose, experienced as subsidy with no extractive face. From the security-service seat the same structure is a hard ceiling with no emergency valve: method families closed categorically, officers exposed to prosecution, monitors entitled to access — experienced as imposed cost with constrained exit. From the ratifying-states seat it is order maintenance: a floor the authors still endorse because the alternative (reciprocal cruelty spirals) is worse, tempered by irritation at enforcement selectivity that spares the powerful. The payer coalition deserves note: the states bearing compliance costs could in principle amend or dilute the floor collectively, and do not — evidence that the authoring seat's net-benefit judgment still holds, and the strongest single datum separating this arrangement from a pure extraction structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows. Detainees, conflict-zone civilians, humanitarian organizations, and ratifying states' armed forces are declared beneficiaries: low derived d, with the detainees' trapped exit locking their protection in place. Security services and accountability-exposed governments are declared payers: high derived d, with constrained exit keeping them near the full-target end — they cannot withdraw from customary coverage, and denunciation costs exceed any government's appetite. The ratifying-state-parties seat is dual-positioned (agenda_setter with beneficiary secondary): derivation places it low-d through its beneficiary declaration, but its payer-side exposure (litigation, conditionality, compliance cost) is real. No directionality_overrides are authored: the schema's override granularity is per power atom, and this story holds five institutionally-powered seats whose true d values diverge widely — flattening them to one override value would corrupt more than it corrects. The one nuance the derivation misses — security services' reciprocal returns (their own captured personnel fall under adversaries' mirror obligations) — is carried as an omega instead of an override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — no law reaching captives in internal conflict — is live, corroborated by documentation apparatuses outside the benefiting parties, so the mismatch consumer finds status=live paired with verdict=world_rearranges: no zombie flag, no mandatrophy. The classification's preventive work runs in both directions: this arrangement is chronically mislabeled from both sides. Narrated as pure moral achievement, it escapes scrutiny of its selective enforcement and growing performative layer; narrated as pure hypocrisy, it erases the genuine coordination function that keeps custody floors in doctrine even where practice fails. The hybrid reading keeps both facts load-bearing. The theater series (0.10 to 0.45) is the leading indicator worth watching: if condemnation output continues substituting for consequence delivery, the arrangement drifts toward a maintained-performance regime whose floor survives in text while dying in custody — a mandatrophy-shaped future the current status=live verdict does not yet warrant.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This story instantiates the absolute_prohibition reading of the humane_treatment_standard kernel. Would instantiating the contextual_necessity or proportionality_balancing reading instead change the constraint''s structural classification?',
    'Classify the sibling stories (humane_treatment_standard__contextual_necessity, humane_treatment_standard__proportionality_balancing) from their own structural data and compare per-seat outputs; track which reading governs actual custody doctrine in state practice.',
    'Under contextual_necessity, detainees drop out of the full rights-holder set during declared security emergencies and the burden redistributes toward captive populations; under proportionality_balancing the threshold becomes tradable case-by-case and enforcement migrates from categorical prosecution to weighting exercises. Either sibling would move the paying seat''s composition and the epsilon profile.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Kernel-reading contingency: this classification is indexed to the absolute_prohibition reading of the humane_treatment_standard kernel.').

omega_variable(
    non_reciprocity_durability,
    'The floor''s protections are owed unconditionally — detached from adversary reciprocity. Does that non-reciprocal structure survive sustained great-power non-compliance, or does practice decay toward reciprocal-only restraint?',
    'Longitudinal comparison of custody-abuse rates and monitor access across conflicts where adversaries reciprocate versus conflicts where one party openly repudiates the floor.',
    'Decay toward reciprocity-only would shrink the protected set to the exchangeable and strand detainees of non-reciprocating parties, raising effective extraction on the unprotected and weakening the coordination claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_reciprocity_durability, empirical, 'Whether unconditional (non-reciprocal) protection survives open defection by powerful parties.').

omega_variable(
    selective_enforcement_stability,
    'Enforcement visibly spares powerful states and concentrates on weaker or defeated parties. Is that asymmetry a correctable pathology or a stable structural feature of a state-authored enforcement regime?',
    'Compare prosecution and sanction rates against violator power across the interval; test whether accountability ever reaches permanent Security Council members or their clients.',
    'If stable, the arrangement''s costs concentrate on compliant mid-powers while the strongest pay only performance costs — pushing the paying seats'' computed experience toward pure extraction and eroding the coordination claim''s credibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selective_enforcement_stability, empirical, 'Whether enforcement selectivity by violator power is structural or correctable.').

omega_variable(
    jus_cogens_reversibility,
    'Is the absolute core a settled peremptory norm of the legal order — a structural feature no state can contract out of — or a constructed rule kept in place by continuous enforcement that persistent contrary practice could erode?',
    'Track customary-law formation: whether contrary state practice accretes with opinio juris, or whether violations continue to be framed as breaches rather than new law.',
    'If the core behaves as an emergent structural limit, the arrangement drifts mountain-ward and enforcement theater becomes irrelevant to its persistence; if it is enforcement-dependent, the theater and suppression trajectories dominate its fate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(jus_cogens_reversibility, conceptual, 'Natural-law-versus-constructed ambiguity of the absolute core: emergent legal-structural limit or enforcement-maintained rule.').

omega_variable(
    covert_program_undercount,
    'Resistance and violation rates are measured from documented cases; clandestine programs are systematically undercounted. How much does hidden practice exceed the documented record?',
    'Triangulate declassified program disclosures, forensic exhumations, and monitor-access gaps against official denial rates.',
    'A large covert excess would raise true resistance above the authored 0.65 and imply the suppression series understates the enforcement burden the arrangement actually requires.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(covert_program_undercount, empirical, 'Documentation-gap uncertainty in the resistance and suppression measurements.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(humane_treatment_standard__absolute_prohibition, 1949, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t1949, humane_treatment_standard__absolute_prohibition, theater_ratio, 1949, 0.1).
narrative_ontology:measurement(huma_tr_t1960, humane_treatment_standard__absolute_prohibition, theater_ratio, 1960, 0.14).
narrative_ontology:measurement(huma_tr_t1970, humane_treatment_standard__absolute_prohibition, theater_ratio, 1970, 0.17).
narrative_ontology:measurement(huma_tr_t1980, humane_treatment_standard__absolute_prohibition, theater_ratio, 1980, 0.22).
narrative_ontology:measurement(huma_tr_t1990, humane_treatment_standard__absolute_prohibition, theater_ratio, 1990, 0.28).
narrative_ontology:measurement(huma_tr_t2000, humane_treatment_standard__absolute_prohibition, theater_ratio, 2000, 0.32).
narrative_ontology:measurement(huma_tr_t2006, humane_treatment_standard__absolute_prohibition, theater_ratio, 2006, 0.4).
narrative_ontology:measurement(huma_tr_t2014, humane_treatment_standard__absolute_prohibition, theater_ratio, 2014, 0.42).
narrative_ontology:measurement(huma_tr_t2025, humane_treatment_standard__absolute_prohibition, theater_ratio, 2025, 0.45).

% Extraction over time
narrative_ontology:measurement(huma_be_t1949, humane_treatment_standard__absolute_prohibition, base_extractiveness, 1949, 0.18).
narrative_ontology:measurement(huma_be_t1960, humane_treatment_standard__absolute_prohibition, base_extractiveness, 1960, 0.21).
narrative_ontology:measurement(huma_be_t1970, humane_treatment_standard__absolute_prohibition, base_extractiveness, 1970, 0.24).
narrative_ontology:measurement(huma_be_t1980, humane_treatment_standard__absolute_prohibition, base_extractiveness, 1980, 0.27).
narrative_ontology:measurement(huma_be_t1990, humane_treatment_standard__absolute_prohibition, base_extractiveness, 1990, 0.31).
narrative_ontology:measurement(huma_be_t2000, humane_treatment_standard__absolute_prohibition, base_extractiveness, 2000, 0.34).
narrative_ontology:measurement(huma_be_t2006, humane_treatment_standard__absolute_prohibition, base_extractiveness, 2006, 0.39).
narrative_ontology:measurement(huma_be_t2014, humane_treatment_standard__absolute_prohibition, base_extractiveness, 2014, 0.37).
narrative_ontology:measurement(huma_be_t2025, humane_treatment_standard__absolute_prohibition, base_extractiveness, 2025, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t1949, humane_treatment_standard__absolute_prohibition, suppression_requirement, 1949, 0.15).
narrative_ontology:measurement(huma_su_t1960, humane_treatment_standard__absolute_prohibition, suppression_requirement, 1960, 0.18).
narrative_ontology:measurement(huma_su_t1970, humane_treatment_standard__absolute_prohibition, suppression_requirement, 1970, 0.22).
narrative_ontology:measurement(huma_su_t1980, humane_treatment_standard__absolute_prohibition, suppression_requirement, 1980, 0.27).
narrative_ontology:measurement(huma_su_t1990, humane_treatment_standard__absolute_prohibition, suppression_requirement, 1990, 0.35).
narrative_ontology:measurement(huma_su_t2000, humane_treatment_standard__absolute_prohibition, suppression_requirement, 2000, 0.45).
narrative_ontology:measurement(huma_su_t2006, humane_treatment_standard__absolute_prohibition, suppression_requirement, 2006, 0.58).
narrative_ontology:measurement(huma_su_t2014, humane_treatment_standard__absolute_prohibition, suppression_requirement, 2014, 0.62).
narrative_ontology:measurement(huma_su_t2025, humane_treatment_standard__absolute_prohibition, suppression_requirement, 2025, 0.66).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(humane_treatment_standard__absolute_prohibition, enforcement_mechanism).
narrative_ontology:affects_constraint(humane_treatment_standard__absolute_prohibition, humane_treatment_standard__contextual_necessity).
narrative_ontology:affects_constraint(humane_treatment_standard__absolute_prohibition, humane_treatment_standard__proportionality_balancing).

% DUAL FORMULATION NOTE:
% Constraint family: the label 'humane treatment standard' decomposes into three readings of one kernel. This file (absolute_prohibition) is the upstream member — the entrenched reference the other two define themselves against; its epsilon (0.35) reflects an arrangement whose costs fall on security apparatuses and whose benefits are unconditional. The contextual_necessity sibling relocates extraction onto captive populations during declared emergencies (higher epsilon for the detained seat); the proportionality_balancing sibling renders the threshold tradable (intermediate epsilon, enforcement shifted from categorical prosecution to weighting). Family edges run from this file to both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
