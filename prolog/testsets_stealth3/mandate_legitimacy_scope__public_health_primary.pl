% ============================================================================
% CONSTRAINT STORY: mandate_legitimacy_scope__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mandate_legitimacy_scope__public_health_primary, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: mandate_legitimacy_scope__public_health_primary
 *   human_readable: Voluntarist Vaccination Settlement as Assessed by the Public-Health-Primary Reading
 *   domain: public_health_ethics/constitutional_law
 *
 * SUMMARY:
 *   This story classifies the standing vaccination-governance arrangement —
 *   compulsion legally available in principle but practically fenced off by
 *   broad personal-belief and religious exemptions, easy school-entry
 *   opt-outs, and a well-organized exemption-preserving coalition — as
 *   assessed by the public_health_primary reading of the
 *   mandate_legitimacy_scope kernel. Through this reading's lights, the
 *   settlement's costs land on those who cannot vaccinate: immunocompromised
 *   patients and infants too young for the series depend on neighbors' uptake
 *   for protection they cannot generate, and the settlement lets others
 *   decline that contribution at will. The arrangement retains a genuine
 *   coordination achievement (voluntary cooperation at scale, consent norms
 *   honored) while transferring serious-harm risk onto the unprotectable —
 *   hence the tangled_rope claim. Per the epsilon-invariance principle, the
 *   colloquial mandate-legitimacy debate is decomposed into a constraint
 *   family; this file is the public_health_primary member, and the sibling
 *   readings are separate stories with their own epsilon referents. KEY
 *   AGENTS (by structural relationship): - state_legislatures: Agenda-setter
 *   (institutional/constrained) — writes the exemption rules; electorally
 *   bound - state_health_departments: Administering agenda-setter
 *   (institutional/constrained) — runs the machinery inside those rules -
 *   anti_mandate_advocacy_networks: Organized beneficiary with agenda-setting
 *   secondary role (organized/arbitrage) — collects revenue and relevance
 *   from maintaining the settlement - conscientious_refusers: Beneficiary
 *   (moderate/mobile) — captures the private benefit of declination,
 *   reversible at will - refusal_identity_communities: Identity-locked
 *   beneficiary (moderate/identity_locked) — declination fused with belonging
 *   - immunocompromised_patients: Primary target (powerless/trapped) — bears
 *   the risk they cannot shed - infants_too_young_to_vaccinate: Primary
 *   target (powerless/trapped) — highest complication rates, zero voice -
 *   pro_mandate_parent_majority: Excluded voice (organized/constrained) —
 *   majority preference with no channel into design - constitutional_courts:
 *   Analytical observer (institutional/analytical) — adjudicates the
 *   boundary, collects nothing
 *
 * KEY AGENTS:
 *   - state_legislatures: agenda-setter seat; authors exemption breadth; constrained by electorate
 *   - state_health_departments: administering agenda-setter; absorbs outbreak response costs
 *   - anti_mandate_advocacy_networks: organized beneficiary and co-agenda-setter; arbitrage-grade mobility across jurisdictions
 *   - conscientious_refusers: mobile beneficiary; declination reversible at negligible cost
 *   - refusal_identity_communities: identity-locked beneficiary; exit costs membership itself
 *   - immunocompromised_patients: trapped payer; depends on others' uptake for survival margin
 *   - infants_too_young_to_vaccinate: trapped payer; no voice, highest severity
 *   - pro_mandate_parent_majority: excluded seat; overridden majority preference
 *   - constitutional_courts: observer seat; sets boundary conditions, collects nothing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mandate_legitimacy_scope__public_health_primary, 0.72).
domain_priors:suppression_score(mandate_legitimacy_scope__public_health_primary, 0.32).
domain_priors:theater_ratio(mandate_legitimacy_scope__public_health_primary, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, extractiveness, 0.72).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mandate_legitimacy_scope__public_health_primary, tangled_rope).
narrative_ontology:human_readable(mandate_legitimacy_scope__public_health_primary, "Voluntarist Vaccination Settlement as Assessed by the Public-Health-Primary Reading").
narrative_ontology:topic_domain(mandate_legitimacy_scope__public_health_primary, "public_health_ethics/constitutional_law").

domain_priors:requires_active_enforcement(mandate_legitimacy_scope__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(mandate_legitimacy_scope__public_health_primary, '41cab49c-6a73-4679-bec4-96aaacfae297').
narrative_ontology:cs_kernel_codification('41cab49c-6a73-4679-bec4-96aaacfae297', fixed_text).
narrative_ontology:cs_authority_grounding('41cab49c-6a73-4679-bec4-96aaacfae297', lineage).
narrative_ontology:cs_interpretation_layer_present('41cab49c-6a73-4679-bec4-96aaacfae297').
narrative_ontology:cs_reading_relation('41cab49c-6a73-4679-bec4-96aaacfae297', mandate_legitimacy_scope__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('41cab49c-6a73-4679-bec4-96aaacfae297', mandate_legitimacy_scope__proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('41cab49c-6a73-4679-bec4-96aaacfae297', foundational, necessity_licenses_compulsion_for_vulnerable_protection).
narrative_ontology:cs_axiom_status(necessity_licenses_compulsion_for_vulnerable_protection, holdable).
narrative_ontology:cs_axiom_grounding('41cab49c-6a73-4679-bec4-96aaacfae297', necessity_licenses_compulsion_for_vulnerable_protection, deontological).
narrative_ontology:cs_axiom('41cab49c-6a73-4679-bec4-96aaacfae297', secondary, refusal_carries_duty_to_protect_others).
narrative_ontology:cs_axiom_status(refusal_carries_duty_to_protect_others, holdable).
narrative_ontology:cs_axiom_grounding('41cab49c-6a73-4679-bec4-96aaacfae297', refusal_carries_duty_to_protect_others, deontological).
narrative_ontology:cs_reference_frame('41cab49c-6a73-4679-bec4-96aaacfae297', collective_protection_baseline).
narrative_ontology:cs_drift_state('41cab49c-6a73-4679-bec4-96aaacfae297', contemporary_post_exemption_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('41cab49c-6a73-4679-bec4-96aaacfae297', '').
narrative_ontology:cs_kernel_id(mandate_legitimacy_scope__public_health_primary, mandate_legitimacy_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__public_health_primary, conscientious_refusers).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__public_health_primary, refusal_identity_communities).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__public_health_primary, anti_mandate_advocacy_networks).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__public_health_primary, immunocompromised_patients).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__public_health_primary, infants_too_young_to_vaccinate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Author and amend the school-entry and workplace vaccination statutes, including the breadth of personal-belief and religious exemptions, thereby setting the terms under which compulsion is available. Electoral dependence on constituents who object to compulsion limits how far they can tighten the rules; the few that eliminated non-medical exemptions absorbed recall campaigns and primary challenges as the cost.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, state_legislatures, agenda_setter,
    institutional, generational, constrained, national).

% Administer school-entry checks, process exemption paperwork, run education campaigns, and lead outbreak containment when clusters ignite. They operate inside the exemption rules the legislature writes, cannot close gaps on their own authority, and divert staff and budget from routine programs each time an outbreak demands response.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, state_health_departments, agenda_setter,
    institutional, generational, constrained, national).

% Organize opposition to compulsion, circulate model exemption legislation, litigate against mandate orders, and fundraise on each outbreak-controversy cycle. Membership, revenue, and political relevance depend on exemption routes remaining open; when one jurisdiction narrows them, resources shift to friendlier ones.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, anti_mandate_advocacy_networks, beneficiary,
    organized, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(mandate_legitimacy_scope__public_health_primary, anti_mandate_advocacy_networks, agenda_setter).

% Decline routine vaccination for personal-belief reasons while living inside communities whose general uptake keeps disease incidence low. They avoid the risks and inconvenience they associate with vaccination and can obtain vaccination at any clinic visit if rules or incentives change; for most, the decision is reversible at negligible cost.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, conscientious_refusers, beneficiary,
    moderate, biographical, mobile, national).

% Insular religious and cultural enclaves in which declining vaccination is bound up with belonging, distrust of outside institutions, and communal identity. Vaccinating against the community norm carries social and familial costs members experience as loss of standing; leaving the practice and leaving the community are the same move.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, refusal_identity_communities, beneficiary,
    moderate, biographical, identity_locked, regional).

% Live with transplanted organs, chemotherapy, immune disorders, or medication regimens that make vaccination ineffective or unsafe. They depend on the vaccination of people around them for protection they cannot generate, cannot relocate away from exposure for the duration of their condition, and bear hospitalization and death risk when local coverage sags.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, immunocompromised_patients, payer,
    powerless, biographical, trapped, global).

% Are below the age of first-series completion and depend entirely on surrounding coverage. They have no voice in exemption policy, no ability to avoid daycare, household, or clinical exposure, and bear the highest complication rates of any group when outbreaks reach them.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, infants_too_young_to_vaccinate, payer,
    powerless, immediate, trapped, global).

% Poll consistently in favor of school-entry vaccination requirements but have no organized channel through which that preference enters exemption-statute design. Their children attend schools housing exemption clusters, and their objections surface mainly as hearing testimony where the statutory outcome is already fixed.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, pro_mandate_parent_majority, excluded,
    organized, biographical, constrained, national).

% Adjudicate challenges to both mandates and exemption restrictions, drawing on the century-old police-power lineage that upheld compulsion and on newer doctrinal attention to religious liberty and bodily integrity. They set the boundary conditions within which the other seats operate and collect nothing from the outcome.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(mandate_legitimacy_scope__public_health_primary, conscientious_refusers).
narrative_ontology:fixing_cost_class(mandate_legitimacy_scope__public_health_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Achieves widespread community immunity without coercive confrontation: voluntary programs, school-entry rules with opt-outs, and public education coordinate vaccination decisions at scale while sparing legislatures the political cost of compulsion and preserving a consent norm in medicine.
% TRANSFER_FUNCTION: Moves disease risk from those who decline vaccination onto those who cannot be vaccinated; moves the political conflict over compulsion away from statute books and onto hospitals, patients, and outbreak responders; moves money and organizational relevance to advocacy networks on each outbreak cycle.
% ABSENT_VOICES: The pro-mandate parent majority is legislatively overridden with no organized channel into exemption design; immunocompromised patients hold no formal seat in rulemaking that sets their exposure; infants hold none at all. The seats with the most at stake in the coverage level are the least represented in setting it.
% DISAPPEARANCE_RATIONALE: If the exemption-heavy settlement vanished overnight and compulsion became the default for school and workplace entry, coverage would rise in the clusters where it currently sags, outbreak frequency and size would fall, refuser households would face comply-or-exclude decisions, advocacy networks would lose their organizing grievance and funding cycle, and health departments would redirect outbreak-response capacity to routine care.
% FOUNDING_PROBLEM: Mid-century vaccination success erased disease memory while post-Nuremberg and post-Tuskegee medicine hardened consent norms; the settlement was built to keep voluntary cooperation high and backlash low by accommodating sincere objection rather than compelling it.
% FOUNDING_PROBLEM_CORROBORATION: Defenders of the settlement attest the founding bargain is still live, citing durable voluntary uptake and backlash risk. Outside the benefiting parties, historical epidemiology documents coverage levels maintained under far narrower exemptions, court opinions in the Jacobson lineage affirm compulsion's availability, and CDC and WHO outbreak investigations attribute recent resurgences to exemption clusters — corroborating the reading that the protective problem the bargain traded against has returned.
narrative_ontology:disappearance_verdict(mandate_legitimacy_scope__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(mandate_legitimacy_scope__public_health_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(mandate_legitimacy_scope__public_health_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(mandate_legitimacy_scope__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(mandate_legitimacy_scope__public_health_primary, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mandate_legitimacy_scope__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(mandate_legitimacy_scope__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(mandate_legitimacy_scope__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72) because the settlement decouples declination's private benefit from its social cost and the residual risk lands on parties who cannot decline it — the reading's own assessment of the mandate-absent arrangement. Suppression is comparatively low (0.32): the settlement coerces almost no one directly; its force is permissive, operating by keeping the compulsory alternative politically unavailable rather than by punishing participants. Theater is moderate-low (0.28): education campaigns and exemption paperwork mix real function with ritual. Accessibility_collapse (0.55) is blended: for the vulnerable, self-protection alternatives are nearly fully collapsed (shielding is partial, costly, and indefinite), while for refusers alternatives abound — the average conceals a bimodal reality. Resistance (0.62) is real: mandate legislation, exemption-closure ballots, and litigation press against the settlement continuously. The temporal series runs on one shared six-point grid (T=0 approximates 1990, the start of the exemption-liberalization wave; T=30 approximates 2020, the COVID-era stress test) so every tracked metric is authored at every examined point. Base_extractiveness rises with exemption-cluster growth and recurrent outbreaks (the 2014-15 and 2019 measles resurgences produce step increases smoothed into the series). Suppression_requirement rises steeply (0.25 to 0.55) while applied suppression stays flat: this tracks the professionalization of the settlement's defense — model legislation, litigation shops, exemption-expansion bills — an enforcement ratchet for the voluntarist arrangement itself. Coalition note: the payer seats are individually powerless but not without coalition potential; patient-advocacy organizations have moved exemption policy in several jurisdictions, which is why resistance is authored above what raw patient power alone would suggest.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute divergently. From the immunocompromised patient's position the settlement is lethal risk imposition they cannot exit; from the conscientious refuser's position it is an ordinary, reversible personal choice exercised inside a safe neighborhood; from the legislature's position it is constituency management between a silent majority and a loud minority; from the court's position it is a doctrinal line between police power and bodily integrity. Same structure, four different experienced types. The engine computes this divergence from the structural data — power, exit, and role — and the divergence itself is the finding.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the refuser seats toward the low-d (subsidized) end: they capture protection without contributing and can leave the position cheaply (mobile) or only at the price of community (identity_locked). Victim declarations drive the immunocompromised and infant seats to the high-d (full-target) end, amplified by trapped exit — they cannot vaccinate, relocate indefinitely, or opt out of exposure. The advocacy networks collect revenue and relevance, placing them near the beneficiary end despite their agenda-setting labor. Legislatures and health departments sit near symmetric: they administer without materially collecting. One override is authored: moderate-power agents are pinned to d=0.15 because the automatic derivation would read refusal_identity_communities' identity_locked exit as target-position lock and drag their d upward — but the lock binds them INTO the benefiting position (exit would cost them the benefit's identity substrate), not out of a burden. The override corrects that specific derivation error for the moderate class, which contains only the two refuser seats in this story.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — keeping voluntary cooperation high and backlash low after disease memory faded, under hardened consent norms — is contested rather than dead: cooperation remains high nationally, but the jurisdictions where the bargain's accommodation ran furthest are precisely where the protective problem returned. The classification prevents mislabeling in both directions. Calling the settlement a rope would erase the identifiable victims whose risk funds the refusers' private benefit; calling it a snare would erase the genuine coordination achievement that makes the arrangement resilient and widely accepted — voluntary uptake really does coordinate, and the settlement is not pretending otherwise. Tangled_rope holds both halves: real coordination function, real asymmetric extraction, active enforcement required to hold the exemption architecture against continuous reform pressure. The R5 mismatch consumer should note the tension between the contested founding status and the world_rearranges verdict: the arrangement's disappearance would rearrange substantial machinery, which is evidence the settlement is doing active work — the question the corpus exists to take is whether that work is worth its victims, which no single seat answers alone.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This story is one reading — public_health_primary — of the mandate_legitimacy_scope kernel; the sibling readings (bodily_autonomy_primary, proportionality_reading) instantiate different constraints over different referents. Where exactly is the disagreement located, and what would a sibling change?',
    'No internal resolution: the disagreement sits in the priority ordering between collective protection and individual consent, which fixes each reading''s epsilon referent. The corpus resolves it by keeping the readings as separate linked stories rather than averaging them.',
    'Merging the readings into one story would make epsilon observable-dependent and violate epsilon-invariance; a sibling adoption would move the victim set (the compelled, under bodily_autonomy_primary) and re-key epsilon to a different arrangement entirely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame record: this constraint is the public_health_primary reading of the mandate_legitimacy_scope kernel; siblings are separate files.').

omega_variable(
    constructed_vs_equilibrium_settlement,
    'Is the exemption-heavy settlement a stable equilibrium of genuine consent culture, or a constructed arrangement maintained by organized advocacy against revealed majority preference?',
    'Cross-jurisdiction comparison of coverage and outbreak outcomes before and after exemption closure, controlling for baseline attitudes.',
    'If constructed, extraction attribution strengthens against the maintaining coalition and the agenda-setter seats; if equilibrium, part of the measured cost is the price of stable voluntary cooperation rather than imposed harm.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constructed_vs_equilibrium_settlement, empirical, 'Whether the standing arrangement reflects preference or organized maintenance.').

omega_variable(
    refusal_vs_barrier_attribution,
    'How much of the coverage shortfall this reading attributes to refusal is actually access barrier — cost, geography, scheduling, work inflexibility — misread as choice?',
    'Cluster-level uptake audits separating barrier-driven under-vaccination from belief-driven declination, using outreach-response data.',
    'If barriers dominate, the duty-to-protect framing misfires against the wrong population, the victim set shifts toward the barrier-burdened, and the beneficiary declaration on refusers over-collects.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(refusal_vs_barrier_attribution, empirical, 'Attribution of under-vaccination between choice and access.').

omega_variable(
    disease_severity_contingency,
    'This reading''s epsilon presumes measles-class seriousness (high R0, severe complications). For milder vaccine-preventable diseases, does the same settlement compute the same way?',
    'Decompose per disease class into separate constraint stories with their own epsilon, victim sets, and measurements, linked by network edges — the epsilon-invariance procedure.',
    'Family expansion: per-disease stories would show the settlement ranging from sharply costly (measles-class) to near-benign (mild-class), preventing a single epsilon from smuggling severity assumptions across diseases.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disease_severity_contingency, conceptual, 'Severity contingency of the reading''s cost assessment across diseases.').

omega_variable(
    refusal_identity_lock_mechanism,
    'Is the persistence of refusal_identity_communities driven by internalized identity fusion or by structural communal sanction?',
    'Post-exit trajectory studies of members who leave enclaves: if low-uptake beliefs persist after exit, the lock is internalized; if uptake follows immediately, it was structural.',
    'Informs how reliable the identity_locked exit atom is for these seats, and whether coverage interventions should target rule change or relational insulation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(refusal_identity_lock_mechanism, empirical, 'Structural versus internalized mechanism sustaining enclave refusal.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mandate_legitimacy_scope__public_health_primary, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mand_tr_t0, mandate_legitimacy_scope__public_health_primary, theater_ratio, 0, 0.18).
narrative_ontology:measurement(mand_tr_t6, mandate_legitimacy_scope__public_health_primary, theater_ratio, 6, 0.2).
narrative_ontology:measurement(mand_tr_t12, mandate_legitimacy_scope__public_health_primary, theater_ratio, 12, 0.23).
narrative_ontology:measurement(mand_tr_t18, mandate_legitimacy_scope__public_health_primary, theater_ratio, 18, 0.25).
narrative_ontology:measurement(mand_tr_t24, mandate_legitimacy_scope__public_health_primary, theater_ratio, 24, 0.27).
narrative_ontology:measurement(mand_tr_t30, mandate_legitimacy_scope__public_health_primary, theater_ratio, 30, 0.28).

% Extraction over time
narrative_ontology:measurement(mand_be_t0, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(mand_be_t6, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 6, 0.6).
narrative_ontology:measurement(mand_be_t12, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 12, 0.64).
narrative_ontology:measurement(mand_be_t18, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 18, 0.68).
narrative_ontology:measurement(mand_be_t24, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 24, 0.7).
narrative_ontology:measurement(mand_be_t30, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 30, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(mand_su_t0, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(mand_su_t6, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 6, 0.31).
narrative_ontology:measurement(mand_su_t12, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 12, 0.37).
narrative_ontology:measurement(mand_su_t18, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 18, 0.44).
narrative_ontology:measurement(mand_su_t24, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 24, 0.5).
narrative_ontology:measurement(mand_su_t30, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 30, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mandate_legitimacy_scope__public_health_primary, resource_allocation).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__public_health_primary, mandate_legitimacy_scope__bodily_autonomy_primary).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__public_health_primary, mandate_legitimacy_scope__proportionality_reading).

% DUAL FORMULATION NOTE:
% The colloquial question 'are vaccine mandates legitimate?' decomposes into three structurally distinct constraints sharing the mandate_legitimacy_scope kernel. This story (public_health_primary) authors epsilon over the mandate-absent arrangement — the exemption-heavy voluntarist settlement — and finds it highly costly to the unprotectable. The bodily_autonomy_primary sibling authors epsilon over the mandate itself as a violation of bodily integrity. The proportionality_reading sibling authors epsilon conditionally, per disease-severity/safety/alternative pairing. Same label, different referents, different victim sets; hence separate stories linked here rather than one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(mandate_legitimacy_scope__public_health_primary, moderate, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
