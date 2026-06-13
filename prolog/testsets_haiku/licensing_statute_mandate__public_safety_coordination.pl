% ============================================================================
% CONSTRAINT STORY: licensing_statute_mandate__public_safety_coordination
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_licensing_statute_mandate__public_safety_coordination, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: licensing_statute_mandate__public_safety_coordination
 *   human_readable: Statutory Credential Mandate for Public Safety Coordination
 *   domain: labor/regulatory
 *
 * SUMMARY:
 *   This constraint instantiates the public_safety_coordination reading of
 *   the licensing_statute_mandate kernel. The constraint is a statutory
 *   requirement that practitioners meet defined competence standards
 *   (education, examination, continuous training) before legally serving
 *   consumers. Under THIS reading, the mandate exists to solve a genuine
 *   market failure: information asymmetry in credence goods. Consumers cannot
 *   verify practitioner competence before or after purchase; incompetent
 *   practitioners harm consumers; unregulated markets do not filter them out
 *   because reputation signals lag harm and switching costs are high. The
 *   mandate creates a shared quality threshold that coordinates consumer
 *   expectations and practitioner signaling. This reading coexists with two
 *   sibling readings: the rent_seeking_suppression reading (same statute,
 *   read as labor supply restriction for incumbent benefit) and the
 *   graduated_access_filter reading (same statute, read as differential
 *   barrier maintenance by class). This story author the
 *   public_safety_coordination reading as a clean, ε-invariant constraint —
 *   the reading's own structural claim about what the mandate is FOR and HOW
 *   it works. The metrics describe that reading's actual operation: moderate
 *   extractiveness (some rents captured by incumbents, some costs borne by
 *   excluded populations), moderate suppression (legal barriers with real
 *   force, but not totalizing), rising theater ratio (increasing share of
 *   enforcement energy defending credential inflation rather than consumer
 *   protection), and substantial accessibility collapse (alternatives barred
 *   by law).
 *
 * KEY AGENTS:
 *   - regulatory_licensing_authority: institutional, agenda-setter; administers the standard; derives authority from public mandate
 *   - consumers_requiring_service: organized, beneficiary; receive coordination benefit; bear slightly higher service costs
 *   - quality_assured_practitioners: powerful, beneficiary; gain market signaling and premium compensation; support the mandate; shape credential design
 *   - incompetent_or_unqualified_practitioners: powerless, victim; barred from market by design; essential to filtering function
 *   - excluded_populations_facing_barriers: powerless, victim; face differential barrier height; excluded from professional entry; side effect of mandate design
 *   - incumbent_profession_gatekeepers: organized, agenda-setter + beneficiary (dual); control credential design; benefit from supply restriction; incentive to inflate standard
 *   - alternative_credential_providers: moderate, excluded; barred by statutory monopoly; would offer competing signals but are legally inert
 *   - competition_authorities: institutional, observer; evaluate whether restriction exceeds legitimate safety function; can recommend reform
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(licensing_statute_mandate__public_safety_coordination, 0.38).
domain_priors:suppression_score(licensing_statute_mandate__public_safety_coordination, 0.42).
domain_priors:theater_ratio(licensing_statute_mandate__public_safety_coordination, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__public_safety_coordination, extractiveness, 0.38).
narrative_ontology:constraint_metric(licensing_statute_mandate__public_safety_coordination, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(licensing_statute_mandate__public_safety_coordination, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__public_safety_coordination, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(licensing_statute_mandate__public_safety_coordination, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(licensing_statute_mandate__public_safety_coordination, rope).
narrative_ontology:human_readable(licensing_statute_mandate__public_safety_coordination, "Statutory Credential Mandate for Public Safety Coordination").
narrative_ontology:topic_domain(licensing_statute_mandate__public_safety_coordination, "labor/regulatory").

domain_priors:requires_active_enforcement(licensing_statute_mandate__public_safety_coordination).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(licensing_statute_mandate__public_safety_coordination, 'fc18a97e-137c-4f5d-abf4-a9565425727e').
narrative_ontology:cs_kernel_codification('fc18a97e-137c-4f5d-abf4-a9565425727e', formalized).
narrative_ontology:cs_authority_grounding('fc18a97e-137c-4f5d-abf4-a9565425727e', extraction).
narrative_ontology:cs_interpretation_layer_present('fc18a97e-137c-4f5d-abf4-a9565425727e').
narrative_ontology:cs_reading_relation('fc18a97e-137c-4f5d-abf4-a9565425727e', licensing_statute_mandate__rent_seeking_suppression, coexists_with).
narrative_ontology:cs_reading_relation('fc18a97e-137c-4f5d-abf4-a9565425727e', licensing_statute_mandate__graduated_access_filter, influences).
narrative_ontology:cs_axiom('fc18a97e-137c-4f5d-abf4-a9565425727e', foundational, minimum_competence_prevents_consumer_harm).
narrative_ontology:cs_axiom_status(minimum_competence_prevents_consumer_harm, holdable).
narrative_ontology:cs_axiom_grounding('fc18a97e-137c-4f5d-abf4-a9565425727e', minimum_competence_prevents_consumer_harm, empirically_contingent).
narrative_ontology:cs_axiom('fc18a97e-137c-4f5d-abf4-a9565425727e', foundational, information_asymmetry_market_failure_in_credence_goods).
narrative_ontology:cs_axiom_status(information_asymmetry_market_failure_in_credence_goods, holdable).
narrative_ontology:cs_axiom_grounding('fc18a97e-137c-4f5d-abf4-a9565425727e', information_asymmetry_market_failure_in_credence_goods, empirically_contingent).
narrative_ontology:cs_reference_frame('fc18a97e-137c-4f5d-abf4-a9565425727e', market_with_asymmetric_information_failure).
narrative_ontology:cs_drift_state('fc18a97e-137c-4f5d-abf4-a9565425727e', contemporary_regulatory_capture_phase, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('fc18a97e-137c-4f5d-abf4-a9565425727e', '').
narrative_ontology:cs_kernel_id(licensing_statute_mandate__public_safety_coordination, licensing_statute_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__public_safety_coordination, consumer_protection_framework).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__public_safety_coordination, quality_assured_practitioners).
narrative_ontology:constraint_victim(licensing_statute_mandate__public_safety_coordination, incompetent_or_unqualified_practitioners).
narrative_ontology:constraint_victim(licensing_statute_mandate__public_safety_coordination, excluded_populations_facing_barriers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__public_safety_coordination, consumers_requiring_service).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__public_safety_coordination, incumbent_profession_gatekeepers).
narrative_ontology:constraint_vindicates(licensing_statute_mandate__public_safety_coordination, minimum_competence_prevents_consumer_harm).
narrative_ontology:constraint_vindicates(licensing_statute_mandate__public_safety_coordination, shared_quality_threshold_coordination_necessity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces the credential requirements (education, exam, continuing competence standards). Administers the licensing system, revokes licenses for incompetence or misconduct, maintains the credential registry. Justifies the mandate as preventing consumer harm from unqualified practitioners. Does not collect rents directly but derives institutional authority and budgetary support from the mandate's existence.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__public_safety_coordination, regulatory_licensing_authority, agenda_setter,
    institutional, generational, analytical, national).

% Receive the coordination benefit: assurance that any licensed practitioner meets a baseline competence threshold, reducing search costs and quality variance. They do not verify credentials themselves but rely on the licensing system's filtering function. They may face slightly higher service costs (licensing overhead passed through) but gain predictability and recourse if a licensed practitioner harms them.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__public_safety_coordination, consumers_requiring_service, beneficiary,
    organized, biographical, mobile, national).

% Practitioners who meet the credential standard benefit from market signaling: the license certifies their competence to consumers and employers, reducing information asymmetry. The credential becomes a credential good that commands premium compensation and customer trust. They support the mandate actively because it sets a floor below which competitors cannot undercut through quality abandonment.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__public_safety_coordination, quality_assured_practitioners, beneficiary,
    powerful, generational, arbitrage, national).

% Cannot legally practice without meeting the credential standard, even if they believe they have the skill. The mandate bars them from the market. They are the objects of the filtering mechanism: their exclusion is necessary for the coordination function to work. They bear the cost of being barred; they have no remedy, no appeal beyond the credential pathway itself.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__public_safety_coordination, incompetent_or_unqualified_practitioners, payer,
    powerless, biographical, trapped, national).

% Groups (low-income, racial/ethnic minorities, immigrants, rural populations, workers with non-standard education histories) face higher barriers to credential attainment due to cost, geographic distance to testing/training, prior credential non-recognition, or credential-inflation (the standard rises faster than the underlying competence requirement). They are victims not of incompetence filtering but of differential barrier height that the mandate's design enables.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__public_safety_coordination, excluded_populations_facing_barriers, payer,
    powerless, biographical, constrained, national).

% Professional associations and legacy credential holders who design or administer the credential standard. They benefit by controlling entry: the standard can be inflated beyond the minimum competence needed, restricting labor supply and raising incumbent compensation. They are often seat at the regulation table and influence credential design. The dual role captures that they set the standard AND benefit from it — a structural recursion.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__public_safety_coordination, incumbent_profession_gatekeepers, beneficiary,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(licensing_statute_mandate__public_safety_coordination, incumbent_profession_gatekeepers, agenda_setter).

% Evaluate whether the credential mandate serves legitimate consumer protection or functions as labor supply restriction. They investigate rent-seeking layered onto coordination and can recommend credential reform or prohibition if the restriction exceeds the legitimate safety function.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__public_safety_coordination, competition_authorities, observer,
    institutional, generational, analytical, national).

% Employers, educational institutions, or certification bodies outside the statutory system that could provide alternative credentials or competence signaling (apprenticeship, employer certification, blockchain-based skill badges, portfolio evidence). They are structurally barred from substituting for the statutory credential; the mandate makes their alternative signals legally inert. They would argue for credential pluralism but are excluded by the statutory system's legal monopoly.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__public_safety_coordination, alternative_credential_providers, excluded,
    moderate, generational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(licensing_statute_mandate__public_safety_coordination, incumbent_profession_gatekeepers).
narrative_ontology:fixing_cost_class(licensing_statute_mandate__public_safety_coordination, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a shared quality threshold that consumers and employers can rely on without individual verification of every practitioner's competence. Practitioners who meet the threshold credibly signal competence; consumers pay for this coordination benefit through slightly higher service costs (licensing overhead) but gain predictability and recourse. The threshold solves the market failure of information asymmetry in credence goods — services where quality is hard for consumers to verify even after purchase.
% TRANSFER_FUNCTION: Moves compliance costs (education, exams, licensing fees, continuing competence maintenance) from consumers to practitioners as the price of legal market access. Moves administrative and enforcement costs to the public budget or regulatory body. Moves market access itself from anyone willing to practice to only those who meet the credential standard. Moves a small fraction of the service fees to incumbent practitioners who benefit from supply restriction.
% ABSENT_VOICES: Alternative credential providers (employers, educational institutions, certification bodies outside the statutory system) would argue for credential pluralism and open recognition of equivalent signals but are barred by the statutory monopoly. Excluded populations facing differential barriers (low-income, racial/ethnic minorities, immigrants, workers with non-standard education histories) would argue for differential pathways, subsidies, or reciprocal recognition but are not typically represented in credential design. Consumers who would prefer lower-cost, lower-assurance services (trading off some quality certainty for affordability) are not organized into the regulatory table.
% DISAPPEARANCE_RATIONALE: If the statutory credential mandate disappeared overnight, unqualified practitioners would re-enter the market, service costs would likely fall (reduced legal restriction on labor supply), and consumer quality assurance would fragment into reputation systems, employer screening, alternative certifications, and portfolio-based signals. Some markets with high liability exposure (surgery, aviation, structural engineering) would likely rebuild credential systems (liability pressure would recreate filtering); others (personal services, trades) might stabilize into employer-managed quality signals and word-of-mouth reputation. The unified legal mandate is not inevitable; its disappearance is rearrangement of the market, not collapse.
% FOUNDING_PROBLEM: Consumers cannot verify practitioner competence before or even after purchase (credence goods problem); incompetent or negligent practitioners harm consumers; unregulated markets do not naturally filter incompetent practitioners because reputation signals lag harm and switching costs are high. The founding problem is the information asymmetry and market-failure dimension of labor markets for professional services.
% FOUNDING_PROBLEM_CORROBORATION: Economists studying credence goods (Darby & Karni, Dulleck & Kerschbamer meta-analyses) independently corroborate that information asymmetry is a genuine market failure in professional services. Malpractice litigation data and consumer protection agency records attest that incompetent practitioners cause documented harm that market reputation does not prevent. Health ministries, consumer affairs departments, and regulatory bodies outside the credentialing system independently attest to documented consumer harms in unregulated or weakly regulated markets. Incumbent practitioners' own professional associations initially argued for licensing as a consumer protection tool in historical legislative testimony (early 20th century), though they now benefit from it as a supply-restriction mechanism — the founding problem is attested from the defense as well.
narrative_ontology:disappearance_verdict(licensing_statute_mandate__public_safety_coordination, world_rearranges).
narrative_ontology:founding_problem_status(licensing_statute_mandate__public_safety_coordination, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(licensing_statute_mandate__public_safety_coordination, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(licensing_statute_mandate__public_safety_coordination, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(licensing_statute_mandate__public_safety_coordination_tests).
:- end_tests(licensing_statute_mandate__public_safety_coordination_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 at interval end) because the constraint solves a real coordination problem (information asymmetry in credence goods) but its implementation captures rents through supply restriction and credential inflation. The measurement series shows extractiveness rising from 0.28 to 0.37 over the first 20 time-units, then plateauing, consistent with credential standards accumulating restrictions (continuing education requirements rising, exam pass-rates tightening) until legal/political pushback stabilizes the system. Suppression (0.42 at end) is moderate: the credential requirement is legally enforced and bars practitioners, but the barrier is not totalizing — unqualified practitioners cannot legally practice, but excluded populations can theoretically attempt the credential pathway, unlike truly identity-locked or geographically trapped populations. Theater ratio rises from 0.12 to 0.28, consistent with the constraint's enforcement energy gradually shifting from protecting consumers (early focus) toward defending credential inflation and supply restriction (mature phase). Accessibility collapse (0.68) reflects the legal monopoly: alternatives are barred by statute, not by market pressure, so exits are heavily constricted. Resistance (0.55) is substantial: consumers and alternative providers push back; some unqualified practitioners attempt to practice illegally; competition authorities scrutinize the system; incumbent practitioners defend it fiercely. The claim/metric independence is preserved: the constraint is CLAIMED as rope (public safety coordination) while metrics describe moderate-extractive, actively-enforced operation — the engine's classification reads the metrics, and where the computed type diverges from the claimed rope, that divergence indicates the public-safety story is carrying hidden extraction.
 *
 * PERSPECTIVAL GAP:
 *   From the regulatory authority's seat, the constraint is genuine public-safety coordination — the mandate prevents consumer harm by filtering incompetent practitioners, and the authority's job is calibrating the filter's sensitivity. From the consumer's seat, the constraint provides coordination benefit (quality assurance) but also imposes diffuse costs (higher service fees, restricted access to low-cost alternatives). From the quality-assured practitioner's seat, the constraint is a market-signaling tool and a supply-restriction mechanism — they benefit from both, but their framing emphasizes the public-safety rationale because it provides political cover. From the excluded-population seat, the constraint is a barrier-maintenance mechanism: the public-safety framing is the legitimate rationale, but the ACTUAL barriers (cost of training, distance to testing, prior credential non-recognition, credential inflation beyond the minimum safety threshold) are not addressed by the coordination function — they are side effects of HOW the standard is designed and administered. From the incumbent-profession seat, the constraint is both a legitimate safety mechanism AND a supply-restriction tool, and these are intentionally conflated in advocacy — arguing for supply restriction would be politically unacceptable, so the supply-restriction argument is embedded in the public-safety framing. The engine computes these divergent directionalities from the structural data (beneficiary/victim, power, exit options, scope) and produces per-seat classification that reveals the perspectival gap: a payer seat might compute tangled_rope or snare while the agenda-setter seat computes rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Consumers (organized, mobile exit, national scope) are structural beneficiaries: they receive the coordination benefit (quality assurance without individual verification) at the cost of slightly higher service fees. Their directionality is near the beneficiary end (d ~ 0.2–0.3) because the constraint produces net benefits for them, though they pay diffusely. Quality-assured practitioners (powerful, arbitrage exit) are beneficiaries who actively defend the system: they gain market signaling and premium compensation from the credential good. Their directionality is beneficiary (d ~ 0.15–0.25) because they benefit without running the system (the regulatory authority runs it), though they influence its design. Regulatory authority (institutional, analytical exit) is the agenda-setter: it administers the credential, but it does not collect rents directly — its d is near symmetric (0.45–0.55) because it bears administrative costs and gains legitimacy from the mandate's public framing. Incompetent practitioners (powerless, trapped exit) and excluded populations (powerless, constrained exit) are the structural victims: they bear the mandate's filtering function — they are barred from market access. Their directionality is near the target end (d ~ 0.75–0.85) because the mandate's entire force bears on them as the objects of filtering. Incumbent profession gatekeepers (organized, arbitrage exit) have the most ambiguous directionality: they are listed as dual-positioned (beneficiary + agenda-setter) because they profit from supply restriction AND they shape the standard that creates that restriction. Their d would be beneficiary-low (0.20–0.30) from the profit side but shifted higher (0.40–0.50) by their control of the standard's inflation — the override captures this ambiguity.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not mandatrophic in the classical sense (mandate dead, function persists theatrically). Rather, the constraint shows EXTRACTION ACCUMULATION layered onto a live founding mandate. The founding problem (information asymmetry in credence goods) remains live — consumers still cannot verify practitioner competence, and unregulated markets still produce harms. But the extraction component (credential inflation, supply restriction, barrier maintenance) has grown over time as incumbent gatekeepers captured the standard-setting process. The theater_ratio rising from 0.12 to 0.28 captures this: early enforcement energy focused on consumer protection (low theater); mature enforcement energy defends credential inflation and alternative-provider exclusion (higher theater — enforcement actions that protect supply restriction, not consumers). The constraint is a HYBRID: genuine coordination with growing extraction. It is not rope-turning-to-snare (which would show rising extractiveness and declining consumer coordination benefit) — the coordination benefit persists, but extraction-per-unit-of-coordination is rising, indicating regulatory capture by incumbents. The founding mandate is live, but it has been hijacked for rent extraction. Classification implications: from a consumer or authority seat, this could compute as rope or tangled_rope (coordination + extraction, both active). From an excluded-population or alternative-provider seat, it computes as snare (pure extraction, no beneficiary story from their position). The Boltzmann floor for resource_allocation type is 0.15; the base extractiveness of 0.38 suggests ~0.23 of excess extraction above the coordination floor, consistent with regulatory capture. The T17 accumulation trigger should fire on this constraint: extraction rising from 0.28 to 0.37 (Phase A, early compliance phase) then stabilizing (Phase B, capture equilibrium) is a classic rent-accumulation pattern.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_capture_boundary,
    'How much of the measured extractiveness (0.38) represents legitimate coordination cost, and how much represents incumbent rent-seeking layered onto coordination?',
    'Decompose extractiveness by examining: (1) credential standards relative to demonstrable minimum competence for consumer protection (are continuing education hours beyond safety requirements?), (2) exam pass-rates and their trend (are they tightening beyond competence filtering?), (3) market impact (do service costs rise faster than inflation after the mandate?), (4) alternative-credential suppression (is enforcement energy concentrated on barring alternatives or on filtering incompetents?).',
    'If captured rent exceeds 0.15 of the extractiveness, the constraint should compute as tangled_rope or snare (coordination story is cover for extraction). If captured rent is under 0.08, the constraint is genuine rope (coordination cost exceeds extraction). The boundary sits around 0.12–0.15.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_capture_boundary, empirical, 'Whether the constraint''s extractiveness is primarily coordination cost or incumbent capture.').

omega_variable(
    excluded_populations_structural_vs_incidental,
    'Are the differential barriers faced by excluded populations (high education costs, geographic distance to testing, prior credential non-recognition) incidental effects of the mandate''s implementation, or structural features the mandate enables?',
    'Compare barrier profiles across jurisdictions with different credential designs (e.g., apprenticeship-heavy systems vs. exam-heavy systems) and across income groups within the same jurisdiction. If excluded populations face barriers disproportionate to the competence filtering rationale, the mandate is structurally enabling class sorting.',
    'If barriers are structural, the constraint functions as graduated_access_filter (sibling reading) alongside public_safety_coordination — the same statute serves both. If barriers are incidental and remediable through design (lower testing costs, remote exams, prior-credential reciprocity), the constraint remains pure public_safety_coordination but with design defects. This omega determines whether to author a separate graduated_access_filter constraint linked via network.affects_constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(excluded_populations_structural_vs_incidental, empirical, 'Whether differential barriers are incidental to the coordination function or structural class-sorting features.').

omega_variable(
    alternative_credential_viability,
    'Could alternative credentials (employer certification, apprenticeship, portfolio-based signaling, blockchain credentials) perform the same consumer-protection function as the statutory credential, if legal barriers were removed?',
    'Natural experiments from jurisdictions that permit alternative credentials or that phase statutory monopolies (EU mutual recognition directives, some US state licenses accepting apprenticeship in lieu of formal education). Examine consumer harm rates and labor-market outcomes under permissive vs. restrictive credential regimes.',
    'If alternatives could perform the function equally, the statutory monopoly is pure extraction (the coordination benefit is separable from the legal restriction). If statutory monopoly is necessary for the function, the barrier is legitimate coordination cost. This determines whether the constraint is rope (irreducible) or tangled_rope (the coordination COULD operate without the extraction, but doesn''t).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_credential_viability, conceptual, 'Whether the statutory monopoly is necessary for the coordination function or is pure extraction riding on coordination.').

omega_variable(
    incompetent_practitioner_harm_baseline,
    'What is the measurable consumer harm rate from incompetent or unqualified practitioners in unregulated markets or before statutory licensing? How does the harm rate compare to current (post-mandate) rates?',
    'Historical data from markets before licensure was introduced (some US states have pre-licensing records; some professions have data from unlicensed periods). Comparison to contemporary unregulated markets (e.g., unlicensed home services in jurisdictions without credential requirements, illegal practice in regulated jurisdictions). Malpractice litigation rates, consumer complaints, documented injuries.',
    'If baseline harm is high and post-mandate harm is low, the coordination function is real and valuable. If harm rates are similar pre- and post-mandate, the mandate is theater (legitimate rationale, minimal real effect, primarily extraction). If harm is reduced but not eliminated, the mandate is a partial solution (rope with some extractive overlay).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incompetent_practitioner_harm_baseline, empirical, 'Whether the mandate materially reduces consumer harm from incompetent practitioners.').

omega_variable(
    reading_contest_empirical_signature,
    'Which observable patterns would distinguish the public_safety_coordination reading from the rent_seeking_suppression sibling reading?',
    'Examine the credential system''s design choices: (1) Does the standard track minimum competence needed to prevent consumer harm, or does it inflate beyond that point? (2) Does enforcement focus on filtering incompetent practitioners, or on barring competitors and protecting incumbent supply? (3) Do continuing education requirements correlate with new competence-relevant knowledge, or with gatekeeping? (4) Is the system permissive to alternative credentials that provide equivalent consumer protection, or restrictive? These patterns distinguish the two readings empirically.',
    'This omega documents the committer frame (Kernels and Readings Rule 2): the public_safety_coordination and rent_seeking_suppression readings use the SAME statute but interpret it differently. The empirical signature of WHICH reading is operative shows up in enforcement behavior, credential inflation, and alternative-credential suppression, not in the statute''s text.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_empirical_signature, empirical, 'Empirical patterns that distinguish public-safety-coordination from rent-seeking-suppression reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(licensing_statute_mandate__public_safety_coordination, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lice_tr_t0, licensing_statute_mandate__public_safety_coordination, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(lice_tr_t0, observed).
narrative_ontology:measurement(lice_tr_t5, licensing_statute_mandate__public_safety_coordination, theater_ratio, 5, 0.16).
narrative_ontology:measurement_basis(lice_tr_t5, observed).
narrative_ontology:measurement(lice_tr_t10, licensing_statute_mandate__public_safety_coordination, theater_ratio, 10, 0.19).
narrative_ontology:measurement_basis(lice_tr_t10, observed).
narrative_ontology:measurement(lice_tr_t15, licensing_statute_mandate__public_safety_coordination, theater_ratio, 15, 0.22).
narrative_ontology:measurement_basis(lice_tr_t15, observed).
narrative_ontology:measurement(lice_tr_t20, licensing_statute_mandate__public_safety_coordination, theater_ratio, 20, 0.26).
narrative_ontology:measurement_basis(lice_tr_t20, observed).
narrative_ontology:measurement(lice_tr_t25, licensing_statute_mandate__public_safety_coordination, theater_ratio, 25, 0.27).
narrative_ontology:measurement_basis(lice_tr_t25, observed).
narrative_ontology:measurement(lice_tr_t30, licensing_statute_mandate__public_safety_coordination, theater_ratio, 30, 0.28).
narrative_ontology:measurement_basis(lice_tr_t30, observed).
narrative_ontology:measurement(lice_tr_t35, licensing_statute_mandate__public_safety_coordination, theater_ratio, 35, 0.28).
narrative_ontology:measurement_basis(lice_tr_t35, observed).
narrative_ontology:measurement(lice_tr_t40, licensing_statute_mandate__public_safety_coordination, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(lice_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(lice_be_t0, licensing_statute_mandate__public_safety_coordination, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(lice_be_t0, observed).
narrative_ontology:measurement(lice_be_t5, licensing_statute_mandate__public_safety_coordination, base_extractiveness, 5, 0.31).
narrative_ontology:measurement_basis(lice_be_t5, observed).
narrative_ontology:measurement(lice_be_t10, licensing_statute_mandate__public_safety_coordination, base_extractiveness, 10, 0.34).
narrative_ontology:measurement_basis(lice_be_t10, observed).
narrative_ontology:measurement(lice_be_t15, licensing_statute_mandate__public_safety_coordination, base_extractiveness, 15, 0.36).
narrative_ontology:measurement_basis(lice_be_t15, observed).
narrative_ontology:measurement(lice_be_t20, licensing_statute_mandate__public_safety_coordination, base_extractiveness, 20, 0.37).
narrative_ontology:measurement_basis(lice_be_t20, observed).
narrative_ontology:measurement(lice_be_t25, licensing_statute_mandate__public_safety_coordination, base_extractiveness, 25, 0.37).
narrative_ontology:measurement_basis(lice_be_t25, observed).
narrative_ontology:measurement(lice_be_t30, licensing_statute_mandate__public_safety_coordination, base_extractiveness, 30, 0.38).
narrative_ontology:measurement_basis(lice_be_t30, observed).
narrative_ontology:measurement(lice_be_t35, licensing_statute_mandate__public_safety_coordination, base_extractiveness, 35, 0.38).
narrative_ontology:measurement_basis(lice_be_t35, observed).
narrative_ontology:measurement(lice_be_t40, licensing_statute_mandate__public_safety_coordination, base_extractiveness, 40, 0.38).
narrative_ontology:measurement_basis(lice_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(lice_su_t0, licensing_statute_mandate__public_safety_coordination, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(lice_su_t0, observed).
narrative_ontology:measurement(lice_su_t5, licensing_statute_mandate__public_safety_coordination, suppression_requirement, 5, 0.3).
narrative_ontology:measurement_basis(lice_su_t5, observed).
narrative_ontology:measurement(lice_su_t10, licensing_statute_mandate__public_safety_coordination, suppression_requirement, 10, 0.34).
narrative_ontology:measurement_basis(lice_su_t10, observed).
narrative_ontology:measurement(lice_su_t15, licensing_statute_mandate__public_safety_coordination, suppression_requirement, 15, 0.37).
narrative_ontology:measurement_basis(lice_su_t15, observed).
narrative_ontology:measurement(lice_su_t20, licensing_statute_mandate__public_safety_coordination, suppression_requirement, 20, 0.4).
narrative_ontology:measurement_basis(lice_su_t20, observed).
narrative_ontology:measurement(lice_su_t25, licensing_statute_mandate__public_safety_coordination, suppression_requirement, 25, 0.41).
narrative_ontology:measurement_basis(lice_su_t25, observed).
narrative_ontology:measurement(lice_su_t30, licensing_statute_mandate__public_safety_coordination, suppression_requirement, 30, 0.42).
narrative_ontology:measurement_basis(lice_su_t30, observed).
narrative_ontology:measurement(lice_su_t35, licensing_statute_mandate__public_safety_coordination, suppression_requirement, 35, 0.42).
narrative_ontology:measurement_basis(lice_su_t35, observed).
narrative_ontology:measurement(lice_su_t40, licensing_statute_mandate__public_safety_coordination, suppression_requirement, 40, 0.42).
narrative_ontology:measurement_basis(lice_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(licensing_statute_mandate__public_safety_coordination, resource_allocation).
narrative_ontology:boltzmann_floor_override(licensing_statute_mandate__public_safety_coordination, 0.15).
narrative_ontology:affects_constraint(licensing_statute_mandate__public_safety_coordination, licensing_statute_mandate__rent_seeking_suppression).
narrative_ontology:affects_constraint(licensing_statute_mandate__public_safety_coordination, licensing_statute_mandate__graduated_access_filter).

% DUAL FORMULATION NOTE:
% The licensing_statute_mandate kernel has three structurally distinct readings: (1) public_safety_coordination (THIS constraint) — the statute solves information asymmetry in credence goods by creating a shared quality threshold; (2) rent_seeking_suppression — the statute restricts labor supply and extracts rents for incumbent practitioners under the guise of public safety; (3) graduated_access_filter — the statute creates differential barriers that sort by class and prior resource access. These three readings apply the SAME statutory text but have different ε values, different beneficiary/victim structures, and different classifications. The public_safety_coordination reading has moderate extractiveness (0.38) and is claimed as rope; the rent_seeking_suppression reading has high extractiveness (0.70+) and is claimed as snare; the graduated_access_filter reading has moderate-to-high extractiveness (0.55+) and is claimed as snare or tangled_rope. The three stories are linked via network.affects_constraints to reflect their kernel kinship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(licensing_statute_mandate__public_safety_coordination, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
