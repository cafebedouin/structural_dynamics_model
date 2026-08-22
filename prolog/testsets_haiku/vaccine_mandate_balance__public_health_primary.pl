% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_balance__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_balance__public_health_primary, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: vaccine_mandate_balance__public_health_primary
 *   human_readable: Vaccine Mandate Balance: Public Health Primacy Reading
 *   domain: public_health/constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The vaccine mandate balance is a contested kernel instantiated here as
 *   the PUBLIC_HEALTH_PRIMARY reading: collective protection of vulnerable
 *   populations supersedes individual consent when voluntary vaccination
 *   fails to achieve herd immunity and immunocompromised, newborn, and
 *   medically fragile populations face lethal exposure risk. This reading
 *   prioritizes the consent-overriding necessity of protecting the
 *   immunologically powerless. The reading's core claim: when a public health
 *   emergency is defined by lethal externality risk to vulnerable
 *   populations, coerced vaccination of the general population is justified
 *   authority exercised in exigency, not extractive coercion. This is one of
 *   three contested readings of the vaccine mandate kernel; the other
 *   readings (bodily_autonomy_primary and proportionality_reading) reject or
 *   constrain this authority claim differently.
 *
 * KEY AGENTS:
 *   - Immunocompromised populations: powerless, trapped, lethal exposure — no voluntary protection option available.
 *   - Public health authority: institutional agenda-setter, enforces herd immunity mandate via employment/school/travel restrictions.
 *   - Unvaccinated coerced: moderate power, constrained exit (comply, lose livelihood, or relocate), face employment loss and social exclusion.
 *   - Vaccine-injured denied exemption: moderate power, constrained exit, medically contraindicated but overridden by collective necessity.
 *   - Civil liberties advocates: excluded from policy framing by the reading's adoption; bodily autonomy claim structurally silenced.
 *   - Voluntary unvaccinated: dual-role payers (coerced to comply) who may simultaneously benefit from herd immunity protection.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_balance__public_health_primary, 0.68).
domain_priors:suppression_score(vaccine_mandate_balance__public_health_primary, 0.71).
domain_priors:theater_ratio(vaccine_mandate_balance__public_health_primary, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_balance__public_health_primary, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_balance__public_health_primary, "Vaccine Mandate Balance: Public Health Primacy Reading").
narrative_ontology:topic_domain(vaccine_mandate_balance__public_health_primary, "public_health/constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(vaccine_mandate_balance__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_balance__public_health_primary, '639028a1-98f3-4568-9b45-b8d40344eef6').
narrative_ontology:cs_kernel_codification('639028a1-98f3-4568-9b45-b8d40344eef6', distributed).
narrative_ontology:cs_authority_grounding('639028a1-98f3-4568-9b45-b8d40344eef6', extraction).
narrative_ontology:cs_interpretation_layer_present('639028a1-98f3-4568-9b45-b8d40344eef6').
narrative_ontology:cs_reading_relation('639028a1-98f3-4568-9b45-b8d40344eef6', vaccine_mandate_balance__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('639028a1-98f3-4568-9b45-b8d40344eef6', vaccine_mandate_balance__proportionality_reading, influences).
narrative_ontology:cs_axiom('639028a1-98f3-4568-9b45-b8d40344eef6', foundational, collective_necessity_overrides_consent).
narrative_ontology:cs_axiom_status(collective_necessity_overrides_consent, holdable).
narrative_ontology:cs_axiom_grounding('639028a1-98f3-4568-9b45-b8d40344eef6', collective_necessity_overrides_consent, deontological).
narrative_ontology:cs_axiom('639028a1-98f3-4568-9b45-b8d40344eef6', secondary, vulnerable_population_protection_supreme).
narrative_ontology:cs_axiom_status(vulnerable_population_protection_supreme, holdable).
narrative_ontology:cs_axiom_grounding('639028a1-98f3-4568-9b45-b8d40344eef6', vulnerable_population_protection_supreme, deontological).
narrative_ontology:cs_reference_frame('639028a1-98f3-4568-9b45-b8d40344eef6', voluntary_vaccination_consensus).
narrative_ontology:cs_drift_state('639028a1-98f3-4568-9b45-b8d40344eef6', endemic_disease_phase_with_undervaccination, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('639028a1-98f3-4568-9b45-b8d40344eef6', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_balance__public_health_primary, vaccine_mandate_balance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, immunocompromised_populations).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, newborn_infants).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, medically_fragile_dependents).
narrative_ontology:constraint_victim(vaccine_mandate_balance__public_health_primary, unvaccinated_coerced).
narrative_ontology:constraint_victim(vaccine_mandate_balance__public_health_primary, vaccine_injured_denied_exemption).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, voluntary_unvaccinated).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, medical_professionals_administering).
narrative_ontology:constraint_victim(vaccine_mandate_balance__public_health_primary, voluntary_unvaccinated).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Cannot mount immune response to vaccines themselves; depend entirely on community vaccination for protection. Face lethal exposure if vaccination rates fall below herd immunity threshold. Their survival is unconditional on behavior — they cannot consent or refuse their own protection, only receive it through others' compliance. No exit options.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, immunocompromised_populations, beneficiary,
    powerless, biographical, trapped, national).

% Too young for vaccination; depend on maternal antibodies and community protection during the critical window before their own immune systems mature. Represent the most vulnerable cohort: they cannot refuse protection, only receive it. Death from vaccine-preventable disease in this group is among the highest case-fatality rates.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, newborn_infants, beneficiary,
    powerless, immediate, trapped, national).

% Children with severe allergies, chronic conditions, or immune dysregulation; vaccination poses direct medical risk. They depend on high community vaccination rates for protection while remaining unable to access the vaccine themselves. Their medical fragility is permanent; they cannot relocate or change their vulnerability.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, medically_fragile_dependents, beneficiary,
    powerless, biographical, trapped, national).

% Face employment loss, school exclusion, or travel restriction if mandates are enforced. Their consent is overridden by collective protection logic. The mandate reading treats this overriding as justified necessity (not victims from the reading's own lights, but they bear the extraction cost). Exit options: comply, lose livelihood, or migrate to non-compliant jurisdictions — all costly.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, unvaccinated_coerced, payer,
    moderate, biographical, constrained, national).

% Have documented serious adverse reaction to vaccine; medically contraindicated but denied exemption because aggregate compliance is prioritized over individual medical exception. Bear the extraction cost of mandatory vaccination despite medical unsuitability. Exit: seek alternative jurisdiction, accept adverse event risk, or litigate — constrained mobility and high cost.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, vaccine_injured_denied_exemption, payer,
    moderate, biographical, constrained, national).

% Sets the vaccination mandate policy, enforcement mechanisms, and exemption criteria. Justifies coercion as necessary to protect the vulnerable. Operates under epidemiological necessity doctrine: when voluntary compliance fails to reach herd immunity threshold, mandate is authorized. Enforces through regulatory authority, licensing boards, and school exclusion rules.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, public_health_authority, agenda_setter,
    institutional, generational, analytical, national).

% Refuse vaccination on personal, religious, or philosophical grounds. Under this reading, their refusal is subordinated to collective necessity — they are not treated as inviolable agents but as carriers of externality risk. They may benefit from herd immunity itself (if unvaccinated but protected by others' vaccination) while being coerced to comply. Dual-role: bear enforcement cost while potentially receiving benefit.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, voluntary_unvaccinated, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_balance__public_health_primary, voluntary_unvaccinated, beneficiary).

% Enforced to participate in mandate implementation: administering vaccines, documenting compliance, and processing exemptions. They gain institutional legitimacy and reduced clinical risk from herd immunity but bear the ethical burden of enforcing coercion on reluctant patients. Constrained by licensure and employment; cannot refuse participation.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, medical_professionals_administering, beneficiary,
    organized, biographical, constrained, national).

% Would object to subordination of individual consent; excluded from the core policy framing because their framework (bodily autonomy primacy) contradicts the public-health-primary reading's foundational axiom. Their voice is structurally silenced by the reading's adoption; litigants but not deliberators.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, civil_liberties_advocates, excluded,
    moderate, biographical, constrained, national).

% Non-agent entity: the empirical findings that herd immunity thresholds exist and that vaccination rates below threshold leave vulnerable populations at lethal risk. This is the evidentiary anchor; it is not a seat or decision-maker but the referent claim the reading's authority grounds itself on.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, epidemiological_scientific_consensus, observer,
    analytical, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(vaccine_mandate_balance__public_health_primary, epidemiological_scientific_consensus).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_balance__public_health_primary, public_health_authority).
narrative_ontology:fixing_cost_class(vaccine_mandate_balance__public_health_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of vaccine-preventable disease transmission: voluntary vaccination is undersupplied because the individual benefit of vaccination (reduced personal disease risk) underweights the collective benefit (herd immunity protecting the vulnerable). The mandate forces coordination onto the herd-immunity equilibrium that voluntary choice cannot reach; it shifts the provision of a pure public good (population-level protection of immunocompromised and medically fragile cohorts) from undersupply to necessity-justified demand.
% TRANSFER_FUNCTION: Moves bodily autonomy and informed consent from the unvaccinated to the state authority enforcing the mandate, justified as necessary to transfer protection (freedom from lethal infection exposure) to immunocompromised and medically fragile populations. Also transfers moral status priority: vulnerable populations' protection claim supersedes reluctant agents' autonomy claim within the reading's framework. The mandate also transfers medical risk: vaccine-injured individuals denied exemptions bear both direct injury risk and the coercive cost of the mandate.
% ABSENT_VOICES: Civil liberties advocates, bodily-autonomy-primary readers, and philosophical libertarians are structurally excluded by the reading's adoption. Vaccine-hesitant populations and their organized advocates are present but subordinated — their objections are heard but not integrated into the policy framework because the reading's foundational axiom (collective necessity can override consent) rejects their autonomy claim as overridable. Families of vaccine-injured individuals face barriers to exemption and are excluded from proportionality deliberation. Unvaccinated individuals are affected but have no deliberative standing in the reading's own framing (they are treated as externality sources, not stakeholders with standing).
% DISAPPEARANCE_RATIONALE: If the public-health-primary reading and its mandate enforcement vanished overnight, voluntary vaccination would drop below herd immunity thresholds in most jurisdictions within weeks. Disease transmission to immunocompromised populations would accelerate. Newborn infants would face increased pertussis and measles exposure. Medically fragile dependents would lose the high-vaccination-rate protection they depend on. Healthcare systems would face outbreaks in vulnerable cohorts. The mandate's disappearance would directly cause lethal exposure to populations it was built to protect, forcing reorganization of disease prevention strategy. The world would have to re-solve the herd immunity problem via alternative mechanisms (targeted vaccination of high-transmission contacts, isolated protection of the vulnerable, rapid treatment protocols, or acceptance of endemic disease burden in vulnerable populations).
% FOUNDING_PROBLEM: Measles, pertussis, COVID-19, and other vaccine-preventable diseases generate negative externalities: an unvaccinated person transmits disease to immunocompromised individuals who cannot be vaccinated themselves, to newborn infants too young for vaccination, and to medically fragile dependents contraindicated for vaccination. These vulnerable populations face lethal infection exposure — case-fatality rates for newborn pertussis and measles are among the highest in medicine. Voluntary vaccination undersupplies herd immunity because individual benefit (reduced personal disease risk) diverges from collective benefit (herd immunity protecting those who cannot vaccinate).
% FOUNDING_PROBLEM_CORROBORATION: Epidemiological research from public health agencies (CDC, WHO, peer-reviewed surveillance studies) confirms that vaccine-preventable disease outbreaks occur in undervaccinated populations and are lethal for immunocompromised and medically fragile cohorts — corroboration from outside the mandate-enforcing beneficiary set. The reading's opponents (bodily-autonomy advocates) contest that the founding problem is as severe as claimed, that the mandate is the only solution, and that consent subordination is justified even if the problem is real. This contest is precisely why three readings exist: they dispute whether the founding problem justifies the reading's authority claim. The founding problem itself (that vulnerable populations exist and are at lethal risk without vaccination) is not deeply contested; the contest is whether that fact authorizes mandatory vaccination of others.
narrative_ontology:disappearance_verdict(vaccine_mandate_balance__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_balance__public_health_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_balance__public_health_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vaccine_mandate_balance__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_balance__public_health_primary, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_balance__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_balance__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vaccine_mandate_balance__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.68 at interval end: the mandate extracts consent and bodily autonomy from the unvaccinated, justified by lethal externality to vulnerable populations. The extraction is asymmetric — unvaccinated-coerced and vaccine-injured-denied-exemption bear costs without collecting benefits, while immunocompromised and newborn populations collect protection without having opted into or maintained vaccination. Suppression is 0.71 at interval end: enforcement is active and escalating (measured as suppression_requirement, rising from 0.38 to 0.71), enforced through employment loss, school exclusion, and travel restriction. Theater ratio is 0.22 at interval end: low-to-moderate theatricality means core function (herd immunity protection of the vulnerable) is real, not purely performative, though enforcement mechanisms accumulate some rhetorical padding. Accessibility_collapse is 0.78: alternatives to vaccination narrow sharply once the mandate is adopted — exit options compress to geographic relocation or medical exemption litigation. Resistance is 0.72 at interval end: substantial active resistance from unvaccinated-coerced and civil liberties advocates; resistance persists despite suppression, indicating the constraint is neither natural law nor overwhelmingly accepted. The measurement series models escalating enforcement (suppression_requirement rises 0.38→0.71) and extraction accumulation (base_extractiveness rises 0.42→0.68) over the interval, consistent with a mandate lifecycle from voluntary encouragement to active coercion. One shared time grid; all metrics authored at all six time points.
 *
 * PERSPECTIVAL GAP:
 *   From the public health authority's seat, the mandate is justified necessity: voluntary vaccination undersupplies herd immunity, leaving vulnerable populations (immunocompromised, newborn, medically fragile) at lethal risk. The constraint is read as pure coordination — solving a collective-action problem with a genuine public good (herd immunity). From the unvaccinated-coerced seat, the mandate is enforced extraction: bodily autonomy is taken without consent, justified by reference to others' needs. From the civil liberties seat, the reading embeds a false moral premise (consent can be overridden by externality risk), which should have been contestable but is structurally excluded by the reading's adoption. The engine computes these divergences from power/exit/beneficiary data: the authority has institutional power and analytical exit (can revise policy); unvaccinated-coerced have moderate power and constrained exit (exit costs employment/livelihood); immunocompromised have powerless status and trapped exit (no alternative protection exists). Different power atoms and exit options drive different computed types.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: immunocompromised populations (d→0.0, full beneficiary: they receive protection, bear no consent cost, cannot exit), newborn infants (d→0.0, trapped powerless beneficiaries), medically fragile dependents (d→0.0, protection is their only survival option). Victims: unvaccinated-coerced (d→1.0, full target: consent overridden, employment/social cost, constrained exit), vaccine-injured-denied-exemption (d→0.95, near-full target: medical contraindication overridden by collective necessity, trapped by medical unsuitability). Public health authority (d→0.3, near-beneficiary: collects institutional authority legitimacy, administers without coercion on itself, analytical exit). Voluntary-unvaccinated dual-role agents (d→0.55, symmetric: bear coercion cost but may receive herd immunity benefit; this dual status is structurally the root of the contest — whether dual benefit-and-burden agents have standing to object). The directionality derivation chain: beneficiary/victim data fixes the raw d values; exit options modulate (trapped immunocompromised remain beneficiary even though they receive by compulsion, not choice; identity-locked agents would shift — none explicitly identity-locked in this reading). No directionality overrides needed; the structural derivation matches the reading's own claim.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is prevented here by the presence of a live coordinate benefit (herd immunity protection of the vulnerable) that cannot be decoupled from the extraction cost (consent subordination). The founding problem (vaccine-preventable diseases pose lethal risk to immunocompromised and medically fragile) remains live and unsolved by voluntary vaccination alone — the mandate persists because its founding function (protecting the vulnerable) is still necessary. If the founding problem were dead (vaccine-preventable diseases eradicated or fully sequestered), the mandate would become extractive without coordination function, and mandatrophy declaration would be warranted. The constraint avoids mandatrophy death because the vulnerable populations remain vulnerable and herd immunity remains the only protection route they have. However, the reading is contestable precisely on this point: bodily-autonomy-primary and proportionality-reading deny that the founding problem justifies this magnitude of consent subordination, or they argue the problem has been substantially mitigated by risk reduction and alternative isolation strategies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    herd_immunity_threshold_empirical_contest,
    'What is the precise herd immunity threshold for COVID-19, measles, and other vaccine-preventable diseases in the actual epidemiological landscape, and what vaccination rate is necessary and sufficient to protect immunocompromised populations?',
    'Controlled epidemiological studies in diverse populations; surveillance data from high-vaccination jurisdictions; modeling of breakthrough infection risk stratified by immunocompromised cohort and vaccine efficacy.',
    'If the threshold is substantially lower than current mandate targets (e.g., 60% vs. 90%), lower extraction is needed to achieve the founding problem''s solution, and the reading''s justification for coercion weakens. If the threshold is confirmed at current mandates or higher, the extraction level is justified by the founding necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(herd_immunity_threshold_empirical_contest, empirical, 'Empirical uncertainty about the herd immunity threshold that actually protects vulnerable populations.').

omega_variable(
    alternative_protection_mechanisms,
    'Are there alternative mechanisms to achieve herd immunity protection of vulnerable populations without subordinating individual consent? (e.g., targeted vaccination of high-transmission contacts, isolated protection of the immunocompromised, rapid treatment protocols)',
    'Implementation and comparative outcome analysis of alternative strategies in pilot jurisdictions; modeling of efficacy; cost-benefit and rights-impact analysis vs. universal mandate.',
    'If alternatives exist and are comparably effective with lower extraction cost, the reading''s justification for subordinating consent is weakened; the constraint would become a choice among multiple solutions, not a necessity-justified singular remedy. The reading claims necessity; the existence of effective alternatives would undermine that claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_protection_mechanisms, conceptual, 'Whether the mandate is the only effective solution or one among multiple possible protections for the vulnerable.').

omega_variable(
    consent_subordination_framework_applicability,
    'Does the logic of consent subordination (collective necessity overrides individual autonomy) apply universally, or only under specific conditions? If specific, are those conditions met in this case?',
    'Philosophical and constitutional analysis: is there a defensible boundary between justified emergency coercion and tyranny of the majority? Application to this case: is COVID-19 / measles emergence a constitutional emergency that meets that boundary, or a chronic public health problem that does not?',
    'If consent subordination is defensible only in true emergencies (imminent mass death, minutes-to-hours timeframe), the public-health-primary reading applies only to a narrow class of diseases and periods. If it applies to chronic endemic disease with predictable yearly burden, the reading''s scope expands, but the constraint approaches pure extraction (the founding necessity persists indefinitely, extraction becomes permanent and normalized). This shapes whether the reading claims exigency or restructuring.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consent_subordination_framework_applicability, conceptual, 'Philosophical boundary between justified emergency coercion and illegitimate authority overreach in the name of collective benefit.').

omega_variable(
    vaccine_injury_undercount_vs_overcount,
    'What is the true causal incidence of serious vaccine injury? Are serious adverse events underreported, overestimated, or approximately accurate in current surveillance?',
    'Epidemiological passive/active surveillance comparison; independent database audits; causality assessment methodologies applied uniformly across vaccine injury and alternative disease harm.',
    'If serious vaccine injury is substantially underestimated, the extraction cost on vaccine-injured-denied-exemption and the general population''s informed risk assessment is higher than claimed; exemption refusal becomes less justified. If overestimated (injury rates lower than feared), the reading''s extraction cost is lower and consent subordination is more justified. This directly affects the reading''s empirical foundation and the legitimacy of denying medical exemptions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vaccine_injury_undercount_vs_overcount, empirical, 'True frequency of serious vaccine-attributable adverse events and accuracy of current surveillance systems.').

omega_variable(
    kernel_reading_forecast_attractor,
    'If the public-health-primary reading is adopted and the vaccine mandate persists indefinitely (founding problem remains live), what is the terminal attractor state? Will exemption criteria expand (mandate erodes) or extraction mechanisms harden (mandate becomes permanent coercion)?',
    'Forecast from sibling reading adoption patterns in other policy domains (e.g., surveillance, conscription, taxation): do exigency-framed constraints typically ratchet toward permanent extraction or relax toward negotiated exemptions? Path-dependence analysis of past mandate lifecycles.',
    'If the attractor is hardening (permanent extraction), the public-health-primary reading''s adoption commits the system to indefinite consent subordination, making the long-term classification snare (pure extraction justified by eternally-live founding problem). If the attractor is exemption expansion (mandates eventually relax), the reading is sustainable as tangled_rope (coordination + extraction) only if exemptions are robust enough to reduce victim concentration. This shapes the long-term type and mandatrophy risk.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_forecast_attractor, conceptual, 'Institutional trajectory of the public-health-primary reading under indefinite founding problem persistence: ratcheting or relaxation.').

omega_variable(
    committer_framing_alternative_premises,
    'The public-health-primary reading grounds authority in ''collective necessity overrides individual consent when vulnerable populations face lethal externality risk.'' Is this foundational axiom itself contestable within a coherent constitutional framework, or does adoption of this reading logically foreclose bodily_autonomy_primary?',
    'Constitutional and philosophical analysis: can a legal system hold both ''collective necessity can override consent'' and ''individual bodily autonomy is inviolable'' simultaneously, or does accepting one logically exclude the other? This determines whether the readings coexist_with or forecloses each other.',
    'If they logically foreclose each other (a constitutional system cannot hold both without incoherence), the reading_relation should be ''forecloses'' rather than ''coexists_with''. If they can coexist (different parties hold different readings within the same system without logical incoherence), the relation is ''coexists_with''. This affects how the readings are classified as institutional framings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_framing_alternative_premises, conceptual, 'Whether the public-health-primary reading logically forecloses bodily autonomy primacy or whether both readings can coexist in a coherent framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_balance__public_health_primary, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_balance__public_health_primary, theater_ratio, 0, 0.08).
narrative_ontology:measurement(vacc_tr_t5, vaccine_mandate_balance__public_health_primary, theater_ratio, 5, 0.12).
narrative_ontology:measurement(vacc_tr_t10, vaccine_mandate_balance__public_health_primary, theater_ratio, 10, 0.16).
narrative_ontology:measurement(vacc_tr_t15, vaccine_mandate_balance__public_health_primary, theater_ratio, 15, 0.2).
narrative_ontology:measurement(vacc_tr_t20, vaccine_mandate_balance__public_health_primary, theater_ratio, 20, 0.22).
narrative_ontology:measurement(vacc_tr_t25, vaccine_mandate_balance__public_health_primary, theater_ratio, 25, 0.22).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_balance__public_health_primary, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(vacc_be_t5, vaccine_mandate_balance__public_health_primary, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(vacc_be_t10, vaccine_mandate_balance__public_health_primary, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(vacc_be_t15, vaccine_mandate_balance__public_health_primary, base_extractiveness, 15, 0.66).
narrative_ontology:measurement(vacc_be_t20, vaccine_mandate_balance__public_health_primary, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(vacc_be_t25, vaccine_mandate_balance__public_health_primary, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_balance__public_health_primary, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(vacc_su_t5, vaccine_mandate_balance__public_health_primary, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(vacc_su_t10, vaccine_mandate_balance__public_health_primary, suppression_requirement, 10, 0.64).
narrative_ontology:measurement(vacc_su_t15, vaccine_mandate_balance__public_health_primary, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(vacc_su_t20, vaccine_mandate_balance__public_health_primary, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(vacc_su_t25, vaccine_mandate_balance__public_health_primary, suppression_requirement, 25, 0.71).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=25
narrative_ontology:measurement(vacc_grid_01, vaccine_mandate_balance__public_health_primary, accessibility_collapse(class), 0, 0.38).
narrative_ontology:measurement(vacc_grid_02, vaccine_mandate_balance__public_health_primary, accessibility_collapse(class), 25, 0.76).
narrative_ontology:measurement(vacc_grid_03, vaccine_mandate_balance__public_health_primary, accessibility_collapse(individual), 0, 0.35).
narrative_ontology:measurement(vacc_grid_04, vaccine_mandate_balance__public_health_primary, accessibility_collapse(individual), 25, 0.78).
narrative_ontology:measurement(vacc_grid_05, vaccine_mandate_balance__public_health_primary, accessibility_collapse(organizational), 0, 0.42).
narrative_ontology:measurement(vacc_grid_06, vaccine_mandate_balance__public_health_primary, accessibility_collapse(organizational), 25, 0.81).
narrative_ontology:measurement(vacc_grid_07, vaccine_mandate_balance__public_health_primary, accessibility_collapse(structural), 0, 0.45).
narrative_ontology:measurement(vacc_grid_08, vaccine_mandate_balance__public_health_primary, accessibility_collapse(structural), 25, 0.8).
narrative_ontology:measurement(vacc_grid_09, vaccine_mandate_balance__public_health_primary, resistance(class), 0, 0.72).
narrative_ontology:measurement(vacc_grid_10, vaccine_mandate_balance__public_health_primary, resistance(class), 25, 0.7).
narrative_ontology:measurement(vacc_grid_11, vaccine_mandate_balance__public_health_primary, resistance(individual), 0, 0.65).
narrative_ontology:measurement(vacc_grid_12, vaccine_mandate_balance__public_health_primary, resistance(individual), 25, 0.58).
narrative_ontology:measurement(vacc_grid_13, vaccine_mandate_balance__public_health_primary, resistance(organizational), 0, 0.68).
narrative_ontology:measurement(vacc_grid_14, vaccine_mandate_balance__public_health_primary, resistance(organizational), 25, 0.62).
narrative_ontology:measurement(vacc_grid_15, vaccine_mandate_balance__public_health_primary, resistance(structural), 0, 0.58).
narrative_ontology:measurement(vacc_grid_16, vaccine_mandate_balance__public_health_primary, resistance(structural), 25, 0.48).
narrative_ontology:measurement(vacc_grid_17, vaccine_mandate_balance__public_health_primary, stakes_inflation(class), 0, 0.25).
narrative_ontology:measurement(vacc_grid_18, vaccine_mandate_balance__public_health_primary, stakes_inflation(class), 25, 0.62).
narrative_ontology:measurement(vacc_grid_19, vaccine_mandate_balance__public_health_primary, stakes_inflation(individual), 0, 0.22).
narrative_ontology:measurement(vacc_grid_20, vaccine_mandate_balance__public_health_primary, stakes_inflation(individual), 25, 0.68).
narrative_ontology:measurement(vacc_grid_21, vaccine_mandate_balance__public_health_primary, stakes_inflation(organizational), 0, 0.28).
narrative_ontology:measurement(vacc_grid_22, vaccine_mandate_balance__public_health_primary, stakes_inflation(organizational), 25, 0.65).
narrative_ontology:measurement(vacc_grid_23, vaccine_mandate_balance__public_health_primary, stakes_inflation(structural), 0, 0.32).
narrative_ontology:measurement(vacc_grid_24, vaccine_mandate_balance__public_health_primary, stakes_inflation(structural), 25, 0.7).
narrative_ontology:measurement(vacc_grid_25, vaccine_mandate_balance__public_health_primary, suppression(class), 0, 0.35).
narrative_ontology:measurement(vacc_grid_26, vaccine_mandate_balance__public_health_primary, suppression(class), 25, 0.68).
narrative_ontology:measurement(vacc_grid_27, vaccine_mandate_balance__public_health_primary, suppression(individual), 0, 0.32).
narrative_ontology:measurement(vacc_grid_28, vaccine_mandate_balance__public_health_primary, suppression(individual), 25, 0.72).
narrative_ontology:measurement(vacc_grid_29, vaccine_mandate_balance__public_health_primary, suppression(organizational), 0, 0.38).
narrative_ontology:measurement(vacc_grid_30, vaccine_mandate_balance__public_health_primary, suppression(organizational), 25, 0.7).
narrative_ontology:measurement(vacc_grid_31, vaccine_mandate_balance__public_health_primary, suppression(structural), 0, 0.42).
narrative_ontology:measurement(vacc_grid_32, vaccine_mandate_balance__public_health_primary, suppression(structural), 25, 0.74).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_balance__public_health_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(vaccine_mandate_balance__public_health_primary, 0.18).
narrative_ontology:affects_constraint(vaccine_mandate_balance__public_health_primary, vaccine_mandate_balance__bodily_autonomy_primary).
narrative_ontology:affects_constraint(vaccine_mandate_balance__public_health_primary, vaccine_mandate_balance__proportionality_reading).

% DUAL FORMULATION NOTE:
% The vaccine mandate balance kernel constrains three readings: public_health_primary (this story, ε=0.68, tangled_rope, collective necessity supreme), bodily_autonomy_primary (ε estimated 0.35-0.42, mountain or rope, individual consent inviolable), and proportionality_reading (ε estimated 0.48-0.55, tangled_rope with robust exemptions). The readings share the same referent (whether mandatory vaccination can be justified by public health exigency) but author different ε values because they dispute whether collective protection of vulnerable populations justifies consent subordination. Each reading is a separate constraint story instantiating a different framing of the kernel. Public-health-primary affects both siblings: it establishes the legitimacy of the collective-necessity framing, which bodily-autonomy-primary rejects and proportionality-reading constrains.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vaccine_mandate_balance__public_health_primary, organized, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
