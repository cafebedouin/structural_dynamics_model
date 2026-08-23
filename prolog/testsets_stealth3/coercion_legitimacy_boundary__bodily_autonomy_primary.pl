% ============================================================================
% CONSTRAINT STORY: coercion_legitimacy_boundary__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coercion_legitimacy_boundary__bodily_autonomy_primary, []).

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
 *   constraint_id: coercion_legitimacy_boundary__bodily_autonomy_primary
 *   human_readable: Categorical Consent Boundary - Bodily Autonomy Primary Reading
 *   domain: public_health/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the coercion_legitimacy_boundary
 *   kernel: the bodily_autonomy_primary reading, under which no medical
 *   intervention may proceed without the individual's (or guardian's)
 *   affirmative consent, however large the collective payoff. The constraint
 *   binds public-health governance absolutely at the level of principle, is
 *   administered by courts, licensing boards, and research-ethics bodies,
 *   coordinates the trust that lets patients enter care and subjects enter
 *   trials, and - this is the structural delta this reading carries - leaves
 *   the infection residue of refusals resting on those who cannot vaccinate
 *   or respond, while relieving public agencies of the burden of running
 *   coercive campaigns. The epsilon referent is the standing arrangement this
 *   story is about: the categorical consent requirement as it actually
 *   operates (settled in clinical and research settings, contested at the
 *   population-health margin), assessed by this reading's own lights,
 *   including the costs its own operation generates for third parties. Family
 *   note: sibling readings (public_health_primary, proportionality_reading)
 *   are separate constraint files with their own epsilon, victim sets, and
 *   types; nothing about them enters this file's metrics. The claimed type
 *   and the metrics below are independently authored facts: I claim
 *   tangled_rope because the structure holds a genuine, load-bearing
 *   coordination function together with a real asymmetric cost borne by a
 *   class that cannot exit, and I author the metrics as descriptively true of
 *   that operation without tuning either to the other.
 *
 * KEY AGENTS:
 *   - immunocompromised_individuals: primary target (powerless/trapped) - bears the infection risk the boundary's non-enforcement leaves uninternalized
 *   - conscientious_objectors: primary beneficiary (moderate/constrained) - refusal never overridden; sheds the risk their refusal creates onto their communities
 *   - public_health_enforcement_agencies: incidental beneficiary with a payer side (institutional/constrained) - relieved of mandate enforcement duty, stripped of the mandate instrument
 *   - courts_and_ethics_bodies: agenda setter (institutional/constrained) - administers the consent boundary through constitutional review, licensure discipline, and protocol vetting
 *   - civil_liberties_institutions: secondary beneficiary (organized/identity_locked) - litigates to hold the boundary; organizationally fused with the cause
 *   - medical_professionals: dual-positioned (organized/constrained) - protected by the consent norm at work, absorbs surge costs of preventable outbreaks at the bedside
 *   - pandemic_preparedness_planners: excluded voice (organized/mobile) - compulsion-dependent playbooks ruled off-table, expertise uninvited
 *   - national_bioethics_commission: analytical observer (institutional/analytical) - audits the whole arrangement, collects nothing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.52).
domain_priors:suppression_score(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.35).
domain_priors:theater_ratio(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, extractiveness, 0.52).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coercion_legitimacy_boundary__bodily_autonomy_primary, tangled_rope).
narrative_ontology:human_readable(coercion_legitimacy_boundary__bodily_autonomy_primary, "Categorical Consent Boundary - Bodily Autonomy Primary Reading").
narrative_ontology:topic_domain(coercion_legitimacy_boundary__bodily_autonomy_primary, "public_health/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(coercion_legitimacy_boundary__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(coercion_legitimacy_boundary__bodily_autonomy_primary, '01ff124e-9654-4c93-91ed-91abffb1d298').
narrative_ontology:cs_kernel_codification('01ff124e-9654-4c93-91ed-91abffb1d298', distributed).
narrative_ontology:cs_authority_grounding('01ff124e-9654-4c93-91ed-91abffb1d298', lineage).
narrative_ontology:cs_interpretation_layer_present('01ff124e-9654-4c93-91ed-91abffb1d298').
narrative_ontology:cs_reading_relation('01ff124e-9654-4c93-91ed-91abffb1d298', coercion_legitimacy_boundary__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('01ff124e-9654-4c93-91ed-91abffb1d298', coercion_legitimacy_boundary__proportionation_reading_placeholder, forecloses).
narrative_ontology:cs_axiom('01ff124e-9654-4c93-91ed-91abffb1d298', foundational, bodily_integrity_requires_affirmative_consent).
narrative_ontology:cs_axiom_status(bodily_integrity_requires_affirmative_consent, holdable).
narrative_ontology:cs_axiom_grounding('01ff124e-9654-4c93-91ed-91abffb1d298', bodily_integrity_requires_affirmative_consent, deontological).
narrative_ontology:cs_axiom('01ff124e-9654-4c93-91ed-91abffb1d298', foundational, collective_benefit_never_overrides_refusal).
narrative_ontology:cs_axiom_status(collective_benefit_never_overrides_refusal, holdable).
narrative_ontology:cs_axiom_grounding('01ff124e-9654-4c93-91ed-91abffb1d298', collective_benefit_never_overrides_refusal, deontological).
narrative_ontology:cs_reference_frame('01ff124e-9654-4c93-91ed-91abffb1d298', absolute_consent_sovereignty).
narrative_ontology:cs_drift_state('01ff124e-9654-4c93-91ed-91abffb1d298', contemporary_mandate_contestation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('01ff124e-9654-4c93-91ed-91abffb1d298', '').
narrative_ontology:cs_kernel_id(coercion_legitimacy_boundary__bodily_autonomy_primary, coercion_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__bodily_autonomy_primary, conscientious_objectors).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__bodily_autonomy_primary, civil_liberties_institutions).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__bodily_autonomy_primary, public_health_enforcement_agencies).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__bodily_autonomy_primary, medical_professionals).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__bodily_autonomy_primary, immunocompromised_individuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__bodily_autonomy_primary, public_health_enforcement_agencies).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__bodily_autonomy_primary, medical_professionals).
narrative_ontology:constraint_vindicates(coercion_legitimacy_boundary__bodily_autonomy_primary, informed_consent_doctrine).
narrative_ontology:constraint_vindicates(coercion_legitimacy_boundary__bodily_autonomy_primary, bodily_integrity_as_fundamental_right).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live with leukemia, transplant immunosuppression, or antibody deficiencies that blunt vaccine response; some cannot receive certain vaccines at all. They rely on the immunity of the people around them to stay safe and cannot opt out of other people's choices. When local uptake slips, their infection risk rises with no compensating channel - they carry the residue of every refusal in their community, and no jurisdiction change reliably escapes the exposure.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, immunocompromised_individuals, payer,
    powerless, biographical, trapped, regional).

% Decline vaccination or treatment on religious, philosophical, or bodily-integrity grounds. Under this arrangement their refusal is final: no school-entry rule, employment condition, or emergency order overrides it. They carry social friction and, in outbreak pockets, elevated personal risk of their own, but no instrument of compulsion reaches them, and the risks their refusal creates for others rest elsewhere.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, conscientious_objectors, beneficiary,
    moderate, biographical, constrained, national).

% Run immunization, screening, and outbreak-response programs. The categorical rule lifts from them the duty to operate coercive campaigns - no compliance sweeps, no exclusion orders to defend in court, no political ownership of forced procedures. The same rule narrows their toolkit: where voluntary uptake stalls they must spend on persuasion, access, and treatment capacity instead of reaching for mandates, and they absorb criticism for outbreaks they lacked the instruments to prevent.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, public_health_enforcement_agencies, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(coercion_legitimacy_boundary__bodily_autonomy_primary, public_health_enforcement_agencies, payer).

% Constitutional benches, licensing boards, research-ethics committees, and malpractice courts administer the consent boundary: they strike compelled procedures, vet research protocols, and discipline practitioners who bypass consent. Holding the boundary consumes review capacity and draws recurring political fire; abandoning it would unsettle the doctrinal foundation their own authority rests on, so they maintain it within the interpretive room their mandates allow.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, courts_and_ethics_bodies, agenda_setter,
    institutional, generational, constrained, national).

% Rights-advocacy organizations whose modern public profile is built substantially on bodily-autonomy litigation. The categorical boundary is their doctrinal home ground; they challenge every proposal to carve collective-benefit exceptions into it. After a decade of reproductive-rights reversals their organizational identity fused with this terrain - pivoting away would mean disowning the cause that now defines them, so they litigate and mobilize for the boundary as such.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, civil_liberties_institutions, beneficiary,
    organized, generational, identity_locked, national).

% Physicians and nurses practice inside the consent norm: it shields them from liability, anchors patient trust, and gives clinical authority a legitimate basis. The same norm leaves them managing the consequences of clustered refusal - treating preventable infections, stretching surge capacity, and delivering hard outcomes to families who declined protection - costs that arrive at the bedside rather than through any bill sent to those whose refusal generated them.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, medical_professionals, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(coercion_legitimacy_boundary__bodily_autonomy_primary, medical_professionals, payer).

% Career public-health strategists whose scenario playbooks assume lawful access to compulsion tools - isolation orders, conditions of entry, workplace requirements. Under a categorical boundary those playbook chapters are dead weight; their expertise is not invited into governance conversations that treat compulsion as permanently off the table, and some relocate to sectors or jurisdictions where their planning craft still has purchase.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, pandemic_preparedness_planners, excluded,
    organized, biographical, mobile, national).

% A standing advisory body that audits how the consent boundary performs: it commissions burden studies, hears testimony from objectors, agencies, clinicians, and patient advocates, and publishes recommendations. It holds no enforcement power and collects nothing from the arrangement; its seat exists to see the whole board and report what it finds.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, national_bioethics_commission, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(coercion_legitimacy_boundary__bodily_autonomy_primary, conscientious_objectors).
narrative_ontology:fixing_cost_class(coercion_legitimacy_boundary__bodily_autonomy_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains the trust precondition of medical care: patients and research subjects can submit to examination, treatment, and experimentation knowing their bodies will not be handled without their agreement. Consent requirements convert a deep asymmetry of knowledge and power between practitioner and patient into a governed exchange, and give clinicians a legitimate basis for action that patients have authorized.
% TRANSFER_FUNCTION: Moves decisional authority over bodily integrity to each individual (or their guardian), and moves the residual infection risk created by refusals onto those who cannot vaccinate or mount a response - uncompensated and unchosen. It also moves enforcement burden off public agencies and onto courts and ethics bodies, who carry the boundary's defense as a standing institutional cost.
% ABSENT_VOICES: Those whose lives depend on community immunity speak mainly through proxy advocacy organizations rather than as a seated constituency; emergency planners holding compulsion-dependent playbooks are structurally out of the conversation once the boundary is drawn as categorical; cognitively impaired wards without engaged guardians have no seat at all. Their absence lets the boundary's unanimity look more consensual than it is.
% DISAPPEARANCE_RATIONALE: If the categorical consent boundary vanished overnight, research-subject protections would unwind toward pre-Nuremberg norms, clinical practice would lose the liability and trust scaffolding that channels patients into care, courts and ethics committees would lose a core docket, and public-health agencies would regain compulsion instruments they currently lack - the entire architecture of modern medical ethics would reorganize around whatever coercion calculus replaced it.
% FOUNDING_PROBLEM: State and professional power over unconscious, anesthetized, captive, or devalued bodies was exercised without limit: experimentation on prisoners and institutionalized people, surgery performed without explanation, treatment of wards as raw material. The consent boundary was built to make the body's owner the final gate against institutional bodily violation.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: the Nuremberg tribunal record and subsequent war-crimes prosecutions document the abuses that produced the Code; the Belmont Report (1979, congressionally chartered commission) and the 1997 presidential apology for the Tuskegee Syphilis Study attest the founding problem from governmental seats; published survivor testimony from research-abuse cohorts attests it from the affected side. None of these corroborators benefits from the categorical boundary.
narrative_ontology:disappearance_verdict(coercion_legitimacy_boundary__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(coercion_legitimacy_boundary__bodily_autonomy_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(coercion_legitimacy_boundary__bodily_autonomy_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(coercion_legitimacy_boundary__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coercion_legitimacy_boundary__bodily_autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(coercion_legitimacy_boundary__bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(coercion_legitimacy_boundary__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.52) and omission-driven: the boundary confiscates nothing and deploys no coercion of its own; its cost flows passively as uninternalized infection risk concentrated on a class that cannot exit susceptibility. That concentration and absence of compensation is why epsilon is well above coordination cost, and its passive character is why it stays well below predatory levels. Suppression (0.35) is structural and seat-asymmetric: the boundary legally incapacitates the state's coercive instruments while liberating individuals from compulsion - net moderate-low, unscaled by scope per the framework's rule. Theater ratio (0.20) is low: the consent machinery (protocol vetting, licensure discipline, malpractice review) does real gating work; the slow rise across the interval tracks growing ritualization of consent documentation without functional collapse. Accessibility collapse (0.40): coercive alternatives remain legally available in much of the landscape - the boundary's claim that they are impermissible is normative, not an accomplished factual closure - so alternatives are narrowed in legitimacy but not erased in fact. Resistance (0.70) is high and sustained: public-health and utilitarian quarters press continuously for severity-conditioned exceptions, and the pressure intensified after the pandemic decade. The measurement series run on one shared time grid (points 0-30, unit: years, spanning the consolidation-and-contest era from post-Belmont consolidation to the present post-pandement contest peak); epidemic-wave oscillation in mandate politics is smoothed into the trend lines, and the rising suppression_requirement series reflects genuine enforcement-capacity maturation (conscience-statute regimes, judicial review practice, ethics-committee expansion), not a static picture.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat and the beneficiary seats should compute as different types from the same structure. From the immunocompromised seat the boundary reads as abandonment dressed as principle: an absolute rule whose costs land, uncompensated, on the one class unable to protect itself - near-pure extraction. From the objector and civil-liberties seats the same structure is the floor of civilized medicine, the Nuremberg lesson made operational - near-pure protection. The agency seat holds both at once: relief from a politically toxic duty and loss of a tool it may desperately need. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Conscientious_objectors sit near the beneficiary pole (declared beneficiary, moderate power, constrained exit): the arrangement subsidizes their refusal and externalizes its cost. Immunocompromised_individuals sit near the full-target pole (declared victim, powerless, trapped): they bear what others' freedom sheds, with no arbitrage available. Public_health_enforcement_agencies derive low-to-mid directionality from their beneficiary declaration, tempered by their declared payer side (lost instrument, absorbed blame) - the dual role is authored rather than overridden because the derivation reads the beneficiary array and the secondary role documents the offset. Medical_professionals similarly derive low d from the beneficiary array with the bedside surge cost noted as their payer side. Courts_and_ethics_bodies hold no array membership; their directionality comes from the fallback, and qualitatively they sit near symmetric - they expend standing maintenance effort and collect legitimacy and jurisdiction in return. No directionality overrides are used: every seat's derived position matches its declared structural relationship, and the two dual-positioned agents carry secondary_role rather than an override, keeping the derivation chain primary.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification discipline matters here in both directions. Calling this a snare would erase the genuine coordination function: consent requirements are load-bearing for medicine itself - remove them and care-seeking, trial participation, and clinician legitimacy degrade together, which is why no serious actor proposes repeal. Calling it a rope would erase the asymmetric cost: the risk transfer is real, concentrated on a class that cannot consent-or-respond its way out, and uncompensated - hallmarks of extraction riding a coordination structure. Tangled_rope preserves both truths and keeps the diagnostic question live: is the immunocompromised burden the price of the coordination or rent-shaped shedding? On mandatrophy proper: the founding problem (institutional violation of captive and devalued bodies) remains live in attenuated forms, so the mandate has not outlived its function and no mandatrophy resolution is declared; the R5 mismatch consumer should find status=live consistent with verdict=world_rearranges, no zombie flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This constraint is one reading (bodily_autonomy_primary) of the coercion_legitimacy_boundary kernel; the sibling readings public_health_primary and proportionality_reading instantiate different constraints. Where exactly is the disagreement located, and what would adopting a sibling change structurally?',
    'Comparative structural analysis across the three reading-files: the disagreement sits in (a) the scope of ''medical intervention'' - whether population-level preventive compulsion counts as intervention the consent rule reaches - and (b) whether collective harm-prevention may enter the legitimacy calculus at all.',
    'Adopting public_health_primary would move immunocompromised_individuals out of the victim set (into the protected), move conscientious_objectors toward payer, and raise the measured burden of refusal-permissive arrangements. Adopting proportionality_reading would split the victim set by pathogen severity, retaining immunocompromised exposure only for low-severity diseases.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Committer structure: which kernel, which reading, what siblings would change.').

omega_variable(
    externality_attribution,
    'Is the infection burden falling on immunocompromised individuals fairly attributable to the categorical consent boundary itself, or to background vaccine hesitancy that would persist under any consent regime?',
    'Cross-jurisdiction comparison of outbreak burden and susceptible-population size in strict-mandate versus broad-exemption regions, controlling for baseline hesitancy, socioeconomic confounders, and pathogen circulation intensity.',
    'If the burden tracks the boundary''s breadth, the asymmetric-cost component of this reading''s structure is confirmed and the moderate extractiveness stands. If background hesitancy explains most of it, the measured burden drops toward ordinary coordination cost and the classification relaxes toward pure coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_attribution, empirical, 'Whether the victim-side cost is caused by the boundary or merely co-occurs with it.').

omega_variable(
    enforcement_durability,
    'Does the categorical boundary persist because it is settled constitutional and professional doctrine, or because a contingent political coalition currently defends it?',
    'Track amendment attempts, supreme-court composition turnover, statutory exemption-regime swings, and professional-body guideline revisions over successive political cycles.',
    'Contingent persistence would make the arrangement transient in crisis windows - a support structure that holds only while its coalition holds - rather than a durable feature of medical governance; settled persistence supports a long-lived hybrid structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_durability, empirical, 'Settlement versus coalition-dependence of the boundary''s maintenance.').

omega_variable(
    cs_framing_underdetermination,
    'Is the distributed multi-text framing of the kernel (no single adjudicator owns the coercion-legitimacy boundary; constitutions, the Nuremberg/Belmont canon, statutes, and case law jointly carry it) the only defensible framing, or does a fixed-text framing (the Nuremberg/Belmont canon as THE kernel, courts as designated interpreters) fit equally well?',
    'Examine whether any single text functions as the operative adjudication source across jurisdictions, or whether adjudication genuinely routes through heterogeneous local authorities; signals: citation practice in landmark rulings, professional-body deference patterns, legislative override frequency.',
    'Under a fixed-text framing, interpretation concentrates in courts, drift would register as codification strain rather than dispersed practice drift, and the reading''s maintenance profile would look more centralized and more brittle than the distributed framing suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Alternative framings of the kernel''s codification and authority produce different drift classifications.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coercion_legitimacy_boundary__bodily_autonomy_primary, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coer_tr_t0, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 0, 0.12).
narrative_ontology:measurement(coer_tr_t6, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 6, 0.13).
narrative_ontology:measurement(coer_tr_t12, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 12, 0.15).
narrative_ontology:measurement(coer_tr_t18, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 18, 0.16).
narrative_ontology:measurement(coer_tr_t24, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 24, 0.18).
narrative_ontology:measurement(coer_tr_t30, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 30, 0.2).

% Extraction over time
narrative_ontology:measurement(coer_be_t0, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(coer_be_t6, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 6, 0.39).
narrative_ontology:measurement(coer_be_t12, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 12, 0.43).
narrative_ontology:measurement(coer_be_t18, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 18, 0.47).
narrative_ontology:measurement(coer_be_t24, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 24, 0.5).
narrative_ontology:measurement(coer_be_t30, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 30, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(coer_su_t0, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(coer_su_t6, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 6, 0.25).
narrative_ontology:measurement(coer_su_t12, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 12, 0.28).
narrative_ontology:measurement(coer_su_t18, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 18, 0.31).
narrative_ontology:measurement(coer_su_t24, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 24, 0.33).
narrative_ontology:measurement(coer_su_t30, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 30, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coercion_legitimacy_boundary__bodily_autonomy_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__bodily_autonomy_primary, coercion_legitimacy_boundary__public_health_primary).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__bodily_autonomy_primary, coercion_legitimacy_boundary__proportionality_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'when may the state coerce medical intervention' decomposes, per the epsilon-invariance principle, into three structurally distinct readings of one kernel, each a separate file with its own epsilon, beneficiary/victim sets, and type: this file (bodily_autonomy_primary - categorical consent, moderate epsilon from non-enforcement, immunocompromised as victims), public_health_primary (conditional compulsion, objectors as bearers), and proportionality_reading (severity-scaled compulsion, victim set split by pathogen). Upstream/downstream: this reading supplies the deontological floor against which both siblings negotiate; proportionality_reading is the downstream synthesis that borrows the categorical reading's clinical-domain settlements while relaxing them at the population margin. Links here are family edges, not shared classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
