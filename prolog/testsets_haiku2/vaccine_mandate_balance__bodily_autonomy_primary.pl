% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_balance__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_balance__bodily_autonomy_primary, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: vaccine_mandate_balance__bodily_autonomy_primary
 *   human_readable: Vaccine Mandate under Bodily Autonomy Frame
 *   domain: public_health/constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   Under the bodily_autonomy_primary reading of vaccine mandate authority,
 *   individual consent to medical intervention is treated as a non-negotiable
 *   constraint on state power — a deontological limit, not a policy
 *   preference. When governments impose vaccine mandates (via employment
 *   conditions, licensing boards, or institutional access rules), this
 *   reading treats the mandate as a categorical violation: the state has
 *   criminalized a refusal to undergo medical intervention, regardless of the
 *   collective benefit that vaccination would provide. The victims are those
 *   coerced into vaccination or expelled from employment, profession, and
 *   institutions for refusing. The beneficiaries are immunocompromised
 *   populations who gain protection through increased herd immunity, but
 *   under this reading that benefit does NOT justify the coercion imposed to
 *   achieve it — the benefit is genuine but the means are categorically
 *   impermissible. This constraint is ONE reading of a contested kernel
 *   (vaccine_mandate_balance); its sibling readings (proportionality_reading
 *   and public_health_primary) instantiate different constraints with
 *   different ε values, different victim sets, and different legitimacy
 *   claims. The kernel contest is genuine: jurisdictions and jurisprudential
 *   traditions differ on which reading is the actual commitment.
 *
 * KEY AGENTS:
 *   - vaccine_hesitant_workers: powerless, identity_locked, face termination for refusal — the primary victims under this reading
 *   - medical_conscientious_objectors: moderate power, constrained exit, lose professional standing for refusing mandate participation
 *   - vaccine_injured_persons: moderate power, constrained exit, face revaccination coercion or continued expulsion despite documented adverse events
 *   - immunocompromised_populations: powerless, trapped, benefit from mandate-driven vaccination but also bear indirect costs from enforcement friction
 *   - state_public_health_authority: institutional agenda-setter, sets and enforces mandates, claims proportionality and necessity
 *   - employers_and_institutions: powerful implementers, benefit from reduced absenteeism and liability avoidance while offloading enforcement costs
 *   - public_health_ethicists and vaccine_safety_researchers: analytical observers, generate contested knowledge about mandate justification and safety
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_balance__bodily_autonomy_primary, 0.81).
domain_priors:suppression_score(vaccine_mandate_balance__bodily_autonomy_primary, 0.77).
domain_priors:theater_ratio(vaccine_mandate_balance__bodily_autonomy_primary, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, extractiveness, 0.81).
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 0.77).
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_balance__bodily_autonomy_primary, snare).
narrative_ontology:human_readable(vaccine_mandate_balance__bodily_autonomy_primary, "Vaccine Mandate under Bodily Autonomy Frame").
narrative_ontology:topic_domain(vaccine_mandate_balance__bodily_autonomy_primary, "public_health/constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(vaccine_mandate_balance__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_balance__bodily_autonomy_primary, '11de3110-32db-4885-bf7c-d87a571ceb5f').
narrative_ontology:cs_kernel_codification('11de3110-32db-4885-bf7c-d87a571ceb5f', fixed_text).
narrative_ontology:cs_authority_grounding('11de3110-32db-4885-bf7c-d87a571ceb5f', lineage).
narrative_ontology:cs_interpretation_layer_present('11de3110-32db-4885-bf7c-d87a571ceb5f').
narrative_ontology:cs_reading_relation('11de3110-32db-4885-bf7c-d87a571ceb5f', vaccine_mandate_balance__proportionality_reading, influences).
narrative_ontology:cs_reading_relation('11de3110-32db-4885-bf7c-d87a571ceb5f', vaccine_mandate_balance__public_health_primary, forecloses).
narrative_ontology:cs_axiom('11de3110-32db-4885-bf7c-d87a571ceb5f', foundational, bodily_autonomy_inviolable).
narrative_ontology:cs_axiom_status(bodily_autonomy_inviolable, holdable).
narrative_ontology:cs_axiom_grounding('11de3110-32db-4885-bf7c-d87a571ceb5f', bodily_autonomy_inviolable, deontological).
narrative_ontology:cs_axiom('11de3110-32db-4885-bf7c-d87a571ceb5f', foundational, consent_precondition_for_medical_intervention).
narrative_ontology:cs_axiom_status(consent_precondition_for_medical_intervention, holdable).
narrative_ontology:cs_axiom_grounding('11de3110-32db-4885-bf7c-d87a571ceb5f', consent_precondition_for_medical_intervention, deontological).
narrative_ontology:cs_reference_frame('11de3110-32db-4885-bf7c-d87a571ceb5f', constitutional_bodily_autonomy_inviolability).
narrative_ontology:cs_drift_state('11de3110-32db-4885-bf7c-d87a571ceb5f', contemporary_pandemic_response_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('11de3110-32db-4885-bf7c-d87a571ceb5f', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(vaccine_mandate_balance__bodily_autonomy_primary, vaccine_mandate_balance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__bodily_autonomy_primary, immunocompromised_populations).
narrative_ontology:constraint_victim(vaccine_mandate_balance__bodily_autonomy_primary, vaccine_hesitant_workers).
narrative_ontology:constraint_victim(vaccine_mandate_balance__bodily_autonomy_primary, medical_conscientious_objectors).
narrative_ontology:constraint_victim(vaccine_mandate_balance__bodily_autonomy_primary, vaccine_injured_persons).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__bodily_autonomy_primary, employers_and_institutions).
narrative_ontology:constraint_victim(vaccine_mandate_balance__bodily_autonomy_primary, immunocompromised_populations).
narrative_ontology:constraint_vindicates(vaccine_mandate_balance__bodily_autonomy_primary, medical_self_determination_doctrine).
narrative_ontology:constraint_vindicates(vaccine_mandate_balance__bodily_autonomy_primary, negative_liberty_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Face employment termination, loss of professional licensing, or restricted access to institutions if they refuse vaccination. Their bodily integrity conviction (whether grounded in religious belief, prior adverse experience, or epistemic distrust) is treated as insufficient grounds for exemption. Exit means abandoning career, relocating to jurisdictions without mandates, or submitting to a medical intervention they believe violates their conscience.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, vaccine_hesitant_workers, payer,
    powerless, biographical, identity_locked, national).

% Depend on others' vaccination for protection against severe COVID outcomes they cannot mount immunity against. They benefit from mandate enforcement raising vaccination rates, reducing transmission vectors. They also bear indirect costs: scarcity of medical exemptions may restrict their own options, and they absorb social friction from mandate resistance.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, immunocompromised_populations, beneficiary,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_balance__bodily_autonomy_primary, immunocompromised_populations, payer).

% Healthcare workers, pharmacists, or physicians whose professional ethics or religious commitments center on informed consent as non-negotiable. They face license suspension, employment loss, and professional expulsion if they refuse mandate participation. Their exit is severely constrained — retraining costs are high, and many jurisdictions have reciprocal license revocation.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, medical_conscientious_objectors, payer,
    moderate, biographical, constrained, national).

% Report severe adverse events (myocarditis, autoimmune flares, neurological symptoms) and face either coerced revaccination (mandates often exempt documented injury but enforcement is inconsistent) or continued employment loss. Their medical history and epistemic standing to claim injury are frequently delegitimized; exit means accepting the contested adverse event as necessary sacrifice or pursuing legal remedies with low success rates.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, vaccine_injured_persons, payer,
    moderate, biographical, constrained, national).

% Issues and enforces vaccine mandates via employment conditions, licensing boards, and institutional access rules. Justifies mandates as necessary for population protection when voluntary uptake falls short of herd immunity thresholds. Sets exemption criteria narrowly (medical contraindication only in strict reading; no religious or philosophical exemptions in many jurisdictions). Maintains the enforcement infrastructure and legitimacy claim.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, state_public_health_authority, agenda_setter,
    institutional, generational, analytical, national).

% Implement mandates per government requirement or liability minimization. They benefit from reduced absenteeism due to illness (coordination function) and reduced legal exposure (extraction function — they avoid mandates' enforcement cost while offloading it onto workers). Many employers adopt mandates without direct legal mandate, citing insurance and client pressure.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, employers_and_institutions, agenda_setter,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_balance__bodily_autonomy_primary, employers_and_institutions, beneficiary).

% Analyze the ethical boundary between permissible public health measures and impermissible bodily autonomy violation. They take testimony and evidence from all other seats, debate foundational principles (autonomy vs. collective benefit), and generate frameworks for adjudication that inform judicial review and policy revision.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, public_health_ethicists, observer,
    analytical, generational, analytical, global).

% Conduct pharmacovigilance and safety meta-analysis. Their findings shape the epistemic ground for whether mandate proportionality claims hold — whether the vaccine's safety profile justifies coercion of the hesitant or injured. They inhabit a contentious knowledge commons where their methodology, funding sources, and institutional affiliations are frequently contested.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, vaccine_safety_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_balance__bodily_autonomy_primary, state_public_health_authority).
narrative_ontology:fixing_cost_class(vaccine_mandate_balance__bodily_autonomy_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Achieves high population vaccination coverage when voluntary uptake would fall short of herd immunity thresholds, reducing transmission and protecting immunocompromised populations who cannot develop immunity themselves. The coordination problem: free riders can benefit from others' vaccination while avoiding risks they perceive in the vaccine itself.
% TRANSFER_FUNCTION: Transfers bodily autonomy and professional security from vaccine-hesitant and conscientious workers to immunocompromised populations and employers seeking to minimize illness-driven absenteeism and litigation risk. The mechanism: employment/licensing revocation and institutional access barriers enforce compliance.
% ABSENT_VOICES: Vaccine-injured persons' testimony is systematically deprioritized in regulatory hearings and policy formation; their claims are often treated as anecdotal despite accumulating pharmacovigilance signals. Religious and philosophical objectors are wholly excluded from exemption discussions in many jurisdictions — they have no seat in mandate design and would argue for conscientious-objection rights if admitted.
% DISAPPEARANCE_RATIONALE: If vaccine mandates and their enforcement mechanisms disappeared, vaccination rates would drop substantially in the hesitant populations, transmission of vaccine-preventable disease would rise, and immunocompromised individuals would face materially higher mortality risk. Employers would face increased absenteeism. The public health system would reorganize around voluntary-uptake strategies (messaging, accessibility, incentives) rather than coercion.
% FOUNDING_PROBLEM: A novel pathogen (SARS-CoV-2) with high transmission and severe outcomes in vulnerable subpopulations emerged; early vaccines showed efficacy at preventing severe disease but breakthrough infections remained possible; voluntary vaccination uptake plateaued below herd immunity thresholds in many jurisdictions, leaving immunocompromised populations at ongoing risk.
% FOUNDING_PROBLEM_CORROBORATION: Public health authorities and immunologists attest the founding problem remains live: new variants, waning immunity, and persistent vaccine hesitancy create ongoing transmission risk. Vaccine-hesitant populations and medical ethicists attest the founding problem has substantially diminished (endemic phase, vaccine availability universal, vulnerable populations can access protection) and mandates persist as a legitimacy artifact. Independent epidemiological data from post-mandate jurisdictions show plateauing returns to mandate enforcement.
narrative_ontology:disappearance_verdict(vaccine_mandate_balance__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_balance__bodily_autonomy_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_balance__bodily_autonomy_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vaccine_mandate_balance__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_balance__bodily_autonomy_primary, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_balance__bodily_autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_balance__bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vaccine_mandate_balance__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.81 at interval end) because the constraint's persistence depends on coercive enforcement — mandates extract bodily autonomy and employment security from those who refuse. The measurement series shows extractiveness rising from 0.58 to plateau at 0.81 by year 3 of mandate enforcement, reflecting: (1) initial period where some jurisdictions permitted medical/religious exemptions (lower extraction); (2) tightening of exemption criteria and narrower recognition of conscientious objection; (3) stabilization once exemptions are legally foreclosed. Suppression tracks extraction closely (0.77 at end) because the constraint's enforcement requires active legal coercion (threat of termination, license revocation, institutional access denial) — these are not passive limits but applied force. Suppression_requirement measurements capture the enforcement machinery's intensification over time: initial education campaigns, then employer mandates, then legal enforcement, then credential systems to verify compliance. Theater_ratio (0.28) is moderate-low: the security/public-health rationale is real (the constraint does increase vaccination rates and reduce transmission), but a substantial and growing portion of enforcement effort is devoted to compulsion of the already-identified refusers rather than persuasion of the wavering. The accessibility_collapse (0.62) reflects that alternatives (medical exemptions, religious exemptions, geographical relocation) exist but are progressively narrowed; for workers, the collapse is near-complete (exit costs are existential), while for institutional actors, it is partial (they can adjust enforcement strategy). Resistance (0.71) is substantial: organized opposition from vaccine-hesitant coalitions, medical libertarians, conscientious-objector networks, and some jurisdictional governments actively resisted mandate enforcement; this resistance never prevented mandate adoption but constrained its design (some exemptions were retained, some jurisdictions declined mandates) and created compliance friction.
 *
 * PERSPECTIVAL GAP:
 *   The state_public_health_authority and employers experience this constraint as coordinated protection (a rope-type commons problem solved); vaccine_hesitant_workers and conscientious_objectors experience it as pure coercion (a snare). The gap is not empirical — both seats see the same vaccination outcomes, the same transmission reductions, the same employment consequences. The gap is axiological: the state's seat privileges collective protection as the legitimacy criterion, while the victim seats privilege bodily autonomy as non-negotiable. The engine computes this divergence from the declared roles and directionality: beneficiary vs. payer seats will see different types because they have opposite d-values. The claim/metric independence holds here: the story CLAIMS snare (this reading's verdict under bodily_autonomy_primary axiom), and the metrics describe high extraction and suppression consistent with that claim. But an alternative story with the same constraint would CLAIM tangled_rope or even rope under proportionality_reading, with lower extractiveness metrics (if vaccine safety is high and threat is severe, the coordination function might justify some extraction). That alternative story is NOT this file — it is a sibling constraint with a different ε, different axioms, different cs_structure.
 *
 * DIRECTIONALITY LOGIC:
 *   The directional divergence between seats is sharp and structural. State_public_health_authority sits at d near 0.0 (full beneficiary from the compliance regime's legitimacy and from the health outcomes it claims to produce); employers sit at d = 0.3-0.4 (they benefit from reduced illness-related absenteeism and liability insulation, but also bear enforcement costs and employee relations friction); immunocompromised_populations sit near d = 0.5 (genuinely benefit from protection, but also internalize enforcement friction and social resentment); vaccine_hesitant_workers sit at d = 0.95+ (full targets — they lose employment, professional credentials, institutional access, and face criminal or civil liability for refusal). Conscientious_objectors and vaccine_injured_persons sit at d = 0.85-0.90 (targets, with slightly more option to contest through legal channels). The reading's axiom (bodily_autonomy_inviolable) anchors this directionality: it asserts that REGARDLESS of the public health benefit, coercion to accept medical intervention is categorically extractive. An alternative reading (proportionality_reading) would modulate d based on threat severity and vaccine safety — reducing the d-value for targets if the threat is imminent and severe, and increasing it if vaccine safety is poor. This reading does not modulate: d is derived from power and exit options alone.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem (pandemic risk to vulnerable populations) is contested between readings: under bodily_autonomy_primary, the problem is real but does NOT justify mandate coercion; under proportionality_reading, the problem may justify mandates if proportionality thresholds are met; under public_health_primary, the problem justifies aggressive coercion. The founding_problem_status = contested reflects this: public health authorities and immunologists attest the problem is live (new variants, waning immunity). Vaccine-hesitant populations and medical ethicists attest the problem has attenuated (endemic phase, universal access to vaccines, vulnerable populations can shield themselves). The disappearance_verdict = world_rearranges indicates the constraint has material consequences — if mandates vanished, vaccination rates would drop and transmission would rise. This prevents mandatrophy classification: even under this reading, if the mandate ceased, the world would reorganize (vaccination uptake would fall, disease outcomes would worsen). A piton would leave the world unchanged. The theater_ratio (0.28) is not high enough to trigger piton classification on its own, and the extraction is too clearly linked to coercion (suppression=0.77) rather than pure inertia. The constraint is a snare under this reading because the coercion is active and the victim set is clearly identifiable, not because it is performative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_selection_bodily_autonomy_vs_proportionality,
    'Is bodily autonomy an inviolable principle that admits NO proportionality exception, or is bodily autonomy a strong presumption that can be overridden by sufficiently severe, imminent, and particularized threats?',
    'Jurisprudential genealogy: trace which competing principle grounds the reading''s authority structure (deontological inviolability vs. instrumental proportionality), and examine whether case law or constitutional amendment has shifted the presumptive weight. Committer-axis answerability: this is a conceptual disagreement about the foundation of the constraint, not resolvable by empirical data alone.',
    'If bodily autonomy is truly inviolable (this reading''s core axiom), mandates are snares by definition, regardless of disease severity or vaccine safety. If proportionality is the true frame (sibling reading), mandates can be legitimate if the threat is severe and alternatives exhausted — reclassifying the constraint as tangled_rope or even rope depending on threshold outcomes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_bodily_autonomy_vs_proportionality, conceptual, 'Foundational disagreement about whether bodily autonomy is deontologically inviolable or instrumentally overrideable.').

omega_variable(
    victim_identity_locking_mechanism,
    'For vaccine-hesitant workers classified as identity_locked, is the locking structural (career path dependence, irreversible credential loss) or internalized (the person has fused their identity with their occupational role such that exit feels existentially impossible)?',
    'Post-mandate relaxation: do workers who exit employment during strict mandate periods and subsequently find alternative employment without vaccine requirements report reduced suppression of their autonomy conviction, or does the conviction remain suppressed by internalized identity fusion with the original profession?',
    'If locking is primarily structural, relaxing mandates and restoring exit options will reduce effective suppression and may shift classification toward tangled_rope (coordination + constrained extraction). If primarily internalized, suppression persists after mandate removal, and the constraint''s extractive character is deeper than enforcement mechanisms alone would suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_identity_locking_mechanism, empirical, 'Whether suppression of vaccine-hesitant workers is structural or internalized.').

omega_variable(
    proportionality_threshold_empirical_dispute,
    'At what combination of disease severity, transmission risk, and vaccine safety would mandates transition from snare to tangled_rope (or rope) under proportionality framing?',
    'Meta-analysis of COVID-19 severity, vaccine effectiveness, and safety signals over time; comparative analysis of mandate policy decisions across jurisdictions with different threshold definitions; expert elicitation on proportionality benchmarks from public health ethics and constitutional law.',
    'High uncertainty here reflects genuine epistemic contestation between readings: bodily_autonomy_primary holds mandates unjustifiable at any empirical threshold; proportionality_reading treats the empirical facts as decisive for whether mandates are legitimate. A reading shift would require the bodily_autonomy_primary axiom itself to weaken (overridden status), not just empirical findings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_threshold_empirical_dispute, empirical, 'Contested empirical thresholds at which mandates would become proportionate under proportionality_reading.').

omega_variable(
    kernel_contest_sibling_readings,
    'Which reading of the vaccine_mandate_balance kernel — bodily_autonomy_primary (this one), proportionality_reading, or public_health_primary — corresponds to the actual constitutional/ethical commitment the authority structure instantiates?',
    'Jurisprudential history: examine constitutional text, case law evolution, and legislative intent across jurisdictions. Each reading makes a claim about what the authority REALLY commits to; the kernel contest is over which claim is grounded and which is a constructed alternative. No single empirical finding resolves this; the resolution is genealogical and interpretive.',
    'If the authority structure''s actual commitment is bodily_autonomy_primary, other readings are overridden or foreclosed by the established doctrine. If the actual commitment is proportionality_reading or public_health_primary, this reading is either overridden (superseded by case law) or coexists as a rejected alternative that some parties still hold.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_contest_sibling_readings, conceptual, 'Which reading of the vaccine_mandate_balance kernel is instantiated by the actual constitutional commitment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_balance__bodily_autonomy_primary, 0, 48).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(vacc_tr_t0, observed).
narrative_ontology:measurement(vacc_tr_t6, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 6, 0.16).
narrative_ontology:measurement_basis(vacc_tr_t6, observed).
narrative_ontology:measurement(vacc_tr_t12, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 12, 0.21).
narrative_ontology:measurement_basis(vacc_tr_t12, observed).
narrative_ontology:measurement(vacc_tr_t24, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 24, 0.27).
narrative_ontology:measurement_basis(vacc_tr_t24, observed).
narrative_ontology:measurement(vacc_tr_t36, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 36, 0.29).
narrative_ontology:measurement_basis(vacc_tr_t36, observed).
narrative_ontology:measurement(vacc_tr_t48, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 48, 0.28).
narrative_ontology:measurement_basis(vacc_tr_t48, observed).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(vacc_be_t0, observed).
narrative_ontology:measurement(vacc_be_t6, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 6, 0.64).
narrative_ontology:measurement_basis(vacc_be_t6, observed).
narrative_ontology:measurement(vacc_be_t12, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 12, 0.71).
narrative_ontology:measurement_basis(vacc_be_t12, observed).
narrative_ontology:measurement(vacc_be_t24, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 24, 0.78).
narrative_ontology:measurement_basis(vacc_be_t24, observed).
narrative_ontology:measurement(vacc_be_t36, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 36, 0.81).
narrative_ontology:measurement_basis(vacc_be_t36, observed).
narrative_ontology:measurement(vacc_be_t48, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 48, 0.81).
narrative_ontology:measurement_basis(vacc_be_t48, observed).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 0, 0.52).
narrative_ontology:measurement_basis(vacc_su_t0, observed).
narrative_ontology:measurement(vacc_su_t6, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 6, 0.61).
narrative_ontology:measurement_basis(vacc_su_t6, observed).
narrative_ontology:measurement(vacc_su_t12, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 12, 0.69).
narrative_ontology:measurement_basis(vacc_su_t12, observed).
narrative_ontology:measurement(vacc_su_t24, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 24, 0.75).
narrative_ontology:measurement_basis(vacc_su_t24, observed).
narrative_ontology:measurement(vacc_su_t36, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 36, 0.77).
narrative_ontology:measurement_basis(vacc_su_t36, observed).
narrative_ontology:measurement(vacc_su_t48, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 48, 0.77).
narrative_ontology:measurement_basis(vacc_su_t48, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_balance__bodily_autonomy_primary, attachment_coordination).
narrative_ontology:boltzmann_floor_override(vaccine_mandate_balance__bodily_autonomy_primary, 0.12).
narrative_ontology:affects_constraint(vaccine_mandate_balance__bodily_autonomy_primary, vaccine_mandate_balance__proportionality_reading).
narrative_ontology:affects_constraint(vaccine_mandate_balance__bodily_autonomy_primary, vaccine_mandate_balance__public_health_primary).
narrative_ontology:affects_constraint(vaccine_mandate_balance__bodily_autonomy_primary, pandemic_emergency_powers__executive_authority).

% DUAL FORMULATION NOTE:
% This constraint is one reading of vaccine_mandate_balance kernel. The bodily_autonomy_primary reading treats mandates as categorical violations of inviolable consent rights. The sibling readings (proportionality_reading and public_health_primary) share the same referent (vaccine mandates in pandemic response) but instantiate different constraints with different ε values, different victim sets, and different legitimacy axioms. All three files must be present in the corpus to represent the kernel contest accurately. Network edges link all three; directionality reverses: bodily_autonomy_primary treats mandates as extractive coercion, while public_health_primary treats mandates as justified coordination. The proportionality_reading occupies the middle ground, providing potential resolution conditions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vaccine_mandate_balance__bodily_autonomy_primary, moderate, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
