% ============================================================================
% CONSTRAINT STORY: catastrophe_avoidance_retention__hybrid_near_miss_learning
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_avoidance_retention__hybrid_near_miss_learning, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: catastrophe_avoidance_retention__hybrid_near_miss_learning
 *   human_readable: Hybrid Near-Miss Learning Regime for Catastrophe-Avoidance Competence
 *   domain: safety_engineering/organizational_learning/high_reliability_systems
 *
 * SUMMARY:
 *   The standing arrangement under contest is the cross-organizational
 *   apparatus by which catastrophe-facing industries keep rare-event
 *   competence alive between the events that would teach it: mandatory and
 *   protected near-miss reporting, standardized incident taxonomies pooled
 *   across competitors, systematic review of other operators' foreign
 *   accidents, and scheduled high-realism rehearsal. This story instantiates
 *   the hybrid_near_miss_learning reading of the
 *   catastrophe_avoidance_retention kernel: neither synthetic rehearsal nor
 *   direct catastrophe experience alone retains competence; the load-bearing
 *   structure is the distributed incident-learning network, and the
 *   observable delta is the aviation/medicine contrast - dense exchange
 *   tracks sustained safety gains, thin exchange tracks preventable repeat
 *   harm. Constraint-family note: the colloquial question 'what keeps crews
 *   competent against catastrophe?' decomposes into three structurally
 *   distinct claims with distinct epsilon referents. This file authors
 *   epsilon for the hybrid network arrangement as this reading sees it
 *   (moderate extraction on a genuine coordination core); the
 *   simulation-as-proxy sibling authors epsilon for drill-only regimes; the
 *   catastrophe-as-selector sibling authors epsilon for experience-only
 *   regimes. The three files are linked via network.affects_constraints. KEY
 *   AGENTS (by structural relationship): - safety_regulators: agenda-setter
 *   (institutional/constrained) - writes mandates, administers databases,
 *   accrues purview - major_airlines: primary beneficiary-payer
 *   (powerful/constrained) - largest contributor and consumer of pooled
 *   lessons - frontline_clinicians: primary target (moderate/trapped) - bear
 *   reporting labor and disclosure risk - first_disclosing_organizations:
 *   target (organized/trapped) - concentrated disclosure costs, costlessly
 *   diffused lessons - small_regional_operators: target
 *   (moderate/constrained) - fixed compliance costs weigh heaviest per unit
 *   of output - frontline_pilots: payer-beneficiary
 *   (organized/identity_locked) - participation fused with professional
 *   self-concept - flying_public_and_patients: diffuse beneficiary
 *   (powerless/mobile) - insurers and simulation_training_industry: secondary
 *   beneficiaries (institutional/arbitrage; organized/mobile) -
 *   punished_reporters: excluded voice (powerless/trapped) - the immunity
 *   promise's casualties - hro_researchers: analytical observer - sees the
 *   full aviation/medicine contrast
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.46).
domain_priors:suppression_score(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.5).
domain_priors:theater_ratio(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, extractiveness, 0.46).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_avoidance_retention__hybrid_near_miss_learning, tangled_rope).
narrative_ontology:human_readable(catastrophe_avoidance_retention__hybrid_near_miss_learning, "Hybrid Near-Miss Learning Regime for Catastrophe-Avoidance Competence").
narrative_ontology:topic_domain(catastrophe_avoidance_retention__hybrid_near_miss_learning, "safety_engineering/organizational_learning/high_reliability_systems").

domain_priors:requires_active_enforcement(catastrophe_avoidance_retention__hybrid_near_miss_learning).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_avoidance_retention__hybrid_near_miss_learning, '2025d466-9198-4fbd-a0fb-cbab44539bf5').
narrative_ontology:cs_kernel_codification('2025d466-9198-4fbd-a0fb-cbab44539bf5', distributed).
narrative_ontology:cs_authority_grounding('2025d466-9198-4fbd-a0fb-cbab44539bf5', distributed).
narrative_ontology:cs_reading_relation('2025d466-9198-4fbd-a0fb-cbab44539bf5', catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, coexists_with).
narrative_ontology:cs_reading_relation('2025d466-9198-4fbd-a0fb-cbab44539bf5', catastrophe_avoidance_retention__catastrophe_as_necessary_selector, influences).
narrative_ontology:cs_axiom('2025d466-9198-4fbd-a0fb-cbab44539bf5', foundational, no_single_channel_suffices_for_competence_retention).
narrative_ontology:cs_axiom_status(no_single_channel_suffices_for_competence_retention, holdable).
narrative_ontology:cs_axiom_grounding('2025d466-9198-4fbd-a0fb-cbab44539bf5', no_single_channel_suffices_for_competence_retention, empirically_contingent).
narrative_ontology:cs_axiom('2025d466-9198-4fbd-a0fb-cbab44539bf5', foundational, operational_incident_signal_irreplaceable).
narrative_ontology:cs_axiom_status(operational_incident_signal_irreplaceable, holdable).
narrative_ontology:cs_axiom_grounding('2025d466-9198-4fbd-a0fb-cbab44539bf5', operational_incident_signal_irreplaceable, empirically_contingent).
narrative_ontology:cs_reference_frame('2025d466-9198-4fbd-a0fb-cbab44539bf5', distributed_incident_learning_equilibrium).
narrative_ontology:cs_drift_state('2025d466-9198-4fbd-a0fb-cbab44539bf5', contemporary_two_regime_contrast, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('2025d466-9198-4fbd-a0fb-cbab44539bf5', '').
narrative_ontology:cs_kernel_id(catastrophe_avoidance_retention__hybrid_near_miss_learning, catastrophe_avoidance_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, flying_public_and_patients).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, major_airlines).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, hospital_systems).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, insurers).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, safety_regulators).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, simulation_training_industry).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__hybrid_near_miss_learning, frontline_clinicians).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__hybrid_near_miss_learning, small_regional_operators).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__hybrid_near_miss_learning, first_disclosing_organizations).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__hybrid_near_miss_learning, punished_reporters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, frontline_pilots).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__hybrid_near_miss_learning, major_airlines).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__hybrid_near_miss_learning, hospital_systems).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__hybrid_near_miss_learning, frontline_pilots).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Writes and enforces the reporting mandates, certifies drill-program adequacy, and administers the shared incident databases. Each expansion of the mandate - safety-management-system rules, just-culture codes, drill-hour minimums - enlarges their staff, budget, and inspection purview. Abandoning the mandate would mean conceding oversight failure after the next publicized accident, so retrenchment is not a live option.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, safety_regulators, agenda_setter,
    institutional, generational, constrained, global).

% Operates large fleets that both generate and consume the largest share of pooled incident data. Funds the system through compliance spending, data contributions, and simulator programs, and recoups it in avoided hull losses and insurance premiums. Leaving the network would mean forfeiting other operators' lessons while remaining exposed to the same hazards.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, major_airlines, beneficiary,
    powerful, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_avoidance_retention__hybrid_near_miss_learning, major_airlines, payer).

% Adopts incident reporting unevenly; legal departments frequently block disclosure of near-misses for fear of discovery in malpractice suits. Draws on aviation-derived protocols - checklists, crew resource management - contributed by others while contributing comparatively little event data of its own.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, hospital_systems, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_avoidance_retention__hybrid_near_miss_learning, hospital_systems, payer).

% Files confidential near-miss reports on personal time and attends recurrent high-fidelity simulator sessions on top of full flight schedules. Professional identity is fused with the reporting norm - admitting error quickly is what a good captain does - so opting out is not a live personal option even where formal immunity makes reporting safe.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, frontline_pilots, payer,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_avoidance_retention__hybrid_near_miss_learning, frontline_pilots, beneficiary).

% Nurses, residents, and attending physicians who encounter near-misses daily. Where just-culture protections are weak, a filed report can surface in credentialing disputes or malpractice discovery, so the rational move is silence. They carry the reporting labor and the disclosure risk while the resulting lessons travel to others.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, frontline_clinicians, payer,
    moderate, biographical, trapped, national).

% Regional carriers and rural hospitals pay roughly fixed costs - reporting infrastructure, simulator contracts, training hours - that weigh far more per revenue passenger-mile or per bed than they do for large incumbents, in exchange for access to the same pooled lessons.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, small_regional_operators, payer,
    moderate, biographical, constrained, regional).

% Whichever operator discloses a serious incident bears the concentrated cost: investigations, litigation exposure, brand damage, and heightened regulatory scrutiny - while the resulting procedural fixes diffuse costlessly to every competitor. Every operator therefore waits for someone else to go first.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, first_disclosing_organizations, payer,
    organized, biographical, trapped, global).

% Receives the safety dividend - declining accident rates - without organizing, paying, or participating. Can choose among certified carriers and hospitals but cannot inspect the learning system behind the certificate.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, flying_public_and_patients, beneficiary,
    powerless, biographical, mobile, global).

% Prices risk off the pooled incident data, requires drill and reporting compliance as policy conditions, and adjusts premiums accordingly. Enters, exits, and reprices lines of business freely and bears no reporting duty itself.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, insurers, beneficiary,
    institutional, biographical, arbitrage, global).

% Builds and sells the full-flight simulators, clinical mannequin labs, and drill curricula that the high-realism component requires. Every increase in mandated drill hours or fidelity standards converts directly into order books.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, simulation_training_industry, beneficiary,
    organized, biographical, mobile, global).

% Frontline staff who reported in good faith and were nonetheless passed over, disciplined, or sued - living evidence that the immunity promise is honored unevenly. They sit outside the consensus conferences and standards committees that describe the system as working; their testimony would qualify every claim of protection.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, punished_reporters, excluded,
    powerless, biographical, trapped, national).

% High-reliability-organization scholars and academically seated accident investigators study which industries learn and which do not, comparing aviation's dense exchange with medicine's thin one. They hold no compliance duties and collect no mandate budgets.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, hro_researchers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_avoidance_retention__hybrid_near_miss_learning, safety_regulators).
narrative_ontology:fixing_cost_class(catastrophe_avoidance_retention__hybrid_near_miss_learning, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converts privately held incident and near-miss knowledge into industry-wide competence: standardized reporting taxonomies, protected reporting channels, cross-organization dissemination of lessons, systematic review of foreign operators' accidents, and scheduled high-realism rehearsal that keeps rare-event skills current between the real events that would otherwise be the only teachers.
% TRANSFER_FUNCTION: Moves reporting labor and disclosure risk from frontline operators and first-disclosing organizations into a shared knowledge pool administered by regulators; moves training budgets from operating organizations to drill providers; moves survival value back diffusely to every participant and to the public.
% ABSENT_VOICES: Punished reporters and data-poor-region operators are outside the consensus. Those retaliated upon despite formal immunity would testify that protections are honored unevenly; operators in developing states would object that the mandates are universal while the network's benefits concentrate in rich-world incumbents with dense data infrastructure. Neither group is seated in the standards committees that describe the system as functioning.
% DISAPPEARANCE_RATIONALE: If the sharing network vanished overnight, each organization would be thrown back on learning from its own accidents; accident rates would climb toward pre-network baselines over a generation as rare-event lessons died with the organizations that paid for them; the aviation-versus-medicine gap would widen wherever local event rates are too low to sustain experiential learning; and the drill industry would lose its mandate-backed demand.
% FOUNDING_PROBLEM: Post-war jet-era accident sequences showed that no single organization generates enough catastrophic events to learn fast enough, that competence decays without rehearsal, and that rare-event lessons die with the organizations that paid for them - so a mechanism was needed to pool what each operator learned at terrible cost.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: accident-investigation bodies (NTSB and BEA final reports repeatedly cite broken information handoffs as causal factors), the academic high-reliability literature (Reason's error-chain work; Weick and Sutcliffe's cross-industry comparisons), and insurer actuarial analyses that price the measurable difference between learning and non-learning fleets. None of these seats collects the compliance rents the mandates generate. The benefiting parties' own attestations (ICAO, trade associations) are self-interested and weighted accordingly.
narrative_ontology:disappearance_verdict(catastrophe_avoidance_retention__hybrid_near_miss_learning, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_avoidance_retention__hybrid_near_miss_learning, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_avoidance_retention__hybrid_near_miss_learning, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_avoidance_retention__hybrid_near_miss_learning, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.46, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_avoidance_retention__hybrid_near_miss_learning_tests).
:- end_tests(catastrophe_avoidance_retention__hybrid_near_miss_learning_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.46 at interval end) and rising: the network's coordination value is real - pooled near-miss data and foreign-incident review convert isolated events into industry-wide procedure - but the costs are asymmetrically distributed: disclosure risk concentrates on whoever goes first, fixed compliance costs weigh hardest on small operators, and drill budgets are compulsory spend. Suppression (0.50) is authored as a raw structural property - the engine scales only extractiveness, never suppression - reflecting enforcement that is real but largely protective in design: mandates, certification ties, and just-culture codes. Aviation's confidentiality shield keeps punitive force below what a pure liability regime would produce; medicine's disclosure-chilling liability environment is the visible counter-case. Theater (0.31) reflects documented checkbox drift in recurrent drills and compliance paperwork layered onto a still-functional core. Accessibility_collapse is low (0.25): the sibling regimes - simulation-only and experience-only - remain live, funded, and practiced, so alternatives do not collapse. Resistance (0.52) is substantive: liability-driven refusal to disclose, budget fights over drill hours, and professional-culture resistance in medicine. All three metric series run on one shared seven-point grid (1976-2024) so no metric row borrows another's end-state values. Suppression_requirement is authored deliberately because this story specifically traces enforcement build-out - voluntary confidential reporting, then SMS mandates, then codified just culture - a rising trajectory, not a static picture. The rising base_extractiveness series is the accumulation signature relevant to T17-style abductive triggers: compliance layers and administrative scope accrete on a working coordination core.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute differently. From the regulator seat the arrangement is legitimate governance it built and staffs; from the trapped payer seats - clinicians facing discovery, first disclosers facing brand damage - the same structure operates as compelled transfer of private information at private risk for diffuse public gain. Major carriers sit near symmetric: they pay heavily and are repaid in avoided losses and premiums. Identity-lock is load-bearing on the pilot seat: the reporting norm is fused with professional self-concept ('a good captain files the report'), so pilot-side participation survives even where formal immunity is privately distrusted; if that frame broke - a publicized betrayal of confidential reports - the voluntary layer of the network would collapse faster than the mandate could replace it. Coalition note: the powerless victim seats (punished reporters, individual clinicians) are weak separately but representable collectively through nursing unions and whistleblower alliances; coalition formation is the principal lever that could rebalance the burden asymmetry, and its absence so far is what keeps their effective burden high.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to d as follows. Flying_public_and_patients, insurers, and simulation_training_industry sit at the beneficiary end - subsidized or paid by the arrangement. Safety_regulators collect administratively and sit near the full-beneficiary pole. Hospital_systems derive as beneficiaries and are left there: they consume more lessons than they fund. Major_airlines carry a deliberate override (powerful -> d 0.32): the structural derivation reads their beneficiary declaration and would land near 0.1, but they fund the system disproportionately and supply most of the pooled data - structurally nearer symmetric than the label alone suggests. The victim declarations drive the target end: frontline_clinicians (trapped, licensure-bound), first_disclosing_organizations (trapped by exposure dynamics), small_regional_operators (constrained), and punished_reporters (fully trapped) all derive high d, amplified further by continental-to-global scope, which makes verification of protection promises harder and pushes effective extraction upward for those seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - no single organization generates enough catastrophic events to learn fast enough, and unpracticed skill decays - is still live: new hazard domains inherit it intact. R5 status live combined with the Q5 verdict world_rearranges produces no mismatch flag: the arrangement persists because the problem persists, not because a corpse is propped up. The mandatrophy discipline earns its keep in both directions here. Reading the growing compliance stack as pure extraction would mislabel a load-bearing coordination core - the accident-rate record tracks network maturity too closely for the enforcement layer to be mere cover. Reading the aviation success story as proof of permanent health would miss the theater accretion and burden drift the temporal series records. The watch-item is the medicine half: where the network never took root, the same mandates operate as unfunded liability traps - a locally snare-shaped instance of a globally tangled-rope-shaped arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_hybrid_vs_siblings,
    'This story instantiates the hybrid_near_miss_learning reading of the catastrophe_avoidance_retention kernel: is the multi-channel account of competence retention correct, or does one of the sibling readings (simulation-as-proxy, catastrophe-as-necessary-selector) hold instead?',
    'Cross-industry natural experiments comparing rare-event skill retention under simulation-only, catastrophe-experience-only, and hybrid regimes, controlling for baseline hazard and operator selection.',
    'If simulation-only suffices, the sharing-network enforcement layer is overbuilt overhead on a cheaper substitute; if catastrophe-selection is necessary, the drill and near-miss components are insufficient and the arrangement merely delays failure until the next real event.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_hybrid_vs_siblings, empirical, 'Whether the hybrid multi-channel reading of the kernel beats its two sibling readings.').

omega_variable(
    irreducible_catastrophe_component,
    'Is there a component of operational competence - mortality salience, organizational memory, stress inoculation - that only real catastrophe installs and that no volume of near-miss data or drill fidelity substitutes?',
    'Compare organizations with and without recent direct catastrophe experience on leading indicators: procedure adherence under load, escalation willingness, near-miss recognition latency.',
    'If such a component exists, hybrid regimes need periodic real-event anchoring and the drill curriculum is systematically miscalibrated; if not, the sibling selector reading loses its proposed mechanism entirely.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(irreducible_catastrophe_component, empirical, 'Whether direct catastrophe experience carries an irreducible instructional component.').

omega_variable(
    network_efficacy_confound,
    'Is aviation''s superior safety record caused by its incident-sharing network, or by confounds - pilot selection, fleet homogeneity, equipment standardization, passenger substitutability - that medicine lacks?',
    'Within-aviation comparison of operators with comparable fleets and hiring pools but different depths of network participation; difference-in-differences around network accession dates.',
    'If confounded, exporting aviation-style mandates to medicine imports compliance costs without the learning benefit, and the expected structural delta (aviation succeeds, medicine fails) reverses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_efficacy_confound, empirical, 'Whether the aviation/medicine contrast isolates the network effect or rides on confounds.').

omega_variable(
    formal_vs_lived_immunity_gap,
    'Does formal reporting immunity translate into de facto protection across jurisdictions, or does the punished-reporter class reveal a systematic gap between written and lived protection?',
    'Longitudinal tracking of reporter career and legal outcomes versus matched non-reporters across jurisdictions with nominally identical immunity statutes.',
    'If the gap is systematic, measured reporting volumes overstate trust and the network runs on compelled disclosure rather than protected candor, raising the effective burden on frontline seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formal_vs_lived_immunity_gap, empirical, 'Whether the immunity promise is honored uniformly enough to support the voluntary reporting layer.').

omega_variable(
    unit_of_analysis_framing,
    'Is the constraint the cross-organizational network itself, or each member organization''s internal just-culture with the network as an emergent byproduct?',
    'Test whether network-level interventions (mandated sharing) shift outcomes where internal cultures are hostile; if outcomes track internal culture regardless of network membership, the internal framing is operative.',
    'Under the internal framing the stakeholder set shrinks to intra-organizational seats, spatial scope drops from global to organizational, and the enforcement story relocates from regulators to managers - a different cs_structure framing with potentially different classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(unit_of_analysis_framing, conceptual, 'Framing under-determination: network-level versus organization-level unit of analysis.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_avoidance_retention__hybrid_near_miss_learning, 1976, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hybrid_near_miss_tr_t1976, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 1976, 0.12).
narrative_ontology:measurement_basis(hybrid_near_miss_tr_t1976, observed).
narrative_ontology:measurement(hybrid_near_miss_tr_t1984, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 1984, 0.15).
narrative_ontology:measurement_basis(hybrid_near_miss_tr_t1984, observed).
narrative_ontology:measurement(hybrid_near_miss_tr_t1992, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 1992, 0.18).
narrative_ontology:measurement_basis(hybrid_near_miss_tr_t1992, observed).
narrative_ontology:measurement(hybrid_near_miss_tr_t2000, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 2000, 0.22).
narrative_ontology:measurement_basis(hybrid_near_miss_tr_t2000, observed).
narrative_ontology:measurement(hybrid_near_miss_tr_t2008, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 2008, 0.26).
narrative_ontology:measurement_basis(hybrid_near_miss_tr_t2008, observed).
narrative_ontology:measurement(hybrid_near_miss_tr_t2016, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 2016, 0.29).
narrative_ontology:measurement_basis(hybrid_near_miss_tr_t2016, observed).
narrative_ontology:measurement(hybrid_near_miss_tr_t2024, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 2024, 0.31).
narrative_ontology:measurement_basis(hybrid_near_miss_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(hybrid_near_miss_be_t1976, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 1976, 0.24).
narrative_ontology:measurement_basis(hybrid_near_miss_be_t1976, observed).
narrative_ontology:measurement(hybrid_near_miss_be_t1984, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 1984, 0.27).
narrative_ontology:measurement_basis(hybrid_near_miss_be_t1984, observed).
narrative_ontology:measurement(hybrid_near_miss_be_t1992, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 1992, 0.31).
narrative_ontology:measurement_basis(hybrid_near_miss_be_t1992, observed).
narrative_ontology:measurement(hybrid_near_miss_be_t2000, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 2000, 0.36).
narrative_ontology:measurement_basis(hybrid_near_miss_be_t2000, observed).
narrative_ontology:measurement(hybrid_near_miss_be_t2008, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 2008, 0.41).
narrative_ontology:measurement_basis(hybrid_near_miss_be_t2008, observed).
narrative_ontology:measurement(hybrid_near_miss_be_t2016, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 2016, 0.44).
narrative_ontology:measurement_basis(hybrid_near_miss_be_t2016, observed).
narrative_ontology:measurement(hybrid_near_miss_be_t2024, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 2024, 0.46).
narrative_ontology:measurement_basis(hybrid_near_miss_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(hybrid_near_miss_su_t1976, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 1976, 0.22).
narrative_ontology:measurement_basis(hybrid_near_miss_su_t1976, observed).
narrative_ontology:measurement(hybrid_near_miss_su_t1984, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 1984, 0.26).
narrative_ontology:measurement_basis(hybrid_near_miss_su_t1984, observed).
narrative_ontology:measurement(hybrid_near_miss_su_t1992, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 1992, 0.3).
narrative_ontology:measurement_basis(hybrid_near_miss_su_t1992, observed).
narrative_ontology:measurement(hybrid_near_miss_su_t2000, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 2000, 0.37).
narrative_ontology:measurement_basis(hybrid_near_miss_su_t2000, observed).
narrative_ontology:measurement(hybrid_near_miss_su_t2008, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 2008, 0.43).
narrative_ontology:measurement_basis(hybrid_near_miss_su_t2008, observed).
narrative_ontology:measurement(hybrid_near_miss_su_t2016, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 2016, 0.47).
narrative_ontology:measurement_basis(hybrid_near_miss_su_t2016, observed).
narrative_ontology:measurement(hybrid_near_miss_su_t2024, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 2024, 0.5).
narrative_ontology:measurement_basis(hybrid_near_miss_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_avoidance_retention__hybrid_near_miss_learning, enforcement_mechanism).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__hybrid_near_miss_learning, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__hybrid_near_miss_learning, catastrophe_avoidance_retention__catastrophe_as_necessary_selector).

% DUAL FORMULATION NOTE:
% Family decomposition of the catastrophe_avoidance_retention kernel into three epsilon-invariant stories: this hybrid reading (epsilon referent: the distributed network arrangement - moderate extraction on a genuine coordination core), simulation_as_proxy_catastrophe (epsilon referent: drill-only regimes), and catastrophe_as_necessary_selector (epsilon referent: experience-only regimes). The hybrid reading sits upstream of the selector reading in evidentiary terms: demonstrated network efficacy is the main empirical pressure on the necessity claim, since a working network reduces the catastrophe frequency the selector's mechanism feeds on. Members are linked bidirectionally via affects_constraints; no member averages across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_avoidance_retention__hybrid_near_miss_learning, powerful, 0.32).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
