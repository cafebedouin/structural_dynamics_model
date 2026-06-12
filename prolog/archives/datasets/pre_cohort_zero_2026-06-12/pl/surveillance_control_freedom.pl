% ============================================================================
% CONSTRAINT STORY: surveillance_control_freedom
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_surveillance_control_freedom, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: surveillance_control_freedom
 *   human_readable: Surveillance Infrastructure as Control Mechanism
 *   domain: theological_ethics/technology_governance/political_economy
 *
 * SUMMARY:
 *   Surveillance infrastructure has evolved from targeted monitoring of
 *   specific threats to pervasive data collection and profiling that shapes
 *   behavior through architectures of visibility. The constraint operates not
 *   primarily through explicit prohibitions but through chilling effects:
 *   individuals self-censor, conform to perceived norms, and avoid dissent
 *   because they know (or believe) they are being watched. This creates a
 *   structural asymmetry: those with power to surveil experience the
 *   infrastructure as a coordination mechanism (public safety, service
 *   improvement, market efficiency), while those under surveillance
 *   experience it as extraction (loss of privacy, autonomy, and interior
 *   freedom). The constraint exhibits rising extraction and suppression over
 *   the 20-year interval as surveillance infrastructure has become more
 *   pervasive, more sophisticated, and more integrated into essential
 *   services. Theater ratio reflects the notice-and-consent regime: privacy
 *   policies and cookie banners create the appearance of user control while
 *   providing minimal actual protection. The constraint is downstream of
 *   digital_power_concentration: platform monopolies and state surveillance
 *   apparatuses are the primary beneficiaries and enforcers.
 *
 * KEY AGENTS:
 *   - Dissidents: Primary victims (powerless/trapped) — face maximum extraction; surveillance infrastructure directly targets political opposition and enables repression
 *   - Privacy-Seeking Citizens: Secondary victims (moderate/constrained) — cannot exit surveillance economy without severe costs; must trade privacy for access to essential services
 *   - GDPR-Protected Citizens: Mixed position (moderate/constrained) — benefit from legal protections but still experience extraction through compliance theater and uneven enforcement
 *   - Platform Corporations: Primary beneficiaries (institutional/arbitrage) — surveillance enables targeted advertising, behavioral prediction, market dominance; can arbitrage across jurisdictions
 *   - Surveillance States: Primary beneficiaries (institutional/arbitrage) — mass surveillance enables population management, dissent suppression, social credit systems; can define rules and exempt themselves
 *   - Privacy Rights Coalition: Organized agents (organized/mobile) — building legal frameworks and technical alternatives; see constraint as temporary with sunset logic
 *   - Privacy Policy Regime: Institutional actor (institutional/constrained) — maintains notice-and-consent ritual that is largely theatrical; sees own process as degraded
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees both coordination and extraction functions as structurally inseparable in current implementations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(surveillance_control_freedom, 0.68).
domain_priors:suppression_score(surveillance_control_freedom, 0.75).
domain_priors:theater_ratio(surveillance_control_freedom, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(surveillance_control_freedom, extractiveness, 0.68).
narrative_ontology:constraint_metric(surveillance_control_freedom, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(surveillance_control_freedom, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(surveillance_control_freedom, snare).
narrative_ontology:human_readable(surveillance_control_freedom, "Surveillance Infrastructure as Control Mechanism").
narrative_ontology:topic_domain(surveillance_control_freedom, "theological_ethics/technology_governance/political_economy").

domain_priors:requires_active_enforcement(surveillance_control_freedom).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(surveillance_control_freedom, '9281a43a-337e-4a3e-af82-cac2c7645730').
narrative_ontology:cs_kernel_codification('9281a43a-337e-4a3e-af82-cac2c7645730', formalized).
narrative_ontology:cs_authority_grounding('9281a43a-337e-4a3e-af82-cac2c7645730', lineage).
narrative_ontology:cs_interpretation_layer_present('9281a43a-337e-4a3e-af82-cac2c7645730').
narrative_ontology:cs_reading_relation('9281a43a-337e-4a3e-af82-cac2c7645730', surveillance_control_freedom__secular_humanist_reading, coexists_with).
narrative_ontology:cs_reading_relation('9281a43a-337e-4a3e-af82-cac2c7645730', surveillance_control_freedom__techno_optimist_reading, coexists_with).
narrative_ontology:cs_reading_relation('9281a43a-337e-4a3e-af82-cac2c7645730', surveillance_control_freedom__pluralist_pragmatic_reading, influences).
narrative_ontology:cs_axiom('9281a43a-337e-4a3e-af82-cac2c7645730', foundational, interior_freedom_constitutive_of_dignity).
narrative_ontology:cs_axiom_status(interior_freedom_constitutive_of_dignity, holdable).
narrative_ontology:cs_axiom_grounding('9281a43a-337e-4a3e-af82-cac2c7645730', interior_freedom_constitutive_of_dignity, deontological).
narrative_ontology:cs_axiom('9281a43a-337e-4a3e-af82-cac2c7645730', foundational, surveillance_violates_imago_dei).
narrative_ontology:cs_axiom_status(surveillance_violates_imago_dei, holdable).
narrative_ontology:cs_axiom_grounding('9281a43a-337e-4a3e-af82-cac2c7645730', surveillance_violates_imago_dei, theological).
narrative_ontology:cs_axiom('9281a43a-337e-4a3e-af82-cac2c7645730', secondary, subsidiarity_requires_privacy).
narrative_ontology:cs_axiom_status(subsidiarity_requires_privacy, holdable).
narrative_ontology:cs_axiom_grounding('9281a43a-337e-4a3e-af82-cac2c7645730', subsidiarity_requires_privacy, deontological).
narrative_ontology:cs_reference_frame('9281a43a-337e-4a3e-af82-cac2c7645730', pre_digital_privacy_norms).
narrative_ontology:cs_drift_state('9281a43a-337e-4a3e-af82-cac2c7645730', contemporary_surveillance_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('9281a43a-337e-4a3e-af82-cac2c7645730', '2026-06-08T14:32:00Z').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(surveillance_control_freedom, surveillance_states).
narrative_ontology:constraint_beneficiary(surveillance_control_freedom, platform_corporations).
narrative_ontology:constraint_beneficiary(surveillance_control_freedom, data_brokers).
narrative_ontology:constraint_beneficiary(surveillance_control_freedom, authoritarian_regimes).
narrative_ontology:constraint_victim(surveillance_control_freedom, dissidents).
narrative_ontology:constraint_victim(surveillance_control_freedom, marginalized_communities).
narrative_ontology:constraint_victim(surveillance_control_freedom, privacy_seeking_individuals).
narrative_ontology:constraint_victim(surveillance_control_freedom, political_minorities).
narrative_ontology:constraint_victim(surveillance_control_freedom, journalists).
narrative_ontology:constraint_victim(surveillance_control_freedom, activists).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISSIDENT (SNARE) — Trapped by pervasive surveillance infrastructure with no viable exit. Every digital action creates a profile; opting out means exclusion from employment, banking, healthcare, social participation. Self-censorship becomes survival strategy. Maximum extraction: the constraint extracts both privacy and interior freedom.
constraint_indexing:constraint_classification(surveillance_control_freedom, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PRIVACY-SEEKING CITIZEN (SNARE) — Constrained by infrastructure lock-in. Can take some protective measures (VPNs, encrypted messaging) but cannot exit the surveillance economy without severe social and economic costs. Experiences the constraint as extraction: must trade privacy for access to essential services.
constraint_indexing:constraint_classification(surveillance_control_freedom, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: GDPR-PROTECTED CITIZEN (TANGLED ROPE) — Benefits from legal protections (data portability, right to erasure, consent requirements) that create genuine coordination around privacy norms. But still bears extraction: compliance theater is widespread, enforcement is uneven, and cross-border data flows undermine protections. Mixed experience: some coordination function, substantial extraction.
constraint_indexing:constraint_classification(surveillance_control_freedom, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: PLATFORM CORPORATION (ROPE) — Primary beneficiary. Surveillance infrastructure enables targeted advertising, behavioral prediction, and market dominance. Experiences the constraint as coordination: data collection 'improves user experience' and 'enables free services.' Can arbitrage across jurisdictions to minimize regulatory burden. Net beneficiary with exit options.
constraint_indexing:constraint_classification(surveillance_control_freedom, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: SURVEILLANCE STATE (ROPE) — Beneficiary of control infrastructure. Mass surveillance enables predictive policing, social credit systems, dissent suppression, and population management. Experiences the constraint as coordination: 'public safety requires visibility.' Can define the rules and has full exit capacity (can exempt itself). Net beneficiary.
constraint_indexing:constraint_classification(surveillance_control_freedom, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: PRIVACY RIGHTS COALITION (SCAFFOLD) — Organized agents (EFF, digital rights NGOs, privacy-focused legislators) see the surveillance infrastructure as a temporary problem being solved through legal frameworks (GDPR, CCPA), encryption standards, and decentralized alternatives. Sunset logic: as privacy-preserving technologies mature and legal protections spread, the extraction mechanism loses force. Estimated sunset: 15-25 years for global norm shift.
constraint_indexing:constraint_classification(surveillance_control_freedom, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: PRIVACY POLICY REGIME (PITON) — The notice-and-consent framework (privacy policies, cookie banners, terms of service) is largely theatrical. Users cannot meaningfully consent to incomprehensible terms, cannot negotiate, and face take-it-or-leave-it choices. The ritual persists through legal inertia despite providing minimal actual privacy protection. Piton classification derives from theater gate: the consent mechanism is performative, not functional.
constraint_indexing:constraint_classification(surveillance_control_freedom, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, surveillance infrastructure exhibits both coordination and extraction. Coordination function: some data collection genuinely improves services, enables fraud prevention, and supports public health. Extraction function: the same infrastructure enables behavioral control, chilling effects, and asymmetric power concentration. The constraint is structurally tangled: both functions are real and inseparable in current implementations.
constraint_indexing:constraint_classification(surveillance_control_freedom, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(surveillance_control_freedom_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(surveillance_control_freedom, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(surveillance_control_freedom, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(surveillance_control_freedom, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(surveillance_control_freedom, TR),
    TR >= 0.70.

:- end_tests(surveillance_control_freedom_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Surveillance infrastructure extracts privacy, autonomy, and interior freedom from individuals while concentrating power and profit in states and corporations. The extraction is not total (some protective measures exist, some jurisdictions have stronger protections) but is substantial and rising. The value reflects that most individuals cannot exit the surveillance economy without severe social and economic costs, and that chilling effects are widespread. Suppression (0.75): High. Alternatives are systematically suppressed through network effects, infrastructure lock-in, legal barriers to encryption, and economic penalties for opting out. Dissidents face direct repression; ordinary citizens face exclusion from essential services. The suppression is not absolute (some privacy-preserving tools exist, some jurisdictions protect encryption) but is severe and rising. Theater ratio (0.58): Moderate-high. The notice-and-consent framework (privacy policies, cookie banners, terms of service) is substantially theatrical. Users cannot meaningfully consent to incomprehensible terms, cannot negotiate, and face take-it-or-leave-it choices. Some genuine privacy protections exist (GDPR data portability, right to erasure) but enforcement is uneven and compliance is often performative. The theater has increased over the interval as surveillance has become more pervasive while the consent ritual has remained unchanged.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a stark perspectival gap between beneficiaries and victims. Platform corporations and surveillance states see coordination (Rope): data collection improves services, enables public safety, and supports market efficiency. They are net beneficiaries with exit options. Dissidents and privacy-seeking citizens see extraction (Snare): surveillance infrastructure undermines interior freedom, enables repression, and forces conformity. They are trapped or constrained with no viable exit. The GDPR-protected citizen sees mixed coordination and extraction (Tangled Rope): legal protections create genuine coordination around privacy norms, but compliance theater and uneven enforcement mean substantial extraction persists. The privacy rights coalition sees a temporary problem with a sunset (Scaffold): legal frameworks and technical alternatives are maturing, and the extraction mechanism will lose force as privacy-preserving technologies spread. The privacy policy regime sees its own degraded ritual (Piton): notice-and-consent persists through legal inertia despite providing minimal protection. The analytical observer sees structural entanglement (Tangled Rope): coordination and extraction are inseparable in current implementations — the same infrastructure that enables service improvement also enables behavioral control.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (surveillance states, platform corporations, data brokers, authoritarian regimes) experience low directionality — the constraint subsidizes them through data access, behavioral control, and market power. They have arbitrage-level exit options (can define rules, exempt themselves, or relocate to favorable jurisdictions). Victims (dissidents, marginalized communities, privacy-seeking individuals, political minorities, journalists, activists) experience high directionality — the constraint extracts from them through loss of privacy, chilling effects, and exclusion from services. Powerless victims with trapped exit options experience maximum extraction. Moderate victims with constrained exit options experience substantial extraction but retain some agency. The GDPR-protected citizen occupies a mixed position: benefits from legal coordination (data rights, consent requirements) but still bears extraction (compliance theater, uneven enforcement, cross-border data flows). The analytical observer sees the constraint as structurally tangled: coordination and extraction functions are inseparable in current surveillance infrastructure implementations.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint has not resolved mandatrophy because the founding mandate (public safety, service improvement, fraud prevention) remains live even as the extraction mechanism has intensified. Surveillance infrastructure was initially justified as targeted monitoring of specific threats; it has evolved into pervasive profiling that shapes behavior and undermines autonomy. The mandate has expanded rather than dissolved: states and corporations now claim that mass surveillance is necessary for public safety, national security, and service quality. The constraint exhibits mandatrophy dynamics (theater ratio rising, extraction accumulating) but the mandate itself has not been abandoned — it has been stretched to cover ever-broader surveillance practices. The privacy rights coalition's scaffold perspective represents an attempt to resolve mandatrophy through sunset logic: as privacy-preserving alternatives mature, the extraction mechanism should lose force. But this resolution is contested and uncertain (see omega variables on encryption backdoors and decentralized alternative viability).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    encryption_backdoor_resolution,
    'Do mandatory encryption backdoors (for law enforcement access) structurally prevent privacy-preserving alternatives from maturing, or are they a temporary political compromise that technical innovation will route around?',
    'Longitudinal analysis of encryption adoption rates in jurisdictions with and without backdoor mandates; assessment of whether backdoor requirements actually prevent strong encryption deployment or merely shift it to non-compliant jurisdictions and open-source tools.',
    'If backdoors structurally prevent privacy tech: scaffold perspective is aspirational, not structural — no sunset exists. If backdoors are routable: scaffold perspective is confirmed — decentralized alternatives can mature despite state opposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(encryption_backdoor_resolution, empirical, 'Whether encryption backdoors structurally prevent privacy-preserving alternatives').

omega_variable(
    behavioral_modification_threshold,
    'At what level of surveillance visibility does self-censorship become universal rather than selective? Is there a threshold beyond which interior freedom collapses entirely, or does some residual private sphere always persist?',
    'Comparative analysis of self-reported self-censorship rates across surveillance regimes of varying intensity; psychological studies of internalized surveillance effects; historical analysis of totalitarian surveillance states.',
    'If threshold exists and is crossable: surveillance can achieve total behavioral control (maximum extraction). If residual sphere persists: extraction is bounded — some interior freedom remains even under pervasive surveillance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(behavioral_modification_threshold, empirical, 'Whether total behavioral control via surveillance is achievable').

omega_variable(
    gdpr_enforcement_effectiveness,
    'Does GDPR-style regulation actually constrain surveillance capitalism, or does it merely add compliance theater while leaving the extraction mechanism intact?',
    'Measurement of actual data collection and profiling practices in GDPR jurisdictions vs. non-GDPR jurisdictions; analysis of enforcement actions and their impact on corporate behavior; assessment of whether consent mechanisms are meaningful or theatrical.',
    'If GDPR is effective: tangled_rope classification for protected citizens is correct — genuine coordination function exists. If GDPR is theater: classification should be snare — the legal framework provides cover without constraining extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gdpr_enforcement_effectiveness, empirical, 'Whether GDPR-style regulation constrains extraction or adds theater').

omega_variable(
    decentralized_alternative_viability,
    'Can decentralized, privacy-preserving alternatives (federated social networks, zero-knowledge protocols, local-first software) achieve sufficient network effects to compete with surveillance-based platforms, or do they face insurmountable coordination problems?',
    'Adoption trajectory analysis of privacy-preserving alternatives; assessment of whether network effects and switching costs create permanent lock-in to surveillance platforms; evaluation of whether interoperability mandates (DMA-style) can lower switching costs.',
    'If alternatives can scale: scaffold sunset is real — exit paths exist. If coordination problems are insurmountable: powerless and moderate agents remain trapped — no structural exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decentralized_alternative_viability, empirical, 'Whether privacy-preserving alternatives can overcome network effects').

omega_variable(
    cs_framing_ambiguity,
    'Is the kernel ''human dignity as imago Dei'' the stabilized commitment, or is it the broader framework of Catholic Social Doctrine principles (common good, subsidiarity, solidarity)? The Magisterial reading treats imago Dei as the ontological foundation, but the encyclical''s governance prescriptions derive from CST principles that are one interpretive layer above the theological anthropology.',
    'Textual analysis of Antiqua et Nova and prior encyclicals to determine whether the kernel is the theological claim (imago Dei) or the derived principles (CST framework). If the kernel is imago Dei, then CST principles are the interpretation layer. If the kernel is CST principles, then imago Dei is the authority grounding, not the kernel itself.',
    'If kernel is imago Dei: cs_structure.kernel_codification should be ''fixed_text'' (Scripture and Tradition), and CST principles are the interpretation layer. If kernel is CST principles: kernel_codification should be ''formalized'' (the principles are codified in encyclicals), and imago Dei is the authority_grounding (''lineage'' — continuity with revealed truth).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_ambiguity, conceptual, 'Whether the kernel is the theological anthropology or the derived CST principles').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(surveillance_control_freedom, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(surv_tr_t0, surveillance_control_freedom, theater_ratio, 0, 0.3).
narrative_ontology:measurement(surv_tr_t5, surveillance_control_freedom, theater_ratio, 5, 0.38).
narrative_ontology:measurement(surv_tr_t10, surveillance_control_freedom, theater_ratio, 10, 0.48).
narrative_ontology:measurement(surv_tr_t15, surveillance_control_freedom, theater_ratio, 15, 0.55).
narrative_ontology:measurement(surv_tr_t20, surveillance_control_freedom, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(surv_be_t0, surveillance_control_freedom, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(surv_be_t5, surveillance_control_freedom, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(surv_be_t10, surveillance_control_freedom, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(surv_be_t15, surveillance_control_freedom, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(surv_be_t20, surveillance_control_freedom, base_extractiveness, 20, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(surv_su_t0, surveillance_control_freedom, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(surv_su_t5, surveillance_control_freedom, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(surv_su_t10, surveillance_control_freedom, suppression_requirement, 10, 0.63).
narrative_ontology:measurement(surv_su_t15, surveillance_control_freedom, suppression_requirement, 15, 0.72).
narrative_ontology:measurement(surv_su_t20, surveillance_control_freedom, suppression_requirement, 20, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(surveillance_control_freedom, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is downstream of digital_power_concentration. The upstream constraint describes the concentration of digital infrastructure and market power in a small number of platform corporations and state actors. This constraint describes one specific mechanism through which that concentrated power is exercised: surveillance infrastructure that enables behavioral control and undermines interior freedom. The two constraints have different ε values (digital_power_concentration is the structural precondition; surveillance_control_freedom is the extraction mechanism) and should be modeled as separate stories linked by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
